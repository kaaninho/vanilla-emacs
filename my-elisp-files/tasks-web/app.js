// Tasks web frontend. Talks to the Python server in this directory.

const STATUSES = ["inbox", "today", "next", "waiting", "someday"];
const LABELS = {
  inbox: "Inbox",
  today: "Today",
  next: "Next",
  waiting: "Waiting",
  someday: "Someday",
};

const board = document.getElementById("board");
const captureInput = document.getElementById("capture-input");
const searchInput = document.getElementById("search-input");
const refreshBtn = document.getElementById("refresh-btn");
const archiveBtn = document.getElementById("toggle-archive-btn");
const toast = document.getElementById("toast");
const modal = document.getElementById("modal");
const editForm = document.getElementById("edit-form");
const modalCancelBtn = document.getElementById("modal-cancel-btn");
const modalArchiveBtn = document.getElementById("modal-archive-btn");

let showingArchive = false;
let allTasks = [];
let searchTerm = "";
let editingFile = null;

// --- API ---

async function api(path, opts = {}) {
  const r = await fetch(path, {
    headers: { "Content-Type": "application/json" },
    ...opts,
  });
  if (!r.ok) {
    let msg = `${r.status}`;
    try { msg = (await r.json()).error || msg; } catch {}
    throw new Error(msg);
  }
  return r.json();
}

const postJSON = (path, body) =>
  api(path, { method: "POST", body: JSON.stringify(body) });

// --- Helpers ---

function todayString() {
  const d = new Date();
  return `${d.getFullYear()}-${String(d.getMonth()+1).padStart(2,'0')}-${String(d.getDate()).padStart(2,'0')}`;
}

function dateState(date) {
  if (!date) return null;
  const t = todayString();
  const d = date.slice(0, 10);
  if (d < t) return "overdue";
  if (d === t) return "today";
  return null;
}

function showToast(msg, isError = false) {
  toast.textContent = msg;
  toast.classList.toggle("error", isError);
  toast.classList.add("show");
  clearTimeout(showToast._t);
  showToast._t = setTimeout(() => toast.classList.remove("show"), 2500);
}

function matchesSearch(task) {
  if (!searchTerm) return true;
  const hay = [task.title, task.project, task.due, task.scheduled]
    .filter(Boolean).join(" ").toLowerCase();
  return hay.includes(searchTerm);
}

// --- Rendering ---

function renderCard(task) {
  const card = document.createElement("div");
  card.className = "card";
  card.draggable = true;
  card.dataset.file = task.file;
  card.dataset.status = task.status;

  const title = document.createElement("div");
  title.className = "card-title";
  title.textContent = task.title;
  card.appendChild(title);

  const meta = document.createElement("div");
  meta.className = "card-meta";

  const addDate = (icon, value, useState = true) => {
    if (!value) return;
    const el = document.createElement("span");
    el.className = "date";
    if (useState) {
      const st = dateState(value);
      if (st) el.classList.add(st);
    }
    el.textContent = `${icon} ${value}`;
    meta.appendChild(el);
  };
  addDate("⏳", task.scheduled);
  addDate("📅", task.due);
  addDate("⏰", task.reminder, false);
  if (task.project) {
    const el = document.createElement("span");
    el.className = "project";
    el.textContent = task.project;
    meta.appendChild(el);
  }
  if (showingArchive && task["archived-at"]) {
    const el = document.createElement("span");
    el.className = "date";
    el.textContent = `📦 ${task["archived-at"]}`;
    meta.appendChild(el);
  }
  if (meta.children.length) card.appendChild(meta);

  // Action buttons (top-right)
  const actions = document.createElement("div");
  actions.className = "card-actions";
  if (showingArchive) {
    actions.appendChild(makeBtn("↩", "Un-archive (zurück nach Inbox)",
      () => doAction(() => postJSON("/api/unarchive",
        { file: task.file, status: "inbox" }))));
  } else {
    actions.appendChild(makeBtn("✓", "Archivieren",
      () => doAction(() => postJSON("/api/archive", { file: task.file }))));
  }
  card.appendChild(actions);

  // Drag handlers (only when not in archive view).
  if (!showingArchive) {
    card.addEventListener("dragstart", e => {
      card.classList.add("dragging");
      e.dataTransfer.setData("text/plain", task.file);
      e.dataTransfer.effectAllowed = "move";
    });
    card.addEventListener("dragend", () => card.classList.remove("dragging"));
  } else {
    card.draggable = false;
  }

  // Click anywhere on the card body opens the edit modal. Buttons inside
  // .card-actions call stopPropagation, so they won't trigger this.
  card.addEventListener("click", () => openEditModal(task));

  if (!matchesSearch(task)) card.classList.add("hidden");
  return card;
}

function makeBtn(label, title, handler) {
  const b = document.createElement("button");
  b.textContent = label;
  b.title = title;
  b.addEventListener("click", e => { e.stopPropagation(); handler(); });
  return b;
}

function renderBoard() {
  board.innerHTML = "";
  board.classList.toggle("archive", showingArchive);

  if (showingArchive) {
    const col = makeColumn("archive", "Archiv", allTasks.length);
    const cardsEl = col.querySelector(".cards");
    if (allTasks.length === 0) {
      cardsEl.appendChild(emptyEl("Archiv ist leer."));
    } else {
      allTasks.forEach(t => cardsEl.appendChild(renderCard(t)));
    }
    board.appendChild(col);
    return;
  }

  for (const status of STATUSES) {
    const tasks = allTasks.filter(t => t.status === status);
    const col = makeColumn(status, LABELS[status], tasks.length);
    const cardsEl = col.querySelector(".cards");
    if (tasks.length === 0) {
      cardsEl.appendChild(emptyEl("—"));
    } else {
      tasks.forEach(t => cardsEl.appendChild(renderCard(t)));
    }
    attachDropHandlers(col, status);
    board.appendChild(col);
  }
}

function emptyEl(text) {
  const e = document.createElement("div");
  e.className = "empty";
  e.textContent = text;
  return e;
}

function makeColumn(status, label, visibleCount) {
  const col = document.createElement("div");
  col.className = "column";
  col.dataset.status = status;
  col.innerHTML = `
    <div class="column-header">
      <h2>${label}</h2>
      <span class="count">${visibleCount}</span>
    </div>
    <div class="cards"></div>
  `;
  return col;
}

function attachDropHandlers(col, status) {
  col.addEventListener("dragover", e => {
    e.preventDefault();
    e.dataTransfer.dropEffect = "move";
    col.classList.add("drag-over");
  });
  col.addEventListener("dragleave", e => {
    // Only un-highlight when leaving the column, not children
    if (e.target === col) col.classList.remove("drag-over");
  });
  col.addEventListener("drop", async e => {
    e.preventDefault();
    col.classList.remove("drag-over");
    const file = e.dataTransfer.getData("text/plain");
    if (!file) return;
    const draggedTask = allTasks.find(t => t.file === file);
    if (!draggedTask || draggedTask.status === status) return;
    await doAction(() => postJSON("/api/status",
      { file, status }), `→ ${LABELS[status]}`);
  });
}

// --- Actions ---

async function doAction(fn, successMsg) {
  try {
    await fn();
    if (successMsg) showToast(successMsg);
    await load();
  } catch (err) {
    showToast(err.message, true);
  }
}

async function load() {
  try {
    allTasks = await api(showingArchive
      ? "/api/tasks/archive" : "/api/tasks");
    renderBoard();
  } catch (err) {
    showToast(err.message, true);
  }
}

// --- Edit modal ---

function openEditModal(task) {
  editingFile = task.file;
  editForm.title.value = task.title || "";
  editForm.status.value =
    STATUSES.includes(task.status) ? task.status : "inbox";
  editForm.due.value = (task.due || "").slice(0, 10);
  editForm.scheduled.value = (task.scheduled || "").slice(0, 10);
  // YAML stores "YYYY-MM-DD HH:MM"; <input type="datetime-local"> wants "T".
  editForm.reminder.value = task.reminder
    ? task.reminder.replace(" ", "T").slice(0, 16) : "";
  editForm.project.value = task.project || "";
  modal.classList.remove("hidden");
  modal.setAttribute("aria-hidden", "false");
  editForm.title.focus();
  editForm.title.select();
}

function closeEditModal() {
  editingFile = null;
  modal.classList.add("hidden");
  modal.setAttribute("aria-hidden", "true");
}

editForm.addEventListener("submit", async e => {
  e.preventDefault();
  if (!editingFile) return;
  const fd = new FormData(editForm);
  const rem = (fd.get("reminder") || "").toString();
  const payload = {
    file: editingFile,
    title: (fd.get("title") || "").toString().trim(),
    status: (fd.get("status") || "").toString(),
    due: (fd.get("due") || "").toString(),
    scheduled: (fd.get("scheduled") || "").toString(),
    reminder: rem ? rem.replace("T", " ") : "",
    project: (fd.get("project") || "").toString().trim(),
  };
  if (!payload.title) {
    showToast("Titel darf nicht leer sein", true);
    return;
  }
  closeEditModal();
  await doAction(() => postJSON("/api/edit", payload), "Gespeichert");
});

modalCancelBtn.addEventListener("click", closeEditModal);

modalArchiveBtn.addEventListener("click", async () => {
  if (!editingFile) return;
  const file = editingFile;
  closeEditModal();
  await doAction(() => postJSON("/api/archive", { file }), "Archiviert");
});

modal.querySelector(".modal-backdrop").addEventListener("click", closeEditModal);

// --- Events ---

captureInput.addEventListener("keydown", async e => {
  if (e.key === "Enter" && captureInput.value.trim()) {
    const title = captureInput.value.trim();
    captureInput.value = "";
    await doAction(() => postJSON("/api/capture", { title }),
      "Captured");
  }
});

searchInput.addEventListener("input", () => {
  searchTerm = searchInput.value.trim().toLowerCase();
  // Update visibility without re-rendering
  document.querySelectorAll(".card").forEach(c => {
    const file = c.dataset.file;
    const t = allTasks.find(x => x.file === file);
    c.classList.toggle("hidden", !t || !matchesSearch(t));
  });
});

refreshBtn.addEventListener("click", load);

archiveBtn.addEventListener("click", () => {
  showingArchive = !showingArchive;
  archiveBtn.classList.toggle("active", showingArchive);
  archiveBtn.textContent = showingArchive ? "← Aktiv" : "📦 Archiv";
  searchInput.value = "";
  searchTerm = "";
  load();
});

// Keyboard shortcuts
document.addEventListener("keydown", e => {
  if (e.key === "Escape" && !modal.classList.contains("hidden")) {
    closeEditModal();
    return;
  }
  if (e.target.tagName === "INPUT" || e.target.tagName === "SELECT") return;
  if (e.key === "/") { e.preventDefault(); searchInput.focus(); }
  if (e.key === "c") { e.preventDefault(); captureInput.focus(); }
  if (e.key === "r") { e.preventDefault(); load(); }
});

// Auto-refresh every 30s when document is visible
setInterval(() => {
  if (!document.hidden) load();
}, 30000);

load();
