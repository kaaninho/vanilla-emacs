// Tasks web frontend. Talks to the Python server in this directory.

interface Task {
  file: string;
  title: string;
  status?: string;
  due?: string;
  scheduled?: string;
  reminder?: string;
  project?: string;
  created?: string;
  "archived-at"?: string;
  "mu4e-msgid"?: string;
  archived?: boolean;
  tags?: string[];
  contexts?: string[];
}

type Status = "inbox" | "today" | "next" | "waiting" | "someday";
type DateState = "overdue" | "today" | null;

interface EditPayload {
  file: string;
  title: string;
  status: string;
  due: string;
  scheduled: string;
  reminder: string;
  project: string;
  contexts: string[];
}

const STATUSES: readonly Status[] = ["inbox", "today", "next", "waiting", "someday"];
const LABELS: Record<Status, string> = {
  inbox: "Inbox",
  today: "Today",
  next: "Next",
  waiting: "Waiting",
  someday: "Someday",
};

// --- DOM ---

const $ = <T extends HTMLElement = HTMLElement>(id: string): T =>
  document.getElementById(id) as T;

const board = $("board");
const captureInput = $<HTMLInputElement>("capture-input");
const searchInput = $<HTMLInputElement>("search-input");
const contextFilterSelect = $<HTMLSelectElement>("context-filter");
const refreshBtn = $<HTMLButtonElement>("refresh-btn");
const archiveBtn = $<HTMLButtonElement>("toggle-archive-btn");
const toast = $("toast");
const modal = $("modal");
const editForm = $<HTMLFormElement>("edit-form");
const modalCancelBtn = $<HTMLButtonElement>("modal-cancel-btn");
const modalArchiveBtn = $<HTMLButtonElement>("modal-archive-btn");
const contextsCheckboxes = $("contexts-checkboxes");

let showingArchive = false;
let allTasks: Task[] = [];
let searchTerm = "";
let editingFile: string | null = null;
let toastTimer: number | undefined;
let availableContexts: string[] = [];
let contextFilter = "";

// --- API ---

async function api<T = unknown>(
  path: string,
  opts: RequestInit = {},
): Promise<T> {
  const r = await fetch(path, {
    headers: { "Content-Type": "application/json" },
    ...opts,
  });
  if (!r.ok) {
    let msg = `${r.status}`;
    try {
      const body = (await r.json()) as { error?: string };
      if (body.error) msg = body.error;
    } catch {
      /* not JSON */
    }
    throw new Error(msg);
  }
  return (await r.json()) as T;
}

const postJSON = <T = unknown>(path: string, body: unknown): Promise<T> =>
  api<T>(path, { method: "POST", body: JSON.stringify(body) });

// --- Helpers ---

function todayString(): string {
  const d = new Date();
  return `${d.getFullYear()}-${String(d.getMonth() + 1).padStart(2, "0")}-${String(d.getDate()).padStart(2, "0")}`;
}

function dateState(date: string | undefined | null): DateState {
  if (!date) return null;
  const t = todayString();
  const d = date.slice(0, 10);
  if (d < t) return "overdue";
  if (d === t) return "today";
  return null;
}

function showToast(msg: string, isError = false): void {
  toast.textContent = msg;
  toast.classList.toggle("error", isError);
  toast.classList.add("show");
  if (toastTimer !== undefined) clearTimeout(toastTimer);
  toastTimer = window.setTimeout(
    () => toast.classList.remove("show"),
    2500,
  );
}

function matchesSearch(task: Task): boolean {
  if (!searchTerm) return true;
  const ctxStr = Array.isArray(task.contexts) ? task.contexts.join(" ") : "";
  const hay = [task.title, task.project, task.due, task.scheduled, ctxStr]
    .filter(Boolean)
    .join(" ")
    .toLowerCase();
  return hay.includes(searchTerm);
}

function matchesContextFilter(task: Task): boolean {
  if (!contextFilter) return true;
  return Array.isArray(task.contexts) && task.contexts.includes(contextFilter);
}

function cardVisible(task: Task): boolean {
  return matchesSearch(task) && matchesContextFilter(task);
}

// --- Rendering ---

function makeBtn(
  label: string,
  title: string,
  handler: () => void,
): HTMLButtonElement {
  const b = document.createElement("button");
  b.textContent = label;
  b.title = title;
  b.addEventListener("click", (e) => {
    e.stopPropagation();
    handler();
  });
  return b;
}

function renderCard(task: Task): HTMLDivElement {
  const card = document.createElement("div");
  card.className = "card";
  card.draggable = true;
  card.dataset.file = task.file;
  if (task.status) card.dataset.status = task.status;

  const title = document.createElement("div");
  title.className = "card-title";
  title.textContent = task.title;
  card.appendChild(title);

  const meta = document.createElement("div");
  meta.className = "card-meta";

  const addDate = (
    icon: string,
    value: string | undefined,
    useState = true,
  ): void => {
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
  if (Array.isArray(task.contexts)) {
    for (const c of task.contexts) {
      const el = document.createElement("span");
      el.className = "context";
      el.textContent = c;
      meta.appendChild(el);
    }
  }
  if (showingArchive && task["archived-at"]) {
    const el = document.createElement("span");
    el.className = "date";
    el.textContent = `📦 ${task["archived-at"]}`;
    meta.appendChild(el);
  }
  if (meta.children.length) card.appendChild(meta);

  const actions = document.createElement("div");
  actions.className = "card-actions";
  if (showingArchive) {
    actions.appendChild(
      makeBtn("↩", "Un-archive (zurück nach Inbox)", () =>
        doAction(() =>
          postJSON("/api/unarchive", { file: task.file, status: "inbox" }),
        ),
      ),
    );
  } else {
    actions.appendChild(
      makeBtn("✓", "Archivieren", () =>
        doAction(() => postJSON("/api/archive", { file: task.file })),
      ),
    );
  }
  card.appendChild(actions);

  if (!showingArchive) {
    card.addEventListener("dragstart", (e) => {
      card.classList.add("dragging");
      if (e.dataTransfer) {
        e.dataTransfer.setData("text/plain", task.file);
        e.dataTransfer.effectAllowed = "move";
      }
    });
    card.addEventListener("dragend", () => card.classList.remove("dragging"));
  } else {
    card.draggable = false;
  }

  card.addEventListener("click", () => openEditModal(task));

  if (!cardVisible(task)) card.classList.add("hidden");
  return card;
}

function emptyEl(text: string): HTMLDivElement {
  const e = document.createElement("div");
  e.className = "empty";
  e.textContent = text;
  return e;
}

function makeColumn(
  status: string,
  label: string,
  visibleCount: number,
): HTMLDivElement {
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

function attachDropHandlers(col: HTMLDivElement, status: Status): void {
  col.addEventListener("dragover", (e) => {
    e.preventDefault();
    if (e.dataTransfer) e.dataTransfer.dropEffect = "move";
    col.classList.add("drag-over");
  });
  col.addEventListener("dragleave", (e) => {
    if (e.target === col) col.classList.remove("drag-over");
  });
  col.addEventListener("drop", async (e) => {
    e.preventDefault();
    col.classList.remove("drag-over");
    const file = e.dataTransfer?.getData("text/plain");
    if (!file) return;
    const draggedTask = allTasks.find((t) => t.file === file);
    if (!draggedTask || draggedTask.status === status) return;
    await doAction(
      () => postJSON("/api/status", { file, status }),
      `→ ${LABELS[status]}`,
    );
  });
}

function renderBoard(): void {
  board.innerHTML = "";
  board.classList.toggle("archive", showingArchive);

  if (showingArchive) {
    const col = makeColumn("archive", "Archiv", allTasks.length);
    const cardsEl = col.querySelector(".cards") as HTMLDivElement;
    if (allTasks.length === 0) {
      cardsEl.appendChild(emptyEl("Archiv ist leer."));
    } else {
      allTasks.forEach((t) => cardsEl.appendChild(renderCard(t)));
    }
    board.appendChild(col);
    return;
  }

  for (const status of STATUSES) {
    const tasks = allTasks.filter((t) => t.status === status);
    const col = makeColumn(status, LABELS[status], tasks.length);
    const cardsEl = col.querySelector(".cards") as HTMLDivElement;
    if (tasks.length === 0) {
      cardsEl.appendChild(emptyEl("—"));
    } else {
      tasks.forEach((t) => cardsEl.appendChild(renderCard(t)));
    }
    attachDropHandlers(col, status);
    board.appendChild(col);
  }
}

// --- Actions ---

async function doAction(
  fn: () => Promise<unknown>,
  successMsg?: string,
): Promise<void> {
  try {
    await fn();
    if (successMsg) showToast(successMsg);
    await load();
  } catch (err) {
    showToast((err as Error).message, true);
  }
}

async function load(): Promise<void> {
  try {
    allTasks = await api<Task[]>(
      showingArchive ? "/api/tasks/archive" : "/api/tasks",
    );
    renderBoard();
  } catch (err) {
    showToast((err as Error).message, true);
  }
}

// --- Contexts ---

async function loadContexts(): Promise<void> {
  try {
    availableContexts = await api<string[]>("/api/contexts");
  } catch {
    availableContexts = [];
  }
  populateContextFilter();
  renderContextsCheckboxes([]);
}

function populateContextFilter(): void {
  const current = contextFilterSelect.value;
  contextFilterSelect.innerHTML =
    '<option value="">Alle Kontexte</option>' +
    availableContexts
      .map((c) => `<option value="${c}">${c}</option>`)
      .join("");
  contextFilterSelect.value = current;
}

function renderContextsCheckboxes(selected: readonly string[]): void {
  contextsCheckboxes.innerHTML = "";
  for (const ctx of availableContexts) {
    const label = document.createElement("label");
    const cb = document.createElement("input");
    cb.type = "checkbox";
    cb.name = "ctx";
    cb.value = ctx;
    cb.checked = selected.includes(ctx);
    label.appendChild(cb);
    label.appendChild(document.createTextNode(" " + ctx));
    contextsCheckboxes.appendChild(label);
  }
}

// --- Edit modal ---

function formField<T extends HTMLInputElement | HTMLSelectElement>(
  name: string,
): T {
  return editForm.elements.namedItem(name) as T;
}

function openEditModal(task: Task): void {
  editingFile = task.file;
  const titleEl = formField<HTMLInputElement>("title");
  titleEl.value = task.title || "";
  formField<HTMLSelectElement>("status").value = (
    STATUSES as readonly string[]
  ).includes(task.status ?? "")
    ? (task.status as string)
    : "inbox";
  formField<HTMLInputElement>("due").value = (task.due || "").slice(0, 10);
  formField<HTMLInputElement>("scheduled").value = (task.scheduled || "").slice(0, 10);
  // YAML stores "YYYY-MM-DD HH:MM"; <input type="datetime-local"> wants "T".
  formField<HTMLInputElement>("reminder").value = task.reminder
    ? task.reminder.replace(" ", "T").slice(0, 16)
    : "";
  formField<HTMLInputElement>("project").value = task.project || "";
  renderContextsCheckboxes(
    Array.isArray(task.contexts) ? task.contexts : [],
  );
  modal.classList.remove("hidden");
  modal.setAttribute("aria-hidden", "false");
  titleEl.focus();
  titleEl.select();
}

function closeEditModal(): void {
  editingFile = null;
  modal.classList.add("hidden");
  modal.setAttribute("aria-hidden", "true");
}

editForm.addEventListener("submit", async (e) => {
  e.preventDefault();
  if (!editingFile) return;
  const fd = new FormData(editForm);
  const rem = (fd.get("reminder") ?? "").toString();
  const checked = contextsCheckboxes.querySelectorAll<HTMLInputElement>(
    'input[name="ctx"]:checked',
  );
  const payload: EditPayload = {
    file: editingFile,
    title: (fd.get("title") ?? "").toString().trim(),
    status: (fd.get("status") ?? "").toString(),
    due: (fd.get("due") ?? "").toString(),
    scheduled: (fd.get("scheduled") ?? "").toString(),
    reminder: rem ? rem.replace("T", " ") : "",
    project: (fd.get("project") ?? "").toString().trim(),
    contexts: Array.from(checked).map((b) => b.value),
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

const modalBackdrop = modal.querySelector(".modal-backdrop") as HTMLElement;
modalBackdrop.addEventListener("click", closeEditModal);

// --- Events ---

captureInput.addEventListener("keydown", async (e) => {
  if (e.key === "Enter" && captureInput.value.trim()) {
    const title = captureInput.value.trim();
    captureInput.value = "";
    await doAction(() => postJSON("/api/capture", { title }), "Captured");
  }
});

function applyCardVisibility(): void {
  document.querySelectorAll<HTMLDivElement>(".card").forEach((c) => {
    const file = c.dataset.file;
    const t = allTasks.find((x) => x.file === file);
    c.classList.toggle("hidden", !t || !cardVisible(t));
  });
}

searchInput.addEventListener("input", () => {
  searchTerm = searchInput.value.trim().toLowerCase();
  applyCardVisibility();
});

contextFilterSelect.addEventListener("change", () => {
  contextFilter = contextFilterSelect.value;
  applyCardVisibility();
});

refreshBtn.addEventListener("click", () => {
  void load();
});

archiveBtn.addEventListener("click", () => {
  showingArchive = !showingArchive;
  archiveBtn.classList.toggle("active", showingArchive);
  archiveBtn.textContent = showingArchive ? "← Aktiv" : "📦 Archiv";
  searchInput.value = "";
  searchTerm = "";
  void load();
});

document.addEventListener("keydown", (e) => {
  if (e.key === "Escape" && !modal.classList.contains("hidden")) {
    closeEditModal();
    return;
  }
  const target = e.target as HTMLElement | null;
  if (target && (target.tagName === "INPUT" || target.tagName === "SELECT"))
    return;
  if (e.key === "/") {
    e.preventDefault();
    searchInput.focus();
  }
  if (e.key === "c") {
    e.preventDefault();
    captureInput.focus();
  }
  if (e.key === "r") {
    e.preventDefault();
    void load();
  }
});

setInterval(() => {
  if (!document.hidden) void load();
}, 30_000);

void (async () => {
  await loadContexts();
  await load();
})();
