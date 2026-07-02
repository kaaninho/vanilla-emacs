"use strict";
// Tasks web frontend. Talks to the Python server in this directory.
const STATUSES = ["inbox", "today", "next", "waiting", "someday"];
const LABELS = {
    inbox: "Inbox",
    today: "Today",
    next: "Next",
    waiting: "Waiting",
    someday: "Someday",
};
// --- DOM ---
const $ = (id) => document.getElementById(id);
const board = $("board");
const captureInput = $("capture-input");
const searchInput = $("search-input");
const contextFilterSelect = $("context-filter");
const refreshBtn = $("refresh-btn");
const archiveBtn = $("toggle-archive-btn");
const toast = $("toast");
const modal = $("modal");
const editForm = $("edit-form");
const modalCancelBtn = $("modal-cancel-btn");
const modalArchiveBtn = $("modal-archive-btn");
const contextsCheckboxes = $("contexts-checkboxes");
const statsEl = $("stats");
let showingArchive = false;
let allTasks = [];
let searchTerm = "";
let editingFile = null;
let toastTimer;
let availableContexts = [];
let contextFilter = "";
// --- API ---
async function api(path, opts = {}) {
    const r = await fetch(path, {
        headers: { "Content-Type": "application/json" },
        ...opts,
    });
    if (!r.ok) {
        let msg = `${r.status}`;
        try {
            const body = (await r.json());
            if (body.error)
                msg = body.error;
        }
        catch {
            /* not JSON */
        }
        throw new Error(msg);
    }
    return (await r.json());
}
const postJSON = (path, body) => api(path, { method: "POST", body: JSON.stringify(body) });
// --- Helpers ---
function todayString() {
    const d = new Date();
    return `${d.getFullYear()}-${String(d.getMonth() + 1).padStart(2, "0")}-${String(d.getDate()).padStart(2, "0")}`;
}
function dateState(date) {
    if (!date)
        return null;
    const t = todayString();
    const d = date.slice(0, 10);
    if (d < t)
        return "overdue";
    if (d === t)
        return "today";
    return null;
}
function showToast(msg, isError = false) {
    toast.textContent = msg;
    toast.classList.toggle("error", isError);
    toast.classList.add("show");
    if (toastTimer !== undefined)
        clearTimeout(toastTimer);
    toastTimer = window.setTimeout(() => toast.classList.remove("show"), 2500);
}
function matchesSearch(task) {
    if (!searchTerm)
        return true;
    const ctxStr = Array.isArray(task.contexts) ? task.contexts.join(" ") : "";
    const hay = [task.title, task.project, task.due, task.scheduled, ctxStr]
        .filter(Boolean)
        .join(" ")
        .toLowerCase();
    return hay.includes(searchTerm);
}
function matchesContextFilter(task) {
    if (!contextFilter)
        return true;
    return Array.isArray(task.contexts) && task.contexts.includes(contextFilter);
}
function cardVisible(task) {
    return matchesSearch(task) && matchesContextFilter(task);
}
// --- Rendering ---
function makeBtn(label, title, handler) {
    const b = document.createElement("button");
    b.textContent = label;
    b.title = title;
    b.addEventListener("click", (e) => {
        e.stopPropagation();
        handler();
    });
    return b;
}
function renderCard(task) {
    const card = document.createElement("div");
    card.className = "card";
    card.draggable = true;
    card.dataset.file = task.file;
    if (task.status)
        card.dataset.status = task.status;
    const title = document.createElement("div");
    title.className = "card-title";
    title.textContent = task.title;
    card.appendChild(title);
    const meta = document.createElement("div");
    meta.className = "card-meta";
    const addDate = (icon, value, useState = true) => {
        if (!value)
            return;
        const el = document.createElement("span");
        el.className = "date";
        if (useState) {
            const st = dateState(value);
            if (st)
                el.classList.add(st);
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
    if (meta.children.length)
        card.appendChild(meta);
    const actions = document.createElement("div");
    actions.className = "card-actions";
    if (showingArchive) {
        actions.appendChild(makeBtn("↩", "Un-archive (zurück nach Inbox)", () => doAction(() => postJSON("/api/unarchive", { file: task.file, status: "inbox" }))));
    }
    else {
        actions.appendChild(makeBtn("✓", "Archivieren", () => doAction(() => postJSON("/api/archive", { file: task.file }))));
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
    }
    else {
        card.draggable = false;
    }
    card.addEventListener("click", () => openEditModal(task));
    if (!cardVisible(task))
        card.classList.add("hidden");
    return card;
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
    col.addEventListener("dragover", (e) => {
        e.preventDefault();
        if (e.dataTransfer)
            e.dataTransfer.dropEffect = "move";
        col.classList.add("drag-over");
    });
    col.addEventListener("dragleave", (e) => {
        if (e.target === col)
            col.classList.remove("drag-over");
    });
    col.addEventListener("drop", async (e) => {
        e.preventDefault();
        col.classList.remove("drag-over");
        const file = e.dataTransfer?.getData("text/plain");
        if (!file)
            return;
        const draggedTask = allTasks.find((t) => t.file === file);
        if (!draggedTask || draggedTask.status === status)
            return;
        await doAction(() => postJSON("/api/status", { file, status }), `→ ${LABELS[status]}`);
    });
}
function renderBoard() {
    board.innerHTML = "";
    board.classList.toggle("archive", showingArchive);
    if (showingArchive) {
        const col = makeColumn("archive", "Archiv", allTasks.length);
        const cardsEl = col.querySelector(".cards");
        if (allTasks.length === 0) {
            cardsEl.appendChild(emptyEl("Archiv ist leer."));
        }
        else {
            allTasks.forEach((t) => cardsEl.appendChild(renderCard(t)));
        }
        board.appendChild(col);
        return;
    }
    for (const status of STATUSES) {
        const tasks = allTasks.filter((t) => t.status === status);
        const col = makeColumn(status, LABELS[status], tasks.length);
        const cardsEl = col.querySelector(".cards");
        if (tasks.length === 0) {
            cardsEl.appendChild(emptyEl("—"));
        }
        else {
            tasks.forEach((t) => cardsEl.appendChild(renderCard(t)));
        }
        attachDropHandlers(col, status);
        board.appendChild(col);
    }
}
// --- Actions ---
async function doAction(fn, successMsg) {
    try {
        await fn();
        if (successMsg)
            showToast(successMsg);
        await load();
    }
    catch (err) {
        showToast(err.message, true);
    }
}
async function load() {
    try {
        allTasks = await api(showingArchive ? "/api/tasks/archive" : "/api/tasks");
        renderBoard();
        void loadStreak();
    }
    catch (err) {
        showToast(err.message, true);
    }
}
async function loadStreak() {
    let s;
    try {
        s = await api("/api/streak");
    }
    catch {
        statsEl.innerHTML = "";
        return;
    }
    const chips = [];
    if (s.current > 0) {
        const title = s.longest > 0 ? ` title="Longest: ${s.longest}d"` : "";
        chips.push(`<span class="stat streak"${title}>🔥 ${s.current}d streak</span>`);
    }
    if (s.done_today > 0) {
        chips.push(`<span class="stat done">✓ ${s.done_today} done today</span>`);
    }
    statsEl.innerHTML = chips.join("");
}
// --- Contexts ---
async function loadContexts() {
    try {
        availableContexts = await api("/api/contexts");
    }
    catch {
        availableContexts = [];
    }
    populateContextFilter();
    renderContextsCheckboxes([]);
}
function populateContextFilter() {
    const current = contextFilterSelect.value;
    contextFilterSelect.innerHTML =
        '<option value="">Alle Kontexte</option>' +
            availableContexts
                .map((c) => `<option value="${c}">${c}</option>`)
                .join("");
    contextFilterSelect.value = current;
}
function renderContextsCheckboxes(selected) {
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
function formField(name) {
    return editForm.elements.namedItem(name);
}
function openEditModal(task) {
    editingFile = task.file;
    const titleEl = formField("title");
    titleEl.value = task.title || "";
    formField("status").value = STATUSES.includes(task.status ?? "")
        ? task.status
        : "inbox";
    formField("due").value = (task.due || "").slice(0, 10);
    formField("scheduled").value = (task.scheduled || "").slice(0, 10);
    // YAML stores "YYYY-MM-DD HH:MM"; <input type="datetime-local"> wants "T".
    formField("reminder").value = task.reminder
        ? task.reminder.replace(" ", "T").slice(0, 16)
        : "";
    formField("project").value = task.project || "";
    renderContextsCheckboxes(Array.isArray(task.contexts) ? task.contexts : []);
    modal.classList.remove("hidden");
    modal.setAttribute("aria-hidden", "false");
    titleEl.focus();
    titleEl.select();
}
function closeEditModal() {
    editingFile = null;
    modal.classList.add("hidden");
    modal.setAttribute("aria-hidden", "true");
}
editForm.addEventListener("submit", async (e) => {
    e.preventDefault();
    if (!editingFile)
        return;
    const fd = new FormData(editForm);
    const rem = (fd.get("reminder") ?? "").toString();
    const checked = contextsCheckboxes.querySelectorAll('input[name="ctx"]:checked');
    const payload = {
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
    if (!editingFile)
        return;
    const file = editingFile;
    closeEditModal();
    await doAction(() => postJSON("/api/archive", { file }), "Archiviert");
});
const modalBackdrop = modal.querySelector(".modal-backdrop");
modalBackdrop.addEventListener("click", closeEditModal);
// --- Events ---
captureInput.addEventListener("keydown", async (e) => {
    if (e.key === "Enter" && captureInput.value.trim()) {
        const title = captureInput.value.trim();
        captureInput.value = "";
        await doAction(() => postJSON("/api/capture", { title }), "Captured");
    }
});
function applyCardVisibility() {
    document.querySelectorAll(".card").forEach((c) => {
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
    const target = e.target;
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
    if (!document.hidden)
        void load();
}, 30000);
void (async () => {
    await loadContexts();
    await load();
})();
