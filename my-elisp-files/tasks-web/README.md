# Tasks Web Frontend

Lokales Kanban-UI für das `tasks.el`-System. Liest und schreibt die
gleichen Markdown-Dateien wie Emacs.

## Start

```sh
python3 server.py
```

Dann im Browser: <http://localhost:8765>

Konfiguration via Env-Variablen:

- `OBSIDIAN_DIR` — Pfad zum Vault (Default: dein iCloud-Vault)
- `TASKS_PORT` — Port (Default: 8765)

## Features

- 5-Spalten-Kanban: Inbox / Today / Next / Waiting / Someday
- Drag&Drop zwischen Spalten → setzt `status:` im YAML-Frontmatter
- **Klick auf Karte → Edit-Modal** mit Titel, Status, Due, Scheduled,
  Reminder, Projekt. Mehrfach-Edit in einem Speichern; leere Felder
  entfernen die Property; Status-Wechsel aus dem Archiv heraus
  un-archiviert automatisch.
- Archive-View (📦-Button); un-archive per ↩-Button auf der Karte
- ✓-Button auf Karte → archivieren
- Quick-Capture oben (Enter)
- Live-Suche
- Datum-Farben: rot = überfällig, gelb = heute fällig
- Auto-Refresh alle 30s wenn Tab im Vordergrund
- Tastenkürzel:
  - `/` Suche fokussieren
  - `c` Capture fokussieren
  - `r` Reload
  - `Esc` Modal schließen

## Architektur

- `server.py` — Python-3-HTTP-Server (nur stdlib), serviert statische
  Dateien und exponiert eine kleine REST-API:
  - `GET /api/tasks`, `GET /api/tasks/archive`
  - `POST /api/capture` `{title}`
  - `POST /api/status` `{file, status}` (auto-archiviert bei `done`,
    un-archiviert wenn aus Archive heraus aufgerufen)
  - `POST /api/archive` `{file}`
  - `POST /api/unarchive` `{file, status}`
  - `POST /api/update` `{file, key, value}` (generisches
    Property-Update; `value: null` löscht das Feld)
  - `POST /api/edit` `{file, title?, status?, due?, scheduled?,
    reminder?, project?}` (atomarer Multi-Feld-Edit; nicht
    übergebene Keys bleiben unverändert, leere Strings entfernen
    YAML-Properties)
- `index.html` / `style.css` / `app.js` — statisches Frontend, kein
  Build-Step.

Bindet nur an `127.0.0.1`, also nur lokal erreichbar.
