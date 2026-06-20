# Tasks Web Frontend

Lokales Kanban-UI für das `tasks.el`-System. Liest und schreibt die
gleichen Markdown-Dateien wie Emacs.

## Komplettes Setup (Neu­installation)

Ein einziges Skript bringt den Web-Server UND den Notifier als
launchd-Agents hoch:

```sh
./setup.sh
```

Was es macht:
1. Checkt `python3` / `tsc`
2. (Re-)kompiliert `app.ts → app.js` falls `tsc` da ist
3. Läuft die Python-Tests als Sanity-Check
4. Entlädt etwaige alte tasks-Agents
5. Kopiert beide `.plist`-Dateien nach `~/Library/LaunchAgents/`
6. `launchctl load` beide

Danach ist `http://localhost:8765` immer erreichbar und Notifications
ticken alle 60 s — auch nach Reboot. Idempotent, also einfach
nochmal aufrufen wenn sich was an den Plists ändert.

## Manuell starten (Entwicklung)

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

## Notifications (macOS)

`notify.py` ist ein launchd-Script, das alle 60 s die aktiven Task-
Dateien scannt und macOS-Banner für die drei Datums-Felder feuert:

- `reminder: 2026-06-20 09:00` — One-shot, feuert sobald `now ≥ Zeit`
- `scheduled: 2026-06-20`     — Einmal pro Tag ab dem Datum, ab
  `TASKS_NOTIFY_MORNING_HOUR` (default 09:00)
- `due: 2026-06-20`           — Genauso. Überfällige Tasks feuern an
  jedem neuen Tag erneut (genau einmal), bis sie archiviert sind

Wenn dein Rechner um 09:00 schläft und du um 10:00 aufmachst,
fängt launchd das nachträglich ab — du bekommst die Notification.
Ein State-File (`~/.tasks-notify-state.json`) verhindert Doppel-
Notifications am selben Tag.

### Install

```sh
cp tasks-web/com.kaan.tasks-notify.plist ~/Library/LaunchAgents/
launchctl load ~/Library/LaunchAgents/com.kaan.tasks-notify.plist
```

Beim ersten Notification-Versuch fragt macOS nach der "Notify"-
Berechtigung — einmal erlauben.

### Uninstall

```sh
launchctl unload ~/Library/LaunchAgents/com.kaan.tasks-notify.plist
rm ~/Library/LaunchAgents/com.kaan.tasks-notify.plist
```

### Konfiguration (alle optional)

| Env-Var | Default | Effekt |
|---|---|---|
| `OBSIDIAN_DIR` | iCloud-Pfad | Wo `tasks/` liegt |
| `TASKS_NOTIFY_STATE` | `~/.tasks-notify-state.json` | State-Datei |
| `TASKS_NOTIFY_MORNING_HOUR` | `9` | Stunde für date-only Trigger |
| `TASKS_NOTIFY_SOUND` | `Glass` | macOS-Notification-Sound |

In der `.plist` einfach unter `EnvironmentVariables` setzen.

### Debug

Logs landen in `/tmp/tasks-notify.log`. Manuell auslösen:

```sh
python3 tasks-web/notify.py
```

## Frontend bauen

`app.js` wird aus `app.ts` (TypeScript) generiert. Falls du das
Frontend änderst:

```sh
tsc
```

Im selben Verzeichnis. Erzeugt das aktuelle `app.js` neben `app.ts`.
Der Server serviert immer `app.js`.

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
- `app.ts` — TypeScript-Quelle des Frontends, mit Typen für `Task`,
  Status etc. Kompiliert via `tsc` (TypeScript 5+) zu `app.js`.
- `index.html` / `style.css` / `app.js` — statisches Frontend.

Bindet nur an `127.0.0.1`, also nur lokal erreichbar.
