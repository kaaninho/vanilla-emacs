# Tasks — One-file-per-task GTD system

Ein persönliches GTD-Setup mit drei Frontends auf demselben
Daten-Modell: ein YAML-Frontmatter-Markdown-File pro Task im
Obsidian-Vault.

```
tasks/
├── README.md          ← du bist hier
├── setup.sh           ← einmal-installiert alles
├── lib/               ← shared Python: Parsing, YAML, Aktionen
│   ├── tasks_lib.py
│   └── test_tasks_lib.py
├── emacs/             ← Emacs-Frontend (primär)
│   ├── tasks.el
│   └── tasks-test.el
├── notify/            ← macOS-Notifications via launchd
│   ├── notify.py
│   ├── test_notify.py
│   └── com.kaan.tasks-notify.plist
└── tasks-web/         ← Browser-Kanban (Python-HTTP + TS)
    ├── server.py
    ├── app.ts  app.js  index.html  style.css
    ├── tsconfig.json
    └── com.kaan.tasks-web.plist
```

## Datenmodell

Jeder Task lebt in `$OBSIDIAN_DIR/tasks/<slug>.md` (aktiv) oder
`$OBSIDIAN_DIR/tasks/archive/YYYY-MM-DD-<slug>.md` (erledigt) mit
YAML-Frontmatter:

```yaml
---
status: next                      # inbox | next | today | waiting | someday | done
due: 2026-06-25
scheduled: 2026-06-24
reminder: 2026-06-24 09:00
project: "[[Projekt X]]"
contexts:
  - "@work"
  - "@computer"
created: 2026-06-20 14:30
mu4e-msgid: "abcd1234@host"
---

# Titel der Aufgabe (= nächste konkrete Action)

Optionaler Body mit Notizen, Links, Sub-Checkboxen.
```

Alle drei Frontends lesen / schreiben diese Files direkt — kein
Server-im-Weg, keine Datenbank.

## Komplettes Setup (Neuinstallation)

Ein Script bringt Web-Server und Notifier als launchd-Agents hoch:

```sh
./setup.sh
```

Was es macht:

1. Checkt `python3` / `tsc`
2. (Re-)kompiliert `app.ts → app.js`, falls `tsc` da ist
3. Läuft die Python-Tests in `lib/` und `notify/`
4. Entlädt etwaige bereits installierte tasks-Agents (idempotent)
5. Kopiert beide `.plist`-Dateien nach `~/Library/LaunchAgents/`
6. Lädt beide Agents

Danach ist `http://localhost:8765` immer erreichbar und Notifications
ticken alle 60 s — auch nach Reboot. Idempotent, einfach erneut
aufrufen wenn sich was an den Plists ändert.

## Emacs-Frontend

Lädt aus `emacs/tasks.el`. In deiner `init.el`:

```elisp
(add-to-list 'load-path "~/emacsdotfiles/my-elisp-files/tasks/emacs")
(require 'tasks)
```

`(require 'org)` und `obsidian-directory` müssen vorher gesetzt sein.

### Wichtige Keybindings

Globale Tasten (überall):

| Key | Funktion |
|---|---|
| `C-c t c` | Capture (öffnet Compose-Buffer) |
| `C-c t i` / `t` / `n` / `s` / `w` | View für Inbox / Today / Next / Someday / Waiting |
| `C-c t A` | Archive-View |
| `C-c t @` | Tasks zu einem Kontext anzeigen |
| `C-c t /` | Suche über alle Tasks (active + archive) |
| `C-c t I` | Inbox-Processing-Wizard |
| `C-c t x` / `o` / `p` / `k` / `d` / `S` / `r` / `u` | mark done / toggle today / set status / set contexts / set due / scheduled / reminder / unarchive |
| `C-c t m` | mu4e-Mail des Tasks öffnen |

In jeder View außerdem `RET TAB t d s r p k m x u f / g v i T A I ?`
— `?` zeigt die volle Liste. Per-Task-Tasten oben rechts in der
Header-Line dauerhaft eingeblendet.

### Inbox-Wizard

`M-x my/tasks-process-inbox` (oder `C-c t I`) läuft sequentiell durch
jeden Inbox-Eintrag: erst Title-Reformulierung (concrete next
action), dann Single-Key-Status — `t` today, `n` next, `w` waiting,
`s` someday, `x` done (<2min, archiviert), `T` trash (löschen), `+`
defer (+1d/+1w/+1mo/pick), `i` skip, `q` quit.

## tasks-web (Browser-Kanban)

Nach `./setup.sh`: <http://localhost:8765>.

Manuell starten (für Entwicklung):

```sh
cd tasks-web
python3 server.py
```

### Features

- 5-Spalten-Kanban: Inbox / Today / Next / Waiting / Someday
- HTML5 Drag&Drop zwischen Spalten → `status:` wird gesetzt
- Klick auf Karte → Edit-Modal (Titel, Status, Due, Scheduled,
  Reminder, Projekt, Kontexte als Checkboxes)
- Archive-View; un-archive (↩) und archive (✓) pro Karte
- Quick-Capture (Enter), Live-Suche, Kontext-Filter
- Datum-Farben: rot = überfällig, gelb = heute fällig
- Auto-Refresh alle 30 s, wenn Tab im Vordergrund
- Tastenkürzel im Browser: `/` Suche, `c` Capture, `r` Reload, `Esc`
  schließt Modal

### TypeScript bauen

`app.js` wird aus `app.ts` generiert. Nach Änderung:

```sh
cd tasks-web && tsc
```

Server serviert immer das kompilierte `app.js`. `./setup.sh` macht
das automatisch, falls `tsc` installiert ist.

### REST-API (intern)

`server.py` ist eine dünne HTTP-Schicht über `lib/tasks_lib.py`.
Bindet nur an `127.0.0.1`.

| Endpoint | Body | Effekt |
|---|---|---|
| `GET /api/tasks` | — | aktive Tasks |
| `GET /api/tasks/archive` | — | archivierte Tasks |
| `GET /api/contexts` | — | erlaubte Kontexte (fixe Liste) |
| `POST /api/capture` | `{title}` | neue Task |
| `POST /api/status` | `{file, status}` | auto archivert / un-archivert |
| `POST /api/archive` | `{file}` | archivieren |
| `POST /api/unarchive` | `{file, status}` | zurück nach active |
| `POST /api/update` | `{file, key, value}` | generisches Property-Update |
| `POST /api/edit` | `{file, title?, status?, due?, scheduled?, reminder?, project?, contexts?}` | Multi-Feld-Edit (atomar) |

## Notifications (macOS)

`notify/notify.py` läuft via launchd alle 60 s und feuert macOS-Banner
für die drei Datums-Felder:

- `reminder: 2026-06-20 09:00` — One-shot, sobald `now ≥ Zeit`
- `scheduled: 2026-06-20` — einmal pro Tag ab dem Datum, ab
  `TASKS_NOTIFY_MORNING_HOUR` (default 09:00)
- `due: 2026-06-20` — analog. Überfällige Tasks feuern an jedem
  neuen Tag erneut (genau einmal pro Tag), bis sie archiviert sind

Wenn dein Rechner um 09:00 schläft und du um 10:00 hochfährst, fängt
launchd das nach. State-File (`~/.tasks-notify-state.json`)
verhindert Doppel-Notifications.

### Konfiguration (alle optional)

| Env-Var | Default | Effekt |
|---|---|---|
| `OBSIDIAN_DIR` | iCloud-Pfad | wo `tasks/` liegt |
| `TASKS_NOTIFY_STATE` | `~/.tasks-notify-state.json` | State-File |
| `TASKS_NOTIFY_MORNING_HOUR` | `9` | Stunde für date-only Trigger |
| `TASKS_NOTIFY_SOUND` | `Glass` | macOS-Notification-Sound |

In der Plist unter `EnvironmentVariables` setzen.

### Debug

Logs: `/tmp/tasks-notify.log` und `/tmp/tasks-web.log`. Manuell:

```sh
cd notify && python3 notify.py
```

## Tests

```sh
# Python
( cd lib    && python3 -m unittest test_tasks_lib )
( cd notify && python3 -m unittest test_notify )

# Elisp
emacs --batch -L emacs -l tasks-test -f ert-run-tests-batch-and-exit
```

Aktueller Stand: **44 lib + 21 notify + 92 elisp = 157 Tests**.

## Architektur-Überblick

```
                 ┌──────────────┐
                 │ tasks_lib.py │   parse / write YAML, archive ops, …
                 └──┬───────┬───┘
            ┌───────┘       └───────┐
            ▼                       ▼
      ┌─────────────┐         ┌────────────┐
      │  server.py  │         │ notify.py  │
      │  (HTTP)     │         │ (launchd)  │
      └──────┬──────┘         └────────────┘
             │
             ▼
   index.html / style.css / app.js  ← TypeScript-kompiliert

   ┌────────────────────────────┐
   │  emacs/tasks.el            │  ← liest dieselben .md-Files
   │  (Emacs-Frontend, primär)  │     direkt; keine HTTP-Kopplung
   └────────────────────────────┘
```

Bindet nur an `127.0.0.1`, also nur lokal erreichbar.
