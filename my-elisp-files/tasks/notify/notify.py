#!/usr/bin/env python3
"""Tasks notifier — fires macOS notifications for reminder / scheduled / due.

Designed to run every 60 s via launchd (see `com.kaan.tasks-notify.plist').

Fire rules:
- `reminder: 2026-06-20 09:00' → one-shot, fires once when now >= the moment.
- `scheduled: 2026-06-20'      → once per day from the target date onward,
                                  starting at $TASKS_NOTIFY_MORNING_HOUR (09 default).
- `due: 2026-06-20'            → same as scheduled.
- A date with a time component on `scheduled' / `due' behaves like a reminder.

State (`~/.tasks-notify-state.json' by default) records the last fire-token
per (file, field). Datetime fields store the value; date-only fields store
the day they fired, so an overdue task re-fires once each new day until it
leaves the active dir (archived or deleted).

Configuration via env:
  OBSIDIAN_DIR                       inherited from tasks_lib
  TASKS_NOTIFY_STATE                 state file path
  TASKS_NOTIFY_MORNING_HOUR          int hour, default 9
  TASKS_NOTIFY_SOUND                 macOS notification sound, default "Glass"
"""

import json
import os
import subprocess
import sys
from datetime import datetime
from pathlib import Path

sys.path.insert(0, str(Path(__file__).resolve().parent.parent / "lib"))
import tasks_lib as lib  # noqa: E402


STATE_FILE = Path(os.environ.get(
    "TASKS_NOTIFY_STATE",
    str(Path.home() / ".tasks-notify-state.json"),
))
MORNING_HOUR = int(os.environ.get("TASKS_NOTIFY_MORNING_HOUR", "9"))
SOUND = os.environ.get("TASKS_NOTIFY_SOUND", "Glass")

FIELD_LABEL = {"reminder": "⏰", "scheduled": "⏳", "due": "📅"}


def parse_dt(value):
    """Parse YYYY-MM-DD or YYYY-MM-DD HH:MM into a datetime. Return None on failure."""
    if not value:
        return None
    value = value.strip()
    for fmt in ("%Y-%m-%d %H:%M", "%Y-%m-%d"):
        try:
            return datetime.strptime(value, fmt)
        except ValueError:
            continue
    return None


def should_fire(field, value, now, last_fired):
    """Return the new fire-token to record if we should fire, else None.

    field       — 'reminder' | 'scheduled' | 'due'
    value       — raw YAML string ('2026-06-20' or '2026-06-20 09:00')
    now         — current datetime
    last_fired  — previously recorded token for (file, field), or None
    """
    dt = parse_dt(value)
    if not dt:
        return None
    has_time = bool(dt.hour or dt.minute)
    if has_time:
        # Datetime: one-shot fire at or after the moment.
        if now < dt:
            return None
        if last_fired == value:
            return None
        return value
    # Date-only.
    if dt.date() > now.date():
        return None
    if now.hour < MORNING_HOUR:
        return None
    today_str = now.strftime("%Y-%m-%d")
    if field == "reminder":
        # Date-only reminder: fire once total (any day on/after target).
        if last_fired:
            return None
        return today_str
    # scheduled / due: fire once per day from target date onward.
    if last_fired == today_str:
        return None
    return today_str


def load_state():
    if STATE_FILE.exists():
        try:
            return json.loads(STATE_FILE.read_text(encoding="utf-8"))
        except json.JSONDecodeError:
            return {}
    return {}


def save_state(state):
    STATE_FILE.parent.mkdir(parents=True, exist_ok=True)
    STATE_FILE.write_text(
        json.dumps(state, indent=2, ensure_ascii=False), encoding="utf-8")


def macos_notify(title, body):
    """Fire a macOS banner via osascript. Best-effort: failures are swallowed."""
    safe_title = (title or "").replace('"', "'")
    safe_body = (body or "").replace('"', "'")
    script = (
        f'display notification "{safe_body}" with title "{safe_title}"'
        f' sound name "{SOUND}"'
    )
    subprocess.run(["osascript", "-e", script], check=False)


def check_tasks(now=None, notify=macos_notify):
    """Iterate active tasks and fire notifications for matching fields.

    Returns a list of (file, field, value) entries that fired.
    """
    if now is None:
        now = datetime.now()
    state = load_state()
    fired = []
    tasks = lib.list_active_tasks()
    seen_files = set()
    for task in tasks:
        file = str((lib.TASKS_DIR / task["file"]).resolve())
        seen_files.add(file)
        file_state = state.setdefault(file, {})
        title = task.get("title", task.get("file", "Task"))
        for field in ("reminder", "scheduled", "due"):
            value = task.get(field)
            if not value:
                continue
            token = should_fire(field, value, now, file_state.get(field))
            if token is None:
                continue
            notify(f"Task: {title}", f"{FIELD_LABEL[field]} {value}")
            file_state[field] = token
            fired.append((file, field, value))
    # Prune state entries for files that are no longer active and gone from disk.
    for file in list(state.keys()):
        if file not in seen_files and not Path(file).exists():
            del state[file]
    save_state(state)
    return fired


def main():
    fired = check_tasks()
    if fired:
        for file, field, value in fired:
            print(f"[{datetime.now().isoformat(timespec='seconds')}] "
                  f"notified {field}={value} ({Path(file).name})")


if __name__ == "__main__":
    main()
