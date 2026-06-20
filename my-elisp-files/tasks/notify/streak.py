#!/usr/bin/env python3
"""Inbox-zero streak counter.

Runs nightly via launchd (see `com.kaan.tasks-streak.plist'). When the
inbox is empty at the time of the check, the consecutive-zero-days
counter is advanced. Emacs also calls into the same JSON state file
from `my/tasks-show-inbox' so a same-day inbox-zero moment counts even
if the Mac is asleep at 23:55.

State file `~/.tasks-streak.json' (override via $TASKS_STREAK_STATE):

    {
      "current":         5,
      "longest":         12,
      "last_zero_date":  "2026-06-20"
    }
"""

import json
import os
import sys
from datetime import date, timedelta
from pathlib import Path

sys.path.insert(0, str(Path(__file__).resolve().parent.parent / "lib"))
import tasks_lib as lib  # noqa: E402


STATE_FILE = Path(os.environ.get(
    "TASKS_STREAK_STATE",
    str(Path.home() / ".tasks-streak.json"),
))


def load_state():
    if STATE_FILE.exists():
        try:
            return json.loads(STATE_FILE.read_text(encoding="utf-8"))
        except json.JSONDecodeError:
            pass
    return {"current": 0, "longest": 0, "last_zero_date": ""}


def save_state(state):
    STATE_FILE.parent.mkdir(parents=True, exist_ok=True)
    STATE_FILE.write_text(
        json.dumps(state, indent=2, ensure_ascii=False), encoding="utf-8")


def update_streak(state, today, yesterday, inbox_count):
    """Pure helper: mutate STATE in place based on inputs.

    - Inbox not zero → do nothing (streak is broken at read-time).
    - Inbox zero, same day already counted → do nothing.
    - Inbox zero, last zero was yesterday → continue streak.
    - Inbox zero, gap or first time ever → start fresh at 1.

    Always tracks the all-time longest streak.
    """
    if inbox_count > 0:
        return state
    if state.get("last_zero_date") == today:
        return state
    if state.get("last_zero_date") == yesterday:
        state["current"] = state.get("current", 0) + 1
    else:
        state["current"] = 1
    state["last_zero_date"] = today
    state["longest"] = max(state.get("longest", 0), state["current"])
    return state


def inbox_count():
    return sum(1 for t in lib.list_active_tasks()
               if t.get("status") == "inbox")


def main():
    today = date.today().isoformat()
    yesterday = (date.today() - timedelta(days=1)).isoformat()
    state = load_state()
    before = dict(state)
    update_streak(state, today, yesterday, inbox_count())
    if state != before:
        save_state(state)
        print(f"[{today}] inbox-zero streak: {state['current']} "
              f"(longest: {state['longest']})")


if __name__ == "__main__":
    main()
