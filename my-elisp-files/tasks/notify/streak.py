#!/usr/bin/env python3
"""Inbox-zero streak counter.

Runs nightly via launchd (see `com.kaan.tasks-streak.plist'). When the
inbox is empty at the time of the check, the consecutive-zero-days
counter is advanced. Emacs also calls into the same JSON state file
from `my/tasks-show-inbox' so a same-day inbox-zero moment counts even
if the Mac is asleep at 23:55.

Only working days count (Mon-Fri by default, override via
$TASKS_WORKING_DAYS as ISO weekday numbers "1,2,3,4,5" where 1=Monday).
Non-working days (weekends) never break the streak and never advance it,
so a Friday inbox-zero still connects to the following Monday.

State file `~/.tasks-streak.json' (override via $TASKS_STREAK_STATE):

    {
      "current":         5,
      "longest":         12,
      "last_zero_date":  "2026-06-20",
      "prev_current":    0,             # streak value before the last reset
      "prev_zero_date":  ""             # last_zero_date before the last reset
    }

The two `prev_*' fields let Emacs' `my/tasks-streak-bridge' recover the
streak after an excused absence (vacation) even once a reset happened.
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


def working_days():
    """ISO weekday numbers (1=Mon … 7=Sun) that count as working days."""
    raw = os.environ.get("TASKS_WORKING_DAYS", "1,2,3,4,5")
    return {int(x) for x in raw.split(",") if x.strip()}


def is_working_day(d, working=None):
    return d.isoweekday() in (working if working is not None else working_days())


def prev_working_day(d, working=None):
    """Most recent working day strictly before D (skips weekends)."""
    working = working if working is not None else working_days()
    x = d - timedelta(days=1)
    while x.isoweekday() not in working:
        x -= timedelta(days=1)
    return x


def update_streak(state, today, inbox_count):
    """Pure helper: mutate STATE in place based on inputs. TODAY is a `date'.

    - Inbox not zero → do nothing (streak is broken at read-time).
    - Non-working day → do nothing (weekends neither count nor break).
    - Inbox zero, same day already counted → do nothing.
    - Inbox zero, last zero was the previous working day → continue streak.
    - Inbox zero, gap or first time ever → stash the old value into
      `prev_*' (so a later bridge can recover it) and start fresh at 1.

    Always tracks the all-time longest streak.
    """
    if inbox_count > 0:
        return state
    if not is_working_day(today):
        return state
    today_s = today.isoformat()
    if state.get("last_zero_date") == today_s:
        return state
    if state.get("last_zero_date") == prev_working_day(today).isoformat():
        state["current"] = state.get("current", 0) + 1
    else:
        state["prev_current"] = state.get("current", 0)
        state["prev_zero_date"] = state.get("last_zero_date", "")
        state["current"] = 1
    state["last_zero_date"] = today_s
    state["longest"] = max(state.get("longest", 0), state["current"])
    return state


def inbox_count():
    return sum(1 for t in lib.list_active_tasks()
               if t.get("status") == "inbox")


def main():
    today = date.today()
    state = load_state()
    before = dict(state)
    update_streak(state, today, inbox_count())
    if state != before:
        save_state(state)
        print(f"[{today.isoformat()}] inbox-zero streak: {state['current']} "
              f"(longest: {state['longest']})")


if __name__ == "__main__":
    main()
