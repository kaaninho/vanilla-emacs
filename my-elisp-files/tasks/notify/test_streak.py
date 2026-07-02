"""Unit tests for streak.py.

    python3 -m unittest test_streak.py
"""

import importlib
import json
import os
import sys
import tempfile
import unittest
from datetime import date
from pathlib import Path

sys.path.insert(
    0, str(Path(__file__).resolve().parent.parent / "lib"))


def load_streak_module(root, state_file):
    os.environ["OBSIDIAN_DIR"] = str(root)
    os.environ["TASKS_STREAK_STATE"] = str(state_file)
    import tasks_lib
    import streak
    importlib.reload(tasks_lib)
    importlib.reload(streak)
    tasks_dir = root / "tasks"
    archive_dir = tasks_dir / "archive"
    tasks_dir.mkdir(exist_ok=True)
    archive_dir.mkdir(exist_ok=True)
    tasks_lib.TASKS_DIR = tasks_dir
    tasks_lib.ARCHIVE_DIR = archive_dir
    streak.STATE_FILE = state_file
    return streak, tasks_lib, tasks_dir


class StreakLogicTests(unittest.TestCase):
    """Pure-function tests for `update_streak`. No filesystem needed."""

    def setUp(self):
        self.tmp = tempfile.TemporaryDirectory()
        root = Path(self.tmp.name)
        self.streak, _, _ = load_streak_module(
            root, root / "state.json")

    def tearDown(self):
        self.tmp.cleanup()

    # 2026-06-19 is a Friday, 2026-06-22 a Monday, 2026-06-23 a Tuesday.

    def test_first_zero_day_starts_streak(self):
        state = {"current": 0, "longest": 0, "last_zero_date": ""}
        self.streak.update_streak(state, date(2026, 6, 23), 0)
        self.assertEqual(state["current"], 1)
        self.assertEqual(state["longest"], 1)
        self.assertEqual(state["last_zero_date"], "2026-06-23")

    def test_consecutive_zero_days_continue_streak(self):
        state = {"current": 4, "longest": 4, "last_zero_date": "2026-06-22"}
        self.streak.update_streak(state, date(2026, 6, 23), 0)
        self.assertEqual(state["current"], 5)
        self.assertEqual(state["longest"], 5)

    def test_weekend_does_not_break_streak(self):
        # Friday zero, then Monday zero: the weekend is bridged.
        state = {"current": 4, "longest": 4, "last_zero_date": "2026-06-19"}
        self.streak.update_streak(state, date(2026, 6, 22), 0)
        self.assertEqual(state["current"], 5)

    def test_weekend_day_neither_counts_nor_breaks(self):
        # Saturday 2026-06-20: inbox zero must leave the state untouched.
        state = {"current": 5, "longest": 5, "last_zero_date": "2026-06-19"}
        self.streak.update_streak(state, date(2026, 6, 20), 0)
        self.assertEqual(state, {"current": 5, "longest": 5,
                                 "last_zero_date": "2026-06-19"})

    def test_same_day_call_is_idempotent(self):
        state = {"current": 5, "longest": 5, "last_zero_date": "2026-06-23"}
        self.streak.update_streak(state, date(2026, 6, 23), 0)
        self.assertEqual(state["current"], 5)

    def test_non_zero_inbox_leaves_state(self):
        state = {"current": 5, "longest": 5, "last_zero_date": "2026-06-22"}
        self.streak.update_streak(state, date(2026, 6, 23), 3)
        self.assertEqual(state, {"current": 5, "longest": 5,
                                 "last_zero_date": "2026-06-22"})

    def test_gap_resets_streak_and_stashes_prev(self):
        state = {"current": 5, "longest": 7, "last_zero_date": "2026-06-15"}
        self.streak.update_streak(state, date(2026, 6, 23), 0)
        self.assertEqual(state["current"], 1)
        self.assertEqual(state["longest"], 7)
        self.assertEqual(state["last_zero_date"], "2026-06-23")
        # Old value stashed so a bridge can recover it later.
        self.assertEqual(state["prev_current"], 5)
        self.assertEqual(state["prev_zero_date"], "2026-06-15")

    def test_longest_records_new_high(self):
        state = {"current": 6, "longest": 6, "last_zero_date": "2026-06-22"}
        self.streak.update_streak(state, date(2026, 6, 23), 0)
        self.assertEqual(state["current"], 7)
        self.assertEqual(state["longest"], 7)


class StreakIOTests(unittest.TestCase):
    """Filesystem-touching tests for load/save and the inbox count helper."""

    def setUp(self):
        self.tmp = tempfile.TemporaryDirectory()
        self.root = Path(self.tmp.name)
        self.state_file = self.root / "state.json"
        self.streak, self.lib, self.tasks_dir = load_streak_module(
            self.root, self.state_file)

    def tearDown(self):
        self.tmp.cleanup()

    def test_load_state_returns_defaults_when_missing(self):
        state = self.streak.load_state()
        self.assertEqual(
            state, {"current": 0, "longest": 0, "last_zero_date": ""})

    def test_load_state_recovers_from_corrupt_json(self):
        self.state_file.write_text("not valid json", encoding="utf-8")
        state = self.streak.load_state()
        self.assertEqual(state["current"], 0)

    def test_save_load_roundtrip(self):
        self.streak.save_state(
            {"current": 3, "longest": 5, "last_zero_date": "2026-06-20"})
        loaded = self.streak.load_state()
        self.assertEqual(loaded["current"], 3)
        self.assertEqual(loaded["longest"], 5)

    def test_inbox_count_counts_only_inbox_status(self):
        (self.tasks_dir / "a.md").write_text(
            "---\nstatus: inbox\n---\n\n# A\n")
        (self.tasks_dir / "b.md").write_text(
            "---\nstatus: next\n---\n\n# B\n")
        (self.tasks_dir / "c.md").write_text(
            "---\nstatus: inbox\n---\n\n# C\n")
        self.assertEqual(self.streak.inbox_count(), 2)

    def test_inbox_count_zero_when_no_tasks(self):
        self.assertEqual(self.streak.inbox_count(), 0)


if __name__ == "__main__":
    unittest.main()
