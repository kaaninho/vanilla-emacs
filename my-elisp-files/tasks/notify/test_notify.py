"""Unit tests for notify.py. Run via:

    python3 -m unittest test_notify.py
"""

import importlib
import os
import sys
import tempfile
import unittest
from datetime import datetime
from pathlib import Path

# `server' lives in the sibling tasks-web/ directory.
sys.path.insert(
    0, str(Path(__file__).resolve().parent.parent / "tasks-web"))


def load_modules(root, state_file):
    """(Re)load server + notify so they pick up the temporary env vars."""
    os.environ["OBSIDIAN_DIR"] = str(root)
    os.environ["TASKS_NOTIFY_STATE"] = str(state_file)
    os.environ["TASKS_NOTIFY_MORNING_HOUR"] = "9"
    import server
    import notify
    importlib.reload(server)
    importlib.reload(notify)
    tasks_dir = root / "tasks"
    archive_dir = tasks_dir / "archive"
    tasks_dir.mkdir(exist_ok=True)
    archive_dir.mkdir(exist_ok=True)
    server.TASKS_DIR = tasks_dir
    server.ARCHIVE_DIR = archive_dir
    notify.STATE_FILE = state_file
    return server, notify, tasks_dir, archive_dir


class ShouldFireTests(unittest.TestCase):
    """Pure-logic tests for the trigger predicate. No filesystem needed."""

    def setUp(self):
        self.tmp = tempfile.TemporaryDirectory()
        root = Path(self.tmp.name)
        self.server, self.notify, _, _ = load_modules(
            root, root / "state.json")

    def tearDown(self):
        self.tmp.cleanup()

    # --- reminder ---

    def test_reminder_future_does_not_fire(self):
        now = datetime(2026, 6, 20, 9, 0)
        self.assertIsNone(
            self.notify.should_fire("reminder", "2026-06-20 10:00", now, None))

    def test_reminder_at_time_fires(self):
        now = datetime(2026, 6, 20, 10, 0)
        self.assertEqual(
            self.notify.should_fire("reminder", "2026-06-20 10:00", now, None),
            "2026-06-20 10:00")

    def test_reminder_after_time_still_fires_once(self):
        """Came back to computer at 11:30 — reminder set for 10:00 still fires."""
        now = datetime(2026, 6, 20, 11, 30)
        self.assertEqual(
            self.notify.should_fire("reminder", "2026-06-20 10:00", now, None),
            "2026-06-20 10:00")

    def test_reminder_not_fired_twice_for_same_value(self):
        now = datetime(2026, 6, 20, 12, 0)
        self.assertIsNone(self.notify.should_fire(
            "reminder", "2026-06-20 10:00", now, "2026-06-20 10:00"))

    def test_reminder_refires_after_value_changed(self):
        """User changes the reminder — the new value is unseen state."""
        now = datetime(2026, 6, 20, 12, 0)
        self.assertEqual(self.notify.should_fire(
            "reminder", "2026-06-20 11:30", now, "2026-06-20 10:00"),
            "2026-06-20 11:30")

    # --- due / scheduled (date-only) ---

    def test_due_today_morning_fires(self):
        now = datetime(2026, 6, 20, 9, 30)
        self.assertEqual(
            self.notify.should_fire("due", "2026-06-20", now, None),
            "2026-06-20")

    def test_due_too_early(self):
        """Before the configured morning hour, no fire."""
        now = datetime(2026, 6, 20, 8, 30)
        self.assertIsNone(
            self.notify.should_fire("due", "2026-06-20", now, None))

    def test_due_future_does_not_fire(self):
        now = datetime(2026, 6, 19, 10, 0)
        self.assertIsNone(
            self.notify.should_fire("due", "2026-06-20", now, None))

    def test_due_already_fired_today(self):
        now = datetime(2026, 6, 20, 14, 0)
        self.assertIsNone(self.notify.should_fire(
            "due", "2026-06-20", now, "2026-06-20"))

    def test_due_overdue_refires_next_day(self):
        """Yesterday's due not done → today fires again, once."""
        now = datetime(2026, 6, 21, 9, 30)
        self.assertEqual(self.notify.should_fire(
            "due", "2026-06-20", now, "2026-06-20"),
            "2026-06-21")

    def test_due_overdue_only_once_per_day(self):
        now = datetime(2026, 6, 21, 15, 0)
        self.assertIsNone(self.notify.should_fire(
            "due", "2026-06-20", now, "2026-06-21"))

    def test_scheduled_uses_same_rules(self):
        now = datetime(2026, 6, 20, 10, 0)
        self.assertEqual(
            self.notify.should_fire("scheduled", "2026-06-20", now, None),
            "2026-06-20")
        # Next-day overdue also refires.
        self.assertEqual(
            self.notify.should_fire(
                "scheduled", "2026-06-20",
                datetime(2026, 6, 21, 10, 0), "2026-06-20"),
            "2026-06-21")

    # --- date with embedded time on due/scheduled ---

    def test_due_with_time_behaves_like_reminder(self):
        now = datetime(2026, 6, 20, 14, 30)
        self.assertEqual(
            self.notify.should_fire("due", "2026-06-20 14:00", now, None),
            "2026-06-20 14:00")
        self.assertIsNone(self.notify.should_fire(
            "due", "2026-06-20 14:00", now, "2026-06-20 14:00"))


class CheckTasksTests(unittest.TestCase):
    """Filesystem-touching tests for the full check_tasks orchestration."""

    def setUp(self):
        self.tmp = tempfile.TemporaryDirectory()
        self.root = Path(self.tmp.name)
        self.state_file = self.root / "state.json"
        self.server, self.notify, self.tasks_dir, self.archive_dir = \
            load_modules(self.root, self.state_file)
        self.calls = []
        self.fake_notify = lambda title, body: self.calls.append((title, body))

    def tearDown(self):
        self.tmp.cleanup()

    def _write_task(self, name, frontmatter):
        body = "---\n" + frontmatter + "\n---\n\n# " + name + "\n"
        (self.tasks_dir / (name.lower() + ".md")).write_text(body)

    def test_fires_for_due_today(self):
        today = datetime.now().strftime("%Y-%m-%d")
        self._write_task("Test", f"status: next\ndue: {today}")
        now = datetime.now().replace(hour=10, minute=0)
        fired = self.notify.check_tasks(now=now, notify=self.fake_notify)
        self.assertEqual(len(fired), 1)
        self.assertEqual(len(self.calls), 1)
        self.assertIn("Test", self.calls[0][0])
        self.assertIn(today, self.calls[0][1])

    def test_does_not_refire_same_day(self):
        today = datetime.now().strftime("%Y-%m-%d")
        self._write_task("Once", f"status: next\ndue: {today}")
        now = datetime.now().replace(hour=10, minute=0)
        self.notify.check_tasks(now=now, notify=self.fake_notify)
        # Second call right after — already in state.
        self.notify.check_tasks(now=now.replace(minute=5),
                                notify=self.fake_notify)
        self.assertEqual(len(self.calls), 1)

    def test_state_persists_to_disk(self):
        today = datetime.now().strftime("%Y-%m-%d")
        self._write_task("Persist", f"status: next\ndue: {today}")
        now = datetime.now().replace(hour=10, minute=0)
        self.notify.check_tasks(now=now, notify=self.fake_notify)
        self.assertTrue(self.state_file.exists())
        state = self.notify.load_state()
        self.assertEqual(len(state), 1)
        file_state = list(state.values())[0]
        self.assertEqual(file_state["due"], today)

    def test_state_prunes_files_that_are_gone(self):
        # Pre-existing state for a path that no longer exists.
        stale = str(self.tasks_dir / "vanished.md")
        self.notify.save_state({stale: {"due": "2026-06-20"}})
        self.notify.check_tasks(now=datetime.now(), notify=self.fake_notify)
        state = self.notify.load_state()
        self.assertNotIn(stale, state)

    def test_state_does_not_prune_still_active(self):
        today = datetime.now().strftime("%Y-%m-%d")
        self._write_task("Active", f"status: next\ndue: {today}")
        now = datetime.now().replace(hour=10, minute=0)
        self.notify.check_tasks(now=now, notify=self.fake_notify)
        # Calling again should keep state.
        self.notify.check_tasks(now=now.replace(minute=1),
                                notify=self.fake_notify)
        self.assertEqual(len(self.notify.load_state()), 1)

    def test_no_active_tasks_no_notifications(self):
        result = self.notify.check_tasks(
            now=datetime.now(), notify=self.fake_notify)
        self.assertEqual(result, [])
        self.assertEqual(self.calls, [])

    def test_skips_tasks_without_relevant_fields(self):
        today = datetime.now().strftime("%Y-%m-%d")
        self._write_task("NoDates", "status: next")
        now = datetime.now().replace(hour=10, minute=0)
        result = self.notify.check_tasks(now=now, notify=self.fake_notify)
        self.assertEqual(result, [])

    def test_overdue_refires_on_subsequent_call_next_day(self):
        # Task with due yesterday. State: notified yesterday.
        yesterday = "2026-06-19"
        today = "2026-06-20"
        self._write_task("Overdue", f"status: next\ndue: {yesterday}")
        # Simulate yesterday's state.
        file_path = str((self.tasks_dir / "overdue.md").resolve())
        self.notify.save_state({file_path: {"due": yesterday}})
        # Today, 10:00.
        now = datetime(2026, 6, 20, 10, 0)
        result = self.notify.check_tasks(now=now, notify=self.fake_notify)
        self.assertEqual(len(result), 1)
        self.assertEqual(self.notify.load_state()[file_path]["due"], today)


if __name__ == "__main__":
    unittest.main()
