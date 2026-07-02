"""Unit tests for tasks_lib.py. Run via:

    python3 -m unittest test_tasks_lib.py
"""

import importlib
import os
import tempfile
import unittest
from pathlib import Path


def load_tasks_lib(tasks_dir, archive_dir):
    os.environ["OBSIDIAN_DIR"] = str(tasks_dir.parent)
    # Re-import the module so it picks up the env var.
    import tasks_lib
    importlib.reload(tasks_lib)
    # Override directly in case OBSIDIAN_DIR layout differs.
    tasks_lib.TASKS_DIR = tasks_dir
    tasks_lib.ARCHIVE_DIR = archive_dir
    return tasks_lib


class TasksLibTests(unittest.TestCase):
    def setUp(self):
        self.tmp = tempfile.TemporaryDirectory()
        root = Path(self.tmp.name)
        self.tasks_dir = root / "tasks"
        self.archive_dir = self.tasks_dir / "archive"
        self.tasks_dir.mkdir()
        self.archive_dir.mkdir()
        self.lib = load_tasks_lib(self.tasks_dir, self.archive_dir)

    def tearDown(self):
        self.tmp.cleanup()

    # --- parse_task ---

    def test_parse_task_basic(self):
        p = self.tasks_dir / "t.md"
        p.write_text("---\nstatus: inbox\ndue: 2026-06-08\n---\n\n# Hello\n")
        t = self.lib.parse_task(p)
        self.assertEqual(t["status"], "inbox")
        self.assertEqual(t["due"], "2026-06-08")
        self.assertEqual(t["title"], "Hello")
        self.assertEqual(t["file"], "t.md")
        self.assertFalse(t["archived"])

    def test_parse_task_quoted_value(self):
        p = self.tasks_dir / "t.md"
        p.write_text('---\nstatus: inbox\nproject: "[[Projekt X]]"\n---\n\n# T\n')
        t = self.lib.parse_task(p)
        self.assertEqual(t["project"], "[[Projekt X]]")

    def test_parse_task_no_frontmatter(self):
        p = self.tasks_dir / "t.md"
        p.write_text("just notes\n")
        self.assertIsNone(self.lib.parse_task(p))

    def test_parse_task_archived_flag(self):
        p = self.archive_dir / "2026-05-26-old.md"
        p.write_text("---\nstatus: done\n---\n\n# Old\n")
        t = self.lib.parse_task(p)
        self.assertTrue(t["archived"])

    # --- list_tasks ---

    def test_list_tasks_skips_non_tasks(self):
        (self.tasks_dir / "a.md").write_text(
            "---\nstatus: inbox\n---\n\n# A\n")
        (self.tasks_dir / "b.md").write_text("notes")
        (self.tasks_dir / "c.md").write_text(
            "---\ntitle: c\n---\n# C\n")  # no status
        tasks = self.lib.list_tasks(self.tasks_dir)
        self.assertEqual([t["file"] for t in tasks], ["a.md"])

    # --- slugify ---

    def test_slugify(self):
        f = self.lib.slugify
        self.assertEqual(f("Hello World"), "hello-world")
        self.assertEqual(f("Wäsche waschen"), "waesche-waschen")
        self.assertEqual(f("Größe ändern"), "groesse-aendern")
        self.assertEqual(f("[[E-Mail schreiben]]"), "e-mail-schreiben")
        self.assertEqual(f(""), "task")

    # --- yaml_quote ---

    def test_yaml_quote(self):
        f = self.lib.yaml_quote
        self.assertEqual(f("plain"), "plain")
        self.assertEqual(f("[[link]]"), '"[[link]]"')
        self.assertEqual(f('with "quote"'), 'with "quote"')  # no leading reserved
        self.assertEqual(f('"leading"'), '"\\"leading\\""')

    # --- update_property ---

    def test_update_property_replace(self):
        p = self.tasks_dir / "t.md"
        p.write_text("---\nstatus: inbox\n---\n\n# T\n")
        self.lib.update_property(p, "status", "today")
        self.assertIn("status: today", p.read_text())
        self.assertNotIn("status: inbox", p.read_text())

    def test_update_property_add(self):
        p = self.tasks_dir / "t.md"
        p.write_text("---\nstatus: inbox\n---\n\n# T\n")
        self.lib.update_property(p, "due", "2026-06-08")
        text = p.read_text()
        self.assertIn("status: inbox", text)
        self.assertIn("due: 2026-06-08", text)

    def test_update_property_remove(self):
        p = self.tasks_dir / "t.md"
        p.write_text("---\nstatus: inbox\ndue: 2026-06-08\n---\n\n# T\n")
        self.lib.update_property(p, "due", None)
        text = p.read_text()
        self.assertNotIn("due:", text)
        self.assertIn("status: inbox", text)

    # --- capture ---

    def test_capture_creates_file(self):
        name = self.lib.capture("Test Task")
        self.assertEqual(name, "test-task.md")
        path = self.tasks_dir / name
        self.assertTrue(path.exists())
        text = path.read_text()
        self.assertIn("status: inbox", text)
        self.assertIn("# Test Task", text)

    def test_capture_collision(self):
        self.lib.capture("Same")
        n2 = self.lib.capture("Same")
        self.assertEqual(n2, "same-2.md")

    # --- archive / unarchive ---

    def test_archive_file_moves_with_date_prefix(self):
        (self.tasks_dir / "a.md").write_text(
            "---\nstatus: inbox\n---\n\n# A\n")
        self.lib.archive_file("a.md")
        self.assertFalse((self.tasks_dir / "a.md").exists())
        archived = list(self.archive_dir.glob("*-a.md"))
        self.assertEqual(len(archived), 1)
        text = archived[0].read_text()
        self.assertIn("status: done", text)
        self.assertIn("archived-at:", text)

    def test_unarchive_strips_date_prefix(self):
        (self.archive_dir / "2026-05-26-old.md").write_text(
            "---\nstatus: done\narchived-at: 2026-05-26\n---\n\n# Old\n")
        self.lib.unarchive_file("2026-05-26-old.md", "next")
        restored = self.tasks_dir / "old.md"
        self.assertTrue(restored.exists())
        text = restored.read_text()
        self.assertIn("status: next", text)
        self.assertNotIn("archived-at", text)

    def test_unarchive_rejects_invalid_status(self):
        (self.archive_dir / "2026-05-26-old.md").write_text(
            "---\nstatus: done\n---\n\n# Old\n")
        with self.assertRaises(ValueError):
            self.lib.unarchive_file("2026-05-26-old.md", "garbage")
        with self.assertRaises(ValueError):
            self.lib.unarchive_file("2026-05-26-old.md", "done")

    # --- set_status ---

    def test_set_status_in_place(self):
        (self.tasks_dir / "a.md").write_text(
            "---\nstatus: inbox\n---\n\n# A\n")
        self.lib.set_status("a.md", "next")
        text = (self.tasks_dir / "a.md").read_text()
        self.assertIn("status: next", text)

    def test_set_status_done_archives(self):
        (self.tasks_dir / "a.md").write_text(
            "---\nstatus: inbox\n---\n\n# A\n")
        self.lib.set_status("a.md", "done")
        self.assertFalse((self.tasks_dir / "a.md").exists())
        self.assertEqual(len(list(self.archive_dir.glob("*-a.md"))), 1)

    def test_set_status_from_archive_unarchives(self):
        (self.archive_dir / "2026-05-26-old.md").write_text(
            "---\nstatus: done\narchived-at: 2026-05-26\n---\n\n# Old\n")
        self.lib.set_status("2026-05-26-old.md", "inbox")
        self.assertTrue((self.tasks_dir / "old.md").exists())
        self.assertFalse((self.archive_dir / "2026-05-26-old.md").exists())

    # --- find_task: rejects path traversal ---

    def test_find_task_rejects_traversal(self):
        outside = self.tmp.name + "/escape.md"
        Path(outside).write_text("nope")
        self.assertIsNone(self.lib.find_task("../escape.md"))

    # --- update_title ---

    def test_update_title_replaces_h1(self):
        p = self.tasks_dir / "a.md"
        p.write_text("---\nstatus: inbox\n---\n\n# Old title\n\nbody\n")
        self.lib.update_title(p, "New title")
        text = p.read_text()
        self.assertIn("# New title", text)
        self.assertNotIn("# Old title", text)
        self.assertIn("body", text)

    def test_update_title_rejects_empty(self):
        p = self.tasks_dir / "a.md"
        p.write_text("---\nstatus: inbox\n---\n\n# T\n")
        with self.assertRaises(ValueError):
            self.lib.update_title(p, "")
        with self.assertRaises(ValueError):
            self.lib.update_title(p, "   ")

    def test_update_title_inserts_h1_if_missing(self):
        p = self.tasks_dir / "a.md"
        p.write_text("---\nstatus: inbox\n---\n\nbody only\n")
        self.lib.update_title(p, "Inserted")
        self.assertIn("# Inserted", p.read_text())

    # --- edit_task ---

    def test_edit_task_multiple_fields(self):
        p = self.tasks_dir / "a.md"
        p.write_text("---\nstatus: inbox\n---\n\n# Old\n")
        self.lib.edit_task("a.md", {
            "title": "Renamed",
            "due": "2026-12-31",
            "scheduled": "2026-12-30",
            "project": "[[Proj]]",
        })
        text = p.read_text()
        self.assertIn("# Renamed", text)
        self.assertIn("due: 2026-12-31", text)
        self.assertIn("scheduled: 2026-12-30", text)
        self.assertIn('project: "[[Proj]]"', text)

    def test_edit_task_clear_field(self):
        p = self.tasks_dir / "a.md"
        p.write_text("---\nstatus: inbox\ndue: 2026-06-08\n---\n\n# A\n")
        self.lib.edit_task("a.md", {"due": ""})
        self.assertNotIn("due:", p.read_text())

    def test_edit_task_status_change_applied_last(self):
        """Status change can move the file; properties applied first
        on the active file are preserved after the move."""
        p = self.tasks_dir / "a.md"
        p.write_text("---\nstatus: inbox\n---\n\n# A\n")
        self.lib.edit_task("a.md", {"due": "2026-06-08", "status": "done"})
        self.assertFalse((self.tasks_dir / "a.md").exists())
        archived = list(self.archive_dir.glob("*-a.md"))
        self.assertEqual(len(archived), 1)
        text = archived[0].read_text()
        self.assertIn("due: 2026-06-08", text)
        self.assertIn("status: done", text)

    def test_edit_task_missing_keys_left_untouched(self):
        p = self.tasks_dir / "a.md"
        p.write_text(
            "---\nstatus: inbox\ndue: 2026-06-08\n---\n\n# Keep me\n")
        self.lib.edit_task("a.md", {"project": "[[X]]"})
        text = p.read_text()
        self.assertIn("# Keep me", text)
        self.assertIn("due: 2026-06-08", text)
        self.assertIn('project: "[[X]]"', text)

    def test_edit_task_not_found(self):
        with self.assertRaises(FileNotFoundError):
            self.lib.edit_task("nope.md", {"title": "x"})

    # --- YAML list parsing ---

    def test_parse_inline_flow_list(self):
        p = self.tasks_dir / "t.md"
        p.write_text("---\nstatus: inbox\ntags: [work, urgent]\n---\n\n# T\n")
        t = self.lib.parse_task(p)
        self.assertEqual(t["status"], "inbox")
        self.assertEqual(t["tags"], ["work", "urgent"])

    def test_parse_block_list(self):
        p = self.tasks_dir / "t.md"
        p.write_text(
            "---\nstatus: inbox\ntags:\n  - work\n  - urgent\n---\n\n# T\n")
        t = self.lib.parse_task(p)
        self.assertEqual(t["status"], "inbox")
        self.assertEqual(t["tags"], ["work", "urgent"])

    def test_parse_block_list_quoted_items(self):
        p = self.tasks_dir / "t.md"
        p.write_text(
            "---\nstatus: inbox\ncontexts:\n"
            '  - "@work"\n  - "@phone"\n---\n\n# T\n')
        t = self.lib.parse_task(p)
        self.assertEqual(t["contexts"], ["@work", "@phone"])

    def test_parse_empty_flow_list(self):
        p = self.tasks_dir / "t.md"
        p.write_text("---\nstatus: inbox\ntags: []\n---\n\n# T\n")
        t = self.lib.parse_task(p)
        self.assertEqual(t["tags"], [])

    def test_parse_list_followed_by_scalar(self):
        p = self.tasks_dir / "t.md"
        p.write_text(
            "---\nstatus: inbox\ntags:\n  - a\n  - b\n"
            "due: 2026-06-08\n---\n\n# T\n")
        t = self.lib.parse_task(p)
        self.assertEqual(t["tags"], ["a", "b"])
        self.assertEqual(t["due"], "2026-06-08")

    def test_parse_coerces_scalar_status_from_block_list(self):
        """Obsidian's List property type → single-item block list."""
        p = self.tasks_dir / "t.md"
        p.write_text("---\nstatus:\n  - next\n---\n\n# T\n")
        t = self.lib.parse_task(p)
        self.assertEqual(t["status"], "next")
        self.assertIsInstance(t["status"], str)

    def test_parse_coerces_scalar_due_from_flow_list(self):
        p = self.tasks_dir / "t.md"
        p.write_text(
            "---\nstatus: inbox\ndue: [2026-06-08]\n---\n\n# T\n")
        t = self.lib.parse_task(p)
        self.assertEqual(t["due"], "2026-06-08")
        self.assertIsInstance(t["due"], str)

    def test_parse_keeps_true_list_fields(self):
        p = self.tasks_dir / "t.md"
        p.write_text(
            "---\nstatus: inbox\ntags:\n  - work\n  - urgent\n---\n\n# T\n")
        t = self.lib.parse_task(p)
        self.assertEqual(t["status"], "inbox")
        self.assertEqual(t["tags"], ["work", "urgent"])

    def test_update_property_removes_block_list(self):
        p = self.tasks_dir / "a.md"
        p.write_text(
            "---\nstatus: inbox\ntags:\n  - a\n  - b\n"
            "due: 2026-06-08\n---\n\n# T\n")
        self.lib.update_property(p, "tags", None)
        text = p.read_text()
        self.assertNotIn("tags:", text)
        self.assertNotIn("- a", text)
        self.assertNotIn("- b", text)
        self.assertIn("status: inbox", text)
        self.assertIn("due: 2026-06-08", text)

    def test_update_property_replaces_block_list_with_scalar(self):
        p = self.tasks_dir / "a.md"
        p.write_text(
            "---\nstatus: inbox\ntags:\n  - a\n  - b\n---\n\n# T\n")
        self.lib.update_property(p, "tags", "single")
        text = p.read_text()
        self.assertIn("tags: single", text)
        self.assertNotIn("- a", text)
        self.assertNotIn("- b", text)

    # --- update_list_property + contexts ---

    def test_update_list_property_writes_block(self):
        p = self.tasks_dir / "a.md"
        p.write_text("---\nstatus: inbox\n---\n\n# T\n")
        self.lib.update_list_property(p, "contexts", ["@work", "@computer"])
        text = p.read_text()
        self.assertIn('contexts:\n  - "@work"\n  - "@computer"', text)
        self.assertEqual(
            self.lib.parse_task(p)["contexts"], ["@work", "@computer"])

    def test_update_list_property_removes(self):
        p = self.tasks_dir / "a.md"
        p.write_text(
            "---\nstatus: inbox\ncontexts:\n"
            '  - "@work"\n  - "@home"\n'
            "due: 2026-06-08\n---\n\n# T\n")
        self.lib.update_list_property(p, "contexts", [])
        text = p.read_text()
        self.assertNotIn("contexts:", text)
        self.assertNotIn("@work", text)
        self.assertIn("status: inbox", text)
        self.assertIn("due: 2026-06-08", text)

    def test_update_list_property_replaces_scalar(self):
        p = self.tasks_dir / "a.md"
        p.write_text("---\nstatus: inbox\ncontexts: stale\n---\n\n# T\n")
        self.lib.update_list_property(p, "contexts", ["@a"])
        self.assertEqual(self.lib.parse_task(p)["contexts"], ["@a"])

    def test_edit_task_sets_contexts(self):
        p = self.tasks_dir / "a.md"
        p.write_text("---\nstatus: inbox\n---\n\n# T\n")
        self.lib.edit_task("a.md", {"contexts": ["@work", "@phone"]})
        self.assertEqual(
            self.lib.parse_task(p)["contexts"], ["@work", "@phone"])

    def test_edit_task_clears_contexts(self):
        p = self.tasks_dir / "a.md"
        p.write_text(
            "---\nstatus: inbox\ncontexts:\n  - \"@work\"\n---\n\n# T\n")
        self.lib.edit_task("a.md", {"contexts": []})
        self.assertNotIn("contexts:", p.read_text())

    def test_edit_task_contexts_rejects_non_list(self):
        p = self.tasks_dir / "a.md"
        p.write_text("---\nstatus: inbox\n---\n\n# T\n")
        with self.assertRaises(ValueError):
            self.lib.edit_task("a.md", {"contexts": "@work"})

    def test_unquote_yaml(self):
        f = self.lib.unquote_yaml
        self.assertEqual(f("plain"), "plain")
        self.assertEqual(f('"quoted"'), "quoted")
        self.assertEqual(f("'single'"), "single")
        self.assertEqual(f('"with \\"escape\\""'), 'with "escape"')
        self.assertEqual(f(""), "")

    # --- Audit log on status changes ---

    def test_status_change_appends_log_line(self):
        p = self.tasks_dir / "t.md"
        p.write_text("---\nstatus: inbox\n---\n\n# T\n")
        self.lib.update_property(p, "status", "next")
        self.assertRegex(p.read_text(), r"- .+: inbox → next")

    def test_multiple_status_changes_stack(self):
        p = self.tasks_dir / "t.md"
        p.write_text("---\nstatus: inbox\n---\n\n# T\n")
        self.lib.update_property(p, "status", "next")
        self.lib.update_property(p, "status", "today")
        text = p.read_text()
        self.assertRegex(text, r"- .+: inbox → next")
        self.assertRegex(text, r"- .+: next → today")
        # Lines appear in order.
        self.assertLess(text.index("inbox → next"),
                        text.index("next → today"))

    def test_no_log_when_status_unchanged(self):
        p = self.tasks_dir / "t.md"
        p.write_text("---\nstatus: next\n---\n\n# T\n")
        self.lib.update_property(p, "status", "next")
        self.assertNotIn("→", p.read_text())

    def test_no_log_for_non_status_property(self):
        p = self.tasks_dir / "t.md"
        p.write_text("---\nstatus: next\n---\n\n# T\n")
        self.lib.update_property(p, "due", "2026-06-25")
        self.assertNotIn("→", p.read_text())

    def test_archive_logs_transition_to_done(self):
        p = self.tasks_dir / "t.md"
        p.write_text("---\nstatus: next\n---\n\n# T\n")
        self.lib.archive_file("t.md")
        archived = list(self.archive_dir.glob("*-t.md"))
        self.assertEqual(len(archived), 1)
        self.assertRegex(archived[0].read_text(), r"- .+: next → done")

    # --- Waiting-since auto-stamping ---

    def test_entering_waiting_sets_waiting_since(self):
        from datetime import date
        p = self.tasks_dir / "t.md"
        p.write_text("---\nstatus: next\n---\n\n# T\n")
        self.lib.update_property(p, "status", "waiting")
        task = self.lib.parse_task(p)
        self.assertEqual(task.get("status"), "waiting")
        self.assertEqual(task.get("waiting-since"), date.today().isoformat())

    def test_leaving_waiting_clears_waiting_since(self):
        p = self.tasks_dir / "t.md"
        p.write_text(
            "---\nstatus: waiting\nwaiting-since: 2020-01-01\n---\n\n# T\n")
        self.lib.update_property(p, "status", "next")
        task = self.lib.parse_task(p)
        self.assertEqual(task.get("status"), "next")
        self.assertNotIn("waiting-since", task)

    def test_non_status_update_keeps_waiting_since(self):
        p = self.tasks_dir / "t.md"
        p.write_text(
            "---\nstatus: waiting\nwaiting-since: 2020-01-01\n---\n\n# T\n")
        self.lib.update_property(p, "due", "2026-12-31")
        task = self.lib.parse_task(p)
        self.assertEqual(task.get("waiting-since"), "2020-01-01")

    def test_waiting_to_waiting_noop(self):
        p = self.tasks_dir / "t.md"
        p.write_text(
            "---\nstatus: waiting\nwaiting-since: 2020-01-01\n---\n\n# T\n")
        self.lib.update_property(p, "status", "waiting")
        task = self.lib.parse_task(p)
        self.assertEqual(task.get("waiting-since"), "2020-01-01")

    # --- Recurrence ----

    def test_parse_recurrence_aliases(self):
        f = self.lib.parse_recurrence
        self.assertEqual(f("daily"), (1, "d"))
        self.assertEqual(f("weekly"), (1, "w"))
        self.assertEqual(f("monthly"), (1, "m"))
        self.assertEqual(f("Weekly"), (1, "w"))   # case-insensitive
        self.assertEqual(f("every 2d"), (2, "d"))
        self.assertEqual(f("every 3w"), (3, "w"))
        self.assertEqual(f("every 6m"), (6, "m"))

    def test_parse_recurrence_rejects_garbage(self):
        f = self.lib.parse_recurrence
        self.assertIsNone(f(""))
        self.assertIsNone(f(None))
        self.assertIsNone(f("every 5"))
        self.assertIsNone(f("each week"))
        self.assertIsNone(f("daily-ish"))

    def test_bump_date_days(self):
        f = self.lib.bump_date
        self.assertEqual(f("2026-06-20", 1, "d"), "2026-06-21")
        self.assertEqual(f("2026-06-30", 5, "d"), "2026-07-05")
        # Datetime preserved
        self.assertEqual(f("2026-06-20 09:00", 1, "d"), "2026-06-21 09:00")

    def test_bump_date_weeks(self):
        f = self.lib.bump_date
        self.assertEqual(f("2026-06-20", 1, "w"), "2026-06-27")
        self.assertEqual(f("2026-06-20", 4, "w"), "2026-07-18")

    def test_bump_date_months_basic(self):
        f = self.lib.bump_date
        self.assertEqual(f("2026-06-20", 1, "m"), "2026-07-20")
        self.assertEqual(f("2026-06-20", 6, "m"), "2026-12-20")
        # Wrap across year
        self.assertEqual(f("2026-08-15", 12, "m"), "2027-08-15")

    def test_bump_date_months_clamps_day(self):
        f = self.lib.bump_date
        # Jan 31 + 1 month → Feb 28 (or 29 in leap years)
        self.assertEqual(f("2026-01-31", 1, "m"), "2026-02-28")
        self.assertEqual(f("2024-01-31", 1, "m"), "2024-02-29")

    def test_strip_audit_log_removes_trailing_entries(self):
        body = (
            "User notes here.\n"
            "More notes.\n"
            "\n"
            "- 2026-06-20 11:23: inbox → next\n"
            "- 2026-06-21 09:15: next → today\n"
        )
        stripped = self.lib.strip_audit_log(body)
        self.assertIn("User notes here.", stripped)
        self.assertIn("More notes.", stripped)
        self.assertNotIn("→", stripped)

    def test_strip_audit_log_keeps_unrelated_bullets(self):
        body = (
            "- bullet point A\n"
            "- bullet point B\n"
            "- 2026-06-20: inbox → next\n"
        )
        stripped = self.lib.strip_audit_log(body)
        self.assertIn("bullet point A", stripped)
        self.assertIn("bullet point B", stripped)
        self.assertNotIn("→", stripped)

    def test_archive_recurring_task_creates_next_instance(self):
        from datetime import date
        p = self.tasks_dir / "wash.md"
        p.write_text(
            "---\nstatus: next\nscheduled: 2026-06-20\nrecurrence: weekly\n"
            "---\n\n# Wäsche waschen\n\nNotes about laundry.\n")
        self.lib.archive_file("wash.md")
        # Original is now in archive
        self.assertFalse(p.exists())
        archived = list(self.archive_dir.glob("*-wash.md"))
        self.assertEqual(len(archived), 1)
        # A fresh instance lives in tasks/
        actives = [f for f in self.tasks_dir.iterdir()
                   if f.suffix == ".md" and f.is_file()]
        self.assertEqual(len(actives), 1)
        new_task = self.lib.parse_task(actives[0])
        self.assertEqual(new_task["title"], "Wäsche waschen")
        self.assertEqual(new_task["recurrence"], "weekly")
        self.assertEqual(new_task["scheduled"], "2026-06-27")
        self.assertEqual(new_task["status"], "next")  # pre-done status
        # Body carried over, no audit log
        text = actives[0].read_text()
        self.assertIn("Notes about laundry", text)
        self.assertNotIn("→", text)

    def test_archive_recurring_today_keeps_status(self):
        p = self.tasks_dir / "standup.md"
        p.write_text(
            "---\nstatus: today\nscheduled: 2026-06-20\nrecurrence: daily\n"
            "---\n\n# Standup\n")
        self.lib.archive_file("standup.md")
        new = [f for f in self.tasks_dir.iterdir()
               if f.suffix == ".md" and f.is_file()][0]
        t = self.lib.parse_task(new)
        self.assertEqual(t["status"], "today")
        self.assertEqual(t["scheduled"], "2026-06-21")

    def test_archive_recurring_bumps_all_date_fields(self):
        p = self.tasks_dir / "review.md"
        p.write_text(
            "---\nstatus: next\nscheduled: 2026-06-20\ndue: 2026-06-25\n"
            "reminder: 2026-06-20 09:00\nrecurrence: weekly\n"
            "---\n\n# Weekly Review\n")
        self.lib.archive_file("review.md")
        new = [f for f in self.tasks_dir.iterdir()
               if f.suffix == ".md" and f.is_file()][0]
        t = self.lib.parse_task(new)
        self.assertEqual(t["scheduled"], "2026-06-27")
        self.assertEqual(t["due"], "2026-07-02")
        self.assertEqual(t["reminder"], "2026-06-27 09:00")

    def test_archive_recurring_with_no_dates_anchors_to_today(self):
        from datetime import date, timedelta
        p = self.tasks_dir / "habit.md"
        p.write_text(
            "---\nstatus: next\nrecurrence: every 3d\n---\n\n# Habit\n")
        self.lib.archive_file("habit.md")
        new = [f for f in self.tasks_dir.iterdir()
               if f.suffix == ".md" and f.is_file()][0]
        t = self.lib.parse_task(new)
        expected = (date.today() + timedelta(days=3)).isoformat()
        self.assertEqual(t["scheduled"], expected)

    def test_archive_non_recurring_creates_no_instance(self):
        p = self.tasks_dir / "once.md"
        p.write_text("---\nstatus: next\n---\n\n# Once\n")
        self.lib.archive_file("once.md")
        actives = [f for f in self.tasks_dir.iterdir()
                   if f.suffix == ".md" and f.is_file()]
        self.assertEqual(actives, [])

    def test_archive_recurring_with_contexts_and_project_preserved(self):
        p = self.tasks_dir / "x.md"
        p.write_text(
            "---\nstatus: next\nscheduled: 2026-06-20\nrecurrence: weekly\n"
            'project: "[[Health]]"\ncontexts:\n  - "@home"\n  - "@morning"\n'
            "---\n\n# Yoga\n")
        self.lib.archive_file("x.md")
        new = [f for f in self.tasks_dir.iterdir()
               if f.suffix == ".md" and f.is_file()][0]
        t = self.lib.parse_task(new)
        self.assertEqual(t["project"], "[[Health]]")
        self.assertEqual(t["contexts"], ["@home", "@morning"])

    def test_archive_recurring_with_invalid_spec_creates_no_instance(self):
        p = self.tasks_dir / "bad.md"
        p.write_text(
            "---\nstatus: next\nscheduled: 2026-06-20\nrecurrence: gobbledygook\n"
            "---\n\n# Bad\n")
        self.lib.archive_file("bad.md")
        actives = [f for f in self.tasks_dir.iterdir()
                   if f.suffix == ".md" and f.is_file()]
        self.assertEqual(actives, [])


if __name__ == "__main__":
    unittest.main()
