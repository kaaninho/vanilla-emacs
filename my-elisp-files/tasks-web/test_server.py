"""Unit tests for server.py. Run via:

    python3 -m unittest test_server.py
"""

import importlib
import os
import tempfile
import unittest
from pathlib import Path


def load_server(tasks_dir, archive_dir):
    os.environ["OBSIDIAN_DIR"] = str(tasks_dir.parent)
    # Re-import the module so it picks up the env var.
    import server
    importlib.reload(server)
    # Override directly in case OBSIDIAN_DIR layout differs.
    server.TASKS_DIR = tasks_dir
    server.ARCHIVE_DIR = archive_dir
    return server


class ServerTests(unittest.TestCase):
    def setUp(self):
        self.tmp = tempfile.TemporaryDirectory()
        root = Path(self.tmp.name)
        self.tasks_dir = root / "tasks"
        self.archive_dir = self.tasks_dir / "archive"
        self.tasks_dir.mkdir()
        self.archive_dir.mkdir()
        self.server = load_server(self.tasks_dir, self.archive_dir)

    def tearDown(self):
        self.tmp.cleanup()

    # --- parse_task ---

    def test_parse_task_basic(self):
        p = self.tasks_dir / "t.md"
        p.write_text("---\nstatus: inbox\ndue: 2026-06-08\n---\n\n# Hello\n")
        t = self.server.parse_task(p)
        self.assertEqual(t["status"], "inbox")
        self.assertEqual(t["due"], "2026-06-08")
        self.assertEqual(t["title"], "Hello")
        self.assertEqual(t["file"], "t.md")
        self.assertFalse(t["archived"])

    def test_parse_task_quoted_value(self):
        p = self.tasks_dir / "t.md"
        p.write_text('---\nstatus: inbox\nproject: "[[Projekt X]]"\n---\n\n# T\n')
        t = self.server.parse_task(p)
        self.assertEqual(t["project"], "[[Projekt X]]")

    def test_parse_task_no_frontmatter(self):
        p = self.tasks_dir / "t.md"
        p.write_text("just notes\n")
        self.assertIsNone(self.server.parse_task(p))

    def test_parse_task_archived_flag(self):
        p = self.archive_dir / "2026-05-26-old.md"
        p.write_text("---\nstatus: done\n---\n\n# Old\n")
        t = self.server.parse_task(p)
        self.assertTrue(t["archived"])

    # --- list_tasks ---

    def test_list_tasks_skips_non_tasks(self):
        (self.tasks_dir / "a.md").write_text(
            "---\nstatus: inbox\n---\n\n# A\n")
        (self.tasks_dir / "b.md").write_text("notes")
        (self.tasks_dir / "c.md").write_text(
            "---\ntitle: c\n---\n# C\n")  # no status
        tasks = self.server.list_tasks(self.tasks_dir)
        self.assertEqual([t["file"] for t in tasks], ["a.md"])

    # --- slugify ---

    def test_slugify(self):
        f = self.server.slugify
        self.assertEqual(f("Hello World"), "hello-world")
        self.assertEqual(f("Wäsche waschen"), "waesche-waschen")
        self.assertEqual(f("Größe ändern"), "groesse-aendern")
        self.assertEqual(f("[[E-Mail schreiben]]"), "e-mail-schreiben")
        self.assertEqual(f(""), "task")

    # --- yaml_quote ---

    def test_yaml_quote(self):
        f = self.server.yaml_quote
        self.assertEqual(f("plain"), "plain")
        self.assertEqual(f("[[link]]"), '"[[link]]"')
        self.assertEqual(f('with "quote"'), 'with "quote"')  # no leading reserved
        self.assertEqual(f('"leading"'), '"\\"leading\\""')

    # --- update_property ---

    def test_update_property_replace(self):
        p = self.tasks_dir / "t.md"
        p.write_text("---\nstatus: inbox\n---\n\n# T\n")
        self.server.update_property(p, "status", "today")
        self.assertIn("status: today", p.read_text())
        self.assertNotIn("status: inbox", p.read_text())

    def test_update_property_add(self):
        p = self.tasks_dir / "t.md"
        p.write_text("---\nstatus: inbox\n---\n\n# T\n")
        self.server.update_property(p, "due", "2026-06-08")
        text = p.read_text()
        self.assertIn("status: inbox", text)
        self.assertIn("due: 2026-06-08", text)

    def test_update_property_remove(self):
        p = self.tasks_dir / "t.md"
        p.write_text("---\nstatus: inbox\ndue: 2026-06-08\n---\n\n# T\n")
        self.server.update_property(p, "due", None)
        text = p.read_text()
        self.assertNotIn("due:", text)
        self.assertIn("status: inbox", text)

    # --- capture ---

    def test_capture_creates_file(self):
        name = self.server.capture("Test Task")
        self.assertEqual(name, "test-task.md")
        path = self.tasks_dir / name
        self.assertTrue(path.exists())
        text = path.read_text()
        self.assertIn("status: inbox", text)
        self.assertIn("# Test Task", text)

    def test_capture_collision(self):
        self.server.capture("Same")
        n2 = self.server.capture("Same")
        self.assertEqual(n2, "same-2.md")

    # --- archive / unarchive ---

    def test_archive_file_moves_with_date_prefix(self):
        (self.tasks_dir / "a.md").write_text(
            "---\nstatus: inbox\n---\n\n# A\n")
        self.server.archive_file("a.md")
        self.assertFalse((self.tasks_dir / "a.md").exists())
        archived = list(self.archive_dir.glob("*-a.md"))
        self.assertEqual(len(archived), 1)
        text = archived[0].read_text()
        self.assertIn("status: done", text)
        self.assertIn("archived-at:", text)

    def test_unarchive_strips_date_prefix(self):
        (self.archive_dir / "2026-05-26-old.md").write_text(
            "---\nstatus: done\narchived-at: 2026-05-26\n---\n\n# Old\n")
        self.server.unarchive_file("2026-05-26-old.md", "next")
        restored = self.tasks_dir / "old.md"
        self.assertTrue(restored.exists())
        text = restored.read_text()
        self.assertIn("status: next", text)
        self.assertNotIn("archived-at", text)

    def test_unarchive_rejects_invalid_status(self):
        (self.archive_dir / "2026-05-26-old.md").write_text(
            "---\nstatus: done\n---\n\n# Old\n")
        with self.assertRaises(ValueError):
            self.server.unarchive_file("2026-05-26-old.md", "garbage")
        with self.assertRaises(ValueError):
            self.server.unarchive_file("2026-05-26-old.md", "done")

    # --- set_status ---

    def test_set_status_in_place(self):
        (self.tasks_dir / "a.md").write_text(
            "---\nstatus: inbox\n---\n\n# A\n")
        self.server.set_status("a.md", "next")
        text = (self.tasks_dir / "a.md").read_text()
        self.assertIn("status: next", text)

    def test_set_status_done_archives(self):
        (self.tasks_dir / "a.md").write_text(
            "---\nstatus: inbox\n---\n\n# A\n")
        self.server.set_status("a.md", "done")
        self.assertFalse((self.tasks_dir / "a.md").exists())
        self.assertEqual(len(list(self.archive_dir.glob("*-a.md"))), 1)

    def test_set_status_from_archive_unarchives(self):
        (self.archive_dir / "2026-05-26-old.md").write_text(
            "---\nstatus: done\narchived-at: 2026-05-26\n---\n\n# Old\n")
        self.server.set_status("2026-05-26-old.md", "inbox")
        self.assertTrue((self.tasks_dir / "old.md").exists())
        self.assertFalse((self.archive_dir / "2026-05-26-old.md").exists())

    # --- find_task: rejects path traversal ---

    def test_find_task_rejects_traversal(self):
        outside = self.tmp.name + "/escape.md"
        Path(outside).write_text("nope")
        self.assertIsNone(self.server.find_task("../escape.md"))

    # --- update_title ---

    def test_update_title_replaces_h1(self):
        p = self.tasks_dir / "a.md"
        p.write_text("---\nstatus: inbox\n---\n\n# Old title\n\nbody\n")
        self.server.update_title(p, "New title")
        text = p.read_text()
        self.assertIn("# New title", text)
        self.assertNotIn("# Old title", text)
        self.assertIn("body", text)

    def test_update_title_rejects_empty(self):
        p = self.tasks_dir / "a.md"
        p.write_text("---\nstatus: inbox\n---\n\n# T\n")
        with self.assertRaises(ValueError):
            self.server.update_title(p, "")
        with self.assertRaises(ValueError):
            self.server.update_title(p, "   ")

    def test_update_title_inserts_h1_if_missing(self):
        p = self.tasks_dir / "a.md"
        p.write_text("---\nstatus: inbox\n---\n\nbody only\n")
        self.server.update_title(p, "Inserted")
        self.assertIn("# Inserted", p.read_text())

    # --- edit_task ---

    def test_edit_task_multiple_fields(self):
        p = self.tasks_dir / "a.md"
        p.write_text("---\nstatus: inbox\n---\n\n# Old\n")
        self.server.edit_task("a.md", {
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
        self.server.edit_task("a.md", {"due": ""})
        self.assertNotIn("due:", p.read_text())

    def test_edit_task_status_change_applied_last(self):
        """Status change can move the file; properties applied first
        on the active file are preserved after the move."""
        p = self.tasks_dir / "a.md"
        p.write_text("---\nstatus: inbox\n---\n\n# A\n")
        self.server.edit_task("a.md", {"due": "2026-06-08", "status": "done"})
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
        self.server.edit_task("a.md", {"project": "[[X]]"})
        text = p.read_text()
        self.assertIn("# Keep me", text)
        self.assertIn("due: 2026-06-08", text)
        self.assertIn('project: "[[X]]"', text)

    def test_edit_task_not_found(self):
        with self.assertRaises(FileNotFoundError):
            self.server.edit_task("nope.md", {"title": "x"})

    # --- YAML list parsing ---

    def test_parse_inline_flow_list(self):
        p = self.tasks_dir / "t.md"
        p.write_text("---\nstatus: inbox\ntags: [work, urgent]\n---\n\n# T\n")
        t = self.server.parse_task(p)
        self.assertEqual(t["status"], "inbox")
        self.assertEqual(t["tags"], ["work", "urgent"])

    def test_parse_block_list(self):
        p = self.tasks_dir / "t.md"
        p.write_text(
            "---\nstatus: inbox\ntags:\n  - work\n  - urgent\n---\n\n# T\n")
        t = self.server.parse_task(p)
        self.assertEqual(t["status"], "inbox")
        self.assertEqual(t["tags"], ["work", "urgent"])

    def test_parse_block_list_quoted_items(self):
        p = self.tasks_dir / "t.md"
        p.write_text(
            "---\nstatus: inbox\ncontexts:\n"
            '  - "@work"\n  - "@phone"\n---\n\n# T\n')
        t = self.server.parse_task(p)
        self.assertEqual(t["contexts"], ["@work", "@phone"])

    def test_parse_empty_flow_list(self):
        p = self.tasks_dir / "t.md"
        p.write_text("---\nstatus: inbox\ntags: []\n---\n\n# T\n")
        t = self.server.parse_task(p)
        self.assertEqual(t["tags"], [])

    def test_parse_list_followed_by_scalar(self):
        p = self.tasks_dir / "t.md"
        p.write_text(
            "---\nstatus: inbox\ntags:\n  - a\n  - b\n"
            "due: 2026-06-08\n---\n\n# T\n")
        t = self.server.parse_task(p)
        self.assertEqual(t["tags"], ["a", "b"])
        self.assertEqual(t["due"], "2026-06-08")

    def test_parse_coerces_scalar_status_from_block_list(self):
        """Obsidian's List property type → single-item block list."""
        p = self.tasks_dir / "t.md"
        p.write_text("---\nstatus:\n  - next\n---\n\n# T\n")
        t = self.server.parse_task(p)
        self.assertEqual(t["status"], "next")
        self.assertIsInstance(t["status"], str)

    def test_parse_coerces_scalar_due_from_flow_list(self):
        p = self.tasks_dir / "t.md"
        p.write_text(
            "---\nstatus: inbox\ndue: [2026-06-08]\n---\n\n# T\n")
        t = self.server.parse_task(p)
        self.assertEqual(t["due"], "2026-06-08")
        self.assertIsInstance(t["due"], str)

    def test_parse_keeps_true_list_fields(self):
        p = self.tasks_dir / "t.md"
        p.write_text(
            "---\nstatus: inbox\ntags:\n  - work\n  - urgent\n---\n\n# T\n")
        t = self.server.parse_task(p)
        self.assertEqual(t["status"], "inbox")
        self.assertEqual(t["tags"], ["work", "urgent"])

    def test_update_property_removes_block_list(self):
        p = self.tasks_dir / "a.md"
        p.write_text(
            "---\nstatus: inbox\ntags:\n  - a\n  - b\n"
            "due: 2026-06-08\n---\n\n# T\n")
        self.server.update_property(p, "tags", None)
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
        self.server.update_property(p, "tags", "single")
        text = p.read_text()
        self.assertIn("tags: single", text)
        self.assertNotIn("- a", text)
        self.assertNotIn("- b", text)

    def test_unquote_yaml(self):
        f = self.server.unquote_yaml
        self.assertEqual(f("plain"), "plain")
        self.assertEqual(f('"quoted"'), "quoted")
        self.assertEqual(f("'single'"), "single")
        self.assertEqual(f('"with \\"escape\\""'), 'with "escape"')
        self.assertEqual(f(""), "")


if __name__ == "__main__":
    unittest.main()
