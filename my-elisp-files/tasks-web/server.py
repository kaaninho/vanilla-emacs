#!/usr/bin/env python3
"""Local HTTP server + REST API for the one-file-per-task system.

Reads markdown task files with YAML frontmatter from $OBSIDIAN_DIR/tasks/.
Serves a static kanban UI at http://localhost:8765 (override with $TASKS_PORT).

Usage:
    python3 server.py
"""

import json
import os
import re
import sys
from datetime import datetime
from http.server import BaseHTTPRequestHandler, ThreadingHTTPServer
from pathlib import Path
from urllib.parse import urlparse


# --- Configuration ----------------------------------------------------------

OBSIDIAN_DIR = Path(os.environ.get(
    "OBSIDIAN_DIR",
    "/Users/kaan/Library/Mobile Documents/iCloud~md~obsidian/"
    "Documents/ObsidianOnIcloud",
))
TASKS_DIR = OBSIDIAN_DIR / "tasks"
ARCHIVE_DIR = TASKS_DIR / "archive"
PORT = int(os.environ.get("TASKS_PORT", "8765"))
STATIC_DIR = Path(__file__).resolve().parent

ALLOWED_STATUSES = {"inbox", "next", "today", "waiting", "someday", "done"}


# --- Markdown helpers -------------------------------------------------------

FRONTMATTER_RE = re.compile(r"\A---\n(.*?)\n---\n", re.DOTALL)
PROP_RE = re.compile(r"^([a-z][a-z0-9_-]*):\s*(.*?)\s*$")
H1_RE = re.compile(r"^# (.*?)$", re.MULTILINE)
BLOCK_ITEM_RE = re.compile(r"^[ \t]+- *(.*?)\s*$")
EDITABLE_PROPERTIES = ("due", "scheduled", "reminder", "project")

# Frontmatter keys treated as scalar strings even when YAML stores a list.
# Obsidian's Properties UI can write a logically-scalar field as a
# one-element block list when the property type is "List".
SCALAR_PROPERTY_KEYS = {
    "status", "due", "scheduled", "reminder", "project",
    "created", "archived-at", "mu4e-msgid",
}


def unquote_yaml(s):
    """Strip surrounding single/double quotes from a YAML scalar."""
    s = s.strip()
    if len(s) >= 2:
        if s[0] == '"' == s[-1]:
            return s[1:-1].replace('\\"', '"')
        if s[0] == "'" == s[-1]:
            return s[1:-1]
    return s


def parse_flow_list(raw):
    """Parse `[a, b, c]` inline list. Returns the list, or None if RAW
    is not a flow list. Empty `[]` returns []."""
    m = re.match(r"\A\[(.*)\]\Z", raw)
    if not m:
        return None
    inner = m.group(1).strip()
    if not inner:
        return []
    return [unquote_yaml(x) for x in inner.split(",")]


def coerce_scalar_properties(props):
    """Collapse single-item list values of SCALAR_PROPERTY_KEYS in PROPS
    down to the first item."""
    for k in SCALAR_PROPERTY_KEYS:
        v = props.get(k)
        if isinstance(v, list) and v:
            props[k] = v[0]
    return props


def parse_task(path):
    """Parse YAML frontmatter + H1 from PATH. Return dict or None.

    Scalar values become strings; YAML arrays (inline `[a, b]` or block
    `  - a\\n  - b`) become Python lists. Known-scalar fields are coerced
    back to strings when written as single-item lists.
    """
    try:
        content = path.read_text(encoding="utf-8")
    except OSError:
        return None
    m = FRONTMATTER_RE.match(content)
    if not m:
        return None

    props = {}
    lines = m.group(1).split("\n")
    i = 0
    while i < len(lines):
        mp = PROP_RE.match(lines[i])
        if not mp:
            i += 1
            continue
        key, raw = mp.group(1), mp.group(2)
        i += 1
        if not raw:
            items = []
            while i < len(lines):
                mi = BLOCK_ITEM_RE.match(lines[i])
                if not mi:
                    break
                items.append(unquote_yaml(mi.group(1)))
                i += 1
            props[key] = items if items else ""
        elif raw.startswith("["):
            flow = parse_flow_list(raw)
            props[key] = flow if flow is not None else raw
        else:
            props[key] = unquote_yaml(raw)

    coerce_scalar_properties(props)

    body = content[m.end():]
    mh = H1_RE.search(body)
    props["title"] = mh.group(1).strip() if mh else path.stem
    props["file"] = path.name
    props["archived"] = path.parent.resolve() == ARCHIVE_DIR.resolve()
    return props


def list_tasks(directory):
    if not directory.exists():
        return []
    out = []
    for p in sorted(directory.iterdir()):
        if (p.is_file()
                and p.suffix == ".md"
                and not p.name.startswith(".")):
            t = parse_task(p)
            if t and t.get("status"):
                out.append(t)
    return out


def yaml_quote(value):
    if value and value[0] in '"\'[{>|*&!%@`':
        return '"' + value.replace('"', '\\"') + '"'
    return value


def update_property(path, key, value):
    """Set/replace/remove KEY in the frontmatter at PATH.

    VALUE of None or "" removes the property. When the key holds a
    block-style list (indented `- item' lines below), those lines are
    removed/replaced along with the header line.
    """
    content = path.read_text(encoding="utf-8")
    m = FRONTMATTER_RE.match(content)
    if not m:
        raise ValueError(f"No frontmatter in {path}")
    fm_lines = m.group(1).split("\n")
    new_lines = []
    found = False
    key_re = re.compile(rf"^{re.escape(key)}:")
    skip_block = False
    for line in fm_lines:
        if skip_block:
            if BLOCK_ITEM_RE.match(line):
                continue
            skip_block = False
        if key_re.match(line):
            found = True
            skip_block = True   # swallow any block-list items that follow
            if value:
                new_lines.append(f"{key}: {yaml_quote(value)}")
        else:
            new_lines.append(line)
    if not found and value:
        new_lines.append(f"{key}: {yaml_quote(value)}")
    new_fm = "\n".join(new_lines)
    path.write_text(content[:m.start(1)] + new_fm + content[m.end(1):],
                    encoding="utf-8")


def update_title(path, new_title):
    """Replace the H1 heading in PATH (or insert one after frontmatter)."""
    content = path.read_text(encoding="utf-8")
    new_title = (new_title or "").strip()
    if not new_title:
        raise ValueError("Title must not be empty")
    new_content, n = re.subn(
        r"^# .*$",
        f"# {new_title}",
        content,
        count=1,
        flags=re.MULTILINE,
    )
    if n == 0:
        m = FRONTMATTER_RE.match(content)
        if m:
            new_content = (content[:m.end()] + f"\n# {new_title}\n"
                           + content[m.end():])
        else:
            new_content = f"# {new_title}\n{content}"
    path.write_text(new_content, encoding="utf-8")


def edit_task(name, fields):
    """Apply multi-field edit to task NAME.

    FIELDS is a dict. Keys present cause an update; missing keys are
    left untouched.  An empty/None value for a YAML property removes
    that property.

    Supported keys: title, due, scheduled, reminder, project, status.
    Status is applied last because it can rename/move the file.
    """
    src = find_task(name)
    if src is None:
        raise FileNotFoundError(name)
    if "title" in fields:
        update_title(src, fields["title"])
    for k in EDITABLE_PROPERTIES:
        if k in fields:
            update_property(src, k, fields[k] or None)
    if "status" in fields:
        set_status(name, fields["status"])


def slugify(title):
    s = (title or "").lower()
    s = (s.replace("ä", "ae").replace("ö", "oe")
         .replace("ü", "ue").replace("ß", "ss"))
    s = re.sub(r"[^a-z0-9]+", "-", s).strip("-")
    return s or "task"


def unique_path(directory, slug, prefix=""):
    path = directory / f"{prefix}{slug}.md"
    i = 2
    while path.exists():
        path = directory / f"{prefix}{slug}-{i}.md"
        i += 1
    return path


def now_str():
    return datetime.now().strftime("%Y-%m-%d %H:%M")


def today_str():
    return datetime.now().strftime("%Y-%m-%d")


def find_task(filename):
    """Return the actual path of a task by basename (active or archive)."""
    for d in (TASKS_DIR, ARCHIVE_DIR):
        p = d / filename
        if p.exists() and p.is_file():
            # Reject path-escapes.
            if p.resolve().parent in (TASKS_DIR.resolve(),
                                      ARCHIVE_DIR.resolve()):
                return p
    return None


def archive_file(name):
    src = TASKS_DIR / name
    if not src.exists():
        raise FileNotFoundError(name)
    update_property(src, "status", "done")
    update_property(src, "archived-at", now_str())
    ARCHIVE_DIR.mkdir(parents=True, exist_ok=True)
    target = unique_path(ARCHIVE_DIR, src.stem, prefix=f"{today_str()}-")
    src.rename(target)


def unarchive_file(name, status):
    src = ARCHIVE_DIR / name
    if not src.exists():
        raise FileNotFoundError(name)
    if status not in ALLOWED_STATUSES or status == "done":
        raise ValueError(f"Invalid status: {status}")
    update_property(src, "archived-at", None)
    update_property(src, "status", status)
    base = re.sub(r"^\d{4}-\d{2}-\d{2}-", "", src.name)
    slug = base[:-3]  # strip .md
    target = unique_path(TASKS_DIR, slug)
    src.rename(target)


def capture(title):
    TASKS_DIR.mkdir(parents=True, exist_ok=True)
    slug = slugify(title)
    path = unique_path(TASKS_DIR, slug)
    body = (f"---\nstatus: inbox\ncreated: {now_str()}\n---\n\n"
            f"# {title}\n")
    path.write_text(body, encoding="utf-8")
    return path.name


def set_status(name, new_status):
    """Apply a status change: archive, unarchive, or just update property."""
    if new_status not in ALLOWED_STATUSES:
        raise ValueError(f"Invalid status: {new_status}")
    archived = (ARCHIVE_DIR / name).exists()
    active = (TASKS_DIR / name).exists()
    if new_status == "done":
        if active:
            archive_file(name)
        # If already archived: nothing to do.
    else:
        if archived:
            unarchive_file(name, new_status)
        elif active:
            update_property(TASKS_DIR / name, "status", new_status)
        else:
            raise FileNotFoundError(name)


# --- HTTP handler -----------------------------------------------------------

STATIC_FILES = {
    "/": ("index.html", "text/html; charset=utf-8"),
    "/index.html": ("index.html", "text/html; charset=utf-8"),
    "/style.css": ("style.css", "text/css; charset=utf-8"),
    "/app.js": ("app.js", "text/javascript; charset=utf-8"),
}


class Handler(BaseHTTPRequestHandler):
    def log_message(self, fmt, *args):
        sys.stderr.write(f"[tasks-web] {self.address_string()} - {fmt % args}\n")

    def _send_json(self, status, data):
        body = json.dumps(data).encode("utf-8")
        self.send_response(status)
        self.send_header("Content-Type", "application/json; charset=utf-8")
        self.send_header("Content-Length", str(len(body)))
        self.send_header("Cache-Control", "no-store")
        self.end_headers()
        self.wfile.write(body)

    def _send_file(self, name, content_type):
        path = STATIC_DIR / name
        try:
            body = path.read_bytes()
        except FileNotFoundError:
            self.send_error(404, f"missing: {name}")
            return
        self.send_response(200)
        self.send_header("Content-Type", content_type)
        self.send_header("Content-Length", str(len(body)))
        self.send_header("Cache-Control", "no-store")
        self.end_headers()
        self.wfile.write(body)

    def _read_json(self):
        length = int(self.headers.get("Content-Length", 0))
        if length == 0:
            return {}
        return json.loads(self.rfile.read(length).decode("utf-8"))

    def do_GET(self):
        path = urlparse(self.path).path
        if path in STATIC_FILES:
            name, ct = STATIC_FILES[path]
            self._send_file(name, ct)
        elif path == "/api/tasks":
            self._send_json(200, list_tasks(TASKS_DIR))
        elif path == "/api/tasks/archive":
            self._send_json(200, list_tasks(ARCHIVE_DIR))
        else:
            self.send_error(404)

    def do_POST(self):
        path = urlparse(self.path).path
        try:
            data = self._read_json()
            if path == "/api/capture":
                name = capture((data.get("title") or "").strip())
                self._send_json(200, {"file": name})
            elif path == "/api/status":
                set_status(data["file"], data["status"])
                self._send_json(200, {"ok": True})
            elif path == "/api/archive":
                archive_file(data["file"])
                self._send_json(200, {"ok": True})
            elif path == "/api/unarchive":
                unarchive_file(data["file"], data.get("status", "inbox"))
                self._send_json(200, {"ok": True})
            elif path == "/api/update":
                src = find_task(data["file"])
                if src is None:
                    raise FileNotFoundError(data["file"])
                update_property(src, data["key"], data.get("value") or None)
                self._send_json(200, {"ok": True})
            elif path == "/api/edit":
                edit_task(data["file"], data)
                self._send_json(200, {"ok": True})
            else:
                self.send_error(404)
        except KeyError as e:
            self._send_json(400, {"error": f"missing field: {e}"})
        except FileNotFoundError as e:
            self._send_json(404, {"error": str(e)})
        except ValueError as e:
            self._send_json(400, {"error": str(e)})
        except Exception as e:  # noqa: BLE001
            self._send_json(500, {"error": str(e)})


def main():
    print(f"Tasks server: http://localhost:{PORT}")
    print(f"Tasks dir:    {TASKS_DIR}")
    print(f"Archive dir:  {ARCHIVE_DIR}")
    try:
        ThreadingHTTPServer(("127.0.0.1", PORT), Handler).serve_forever()
    except KeyboardInterrupt:
        print("\nbye")


if __name__ == "__main__":
    main()
