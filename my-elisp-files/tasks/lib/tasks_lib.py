"""Shared library: markdown parsing, YAML helpers and file actions.

Imported by both the web server (../tasks-web/server.py) and the
launchd notifier (../notify/notify.py). Pure stdlib, no third-party
dependencies.

Configuration is read from environment variables once at import time
(`OBSIDIAN_DIR'); tests can either set the env before importing or
override `TASKS_DIR' / `ARCHIVE_DIR' attributes directly after reload.
"""

import os
import re
from datetime import datetime
from pathlib import Path


# --- Configuration ----------------------------------------------------------

OBSIDIAN_DIR = Path(os.environ.get(
    "OBSIDIAN_DIR",
    "/Users/kaan/Library/Mobile Documents/iCloud~md~obsidian/"
    "Documents/ObsidianOnIcloud",
))
TASKS_DIR = OBSIDIAN_DIR / "tasks"
ARCHIVE_DIR = TASKS_DIR / "archive"

ALLOWED_STATUSES = {"inbox", "next", "today", "waiting", "someday", "done"}
EDITABLE_PROPERTIES = ("due", "scheduled", "reminder", "project")

# Frontmatter keys treated as scalar strings even when YAML stores a list.
# Obsidian's Properties UI can write a logically-scalar field as a
# one-element block list when the property type is "List".
SCALAR_PROPERTY_KEYS = {
    "status", "due", "scheduled", "reminder", "project",
    "created", "archived-at", "mu4e-msgid", "waiting-since",
}

# Fixed GTD contexts. Mirror of Elisp's `my/tasks-contexts'.
CONTEXTS = ["@home", "@work", "@phone", "@computer", "@errands", "@reading"]


# --- Regexes ----------------------------------------------------------------

FRONTMATTER_RE = re.compile(r"\A---\n(.*?)\n---\n", re.DOTALL)
PROP_RE = re.compile(r"^([a-z][a-z0-9_-]*):\s*(.*?)\s*$")
H1_RE = re.compile(r"^# (.*?)$", re.MULTILINE)
BLOCK_ITEM_RE = re.compile(r"^[ \t]+- *(.*?)\s*$")


# --- Time helpers -----------------------------------------------------------

def now_str():
    return datetime.now().strftime("%Y-%m-%d %H:%M")


def today_str():
    return datetime.now().strftime("%Y-%m-%d")


# --- YAML scalar / list helpers --------------------------------------------

def unquote_yaml(s):
    """Strip surrounding single/double quotes from a YAML scalar."""
    s = s.strip()
    if len(s) >= 2:
        if s[0] == '"' == s[-1]:
            return s[1:-1].replace('\\"', '"')
        if s[0] == "'" == s[-1]:
            return s[1:-1]
    return s


def yaml_quote(value):
    if value and value[0] in '"\'[{>|*&!%@`':
        return '"' + value.replace('"', '\\"') + '"'
    return value


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


# --- Parsing ----------------------------------------------------------------

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


def list_active_tasks():
    return list_tasks(TASKS_DIR)


def list_archived_tasks():
    return list_tasks(ARCHIVE_DIR)


def find_task(filename):
    """Return the actual path of a task by basename (active or archive).
    Rejects path traversal."""
    for d in (TASKS_DIR, ARCHIVE_DIR):
        p = d / filename
        if p.exists() and p.is_file():
            if p.resolve().parent in (TASKS_DIR.resolve(),
                                      ARCHIVE_DIR.resolve()):
                return p
    return None


# --- Frontmatter writers ---------------------------------------------------

def append_log(path, old_status, new_status):
    """Append a `- DATE: old → new' audit line to the end of PATH."""
    line = f"- {now_str()}: {old_status or '?'} → {new_status}\n"
    content = path.read_text(encoding="utf-8")
    if not content.endswith("\n"):
        content += "\n"
    content += line
    path.write_text(content, encoding="utf-8")


def update_property(path, key, value):
    """Set/replace/remove KEY in the frontmatter at PATH.

    VALUE of None or "" removes the property. When the key holds a
    block-style list (indented `- item' lines below), those lines are
    removed/replaced along with the header line. A `status:' change
    appends an audit log line to the end of PATH and, when entering
    or leaving the `waiting' status, automatically sets or clears the
    `waiting-since:' field.
    """
    old_status = None
    if key == "status":
        task = parse_task(path)
        if task:
            old_status = task.get("status")
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
            skip_block = True
            if value:
                new_lines.append(f"{key}: {yaml_quote(value)}")
        else:
            new_lines.append(line)
    if not found and value:
        new_lines.append(f"{key}: {yaml_quote(value)}")
    new_fm = "\n".join(new_lines)
    path.write_text(content[:m.start(1)] + new_fm + content[m.end(1):],
                    encoding="utf-8")
    if key == "status" and value and old_status and old_status != value:
        append_log(path, old_status, value)
        if value == "waiting":
            update_property(path, "waiting-since", today_str())
        elif old_status == "waiting":
            update_property(path, "waiting-since", None)


def update_list_property(path, key, values):
    """Replace KEY in PATH's frontmatter with a YAML block list of VALUES.

    Empty/None VALUES removes the property entirely. Replaces an
    existing scalar or block-list value.
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
            skip_block = True
            if values:
                new_lines.append(f"{key}:")
                for v in values:
                    new_lines.append(f"  - {yaml_quote(v)}")
        else:
            new_lines.append(line)
    if not found and values:
        new_lines.append(f"{key}:")
        for v in values:
            new_lines.append(f"  - {yaml_quote(v)}")
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


# --- Slugs / paths ---------------------------------------------------------

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


# --- Actions ----------------------------------------------------------------

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
    else:
        if archived:
            unarchive_file(name, new_status)
        elif active:
            update_property(TASKS_DIR / name, "status", new_status)
        else:
            raise FileNotFoundError(name)


def edit_task(name, fields):
    """Apply multi-field edit to task NAME.

    FIELDS is a dict. Keys present cause an update; missing keys are
    left untouched. An empty/None value for a YAML property removes
    that property.

    Supported keys: title, due, scheduled, reminder, project,
    contexts (list), status. Status is applied last because it can
    rename/move the file.
    """
    src = find_task(name)
    if src is None:
        raise FileNotFoundError(name)
    if "title" in fields:
        update_title(src, fields["title"])
    for k in EDITABLE_PROPERTIES:
        if k in fields:
            update_property(src, k, fields[k] or None)
    if "contexts" in fields:
        values = fields["contexts"] or []
        if not isinstance(values, list):
            raise ValueError("contexts must be a list")
        update_list_property(src, "contexts", values)
    if "status" in fields:
        set_status(name, fields["status"])
