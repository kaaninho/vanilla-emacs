#!/usr/bin/env python3
"""Local HTTP server + REST API for the one-file-per-task system.

Reads markdown task files from `$OBSIDIAN_DIR/tasks/' (handled by
`tasks_lib') and serves a static kanban UI at http://localhost:8765
(override with `$TASKS_PORT'). Pure stdlib, no third-party deps.

Usage:
    python3 server.py
"""

import json
import os
import sys
from http.server import BaseHTTPRequestHandler, ThreadingHTTPServer
from pathlib import Path
from urllib.parse import urlparse

# `tasks_lib' lives in the sibling lib/ directory.
sys.path.insert(0, str(Path(__file__).resolve().parent.parent / "lib"))
import tasks_lib as lib  # noqa: E402


PORT = int(os.environ.get("TASKS_PORT", "8765"))
STATIC_DIR = Path(__file__).resolve().parent

STATIC_FILES = {
    "/": ("index.html", "text/html; charset=utf-8"),
    "/index.html": ("index.html", "text/html; charset=utf-8"),
    "/style.css": ("style.css", "text/css; charset=utf-8"),
    "/app.js": ("app.js", "text/javascript; charset=utf-8"),
}


class Handler(BaseHTTPRequestHandler):
    def log_message(self, fmt, *args):
        sys.stderr.write(
            f"[tasks-web] {self.address_string()} - {fmt % args}\n")

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
            self._send_json(200, lib.list_active_tasks())
        elif path == "/api/tasks/archive":
            self._send_json(200, lib.list_archived_tasks())
        elif path == "/api/contexts":
            self._send_json(200, lib.CONTEXTS)
        else:
            self.send_error(404)

    def do_POST(self):
        path = urlparse(self.path).path
        try:
            data = self._read_json()
            if path == "/api/capture":
                name = lib.capture((data.get("title") or "").strip())
                self._send_json(200, {"file": name})
            elif path == "/api/status":
                lib.set_status(data["file"], data["status"])
                self._send_json(200, {"ok": True})
            elif path == "/api/archive":
                lib.archive_file(data["file"])
                self._send_json(200, {"ok": True})
            elif path == "/api/unarchive":
                lib.unarchive_file(data["file"],
                                   data.get("status", "inbox"))
                self._send_json(200, {"ok": True})
            elif path == "/api/update":
                src = lib.find_task(data["file"])
                if src is None:
                    raise FileNotFoundError(data["file"])
                lib.update_property(src, data["key"],
                                    data.get("value") or None)
                self._send_json(200, {"ok": True})
            elif path == "/api/edit":
                lib.edit_task(data["file"], data)
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
    print(f"Tasks dir:    {lib.TASKS_DIR}")
    print(f"Archive dir:  {lib.ARCHIVE_DIR}")
    try:
        ThreadingHTTPServer(("127.0.0.1", PORT), Handler).serve_forever()
    except KeyboardInterrupt:
        print("\nbye")


if __name__ == "__main__":
    main()
