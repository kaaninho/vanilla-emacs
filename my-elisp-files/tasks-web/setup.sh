#!/usr/bin/env bash
# Tasks system one-shot setup.
#
# Idempotent — safe to re-run after a fresh macOS install or after
# updating any of the plists. It:
#   1. checks python3 / tsc availability
#   2. (re-)compiles app.ts → app.js if tsc is on PATH
#   3. runs the Python test suite as a sanity check
#   4. unloads any previously-loaded tasks agents
#   5. copies the plists to ~/Library/LaunchAgents/
#   6. (re-)loads both agents
#
# After this, the Web UI lives at http://localhost:8765 and the
# notifier ticks every 60 s.

set -euo pipefail

SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
LAUNCH_AGENTS="$HOME/Library/LaunchAgents"
NOTIFY_PLIST_NAME="com.kaan.tasks-notify.plist"
WEB_PLIST_NAME="com.kaan.tasks-web.plist"

step()    { printf "\n▸ %s\n" "$*"; }
ok()      { printf "  ✓ %s\n" "$*"; }
warn()    { printf "  ⚠ %s\n" "$*"; }
fail()    { printf "  ✗ %s\n" "$*" >&2; exit 1; }

step "Tasks setup ($SCRIPT_DIR)"

# 1. Dependencies -----------------------------------------------------------
step "Checking dependencies"
command -v python3 >/dev/null || fail "python3 not found (install via brew install python)"
ok "python3 → $(command -v python3) ($(python3 --version 2>&1))"

if command -v tsc >/dev/null; then
    ok "tsc     → $(command -v tsc) ($(tsc --version))"
    step "Recompiling app.ts → app.js"
    ( cd "$SCRIPT_DIR" && tsc )
    ok "app.js rebuilt"
else
    warn "tsc not on PATH — keeping the committed app.js as-is"
fi

# 2. Obsidian vault path ---------------------------------------------------
# Pick up whatever server.py considers OBSIDIAN_DIR (env or default).
TASKS_PATH=$(cd "$SCRIPT_DIR" && python3 -c "
import sys; sys.path.insert(0, '.')
from server import TASKS_DIR
print(TASKS_DIR)
")
if [ -d "$TASKS_PATH" ]; then
    ok "tasks dir exists: $TASKS_PATH"
else
    warn "tasks dir does NOT exist yet: $TASKS_PATH"
    warn "  → first run via Emacs / Obsidian will create it"
fi

# 3. Tests -----------------------------------------------------------------
step "Running Python test suite"
( cd "$SCRIPT_DIR" && python3 -m unittest test_server test_notify ) \
    || fail "tests failed — aborting setup so we don't load broken code"
ok "all tests green"

# 4. Stop existing agents (idempotent) -------------------------------------
mkdir -p "$LAUNCH_AGENTS"

unload_if_loaded() {
    local plist_name="$1"
    local target="$LAUNCH_AGENTS/$plist_name"
    local label="${plist_name%.plist}"
    if launchctl list | awk '{print $3}' | grep -qx "$label"; then
        if [ -f "$target" ]; then
            launchctl unload "$target" 2>/dev/null || true
            ok "unloaded $label"
        fi
    fi
}

step "Stopping existing agents (if loaded)"
unload_if_loaded "$NOTIFY_PLIST_NAME"
unload_if_loaded "$WEB_PLIST_NAME"

# 5. Install plists --------------------------------------------------------
step "Installing plists into $LAUNCH_AGENTS"
cp "$SCRIPT_DIR/$NOTIFY_PLIST_NAME" "$LAUNCH_AGENTS/"
ok "$NOTIFY_PLIST_NAME"
cp "$SCRIPT_DIR/$WEB_PLIST_NAME" "$LAUNCH_AGENTS/"
ok "$WEB_PLIST_NAME"

# 6. Load ------------------------------------------------------------------
step "Loading launchd agents"
launchctl load "$LAUNCH_AGENTS/$NOTIFY_PLIST_NAME"
ok "loaded com.kaan.tasks-notify"
launchctl load "$LAUNCH_AGENTS/$WEB_PLIST_NAME"
ok "loaded com.kaan.tasks-web"

# 7. Summary ---------------------------------------------------------------
printf "\n"
echo "──────────────────────────────────────────────────────────"
echo " ✅  Tasks system is up."
echo
echo "   Web UI       http://localhost:8765"
echo "   Web log      /tmp/tasks-web.log"
echo "   Notify log   /tmp/tasks-notify.log"
echo "   State file   ~/.tasks-notify-state.json"
echo
echo "   Disable:     launchctl unload $LAUNCH_AGENTS/$WEB_PLIST_NAME"
echo "                launchctl unload $LAUNCH_AGENTS/$NOTIFY_PLIST_NAME"
echo "──────────────────────────────────────────────────────────"
echo
echo "Loaded agents:"
launchctl list | awk 'NR==1 || /com\.kaan\.tasks/'
