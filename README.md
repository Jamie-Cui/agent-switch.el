# cc-switch.el
Switch model provider in Emacs way, like cc-switch

本项目意图提供同 /home/jamie/proj/cc-switch-cli/ 类似的功能，只是将其转换成 emacs package，用 elisp 实现其对应的逻辑即 ui

`cc-switch.el` is a pure Emacs Lisp companion for
[cc-switch-cli](https://github.com/SaladDay/cc-switch-cli).  It reads the
existing cc-switch SQLite database and switches Claude Code or Codex providers
from Emacs, without shelling out to the `cc-switch` binary.

## Status

V1 is intentionally narrow:

- supported apps: Claude Code and Codex
- supported workflow: list providers, show current provider, switch provider,
  export a Claude provider, and run local diagnostics
- UI: a `tabulated-list-mode` dashboard with a `transient` command menu, plus
  `completing-read` fallback commands
- storage: existing `~/.cc-switch/cc-switch.db`

It does not implement provider CRUD, proxy/daemon takeover, MCP, prompts,
skills, OAuth login, speed tests, stream checks, WebDAV sync, or migration from
legacy `config.json`.

## Requirements

- Emacs 29.1 or newer
- built-in `sqlite` and `json`
- `transient`
- existing cc-switch SQLite database at `~/.cc-switch/cc-switch.db`, or at
  `$CC_SWITCH_CONFIG_DIR/cc-switch.db`
- Codex switching additionally requires `toml.el`

If `~/.cc-switch/config.json` exists but `cc-switch.db` does not, run
cc-switch-cli once first so it can perform its own migration.

## Installation

Place this repository on `load-path`:

```elisp
(add-to-list 'load-path "/path/to/cc-switch.el")
(require 'cc-switch)
```

For Codex support, install the `toml` package before switching Codex providers.
Claude commands continue to work when `toml.el` is unavailable.

## Commands

- `M-x cc-switch`
- `M-x cc-switch-provider-list`
- `M-x cc-switch-provider-current`
- `M-x cc-switch-provider-switch`
- `M-x cc-switch-use`
- `M-x cc-switch-switch-claude`
- `M-x cc-switch-switch-codex`
- `M-x cc-switch-provider-export`
- `M-x cc-switch-diagnose`

Generic commands prompt for `claude` or `codex` every time.  Use
`cc-switch-switch-claude` or `cc-switch-switch-codex` when you want a fixed-app
shortcut.

`M-x cc-switch` opens the dashboard.  In the dashboard:

- `?` opens the transient menu
- `g` refreshes
- `RET` shows secret-safe provider details
- `s` switches to the provider at point
- `S` chooses an app and provider to switch
- `e` exports the Claude provider at point
- `d` opens diagnostics
- `o` opens the live config file
- `b` opens the single-file backup when it exists
- `q` quits the window

## Configuration

```elisp
(setq cc-switch-config-dir "~/.cc-switch")
(setq cc-switch-claude-config-dir "~/.claude")
(setq cc-switch-codex-home "~/.codex")
```

When a directory option is nil, `cc-switch.el` follows the corresponding
environment variable where applicable:

- `CC_SWITCH_CONFIG_DIR`
- `CLAUDE_CONFIG_DIR`
- `CODEX_HOME`, only when it points at an existing directory

## Safety Model

`cc-switch.el` writes live config files directly, so V1 stays conservative:

- refuses to switch if the target app config directory does not already exist
- refuses to switch when cc-switch proxy takeover/live backup state is detected
- writes live config first, then updates SQLite `is_current`
- rolls live files back if the DB update fails
- writes via temporary file plus rename
- keeps one sibling backup named `.<filename>.cc-switch-el.bak`
- never prints provider `settings_config`, API keys, or tokens in candidates,
  diagnostics, or error messages

Claude switching writes `settings.json`.  Codex switching writes `config.toml`;
official Codex providers may also update `auth.json`, while third-party
providers preserve existing `auth.json` to avoid clobbering ChatGPT OAuth state.

## TODO

### P0: correctness and safety

- Make Codex common-config handling structural instead of textual prepend, so
  duplicate TOML keys and nested `model_provider` / `model_providers` tables are
  merged predictably.
- Add focused tests for rollback and atomic-write failure paths, including DB
  update failure after live files have been written.
- Add tests for Claude export, proxy-block refusal, official Codex `auth.json`
  write/delete behavior, and diagnostics around missing or legacy config state.
- Decide whether `toml.el` should remain an optional Codex-only dependency or be
  declared as a package dependency for simpler installation.

### P1: cc-switch feature parity

- Provider CRUD: add, edit, duplicate, delete, reorder, and endpoint management.
- Proxy/daemon integration: show daemon status, understand takeover state, and
  support a safe hot-switch path when cc-switch proxy management is active.
- MCP sync.
- Prompt management.
- Skills management.
- WebDAV sync.
- Speed tests and stream checks.

### P2: broader app and auth support

- Gemini, OpenCode, Hermes, and OpenClaw provider switching.
- Richer Codex auth handling, including official OAuth helpers and safer
  migration between ChatGPT OAuth and third-party API-key providers.
- Legacy `config.json` read-only import or migration helper.

### P3: UI and release polish

- Further dashboard polish, including richer grouping, filtering, and optional
  provider CRUD actions once the write paths are implemented.
- Package metadata and release workflow, including package-lint/checkdoc cleanup.
- User-facing recovery command for restoring `.<filename>.cc-switch-el.bak`
  backups.

## Development

Use the Makefile targets:

```bash
make compile
make test-unit
make coverage
```

Run tests:

```bash
make test
```

Byte-compile:

```bash
make compile
```
