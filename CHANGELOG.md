# Changelog

## 0.1.0 - Unreleased

- Renamed the package and public namespace to `agent-switch`.
- Removed all SQLite, `cc-switch.db`, old command, feature, variable, and
  compatibility paths.
- Added versioned per-Profile JSON storage and separate state storage below
  `user-emacs-directory`.
- Added extensible Client, Adapter, Profile, and synchronous/asynchronous Job
  protocols.
- Added built-in Claude Code, Codex, gptel Default, and OpenCode Global
  Adapters with transactional activation and recovery.
- Replaced `tabulated-list-mode` with an internal section dashboard supporting
  Client folding without a `magit-section` dependency.
- Used standard `hl-line-mode` row highlighting and single-line Profile rows.
- Moved Status into an always-visible top preamble, removed the dashboard
  title, and removed blank lines between sections.
- Rendered Client status inline without overriding semantic status faces.
- Added Evil-aware shared structural keys while preserving native Evil
  navigation/search keys.
- Added managed Profile CRUD, external Profile registration/copying, direct
  JSON editing, semantic faces, file watchers, and diagnostics.
- Kept dashboard reads side-effect free; Profiles are created only by explicit
  user actions.
- Normalized missing Codex provider tables to empty Profile objects so
  persistence, status matching, and activation verification agree.
- Moved operation failure details out of the dashboard and into `*Messages*`,
  and rendered the dashboard data path with the default face.
- Changed `RET` on a Profile to visit its managed JSON for editing and removed
  the separate Profile summary buffer.
- Made rollback preserve post-apply external changes through optimistic hashes.
- Removed speculative Adapter fields, hidden commands, manual Profile ordering,
  custom coverage infrastructure, and generated files from the package recipe.
- Licensed the package under GPL-3.0-or-later with SPDX headers.
