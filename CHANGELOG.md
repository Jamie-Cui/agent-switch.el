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
- Replaced `tabulated-list-mode` with an internal Magit-like section dashboard
  supporting TAB folding without a `magit-section` dependency.
- Added full-range, point-following current section highlighting with a
  lighter theme-aware face and no `magit-section` dependency.
- Moved Status into an always-visible top preamble, removed the dashboard
  title, and removed blank lines between sections.
- Replaced bracketed section indicators with compact disclosure glyphs and
  aligned Client names without overriding semantic status faces.
- Added Evil-aware shared structural keys while preserving native Evil
  navigation/search keys.
- Added managed Profile CRUD, external Profile registration/copying, widget
  editing, Profile ordering, semantic faces, file watchers, and diagnostics.
- Added per-Client `Default` Profile initialization from secret-safe live
  configuration and removed Import Current from the transient menu.
- Normalized missing Codex provider tables to empty Profile objects so capture,
  persistence, status matching, and activation verification agree.
- Moved operation failure details out of the dashboard and into `*Messages*`,
  and rendered the dashboard data path with the default face.
- Licensed the package under GPL-3.0-or-later with SPDX headers.
