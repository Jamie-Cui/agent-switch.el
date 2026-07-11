# agent-switch.el

[![Test](https://github.com/Jamie-Cui/agent-switch.el/actions/workflows/test.yml/badge.svg)](https://github.com/Jamie-Cui/agent-switch.el/actions/workflows/test.yml)
[![Melpazoid](https://github.com/Jamie-Cui/agent-switch.el/actions/workflows/melpazoid.yml/badge.svg)](https://github.com/Jamie-Cui/agent-switch.el/actions/workflows/melpazoid.yml)

`agent-switch.el` is an Emacs control panel for selecting provider profiles
used by LLM agent clients. It includes adapters for Claude Code, Codex, gptel
global defaults, and OpenCode global configuration. Additional clients,
adapters, and profiles can be registered entirely in Emacs Lisp.

The package is a breaking rename and redesign of `cc-switch.el`. It does not
read, import, or migrate `cc-switch.db`, and it does not provide old commands,
features, variables, or compatibility aliases.

## Requirements

- Emacs 29.1 or newer
- `transient` 0.4 or newer
- `toml` 1.0.0 or newer
- `tomelr` 0.4.3 or newer
- gptel is optional and only needed by the `gptel Default` Client

## Installation

Put the repository on `load-path`, install the dependencies, then load the
package:

```elisp
(require 'agent-switch)
```

Open the dashboard with `M-x agent-switch`.

## Dashboard

The dashboard uses an internal section model and derives from `special-mode`.
It does not depend on `magit-section` or `tabulated-list-mode`.
Always-visible status lines appear directly at the top, followed by the
collapsible Client sections and single-line Profile rows.
Client headings show inline status text.
The standard `hl-line-mode` highlights the row at point.

Common keys in Evil and non-Evil sessions:

| Key | Action |
| --- | --- |
| `TAB` | Expand or collapse a Client |
| `RET` | Edit a Profile, or toggle a Client section |
| `?` | Open the transient action menu |
| `q` | Quit the dashboard window |

Additional non-Evil keys:

| Key | Action |
| --- | --- |
| `g` | Refresh |
| `n` / `p` | Next / previous section |

In Evil normal state, `g` and `n/p` keep their native Evil behavior. The
package deliberately does not add `gr`, `C-j/C-k`, or `gj/gk` alternatives.

The `?` menu contains Apply, New, Copy, Edit, Delete, refresh, and diagnostics.
Edit visits the managed Profile JSON using the user's normal Emacs file mode.
Editing and saving never applies a Profile automatically; Apply is always
explicit. Operation failures are logged through `message` to `*Messages*` and
are not retained in the dashboard status preamble.

Opening the dashboard never creates a Profile or writes configuration. A Client
without managed, external, or discovered Profiles shows `No profiles`; use New
to create one explicitly.

## Built-in Clients

### Claude Code

The Claude Adapter manages provider-related keys below `env` in
`~/.claude/settings.json`. Permissions, hooks, MCP configuration, unrelated
environment variables, and unknown settings are preserved.

### Codex

The Codex Adapter structurally parses and merges `~/.codex/config.toml`, then
generates and reparses TOML before committing. It owns `model_provider`,
`model`, optional `small_model`, and the selected `model_providers.<id>` patch.
Sandbox, MCP, project, and unknown configuration are preserved semantically.

TOML comments, blank lines, and field order cannot be preserved by the
parse/generate path. The first rewrite of each source hash shows a diff and
requires confirmation. Every write creates a timestamped backup.

### gptel Default

The gptel Adapter only changes the global defaults of `gptel-backend` and
`gptel-model`. Existing buffer-local, file-local, preset, and one-shot values
are not modified. Profiles store a backend name and model name; backend objects
remain defined by the user's Elisp configuration.

### OpenCode Global

The OpenCode Adapter manages the global `provider.<id>` patch, `model`, and
optional `small_model` in `opencode.json` or `opencode.jsonc`. Other providers
and unknown values are preserved. Project configuration and command/session
overrides are outside this Client's scope.

## Storage

The default data directory is:

```text
~/.emacs.d/agent-switch/
├── profiles/
│   ├── claude/<profile-id>.json
│   ├── codex/<profile-id>.json
│   ├── gptel-default/<profile-id>.json
│   └── opencode-global/<profile-id>.json
└── state.json
```

Customize it with:

```elisp
(setq agent-switch-directory
      (expand-file-name "agent-switch/" user-emacs-directory))
```

Each managed Profile has its own versioned JSON file. `state.json` stores only
last selections, applied payload snapshots, recovery confirmations, and similar
small state. Client visibility is buffer-local and is not persisted.

Profile identity is `(client-id, profile-id)`, so different Clients may reuse
the same Profile ID.

## Secrets

Managed and Elisp-declared Profiles must not contain plaintext API keys,
tokens, passwords, or similar values. Use a secret reference:

```json
{
  "source": "env",
  "name": "ANTHROPIC_AUTH_TOKEN"
}
```

or:

```json
{
  "source": "auth-source",
  "host": "api.example.com",
  "user": "agent"
}
```

References are resolved only during activation. Resolved values are excluded
from Profile/state files, the dashboard, diagnostics, and sanitized error
messages.

## Safety Model

Activation follows this sequence:

1. Validate the Profile.
2. Resolve secret references.
3. Snapshot live Client state.
4. Apply the Adapter-owned patch.
5. Read the Client again and verify the selected Profile.
6. Commit the last selection to `state.json`.

Built-in Adapters restore their snapshot if activation, verification, or state
commit fails. Third-party Adapters without snapshot/rollback support are marked
as unprotected and require confirmation before first activation.

File writes compare the content hash captured at read time immediately before
an atomic same-directory rename. If another process changes a file, the write
is aborted; agent-switch never automatically merges or overwrites the external
change. Rollback uses the hash produced by agent-switch's own write and also
refuses to overwrite a later external change.

A damaged Profile file is shown as a disabled error item without blocking
other Profiles. A damaged `state.json` is treated as empty, read-only state
until the user explicitly resets it; reset first keeps a timestamped copy.

## Elisp Extensions

Adapters use a declarative callback protocol. `:current` and `:activate` are
required. Optional capabilities are `:validate`, `:discover`, `:snapshot`,
`:rollback`, `:profile-current-p`, `:watch-paths`, `:watch-setup`, and
`:profile-template`.

```elisp
(agent-switch-define-adapter my-agent
  :name "My Agent"
  :current
  (lambda (_client _context)
    ;; Return the actual provider-owned state.
    (let ((state (make-hash-table :test #'equal)))
      (puthash "model" "local/model" state)
      state))
  :activate
  (lambda (_client profile _context)
    ;; Apply (agent-switch-profile-payload profile).
    t))

(agent-switch-register-client
 'my-agent-default
 :name "My Agent Default"
 :adapter 'my-agent)
```

Declare a read-only, activatable external Profile from Elisp:

```elisp
(agent-switch-register-profile
 'my-agent-default
 'local
 :name "Local"
 :payload (let ((payload (make-hash-table :test #'equal)))
            (puthash "model" "local/model" payload)
            payload))
```

Callbacks may return a value directly or return an `agent-switch-job` for
asynchronous process/network work. The dashboard tracks pending Jobs, ignores
stale generations, and uses optional Job cancellation during cleanup.

Managed Create starts from the Adapter's optional `:profile-template` JSON
object. Edit visits the Profile JSON file directly.

## Development

```sh
make compile
make test
```

## License

GPL-3.0-or-later. See [LICENSE](LICENSE).
