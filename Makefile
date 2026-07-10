EMACS ?= emacs
EMACS_BATCH = $(EMACS) -Q --batch
COVERAGE_DIR ?= coverage
COVERAGE_MIN ?= 0

# Auto-detect package dependency paths for local batch runs.
TRANSIENT_DIR ?= $(shell find $(HOME)/.emacs.d/elpa -maxdepth 1 -name 'transient-*' -type d 2>/dev/null | head -1)
TOML_DIR ?= $(shell find $(HOME)/.emacs.d/elpa -maxdepth 1 -name 'toml-*' -type d 2>/dev/null | head -1)
TOMELR_DIR ?= $(shell find $(HOME)/.emacs.d/elpa -maxdepth 1 -name 'tomelr-*' -type d 2>/dev/null | head -1)
GPTEL_DIR ?= $(shell find $(HOME)/.emacs.d/elpa -maxdepth 1 -name 'gptel-*' -type d 2>/dev/null | grep -vE 'gptel-(agent|magit)-' | head -1)
COND_LET_DIR ?= $(shell find $(HOME)/.emacs.d/elpa -maxdepth 1 -name 'cond-let-*' -type d 2>/dev/null | head -1)
COMPAT_DIR ?= $(shell find $(HOME)/.emacs.d/elpa -maxdepth 1 -name 'compat-*' -type d 2>/dev/null | head -1)
LLAMA_DIR ?= $(shell find $(HOME)/.emacs.d/elpa -maxdepth 1 -name 'llama-*' -type d 2>/dev/null | head -1)

LOADPATH = -L . \
	$(if $(TRANSIENT_DIR),-L $(TRANSIENT_DIR)) \
	$(if $(TOML_DIR),-L $(TOML_DIR)) \
	$(if $(TOMELR_DIR),-L $(TOMELR_DIR)) \
	$(if $(GPTEL_DIR),-L $(GPTEL_DIR)) \
	$(if $(COND_LET_DIR),-L $(COND_LET_DIR)) \
	$(if $(COMPAT_DIR),-L $(COMPAT_DIR)) \
	$(if $(LLAMA_DIR),-L $(LLAMA_DIR))

BYTE_COMPILE_FLAGS = --eval "(setq byte-compile-error-on-warn nil)"

SRCS = agent-switch-core.el agent-switch-storage.el agent-switch-adapters.el agent-switch-ui.el agent-switch.el
COMPILED = $(SRCS:.el=.elc)

.PHONY: all compile clean test test-unit coverage help

all: compile

help:
	@echo "agent-switch.el"
	@echo ""
	@echo "Targets:"
	@echo "  compile    - Byte compile Elisp files"
	@echo "  test       - Run unit tests"
	@echo "  test-unit  - Run batch unit tests"
	@echo "  coverage   - Run ERT under built-in testcover"
	@echo "  clean      - Remove compiled files and coverage output"
	@echo "  help       - Show this help message"

compile: $(COMPILED)
	@echo "Compilation complete: $(words $(COMPILED)) files"

%.elc: %.el
	@echo "Compiling $<..."
	@out=$$($(EMACS_BATCH) $(LOADPATH) $(BYTE_COMPILE_FLAGS) -f batch-byte-compile $< 2>&1); \
	status=$$?; \
	printf "%s\n" "$$out" | grep -v "^Compiling" | grep -v "^Wrote" || true; \
	exit $$status

test: test-unit

test-unit:
	@echo "Running unit tests..."
	@$(EMACS_BATCH) $(LOADPATH) \
		-l ert \
		-l agent-switch.el \
		-l test/agent-switch-test.el \
		-f ert-run-tests-batch-and-exit

coverage:
	@echo "Running coverage..."
	@$(EMACS_BATCH) $(LOADPATH) \
		--eval "(setq agent-switch-coverage-directory \"$(COVERAGE_DIR)\" agent-switch-coverage-min $(COVERAGE_MIN))" \
		-l test/coverage.el

clean:
	@echo "Cleaning generated files..."
	@rm -f $(COMPILED)
	@rm -rf $(COVERAGE_DIR)
