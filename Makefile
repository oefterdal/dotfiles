SHELL := /bin/bash

OS        := $(shell uname -s)
BREW      := $(shell command -v brew 2>/dev/null || echo "")

# ---- packages (edit here only) ----
COMMON_PKGS := fzf stow uv zoxide tree oh-my-posh wget

# Map per package manager if needed
BREW_PKGS  := $(COMMON_PKGS)
APT_PKGS   := fzf stow python3-pip zoxide
DNF_PKGS   := fzf stow python3-pip zoxide

# Which folders to stow
STOW_DIRS ?= zsh ohmyposh

.PHONY: all bootstrap check mac-bootstrap linux-bootstrap stow

all: bootstrap stow

bootstrap:
ifeq ($(OS),Darwin)
	@$(MAKE) mac-bootstrap
else
	@$(MAKE) linux-bootstrap
endif
	@$(MAKE) check

mac-bootstrap:
	@echo "==> macOS detected"
	@echo "==> Checking Xcode Command Line Tools"
	@xcode-select -p >/dev/null 2>&1 || xcode-select --install || true
	@echo "==> Accepting Xcode license (may require sudo password)"
	@sudo xcodebuild -license accept >/dev/null 2>&1 || true
	@echo "==> Checking Homebrew"
	@if [ -z "$(BREW)" ]; then \
	  echo "==> Installing Homebrew"; \
	  /bin/bash -c "$$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install.sh)"; \
	else \
	  echo "==> Homebrew already installed: $(BREW)"; \
	fi
	@echo "==> Installing brew packages: $(BREW_PKGS)"
	@brew install $(BREW_PKGS) || true

linux-bootstrap:
	@echo "==> Linux detected"
ifneq ($(BREW),)
	@echo "==> Using existing Homebrew: $(BREW)"
	@echo "==> Installing brew packages: $(BREW_PKGS)"
	@brew install $(BREW_PKGS) || true
else
	@echo "==> Homebrew not found; trying system package manager"
	@if command -v apt-get >/dev/null 2>&1; then \
	  echo "==> Installing APT packages: $(APT_PKGS)"; \
	  sudo apt-get update && sudo apt-get install -y $(APT_PKGS); \
	elif command -v dnf >/dev/null 2>&1; then \
	  echo "==> Installing DNF packages: $(DNF_PKGS)"; \
	  sudo dnf install -y $(DNF_PKGS); \
	else \
	  echo "!! No supported package manager found. Install: $(COMMON_PKGS) manually."; \
	fi
endif

check:
	@echo "==> Checking required commands"
	@missing=0; \
	for cmd in $(COMMON_PKGS); do \
	  if ! command -v $$cmd >/dev/null 2>&1; then \
	    echo "  - MISSING: $$cmd"; \
	    missing=1; \
	  else \
	    echo "  - OK: $$cmd"; \
	  fi; \
	done; \
	if [ $$missing -ne 0 ]; then \
	  echo "==> Missing tools. Fix above before proceeding."; \
	  exit 1; \
	fi
	@echo "==> All required tools present."

stow:
	@echo "==> Stowing dotfiles into $$HOME"
	@command -v stow >/dev/null 2>&1 || { echo "stow not installed"; exit 1; }
	@for dir in $(STOW_DIRS); do \
	  if [ -d "$$dir" ]; then \
	    echo "  - stow $$dir"; \
	    stow -v -t $$HOME "$$dir"; \
	  else \
	    echo "  - skip: $$dir (no such directory)"; \
	  fi; \
	done
