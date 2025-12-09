# Dotfiles

Personal macOS + Linux setup using GNU Stow for symlinking and a Makefile for bootstrapping.

## What this repo does

- Installs minimum tools (fzf, stow, uv, zoxide)
- Installs platform dependencies:
  - macOS: Xcode Tools, license accept, Homebrew
  - Linux: Tries Homebrew, APT, or DNF
- Symlinks dotfiles into `$HOME` using `stow`

## Requirements

- macOS or Linux
- `make` available
- This repo checked out somewhere (e.g. `~/.dotfiles`)

## Bootstrap

This installs everything needed:

```sh
make bootstrap
```

## Stow the dotfiles

```sh
make stow
```

## Csutomize which folders to stow:

```sh
make stow STOW_DIRS="zsh git nvim"
```

## Structure

Each dotfile set lives inside a folder

```sh
zsh/
nvim/
git/
...
```

Inside each folder you place your dotfiles with the same structure they should have in $HOME.

## Example:

```sh
zsh/.zshrc
nvim/.config/nvim/init.lua
```

## Stowing creates Symlinks

```sh
~/.zshrc  →  dotfiles/zsh/.zshrc
```

## Notes

- The Makefile centralizes package names so you only maintain them once
- macOS install may prompt for sudo or GUI confirmation if Xcode Tools are missing
- If Linux doesn’t have a supported package manager, install the tools manually
