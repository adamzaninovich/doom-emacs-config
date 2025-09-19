# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Repository Overview

This is a personal Doom Emacs configuration. Doom Emacs is a configuration framework for GNU Emacs that provides a cohesive, performant, and polished experience while maintaining compatibility with vanilla Emacs. This specific configuration focuses on:

1. Vim-like editing experience (Evil mode)
2. Elixir development workflow
3. Org-mode for documentation and literate programming
4. Modern UI elements (modeline, tabs, themes)
5. Integration with Claude Code IDE

## Key Files and Directories

- `config.org`: Main configuration file (literate programming)
- `config.el`: Generated from `config.org` (do not edit directly)
- `init.el`: Doom modules configuration
- `packages.el`: Package declarations (also generated from `config.org`)
- `secret.el`: Private configuration (not committed to git)
- `themes/`: Custom theme files

## Common Commands

### Doom Emacs Commands

```bash
# Update Doom configuration after changes
doom sync

# Check Doom installation health
doom doctor

# Upgrade Doom and its packages
doom upgrade

# Update environment variables for external tools
doom env

# Rebuild Doom configuration
doom build
```

### Emacs Key Bindings

- `SPC` is the leader key (followed by other keys for commands)
- `SPC h d h` to access Doom's documentation
- `SPC a c` to access Claude Code IDE
- `SPC t t` to toggle tabs
- `s-{` and `s-}` to navigate tabs
- `-` to open Dired (file explorer) at current location

## Configuration Architecture

This configuration uses a literate programming approach with `config.org` as the source file. When saved, it automatically tangles to `config.el` and `packages.el`. The configuration is organized into the following sections:

1. **Basic Settings**: User information, frame size, deletion behavior
2. **UI Configuration**: Fonts, themes, modeline settings, line numbers
3. **Editor Behavior**: Evil mode configuration, indentation settings
4. **Org Mode Setup**: Document structure, agenda configuration, export settings
5. **Language Support**: Focused on Elixir with optional LSP integration
6. **Package Configuration**: Custom settings for installed packages

## Key Workflows

### Elixir Development

- Elixir format-on-save is enabled by default
- Polymode is configured to handle web templates in Elixir files (.ex, .eex, .heex)
- LSP integration is available but disabled by default (can be enabled in init.el)
- Key binding: `SPC m f` to format Elixir code

### Org Mode

- Auto-tangles configuration files on save
- Visual enhancements for readability (variable pitch fonts, visual fill column)
- Custom templates for code blocks (e.g., "el" for emacs-lisp, "iex" for elixir)
- Habit tracking integration with FlatHabits

### Claude Code Integration

- Integrated with `claude-code-ide` package
- Configured to use `eat` terminal backend
- Accessible via `SPC a c`

## Project Structure

The configuration uses projectile for project management:

- Projects are searched in `~/projects/` and `~/campaigns/`
- Project-specific configurations are available (e.g., for the "printserver" project)
- Centaur tabs groups tabs by project

## Extension Notes

If you modify this configuration:

1. Always edit `config.org` rather than `config.el` or `packages.el`
2. If editing org files outside of Emacs (e.g., via Claude Code), manually tangle the file after changes:
   ```elisp
   # To tangle config.org to config.el and packages.el
   emacs --batch --eval "(require 'org)" --eval '(org-babel-tangle-file "/Users/a.zaninovich/.config/doom/config.org")'
   ```
3. Run `doom sync` after adding new packages or modules
4. Keep secrets in `secret.el` (not tracked by git)
5. Add new custom themes to the `themes/` directory