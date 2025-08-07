# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Overview

This is a personal Doom Emacs configuration built around a literate programming approach using Org mode. The configuration is primarily defined in `config.org` which generates the actual configuration files.

## Key Architecture

### Core Configuration Pattern
- **`config.org`**: Main configuration file written in Org mode with embedded Emacs Lisp code blocks
- **`config.el`**: Generated automatically from `config.org` via org-babel-tangle (DO NOT EDIT DIRECTLY)
- **`packages.el`**: Generated from the "Packages" section in `config.org` (DO NOT EDIT DIRECTLY) 
- **`init.el`**: Doom module configuration defining which Doom features are enabled

### Configuration Generation
The config uses auto-tangling - when `config.org` is saved, it automatically generates `config.el` and `packages.el`. Always edit `config.org`, never the generated `.el` files directly.

### Secret Management
- **`secret.el`**: Contains private configuration (user info, API keys, etc.)
- **`secret.example.el`**: Template for creating the secret file
- The config loads `secret.el` and expects variables like `me/full-name` and `me/mail-address`

## Common Development Commands

### Doom Emacs Management
```bash
# Sync configuration after changes
doom sync

# Update environment variables for Doom
doom env

# Reload Doom configuration
# From within Emacs: M-x doom/reload
```

### Configuration Editing
- Edit `config.org` in Emacs, not the generated `.el` files
- When you save `config.org`, `config.el` and `packages.el` are automatically generated
- To manually tangle: `C-c C-v t` in the org file

### Language Server Setup (Elixir)
```bash
# Install Elixir Language Server
git clone https://github.com/elixir-lsp/elixir-ls.git ~/.config/elixir-ls
cd ~/.config/elixir-ls
mix deps.get && mix compile && mix elixir_ls.release -o release

# Add to PATH and update Doom environment
export PATH=$PATH:$HOME/.elixir-ls/release
doom env
```

## Important Configuration Patterns

### Module Structure in config.org
The configuration is organized into major sections:
- **Packages**: Package declarations that generate `packages.el`
- **Functions**: Custom Elisp helper functions
- **Initial Setup**: Basic user configuration and startup settings
- **Basic UI Configuration**: Fonts, themes, window behavior
- **Plugin Config**: Specific plugin configurations (evil, magit, treemacs, etc.)
- **Language Configs**: Language-specific settings (Elixir, Org mode, etc.)

### Theme and UI
- Uses Rose Pine Moon theme (`doom-rose-pine-moon`)
- Custom font setup with ComicCode Nerd Font for fixed-width
- Custom banners and quit messages loaded from resources directory

### Key Language Support
- **Elixir**: Full LSP support with auto-formatting on save
- **Org Mode**: Enhanced with custom fonts, auto-tangling, agenda integration
- **Web Development**: Polymode setup for Elixir LiveView templates

### Notable Customizations
- Evil-snipe mode disabled to preserve standard vim `s`/`S` behavior
- LSP file watching disabled to avoid constant prompts
- Treemacs configured with specific width and follow mode
- Custom tab grouping by project rather than major mode