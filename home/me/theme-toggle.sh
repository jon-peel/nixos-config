#!/usr/bin/env bash

# Enable debug mode if DEBUG env var is set
[[ "${DEBUG}" ]] && set -x

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
NC='\033[0m' # No Color

log() {
    echo -e "${GREEN}[THEME]${NC} $*"
}

error() {
    echo -e "${RED}[ERROR]${NC} $*" >&2
}

debug() {
    echo -e "${YELLOW}[DEBUG]${NC} $*"
}

set_theme() {
    local mode="$1"

    log "Setting theme to: $mode"

    if [ "$mode" = "dark" ]; then
        # GTK 3
        debug "Setting GTK3 theme to Adwaita-dark"
        gsettings set org.gnome.desktop.interface gtk-theme "Adwaita-dark" 2>&1 | while read line; do debug "gsettings: $line"; done

        debug "Setting color-scheme to prefer-dark"
        gsettings set org.gnome.desktop.interface color-scheme "prefer-dark" 2>&1 | while read line; do debug "gsettings: $line"; done

        # GTK 4
        debug "Setting GTK4 theme"
        mkdir -p ~/.config/gtk-4.0
        echo "Adwaita-dark" > ~/.config/gtk-4.0/theme

        # GTK 2 (if needed)
        debug "Updating GTK2 settings"
        sed -i 's/gtk-theme-name=.*/gtk-theme-name="Adwaita-dark"/' ~/.gtkrc-2.0 2>/dev/null || \
            echo 'gtk-theme-name="Adwaita-dark"' >> ~/.gtkrc-2.0

        log "Dark mode set"

    elif [ "$mode" = "light" ]; then
        # GTK 3
        debug "Setting GTK3 theme to Adwaita"
        gsettings set org.gnome.desktop.interface gtk-theme "Adwaita" 2>&1 | while read line; do debug "gsettings: $line"; done

        debug "Setting color-scheme to prefer-light"
        gsettings set org.gnome.desktop.interface color-scheme "prefer-light" 2>&1 | while read line; do debug "gsettings: $line"; done

        # GTK 4
        debug "Setting GTK4 theme"
        mkdir -p ~/.config/gtk-4.0
        echo "Adwaita" > ~/.config/gtk-4.0/theme

        # GTK 2 (if needed)
        debug "Updating GTK2 settings"
        sed -i 's/gtk-theme-name=.*/gtk-theme-name="Adwaita"/' ~/.gtkrc-2.0 2>/dev/null || \
            echo 'gtk-theme-name="Adwaita"' >> ~/.gtkrc-2.0

        log "Light mode set"
    fi

    # Store current mode
    echo "$mode" > ~/.config/theme-mode

    # Verify changes
    debug "Verifying changes..."
    current_theme=$(gsettings get org.gnome.desktop.interface gtk-theme)
    current_scheme=$(gsettings get org.gnome.desktop.interface color-scheme)
    debug "Current gtk-theme: $current_theme"
    debug "Current color-scheme: $current_scheme"

    # Check if any GTK apps are running
    debug "Running GTK applications:"
    ps aux | grep -E "(firefox|thunderbird|nautilus|gedit|evince)" | grep -v grep | while read line; do
        debug "  $line"
    done
}

get_current_mode() {
    if [ -f ~/.config/theme-mode ]; then
        cat ~/.config/theme-mode
    else
        # Check current GTK theme
        current_theme=$(gsettings get org.gnome.desktop.interface gtk-theme)
        debug "Current theme from gsettings: $current_theme"
        if [[ "$current_theme" == *"dark"* ]]; then
            echo "dark"
        else
            echo "light"
        fi
    fi
}

# Check environment
debug "Environment check:"
debug "XDG_DATA_DIRS: $XDG_DATA_DIRS"
debug "GSETTINGS_SCHEMA_DIR: $GSETTINGS_SCHEMA_DIR"
debug "Available schemas:"
gsettings list-schemas | grep -E "(gtk|gnome)" | while read schema; do
    debug "  - $schema"
done

# Main logic
case "${1:-toggle}" in
    dark)
        set_theme "dark"
        ;;
    light)
        set_theme "light"
        ;;
    status)
        current=$(get_current_mode)
        log "Current mode: $current"
        debug "GTK theme: $(gsettings get org.gnome.desktop.interface gtk-theme)"
        debug "Color scheme: $(gsettings get org.gnome.desktop.interface color-scheme)"
        debug "GTK4 config: $(cat ~/.config/gtk-4.0/theme 2>/dev/null || echo 'not set')"
        ;;
    toggle|*)
        current=$(get_current_mode)
        log "Current mode: $current"
        if [ "$current" = "dark" ]; then
            set_theme "light"
        else
            set_theme "dark"
        fi
        ;;
esac

log "Theme script completed"
log "Note: Some applications may need to be restarted to reflect theme changes"
