#!/usr/bin/env bash

# Theme toggle script for GTK and Qt applications

set_theme() {
    local mode="$1"

    if [ "$mode" = "dark" ]; then
        # GTK 3
        gsettings set org.gnome.desktop.interface gtk-theme "Adwaita-dark"
        gsettings set org.gnome.desktop.interface color-scheme "prefer-dark"

        # GTK 4
        echo "Adwaita-dark" > ~/.config/gtk-4.0/theme

        # Qt5/Qt6 (if using qt5ct)
        sed -i 's/^style=.*/style=Adwaita-Dark/' ~/.config/qt5ct/qt5ct.conf 2>/dev/null || true

        # Notify
        notify-send "Theme" "Switched to dark mode" -i weather-clear-night

    elif [ "$mode" = "light" ]; then
        # GTK 3
        gsettings set org.gnome.desktop.interface gtk-theme "Adwaita"
        gsettings set org.gnome.desktop.interface color-scheme "prefer-light"

        # GTK 4
        echo "Adwaita" > ~/.config/gtk-4.0/theme

        # Qt5/Qt6 (if using qt5ct)
        sed -i 's/^style=.*/style=Adwaita/' ~/.config/qt5ct/qt5ct.conf 2>/dev/null || true

        # Notify
        notify-send "Theme" "Switched to light mode" -i weather-clear
    fi

    # Store current mode
    echo "$mode" > ~/.config/theme-mode
}

get_current_mode() {
    if [ -f ~/.config/theme-mode ]; then
        cat ~/.config/theme-mode
    else
        # Check current GTK theme
        current_theme=$(gsettings get org.gnome.desktop.interface gtk-theme)
        if [[ "$current_theme" == *"dark"* ]]; then
            echo "dark"
        else
            echo "light"
        fi
    fi
}

# Main logic
case "${1:-toggle}" in
    dark)
        set_theme "dark"
        ;;
    light)
        set_theme "light"
        ;;
    toggle|*)
        current=$(get_current_mode)
        if [ "$current" = "dark" ]; then
            set_theme "light"
        else
            set_theme "dark"
        fi
        ;;
esac
