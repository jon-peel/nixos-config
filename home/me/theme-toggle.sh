#!/usr/bin/env bash

# GTK Dark/Light Mode Toggle Script for Sway
# Toggles or sets the GTK theme preference using configuration files

# Configuration file paths
GTK3_CONFIG="$HOME/.config/gtk-3.0/settings.ini"
GTK4_CONFIG="$HOME/.config/gtk-4.0/settings.ini"

show_help() {
    cat << EOF
Usage: $0 [OPTION]

Toggle or set GTK dark/light mode preference.

Options:
  --dark          Set dark mode
  --light         Set light mode
  --help, -h, -?  Show this help message

No arguments:     Toggle between dark and light mode

Examples:
  $0              # Toggle current mode
  $0 --dark       # Set to dark mode
  $0 --light      # Set to light mode
EOF
}

ensure_config_dirs() {
    mkdir -p "$(dirname "$GTK3_CONFIG")"
    mkdir -p "$(dirname "$GTK4_CONFIG")"
}

get_current_mode() {
    # Check GTK3 config first, fallback to GTK4
    local config_file="$GTK3_CONFIG"
    [[ ! -f "$config_file" ]] && config_file="$GTK4_CONFIG"

    if [[ -f "$config_file" ]]; then
        local theme_name=$(grep -E "^gtk-theme-name" "$config_file" 2>/dev/null | cut -d= -f2)
        local dark_setting=$(grep -E "^gtk-application-prefer-dark-theme" "$config_file" 2>/dev/null)

        # Check theme name first, then fallback to dark preference
        if [[ "$theme_name" == "Adwaita-dark" ]] || [[ "$dark_setting" == *"true"* ]]; then
            echo "dark"
        else
            echo "light"
        fi
    else
        echo "light"  # Default to light if no config exists
    fi
}

set_gtk_config() {
    local config_file="$1"
    local prefer_dark="$2"
    local theme_name="$3"

    # Create config if it doesn't exist
    if [[ ! -f "$config_file" ]]; then
        cat > "$config_file" << EOF
[Settings]
gtk-theme-name=$theme_name
gtk-application-prefer-dark-theme=$prefer_dark
EOF
    else
        # Update existing config
        # Update or add theme name
        if grep -q "gtk-theme-name" "$config_file"; then
            sed -i "s/gtk-theme-name=.*/gtk-theme-name=$theme_name/" "$config_file"
        else
            if grep -q "^\[Settings\]" "$config_file"; then
                sed -i "/^\[Settings\]/a gtk-theme-name=$theme_name" "$config_file"
            else
                cat >> "$config_file" << EOF

[Settings]
gtk-theme-name=$theme_name
EOF
            fi
        fi

        # Update or add dark theme preference
        if grep -q "gtk-application-prefer-dark-theme" "$config_file"; then
            sed -i "s/gtk-application-prefer-dark-theme=.*/gtk-application-prefer-dark-theme=$prefer_dark/" "$config_file"
        else
            if grep -q "^\[Settings\]" "$config_file"; then
                sed -i "/^\[Settings\]/a gtk-application-prefer-dark-theme=$prefer_dark" "$config_file"
            else
                cat >> "$config_file" << EOF

[Settings]
gtk-application-prefer-dark-theme=$prefer_dark
EOF
            fi
        fi
    fi
}

set_mode() {
    local mode="$1"
    ensure_config_dirs

    if [[ "$mode" == "dark" ]]; then
        set_gtk_config "$GTK3_CONFIG" "true" "Adwaita-dark"
        set_gtk_config "$GTK4_CONFIG" "true" "Adwaita-dark"
        echo "Switched to dark mode (Adwaita-dark theme)"
    elif [[ "$mode" == "light" ]]; then
        set_gtk_config "$GTK3_CONFIG" "false" "Adwaita"
        set_gtk_config "$GTK4_CONFIG" "false" "Adwaita"
        echo "Switched to light mode (Adwaita theme)"
    else
        echo "Error: Invalid mode '$mode'" >&2
        exit 1
    fi
}

toggle_mode() {
    local current=$(get_current_mode)

    case "$current" in
        "dark")
            set_mode "light"
            ;;
        "light")
            set_mode "dark"
            ;;
        *)
            echo "Warning: Unknown current mode '$current', defaulting to dark mode"
            set_mode "dark"
            ;;
    esac
}

# Main script logic
case "$1" in
    --dark)
        set_mode "dark"
        ;;
    --light)
        set_mode "light"
        ;;
    --help|-h|-\?)
        show_help
        ;;
    "")
        toggle_mode
        ;;
    *)
        echo "Error: Unknown option '$1'" >&2
        echo "Use --help for usage information" >&2
        exit 1
        ;;
esac
