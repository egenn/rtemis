get_macos_appearance <- function() {
  system("defaults read -g AppleInterfaceStyle 2>/dev/null || echo 'Light'")
}

macos_toggle_dark_mode <- function() {
  system(
    'osascript -e \'tell application "System Events" to tell appearance preferences to set dark mode to not dark mode\''
  )
}
