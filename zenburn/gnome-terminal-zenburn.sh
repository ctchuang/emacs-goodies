#!/usr/bin/env bash
# Reference:
#   - http://www.growingwiththeweb.com/2015/05/colours-in-gnome-terminal.html
#   - https://askubuntu.com/questions/731774/how-to-change-gnome-terminal-profile-preferences-using-dconf-or-gsettings
#   - https://gist.github.com/manuraj17/0e410c3a4d653d2b8f331d32f6a12ea3

set -e

# Find profile UUID:
# $ dconf dump /org/gnome/terminal/legacy/profiles:/
# Do not miss the prefixing colon (:).
#
# or try
# $ dconf dump /org/gnome/terminal/legacy/profiles:/:${UUID}/
#
UUID='TO-BE-DECIDED'

# set palette
dconf write /org/gnome/terminal/legacy/profiles:/:${UUID}/palette "['rgb(63,63,63)','rgb(204,147,147)','rgb(127,159,127)','rgb(227,206,171)','rgb(223,175,143)','rgb(204,147,147)','rgb(140,208,211)','rgb(220,220,204)','rgb(63,63,63)','rgb(204,147,147)','rgb(127,159,127)','rgb(227,206,171)','rgb(223,175,143)','rgb(204,147,147)','rgb(140,208,211)','rgb(220,220,204)']"

# set foreground, background and highlight color
dconf write /org/gnome/terminal/legacy/profiles:/:${UUID}/background-color "'#202020202020'"
dconf write /org/gnome/terminal/legacy/profiles:/:${UUID}/foreground-color "'#DCDCDCDCCCCC'"
dconf write /org/gnome/terminal/legacy/profiles:/:${UUID}/bold-color "'#E3E3CECEABAB'"

# # make sure the profile is set to not use theme colors
dconf write /org/gnome/terminal/legacy/profiles:/:${UUID}/use-theme-colors "false"
dconf write /org/gnome/terminal/legacy/profiles:/:${UUID}/use-theme-transparency "false"

