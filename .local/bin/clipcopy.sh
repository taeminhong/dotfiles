#!/bin/sh

has () {
    command -v "$1" >/dev/null 2>&1
}

if has clip.exe
then
    exec clip.exe
elif has pbcopy
then
    exec pbcopy
elif [ -n "$DISPLAY" ] && has xsel
then
    exec xsel --clipboard
elif [ -n "$DISPLAY" ] && has xclip
then
    exec xclip -sel clipboard
elif [ -n "$WAYLAND_DISPLAY" ] && has wl-copy
then
    exec wl-copy
fi

# Drain stdin, or Emacs's process-send-string fails when this exits early.
cat >/dev/null
exit 1
