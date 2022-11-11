# See junegunn's suggestion for remapping
# https://github.com/junegunn/fzf/issues/546#issuecomment-213289351

# Default readline key bindings
# https://www.man7.org/linux/man-pages/man3/readline.3.html#DEFAULT_KEY_BINDINGS

# Bash 5.1.16 seems to have a bug that overwritting a shell command binding
# breaks the keymap. To workaround, we need to delete the binding first.
#
# Example:
#   $ bind -x '"\C-h": echo "hello"'
#   $ bind -x '"\C-w": echo "world"'
#   $ bind '"\C-w": transpose-chars' # <- overwrite
#   $ bind -X
#   "\C-w": "echo \"world\""
#   bash: bash_execute_unix_command: cannot find keymap for command # <- when pressing C-h
#
# Workaround:
#   $ bind -x '"\C-h": echo "hello"'
#   $ bind -x '"\C-w": echo "world"'
#   $ bind -r "\C-w" # <- delete it first
#   $ bind '"\C-w": transpose-chars'
#   $ bind -X
#   "\C-h": "echo \"hello\""

dup_bind () {
    # escape backslashes
    local from="${2//\\/\\\\}"
    local to="${3//\\/\\\\}"
    local match1="$(bind -m "$1" -s | grep "^\"$from")"
    [ -n "$match1" ] && bind -m "$1" "$(echo "$match1" | sed "s/$from/$to/g")"
    local match2="$( (( BASH_VERSINFO[0] > 3 )) && bind -m "$1" -X | grep "^\"$from")"
    [ -n "$match2" ] && bind -m "$1" -x "$(echo "$match2" | sed "s/$from/$to/")"
}

# Ctrl-R -> Alt-P
# Paste the selected command from history into the command line
for keymap in emacs vi vi-insert; do
    dup_bind "$keymap" "\C-r" "\ep"
    bind -m "$keymap" -r "\C-r"
    bind -m "$keymap" '"\C-r": reverse-search-history'
done

# Alt-C -> Alt-J
# cd into the selected directory
for keymap in emacs vi vi-insert; do
    dup_bind "$keymap" "\ec" "\ej"
    bind -m "$keymap" -r "\ec"
done
bind -m emacs '"\ec": capitalize-word'

# Ctrl-T -> Alt-I
# Paste the selected file path into the command line
for keymap in emacs vi vi-insert; do
    dup_bind "$keymap" "\C-t" "\ei"
    bind -m "$keymap" -r "\C-t"
    bind -m "$keymap" '"\C-t": transpose-chars'
done
