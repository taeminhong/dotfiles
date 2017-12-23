if [ -o emacs ]; then
    # roll back keybindings
    bind '"\C-t": transpose-chars'
    bind '"\ec": capitalize-word'

    # Alt-j cd into the selected directory
    bind '"\ej": " \C-e\C-u`__fzf_cd__`\e\C-e\er\C-m"'

    # Alt-i - Paste the selected file path into the command line
    if [ $BASH_VERSINFO -gt 3 ]; then
        bind -x '"\ei": "fzf-file-widget"'
    elif __fzf_use_tmux__; then
        bind '"\ei": " \C-u \C-a\C-k`__fzf_select_tmux__`\e\C-e\C-y\C-a\C-d\C-y\ey\C-h"'
    else
        bind '"\ei": " \C-u \C-a\C-k`__fzf_select__`\e\C-e\C-y\C-a\C-y\ey\C-h\C-e\er \C-h"'
    fi
fi
