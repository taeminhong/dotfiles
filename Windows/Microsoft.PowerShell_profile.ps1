if (Get-Command Set-PSReadlineOption -ErrorAction SilentlyContinue) {
    Set-PSReadLineOption -EditMode Emacs
}