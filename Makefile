workspace = workspace

default: $(workspace)
	rsync -Rpgol \
		.bash_aliases \
		.bash_logout \
		.bash_profile \
		.bashrc \
		.profile \
		.emacs \
		.emacs.d/sensible-defaults.el \
		.emacs.d/move-lines.el \
		.gitconfig \
		.gitignore_global \
		.ssh/config \
		.tmux.conf \
		.fzf-keybinding-patch.bash \
		$(workspace)

Mac: default
	cat $@/.bash_aliases >> $(workspace)/.bash_aliases
	cat $@/.bashrc >> $(workspace)/.bashrc
	cat $@/.profile >> $(workspace)/.profile

Linux: default
	cat $@/.bash_aliases >>$(workspace)/.bash_aliases
	cat $@/.profile >>$(workspace)/.profile
	cp -a $@/keyboard $(workspace)

Windows: default
	cat $@/.gitconfig >>$(workspace)/.gitconfig
	cp -a $@/.minttyrc $(workspace)

clean:
	-rm -rf $(workspace)

workspace:
	mkdir $@

.PHONY: clean default
.SILENT:
