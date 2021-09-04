all:
	stack install
	systemctl --user restart site
