all:
	stack install
	sudo systemctl restart site
