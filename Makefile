default: install build

run:
	node .

nix:
	nix-shell --run make

install:
	npm install
	psc-package install

build:
	purp bundle
