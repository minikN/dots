# About
This is my entire system configuration using [GNU/Guix](https://guix.gnu.org/) and [RDE](https://sr.ht/abcdw/rde/).
- My (archived) literate config is available [here](https://github.com/minikN/guix/tree/literate).
- An attempt of a vanilla config is available [here](https://github.com/minikN/guix/tree/vanilla).

# Installation
1. This repo relies on the official rde repository being present. If this repo is located at `~/git/guix`, then it expects the rde repo to be available at `~/git/rde`. You can change this in the [Makefile](https://github.com/minikN/guix/blob/main/Makefile#L4).
2. `cd /path/to/this/repo`
3. `make <machine>-<system/home>-<build/reconfigure>`, e.g. `make geekcave-home-reconfigure`.
4. (optional) Add new machines [here](https://github.com/minikN/guix/blob/main/config.scm#L236-L253).
