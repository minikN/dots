# profiles.mk provides guix version specified by rde/channels-lock.scm
# To rebuild channels-lock.scm use `make -B rde/channels-lock.scm`
include profiles.mk

# Also defined in .envrc to make proper guix version available project-wide
GUIX_PROFILE=target/profiles/guix
GUIX=GUILE_LOAD_PATH="../rde/src:../rde/tests:." GUILE_LOAD_COMPILED_PATH="" ${GUIX_PROFILE}/bin/guix

HOST=$(shell cat /etc/hostname)
NPROCS=$(shell grep -c ^processor /proc/cpuinfo)

CONFIGS=./config.scm
PULL_EXTRA_OPTIONS=
# --allow-downgrades

ROOT_MOUNT_POINT=/mnt

VERSION=latest

# hr: home reconfigure
# hb: home build
# sr: system reconfigure
# sb: system build

repl:
	${GUIX} repl \
	-L ../files/emacs/gider/src --listen=tcp:37146

ares-rs:
	${GUIX} shell guile-next guile-ares-rs \
	-e '(@ (rde packages package-management) guix-from-channels-lock)' \
	-- guile \
	-c "((@ (nrepl server) run-nrepl-server) #:port 7888)"

hb: guix
	TARGET=${HOST}-he ${GUIX} home \
	build --cores=${NPROCS} ${CONFIGS}

hr: guix
	TARGET=${HOST}-he ${GUIX} home \
	reconfigure --cores=${NPROCS} ${CONFIGS}

sb: guix
	TARGET=${HOST}-os ${GUIX} system \
	build --cores=${NPROCS} ${CONFIGS}

sr: guix
	TARGET=${HOST}-os ${GUIX} system \
	reconfigure --cores=${NPROCS} ${CONFIGS}

cow-store:
	sudo herd start cow-store ${ROOT_MOUNT_POINT}

init: guix
	TARGET=${HOSTNAME}-os ${GUIX} system \
	init ${CONFIGS} ${ROOT_MOUNT_POINT}

target:
	mkdir -p target

clean-target:
	rm -rf ./target

clean: clean-target
