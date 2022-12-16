CONFIG_FILE = ./config.scm

# Add ./ and ../rde to GUILE_LOAD_PATH
RDE 	:=	../rde/src
GLP 	:=	./:$(RDE)
NPROCS	:=	$(shell grep -c ^processor /proc/cpuinfo)

.DEFAULT_GOAL := hr

# hr: home reconfigure
# hb: home build
# sr: system reconfigure
# sb: system build

hr:
	GUILE_LOAD_PATH=$(GLP) TARGET=${HOSTNAME}-he \
			guix time-machine -C channels/channels-lock.scm -- \
			home reconfigure --cores=$(NPROCS) $(CONFIG_FILE)

sr:
	GUILE_LOAD_PATH=$(GLP) TARGET=${HOSTNAME}-os \
			sudo -E guix time-machine -C channels/channels-lock.scm -- \
			system reconfigure --cores=$(NPROCS) $(CONFIG_FILE)

hb:
	GUILE_LOAD_PATH=$(GLP) TARGET=${HOSTNAME}-he \
			guix time-machine -C channels/channels-lock.scm -- \
			home build --cores=$(NPROCS) $(CONFIG_FILE)


sb:
	GUILE_LOAD_PATH=$(GLP) TARGET=${HOSTNAME}-os \
			guix time-machine -C channels/channels-lock.scm -- \
			system build --cores=$(NPROCS) $(CONFIG_FILE)

channels-pull:
	guix pull -C channels/channels-lock.scm

channels-update-lock:
	guix time-machine -C channels/channels.scm -- \
	describe -f channels > channels/channels-lock.scm
