CONFIG_FILE = ./config.scm

# Add ./ and ../rde to GUILE_LOAD_PATH
RDE = ../rde
GLP = ./:$(RDE)

.DEFAULT_GOAL := hr

# hr: home reconfigure
# hb: home build
# sr: system reconfigure
# sb: system build

hr:
	GUILE_LOAD_PATH=$(GLP) TARGET=${HOSTNAME}-he \
			guix time-machine -C channels/channels-lock.scm -- \
			home reconfigure $(CONFIG_FILE)

hb:
	GUILE_LOAD_PATH=$(GLP) TARGET=${HOSTNAME}-he \
			guix time-machine -C channels/channels-lock.scm -- \
			home build $(CONFIG_FILE)

sr:
	GUILE_LOAD_PATH=$(GLP) TARGET=${HOSTNAME}-os \
			sudo -E guix time-machine -C channels/channels-lock.scm -- \
			system reconfigure $(CONFIG_FILE)

sb:
	GUILE_LOAD_PATH=$(GLP) TARGET=${HOSTNAME}-os \
			guix time-machine -C channels/channels-lock.scm -- \
			system build $(CONFIG_FILE)

channels-pull:
	guix pull -C channels/channels-lock.scm

channels-update-lock:
	guix time-machine -C channels/channels.scm -- \
	describe -f channels > channels/channels-lock.scm
