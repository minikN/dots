CONFIG_FILE = ./config.scm
GLP = ./

%-home-build:
	GUILE_LOAD_PATH=$(GLP) TARGET=$*-he \
			guix home build $(CONFIG_FILE)

%-home-reconfigure:
	GUILE_LOAD_PATH=$(GLP) TARGET=$*-he \
			guix home reconfigure $(CONFIG_FILE)

%-system-build:
	GUILE_LOAD_PATH=$(GLP) TARGET=$*-os \
			guix system build $(CONFIG_FILE)

%-system-reconfigure:
	GUILE_LOAD_PATH=$(GLP) TARGET=$*-os \
			sudo -E guix system reconfigure $(CONFIG_FILE)
