
#
# Profiles
#

# Store items doesn't have useful mtime, so we rely on guix.lock to prevent
# unecessary rebuilds
guix: target/guix-time-marker

target/profiles:
	mkdir -p target/profiles

target/guix-time-marker: channels/channels-lock.scm
	make target/profiles/guix
	touch $@

target/profiles/guix: target/profiles channels/channels-lock.scm
	guix pull -C channels/channels-lock.scm -p ${GUIX_PROFILE} \
	${PULL_EXTRA_OPTIONS}

target/profiles/guix-local: target/profiles channels/channels-lock-local.scm
	guix pull -C channels/channels-lock-local.scm -p ${GUIX_PROFILE} \
	${PULL_EXTRA_OPTIONS}

channels/channels-lock.scm: channels/channels.scm
	echo -e "(use-modules (guix channels))\n" > ./channels/channels-lock-tmp.scm
	guix time-machine -C ./channels/channels.scm -- \
	describe -f channels >> ./channels/channels-lock-tmp.scm
	mv ./channels/channels-lock-tmp.scm ./channels/channels-lock.scm

channels/channels-lock-local.scm: channels/channels-local.scm
	echo -e "(use-modules (guix channels))\n" > ./channels/channels-lock-tmp.scm
	guix time-machine -C ./channels/channels-local.scm -- \
	describe -f channels >> ./channels/channels-lock-tmp.scm
	mv ./channels/channels-lock-tmp.scm ./channels/channels-lock-local.scm
