NAME=etcgit

bindir=$(prefix)/bin
sbindir=$(prefix)/sbin

all:
clean:
install: install-module install-bin install-sbin install-etc
check: check-module

include /usr/share/alterator/build/module.mak

install-sbin:
	for i in sbin/*; do install -Dpm755 $$i $(sbindir)/`basename $$i` ;done

install-bin:
	for i in bin/*; do install -Dpm755 $$i $(bindir)/`basename $$i` ;done

install-etc: install-etckeeper

install-etckeeper:
	cd etc; for f in `find etckeeper/ -type f`; do install -D -m0755 $$f $(sysconfdir)/$$f; done
