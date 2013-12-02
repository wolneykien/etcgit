NAME=etcgit

bindir=$(prefix)/bin
sbindir=$(prefix)/sbin
sysconfdir=/etc
datadir=$(prefix)/share

etcgitdatadir=$(datadir)/etcgit

all:
clean:
install: install-bin install-sbin install-data install-etc


install-sbin:
	for i in sbin/*; do install -Dpm755 $$i $(sbindir)/`basename $$i` ;done

install-bin:
	for i in bin/*; do install -Dpm755 $$i $(bindir)/`basename $$i` ;done

install-etc: install-etckeeper

install-etckeeper:
	cd etc; for f in `find etckeeper/ -type f`; do install -D -m0755 $$f $(sysconfdir)/$$f; done

install-data: install-repo

install-repo:
	mkdir -p $(etcgitdatadir)
	tar xf defaults.git.tar -C $(etcgitdatadir)

update-repo:
	./update-default-repo.sh
