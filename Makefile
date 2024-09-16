SERVICE := lvm
DESTDIR ?= dist_root
SERVICEDIR ?= /srv/$(SERVICE)

.PHONY: build install

build:
	echo nothing to build

install: build
	mkdir -p $(DESTDIR)$(SERVICEDIR)/lvm/backend
	cp -r docker-compose.release.yml $(DESTDIR)$(SERVICEDIR)/docker-compose.yml
	cp -r lvm/frontend $(DESTDIR)$(SERVICEDIR)/lvm/frontend
	cp lvm/backend/*.h $(DESTDIR)$(SERVICEDIR)/lvm/backend/
	cp lvm/lisp lvm/lispc lvm/Dockerfile lvm/Dockerfile.deps $(DESTDIR)$(SERVICEDIR)/lvm/
	mkdir $(DESTDIR)$(SERVICEDIR)/lvm/examples/
	cp examples/lists.lisp $(DESTDIR)$(SERVICEDIR)/lvm/examples/
	mkdir -p $(DESTDIR)/etc/systemd/system/faustctf.target.wants/
	ln -s /etc/systemd/system/docker-compose@.service $(DESTDIR)/etc/systemd/system/faustctf.target.wants/docker-compose@lvm.service
