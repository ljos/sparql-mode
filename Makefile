# This file is part of SPARQL mode.

# Copyright (C) 2014 Bjarte Johansen

# Author: Bjarte Johansen <Bjarte dot Johansen at gmail dot com>

# SPARQL mode is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published
# by the Free Software Foundation, either version 3 of the License,
# or (at your option) any later version.

# SPARQL mode is distributed in the hope that it will be useful, but
# WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
# General Public License for more details.

# You should have received a copy of the GNU General Public License
# along with SPARQL mode.  If not, see <http://www.gnu.org/licenses/>.

.PHONY: all package clean install

CASK?=cask
EMACS?=emacs

VERSION?=$(shell $(CASK) version)

ARCHIVE_NAME=sparql-mode
PACKAGE_NAME=$(ARCHIVE_NAME)-$(VERSION)

all: package

package: dist/$(PACKAGE_NAME).tar

dist/$(PACKAGE_NAME).tar:
	$(CASK) package

INSTALL="(package-install-file \"$(PWD)/dist/$(PACKAGE_NAME).tar\")"

install: package
	$(EMACS) --batch                                                          \
	         --load package                                                   \
	         --funcall package-initialize                                     \
	         --eval $(INSTALL)

clean:
	-rm -r dist/
