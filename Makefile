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

.PHONY: all clean install dist

CASK?=cask
EMACS?=emacs
TAR?=COPYFILE_DISABLE=1 bsdtar
CURL?=curl

VERSION?=$(shell $(CASK) version)

ARCHIVE_NAME=sparql-mode
PACKAGE_NAME=$(ARCHIVE_NAME)-$(VERSION)

all: $(PACKAGE_NAME).tar


$(ARCHIVE_NAME)-pkg.el: $(ARCHIVE_NAME).el
	$(CASK) package

# create a tar ball in package.el format for uploading to
# http://marmalade-repo.org
$(PACKAGE_NAME).tar: COPYING                                                  \
                     README.org                                               \
                     $(ARCHIVE_NAME).el                                       \
                     $(ARCHIVE_NAME)-pkg.el                                   \
                     sparql-mode                                              \
                     ob-sparql.el
	$(TAR) -c -s "@^@$(PACKAGE_NAME)/@" -f $(PACKAGE_NAME).tar $^

install: $(PACKAGE_NAME).tar
	$(EMACS) --batch                                                          \
	         --load package                                                   \
	         --funcall package-initialize                                     \
	         --eval "(package-install-file \"$(PWD)/$(PACKAGE_NAME).tar\")"

clean:
	$(RM) $(ARCHIVE_NAME)-*.tar $(ARCHIVE_NAME)-pkg.el
	$(RM) -r .cask
