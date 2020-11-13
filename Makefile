SHELL := /bin/bash
LIBDIR=/Users/kyleb/Rlibs/lib
PACKAGE=lastdose
VERSION=$(shell grep Version DESCRIPTION |awk '{print $$2}')
TARBALL=${PACKAGE}_${VERSION}.tar.gz
PKGDIR=.
CHKDIR=.

## Set libPaths:
export R_LIBS=${LIBDIR}

travis:
	make build
	R CMD check ${TARBALL} -o ${CHKDIR}

test:
	Rscript -e 'library(testthat)' -e 'test_file("tests/testthat.R")'

rhub:
	Rscript -e 'rhub::check_for_cran()'

cran:
	make doc
	make build
	R CMD check ${TARBALL} --as-cran

covr:
	Rscript inst/covr/covr.R

ec:
	echo ${VERSION}

.PHONY: data
data:
	Rscript inst/script/data.R

all:
	make doc
	make build
	make install

.PHONY: doc
doc:
	Rscript -e 'library(devtools); document()'

build:
	R CMD build --md5 $(PKGDIR)

install:
	R CMD install --install-tests ${TARBALL}

install-build:
	R CMD install --build --install-tests ${TARBALL}

check:
	make doc
	make build
	R CMD check ${TARBALL} -o ${CHKDIR}

readme:
	Rscript -e 'rmarkdown::render("README.Rmd")'

site:
	Rscript -e 'pkgdown::build_site()'

spelling:
	Rscript inst/script/_spelling.R

bump-dev:
	Rscript -e 'usethis::use_version("dev")'

tag-version:
	git tag $(VERSION)
	git push origin $(VERSION)
