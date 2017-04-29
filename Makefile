$(shell eval `opam config env`)

include $(shell opam config var solvuu-build:lib)/solvuu.mk
