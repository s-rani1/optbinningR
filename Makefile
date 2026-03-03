.PHONY: test check full-check

test:
	Rscript scripts/check.R

check: test

full-check:
	Rscript scripts/check.R --full
