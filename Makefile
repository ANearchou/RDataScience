R ?= R
RS ?= Rscript

.PHONY: check
check:
	 - $(RS) -e "Rcpp::compileAttributes(); devtools::check(args = c('--no-multiarch'))"


.PHONY: git
git:
	- git checkout main
	- git add -A .
	- git commit -m "$(msg)"
	- git push -u  origin main

.PHONY: gitamend
gitamend:
	- git checkout main
	- git add -A .
	- git commit --amend --no-edit
	- git push -u  origin main --force
