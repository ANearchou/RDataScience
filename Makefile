R ?= R
RS ?= Rscript

.PHONY: check
check:
# https://support.rstudio.com/hc/en-us/articles/200486518-Customizing-Package-Build-Options
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
