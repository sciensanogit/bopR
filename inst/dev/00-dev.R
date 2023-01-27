## APP set-up
## INFO
## .. 00-dev: general script
## .. 01-statbel-data: script to output statbel dataset
## .. 02-federal-planning-bureau-data: script to output FPB dataset
## .. 03-merge: merge the datasets and save them in a package-ready format

## data on population available from statbel

##++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##            00-DEV
##++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## update package
usethis::use_package()
## update license
usethis::use_mit_license()
## load all functions
devtools::load_all()
## check package
devtools::check()
## build package
devtools::build()
## build readme
#usethis::use_readme_rmd()
devtools::build_readme()

## document
devtools::document()
## add badge
usethis::use_github_actions()
