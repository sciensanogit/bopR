## APP set-up
## INFO
## .. 00-dev
## .. 01-main
## .. 02-update

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
devtools::build_readme()

## document
devtools::document()
## add badge
usethis::use_github_actions()
