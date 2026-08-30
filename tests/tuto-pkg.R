# ## Load install libraries (install first if needed)
# library(devtools)
# library(usethis)
# library(roxygen2)
#
# ## Create package in a version controled Rproject
# ## Requires overriding the .Rproj file
# usethis::create_package(".", open = FALSE)
#
# ## Add dependencies, i.e. all the packages that are needed to run the
# ## package's functions. Here is a non exhaustive list needed for the
# ## barebone app.
# ## + Shiny
# usethis::use_package("bsicons")
# usethis::use_package("bslib")
# usethis::use_package("htmltools")
# usethis::use_package("shiny")
# usethis::use_package("shiny.i18n")
# usethis::use_package("shinyjs")
# usethis::use_package("shinyWidgets")
# usethis::use_package("shinyFiles")
#
# ## + Tidyverse (Better to call packages individually)
# usethis::use_package("dplyr")
# usethis::use_package("rlang")
#
# ## + misc
# usethis::use_package("datasets")
# usethis::use_package("crosstalk")
# usethis::use_dev_package("d3scatter", remote = "jcheng5/d3scatter")
#
# ## Add license
# usethis::use_mit_license()
#
# ## Add github actions
# ## + package checks
# # usethis::use_github_action_check_standard()
# usethis::use_github_action("check-standard")
#
# ## + turn package doc into GH pages
# # usethis::use_pkgdown() ## NOT USE
# # usethis::use_github_action("pkgdown")
#
#
# ## Import R scripts
# dl_dir <- "R"
# gh_paths <- gh::gh(
#   "/repos/{owner}/{repo}/contents/{path}",
#   owner = "openforis", repo = "shinypkg-template", path = dl_dir
# )
#
# if (length(list.files(dl_dir)) == 0) {
#   dir.create(dl_dir)
#   purrr::walk(seq_along(gh_paths), function(x){
#     download.file(
#       url = gh_paths[[x]]$download_url,
#       destfile = gh_paths[[x]]$path
#     )
#   })
# }
#
# ## Import assets
# dl_dir <- "inst/assets"
# gh_paths <- gh::gh(
#   "/repos/{owner}/{repo}/contents/{path}",
#   owner = "openforis", repo = "shinypkg-template", path = dl_dir
# )
#
# if (length(list.files(dl_dir)) == 0) {
#   dir.create("inst")
#   dir.create(dl_dir)
#   purrr::walk(seq_along(gh_paths), function(x){
#     download.file(
#       url = gh_paths[[x]]$download_url,
#       destfile = gh_paths[[x]]$path
#     )
#   })
# }
#
# ## Run often
# devtools::document()
# devtools::install()
# devtools::load_all() ; shiny_run_arenalytics()
# # Checks
# Sys.setenv("_R_CHECK_SYSTEM_CLOCK_" = 0) ; devtools::check()
#
# ## Publish app:
# ## - shinyapps.io: open ~/app.R then run:
# rsconnect::deployApp()
#
# ## UPDATE 2024: To publish to connect cloud, create manifest
# rsconnect::writeManifest()
#
#
# ## Note on git configuration
# ## - main repo: https://github.com/openforis/arenalytics
# ## - On Github: fork then change name to 'arenalytics_dev': https://github.com/gaelso/arenalytics_dev
# ## - In Rstudio: Create Rstudio project with fork
# ## - In Rstudio: Git tab > New Branch, call it 'dev'
# ## - Setup upstream: git remote add upstream https://github.com/openforis/arenalytics
# ## - Before each session: git fetch upstream
# ## - If changes:
# ## git checkout master
# ## git pull upstream master
# ## git push                 # updates your fork's master
# ## git checkout dev
# ## git merge master         # brings changes into dev
# ## git push
# ## - Finally push changes from branch to origin
# ## git checkout master
# ## git merge dev
# ## git push                 # updates fork's master
# ## git push upstream master # pushes to org
# ## git checkout dev         # back to work
#
# ## As diagram:
# work + commit on   local dev
# │
# │  git checkout master
# │  git merge --no-ff dev
# ▼
# local master        ← the only branch that crosses to upstream
# │
# ├─ git push origin master     → your fork's master (clean mirror)
#         └─ git push upstream master   → openforis (the public repo)
#
#    git checkout dev     ← back to work
#
