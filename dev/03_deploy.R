# Building a Prod-Ready, Robust Shiny Application.
# 
# README: each step of the dev files is optional, and you don't have to 
# fill every dev scripts before getting started. 
# 01_start.R should be filled at start. 
# 02_dev.R should be used to keep track of your development during the project.
# 03_deploy.R should be used once you need to deploy your app.
# 
# 
######################################
#### CURRENT FILE: DEPLOY SCRIPT #####
######################################

# Test your app

## Run checks ----
## Check the package before sending to prod
devtools::check()

# Build
## Buld the package tarball and send to the renv cellar directory

## Linux version
devtools::build(path = "renv/cellar")

## Windows version


# Install to renv library
renv::install("renv/cellar/pilot2wrappers_0.2.0.tar.gz")

# Deploy

## Local, CRAN or Package Manager ---- 
## This will build a tar.gz that can be installed locally, 
## sent to CRAN, or to a package manager
devtools::build()

## RStudio ----
## If you want to deploy on RStudio related platforms
golem::add_shinyappsio_file()

## Docker ----
## If you want to deploy via a generic Dockerfile
#golem::add_dockerfile()

