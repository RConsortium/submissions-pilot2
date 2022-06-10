<!-- badges: start -->

[![R-CMD-check](https://github.com/RConsortium/submissions-pilot2/workflows/R-CMD-check/badge.svg)](https://rconsortium.github.io/submissions-pilot2/)
<!-- badges: end -->

## Overview

## Meeting Minutes
[Repo wiki](https://github.com/RConsortium/submissions-pilot2/wiki)


## Installing `teal` 
(may need to update depending on how `teal` is available to public)

### Clone and install manually

1. Clone the repository [teal](https://github.com/insightsengineering/teal)

1. Install `staged.dependencies` with

   ```r
    devtools::install_github("openpharma/staged.dependencies")
   ```

1. Create and use your [github PAT](create and use a Github PAT), Make sure that you enable SSO for the token.
 

1. Make the token available to `staged.dependencies`. More info on the `staged.dependencies` [website](https://github.com/openpharma/staged.dependencies).

    ```r
     Sys.setenv(GITHUB_PAT = "your_access_token_here")
     options(
       staged.dependencies.token_mapping = c(
         "https://github.com" = "GITHUB_PAT" 
       )
     )
    ``` 
  
1. Install the `teal` package dependencies with (make sure the working directory is set to the root of `teal`)

   ```r
   library(staged.dependencies)
   # install the latest release v0.11.1
   x <- dependency_table(
      project = "insightsengineering/teal",
      project_type = "repo@host",
      ref = "v0.11.1",
      verbose = 1
   )

   install_deps(x, install_direction = "upstream", install_project = TRUE)
   ```

 
