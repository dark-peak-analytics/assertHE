# Load the required packages
if (!requireNamespace("RCurl", quietly = TRUE)) {
  install.packages("RCurl")
}
if (!requireNamespace("jsonlite", quietly = TRUE)) {
  install.packages("jsonlite")
}
library(RCurl)
library(jsonlite)

find_package_source_cran <- function (package_name, package_version){

  # Construct the URL to the source tarball on CRAN
  cran_url <- paste0(
    "https://cran.r-project.org/src/contrib/Archive/",
    package_name, "/",
    package_name, "_", package_version, ".tar.gz"
  )

  # Check if the package source is available on CRAN
  if (url.exists(cran_url)) {
    return(cran_url)
  } else {
    message("Source for package ", package_name, " version ", package_version, " not found on CRAN.")
  }

}

find_package_source_github <- function (package_name, package_version){

  # Construct the GitHub search URL
  github_search_url <- paste0(
    "https://api.github.com/search/repositories?q=",
    package_name, "+language:R"
  )

  # Set a user agent for GitHub API requests
  # Addresses: Request forbidden by administrative....
  curl <- getCurlHandle()
  options(RCurlOptions = list(
    header = FALSE,
    verbose = TRUE,
    useragent = "R"
  ))

  # Fetch the search results from GitHub
  search_results <- getURL(github_search_url, curl = curl)
  search_results <- jsonlite::fromJSON(search_results)

  if (length(search_results$items) > 0) {
    # Check for the specific version tag in the GitHub repositories
    for (repo_item in seq_len(nrow(search_results$items)) ) {

      repo <- search_results$items[repo_item, ]

      if (!is.null(repo$url)) {
        tags_url <- paste0(repo$url, "/tags")
        tags_results <- getURL(tags_url, curl = curl)
        tags_results <- jsonlite::fromJSON(tags_results)

        # Regular expression pattern to match version tags
        # examples: "v1.1.3", "1.1.3", "1.1-beta", "2.0.0-rc.1", "v2.3-alpha"
        #
        # This should probably be a parameter !
        pattern <- "^v?[0-9]+\\.[0-9]+(\\.[0-9]+)*(-[a-zA-Z0-9]+)?$"

        # This step is probably unnecessary, but, it seems like a good idea
        # Check which versions match the pattern
        matches <- grepl(pattern, tags_results$name)

        # find the indices which match the requested version
        matches_indices <- grep(package_version, tags_results$name[matches])

        # add to result set
        return(tags_results$zipball_url[matches_indices])
        break
      }
    }
  } else {
    message("Package ", package_name, " not found on GitHub.")
  }

}

# Function to find the source code of an R package on CRAN and GitHub
find_package_source <- function(package_name, package_version) {
  # Initialize result list
  result <- list(CRAN = NULL, GitHub = NULL)
  result$CRAN <- find_package_source_cran(package_name, package_version)
  result$GitHub <- find_package_source_github(package_name, package_version)


  # Check if neither source was found
  if (is.null(result$CRAN) && is.null(result$GitHub)) {
    return(paste("Source for package", package_name, "version", package_version, "not found on CRAN or GitHub."))
  }

  return(result)
}

# Example usage:
# result <- find_package_source("dplyr", "1.1.3")
# find_package_source("datasets", "4.3.3")
