#' Open UserGuide of the package
#'
#' @param ext extension of the book to open: 'html', 'pdf'
#'
#' @importFrom utils browseURL
#'
#' @export
open_guide <- function(ext = "html") {
    if (ext == "html") {
    guide_path <- system.file('NA/_book/index.html', package = 'cdx2cea')
    } else if (ext == "pdf") {
    guide_path <- system.file('NA/_book/NA.pdf', package = 'cdx2cea')
    } else {
    guide_path <- system.file(paste0("NA/_book/NA.", ext[1]), package = 'cdx2cea')
    }

  browseURL(paste0('file://', guide_path))
}
