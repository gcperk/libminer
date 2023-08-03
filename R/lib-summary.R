#' Summary of Libraries
#'
#' Provides a list of number of packages per library
#' location on your machine
#'
#' @param sizes TRUE / FALSE to assess the size of
#'
#' @return A dataframe containing count of packages with libraries
#' @export
#'
#' @examples
#' \dontrun{
#' lib_summary()
#' }
lib_summary <- function(sizes = FALSE){
  pkgs <- utils::installed.packages()
  pkg_tbl <- table(pkgs[, "LibPath"])
  pkg_df <- as.data.frame(pkg_tbl, stringsAsFactors = FALSE)
  names(pkg_df) <- c("library", "n_packages")

  if(sizes) {
    pkg_df$lib_size <- vapply(
      pkg_df$library,
      function(x){
        sum(fs::file_size(fs::dir_ls(x, recurse = TRUE)))
      },
      FUN.VALUE = double(1))
  }
  pkg_df
}

