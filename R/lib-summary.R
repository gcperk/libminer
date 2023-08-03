#' Summary of Libraries
#'
#' Provides a list of number of packages per library
#' location on your machine
#' @return A dataframe containing count of packages with libraries
#' @export
#'
#' @examples
#' \dontrun{
#' lib_summary()
#' }
lib_summary <- function(){
  pkgs <- utils::installed.packages()
  pkg_tbl <- table(pkgs[, "LibPath"])
  pkg_df <- as.data.frame(pkg_tbl, stringsAsFactors = FALSE)
  names(pkg_df) <- c("library", "n_packages")
  pkg_df
}
