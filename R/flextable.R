#' APA Themed Flextable
#'
#' A theme function is a R function where input is a flextable and output is a flextable.
#' You are free to do any formatting inside this function.
#'
#' @param x a flextable
#' @param size font size for the table (default = 10)
#'
#' @return a flextable
#' @export
#'
#' @examples
#' if(FALSE) {
#' ft <- flextable::flextable(head(cars))
#' ft <- theme_apa(ft)
#' ft
#' }
#'
#' @note https://github.com/davidgohel/flextable/blob/master/R/themes.R
#'
theme_apa <- function(x, size = 10) {
  if (!inherits(x, "flextable")) {
    stop("this theme supports only flextable objects.")
  }

  x <- align(x, align = "center", part = "all")

  # font:
  x <- fontsize(x, size = size, part = "all")
  x <- font(x, fontname = "Times New Roman", part = "all")
  x <- bold(x, bold = TRUE, part = "header")

  # colour:
  x <- color(x, color = "black", part = "all")
  # x <- bg(x, bg = "#475f77", part = "body")
  # x <- bg(x, bg = "#eb5555", part = "header")
  # x <- bg(x, bg = "#1bbbda", part = "footer")

  # padding:
  # x <- padding(x, padding = 6, part = "all")

  # border:
  std_border <- fp_border_default(width = 0.5, color = "black")
  x <- border_remove(x)

  x <- hline_top(x, border = std_border, part = "header")
  x <- hline_bottom(x, border = std_border, part = "header")
  x <- hline_bottom(x, border = std_border, part = "body")
  # x <- border_outer(x, part = "all", border = std_border )
  # x <- border_inner_h(x, border = std_border, part="all")
  # x <- border_inner_v(x, border = std_border, part="all")
  # x <- fix_border_issues(x)

  # properties:
  x <- autofit(x)
  x <- set_table_properties(x, layout = "autofit")
  # layout = "fixed"

  # RETURN: ----
  return(x)
}
