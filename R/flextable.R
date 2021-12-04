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

  x <- flextable::align(x, align = "center", part = "all")

  # font:
  x <- flextable::fontsize(x, size = size, part = "all")
  x <- flextable::font(x, fontname = "Times New Roman", part = "all")
  x <- flextable::bold(x, bold = TRUE, part = "header")

  # colour:
  x <- flextable::color(x, color = "black", part = "all")
  # x <- bg(x, bg = "#475f77", part = "body")
  # x <- bg(x, bg = "#eb5555", part = "header")
  # x <- bg(x, bg = "#1bbbda", part = "footer")

  # padding:
  # x <- padding(x, padding = 6, part = "all")

  # border:
  std_border <- flextable::fp_border_default(width = 0.5, color = "black")
  x <- flextable::border_remove(x)

  x <- flextable::hline_top(x, border = std_border, part = "header")
  x <- flextable::hline_bottom(x, border = std_border, part = "header")
  x <- flextable::hline_bottom(x, border = std_border, part = "body")
  # x <- border_outer(x, part = "all", border = std_border )
  # x <- border_inner_h(x, border = std_border, part="all")
  # x <- border_inner_v(x, border = std_border, part="all")
  # x <- fix_border_issues(x)

  # properties:
  x <- flextable::autofit(x)
  x <- flextable::set_table_properties(x, layout = "autofit")
  # layout = "fixed"

  # RETURN: ----
  return(x)
}
