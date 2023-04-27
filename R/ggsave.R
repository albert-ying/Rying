#-----------------------------------------------------------------------------
#' ggsave2
#' @description Save a ggplot2 plot to multiple filetypes
#' @importFrom ggplot ggsave
#' @importFrom glue glue
#' @importFrom stringr str_remove
#' @importFrom ggplot2 last_plot
#' @export
#-----------------------------------------------------------------------------

ggsave2 = function(filename, plot = last_plot(), filetypes = c("pdf", "png"), ...) {
  for (filetype in filetypes) {
    ggsave(glue("{str_remove(filename, "\\.[^\\.]+$")}.{filetype}"), plot = plot, ...)
  }
}
