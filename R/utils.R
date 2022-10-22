#-----------------------------------------------------------------------------
#' Matix to tibble
#' @importFrom tibble rownames_to_column as_tibble
#' @export
#-----------------------------------------------------------------------------

matrix2tibble = function(mat, rowname_to = "rowname") {
  mat |>
    as.data.frame() |>
    rownames_to_column(var = rowname_to) |>
    as_tibble()
}


#-----------------------------------------------------------------------------
#' Tibble transpose
#' @importFrom tibble rownames_to_column as_tibble
#' @export
#-----------------------------------------------------------------------------

tt = function(tb, rownames_to = "id", colnames_from = NA) {
  if (is.na(colnames_from)) {
    colnames_from = colnames(tb)[[1]]
  }
  tb |>
    as.data.frame() |>
    column_to_rownames(colnames_from) |>
    t() |>
    as.data.frame() |>
    rownames_to_column(rownames_to) |>
    as_tibble()
}

#-----------------------------------------------------------------------------
#' not in
#' @export
#-----------------------------------------------------------------------------

`%nin%` = Negate(`%in%`)

#-----------------------------------------------------------------------------
#' enrich_join
#' Join two tibbles, and only include few columns from second tibble
#' @importFrom dplyr common_by select filter collect left_join
#' @export
#-----------------------------------------------------------------------------

enrich_join = function(x, y, ..., by = NULL) {
  by = common_by(by, x, y)

  if (rlang::dots_n(...) > 0) {
    y = y %>%
      select(!!by$y, ...)
  }

  if (!inherits(x, "tbl_lazy")) {
    # x is a local tbl; filter and collect the enrichment table first
    for (i in seq_along(by$x)) {
      x_ids = unique(x[[by$x[i]]])

      y_col = sym(by$y[i])
      y = y %>%
        filter(!!y_col %in% x_ids)
    }
    y = y %>%
      collect()
  }

  result = x %>%
    left_join(y, by = by)

  result
}

#-----------------------------------------------------------------------------
#' install
#' @importFrom devtools install_github
#' @export
#-----------------------------------------------------------------------------

install = function(x) {
  devtools::install_github(paste0("albert-ying/", x))
}

#-----------------------------------------------------------------------------
# debug
#-----------------------------------------------------------------------------
if (FALSE) {
  m = matrix(1, 10, 10)
  debug(m_to_tibble)
  m_to_tibble(m)
  undebug(m_to_tibble)
}
