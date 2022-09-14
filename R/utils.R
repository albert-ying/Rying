#-----------------------------------------------------------------------------
#' Matix to tibble
#' @importFrom tibble rownames_to_column as_tibble
#' @export
#-----------------------------------------------------------------------------

m_to_tibble = function(mat, rowname_to = "rowname") {
  mat |>
    as.data.frame() |>
    rownames_to_column(var = rowname_to) |>
    as_tibble()
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

