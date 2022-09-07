#-----------------------------------------------------------------------------
#' Mimic Base R break
#' @param x string with which to prefix names of \code{ggplot2} functions in order to name the pipe-enabled functions. default: "add_".
#'               Note: You could set this the empty string, in which case the new functions would mask the name of the library function
#' @param y func_regex Regular expression to filter the list of ggplot functions to make pipe-enabled.  The default regex will capture all
#' @param scale_x
#' @param scale_y
#' @importFrom ggplot2 geom_point ggplot_build scale_x_continuous scale_y_continuous theme element_line element_blank aes unit ggretro rstatix ggpubr
#' @export
#-----------------------------------------------------------------------------
test_plot = function(df, group_var, value_var, fill_var, p_label = "p.signif", detailed = F, hide_ns = T) {
  require(rstatix)
  require(ggpubr)
  require(gridExtra)
  require(grid)
  require(patchwork)
  add_pval = function(data) {
    tryCatch(
      {
        stat_pvalue_manual(
          data,
          label = p_label,
          inherit.aes = F,
          hide.ns = hide_ns,
          label.size = 8,
          bracket.size = 0.6,
          tip.length = 0.02,
          step.increase = 0.05,
          # bracket.nudge.y = 1,
          vjust = 0.6
        )
      },
      error = function(err) {
      }
    )
  }
  df = df[, c(group_var, value_var, fill_var)]
  colnames(df) = c("x", "y", "c")
  # df = df |>
  #   mutate(x = as.factor(as.character(x)))
  sdy = sd(df$y, na.rm = T)
  print(sdy)
  res = df |>
    anova_test(y ~ x) |>
    as_tibble()
  # Perform T test
  res_t = df |>
    pairwise_t_test(y ~ x) |>
    adjust_pvalue(method = "holm") |>
    add_significance() |>
    add_y_position(scales = "free", step.increase = 0) |>
    mutate(y.position = y.position + sdy / 2)
  # Ploting
  df = df |>
    cbind(res) |>
    mutate(anova = glue("ANOVA P = {p}")) |>
    mutate(anova = ifelse(p < 0.05, glue("<b style='color:orange'>{anova}</b>"), anova))
  p =
    {
      df +
        ggplot(aes(x = x, y = y, fill = c)) +
        geom_violin(scale = "width", width = 0.6, alpha = 0.3, position = position_dodge(width = 0.6), trim = FALSE, color = "white") +
        geom_boxplot(width = 0.6, alpha = 0.5, position = position_dodge(width = 0.6)) +
        geom_sina(alpha = 0.8, pch = 21, position = position_dodge(width = 0.6)) +
        labs(x = "", y = value_var, fill = fill_var, caption = unique(df$anova))
    } |>
    base_mode() +
    add_pval(res_t)

  if (fill_var == group_var) {
    p = p + theme(legend.position = "none")
  }

  if (detailed) {
    return(list(p, res_t))
  } else {
    return(p)
  }
}
#-----------------------------------------------------------------------------
# debug
#-----------------------------------------------------------------------------
if (FALSE) {
  library(ggRetro)
  library(ggplot2)
  library(dplyr)
  library(stringr)
  library(purrr)
  library(glue)
  library(patchwork)
  library(ggthemes)
  library(ohmyggplot)
  library(hrbrthemes)
  library(ggtext)
  oh_my_ggplot()
  annot_tb = data.frame(x = c(18,24), y = c(4.5,3.0), am = c(0,1), lab = c("Hi", "There"))
  update_geom_defaults("point",list(fill = "gray28", size=3, stroke=.6, shape=21))
  update_geom_defaults("smooth",list(color = "firebrick", fill = "firebrick", alpha = 0.05))
  p = mtcars |>
    # mutate(carb = as.factor(carb)) |>
    ggplot(aes(as.character(am), wt)) +
    geom_point()
    # geom_text(data = annot_tb, aes(x, y, label = lab))
    # geom_smooth(se = T)
  p2 = base_mode(p, flip = F)

  ggsave("./test.pdf", p2, w = 10, h = 8)

  facets = c("am", "vs")
  scales = "free_"
  label_format_number = "{var.name} = {var.value}"
  label_format_string = "{var.value}"
  label_column = NA
  smart_label = T
  guides = "auto"
  nrow = "auto"
  ncol = "auto"

  p |> base_facet(c("am", "vs"), scales = "free")

  face = p + facet_wrap(~am, scale = "fixed")
  bface = base_mode(face)
  ggplot_build(bface)
  bface$layers[[3]]$aes_params
  ggplot_build(p)
    base_mode()

  p |>
    base_facet(c("am", "vs"), guides = "auto", nrow = 2, scales = "fixed")
  
  facets = c("am", "vs")

  base_facet(p2,"am")
}

