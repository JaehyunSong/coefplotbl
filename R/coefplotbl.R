#' Coefficient plot with table
#'
#' @param x a \code{lm} or \code{glm} object.
#' @param intercept a logical. Default is \code{FALSE}.
#' @param add_tbl a logical. Default is \code{TRUE}.
#' @param statistics \code{"ci"} (confidential interval; default), \code{"se"} (standard error), \code{"t"} (t-value), or \code{"p"} (p-value).
#' @param alpha Default is \code{0.05}.
#' @param sig If \code{TRUE} (default), the point-ranges which are statistically significant are highlighted.
#' @param digits Default is \code{3}.
#' @param coef_rename A named character vector.
#' @param coef_omit A character vector.
#' @param highlight A character vector.
#' @param xlab a character.
#' @param ylab a character.
#' @param title a character.
#' @param size a numeric value. Default is \code{1}.
#' @param linewidth A numeric value. Default is \code{0.75}.
#' @param fontsize A numeric value. Default is \code{12}.
#' @param colors A named character vector. The names must be \code{sig}, \code{insig}, and \code{highlight}.
#'
#' @import broom
#' @import ggplot2
#' @import dplyr
#' @import forcats
#' @import stringr
#'
#' @return a \code{ggplot} object
#' @export
#'
#' @examples
#' # Example 1
#' fit <- lm(Sepal.Length ~ Sepal.Width + Petal.Length + Petal.Width + Species, data = iris)
#' coefplotbl(fit)
#'
#' # Example 2: Display only plot & 90\% CI
#' coefplotbl(fit, add_tbl = FALSE, alpha = 0.1)
#'
#' # Example 3: Recoding covariate names
#' coefplotbl(fit, coef_rename = c("Sepal.Width"  = "Sepal Width",
#'                                 "Petal.Length" = "Petal Length",
#'                                 "Petal.Width"  = "Petal Width"))
#'
#' # Example 4: Highlight specific covariates
#' coefplotbl(fit, coef_rename = c("Sepal.Width"  = "Sepal Width",
#'                                 "Petal.Length" = "Petal Length",
#'                                 "Petal.Width"  = "Petal Width"),
#'            highlight = c("Petal.Length", "Petal.Width"))
#'
#' # Example 5: Omit specific covariates
#' coefplotbl(fit, coef_omit = c("Sepal.Width"))
#'
#' # Example 6: Display p-value instead of confidential interval (4-digit)
#' coefplotbl(fit, statistics = "p", digits = 4)
#'

coefplotbl <- function(x,
                       intercept   = FALSE,
                       add_tbl     = TRUE,
                       statistics  = "ci",
                       alpha       = 0.05,
                       sig         = TRUE,
                       digits      = 3,
                       coef_rename = NULL,
                       coef_omit   = NULL,
                       highlight   = NULL,
                       xlab        = NULL,
                       ylab        = NULL,
                       title       = NULL,
                       size        = 1,
                       linewidth   = 0.75,
                       fontsize    = 12,
                       colors      = c(sig = "black", insig = "gray70", highlight = "red")) {

  term <- estimate <- conf.low <- conf.high <- NULL
  est2 <- ll2 <- p2 <- se2 <- t2 <- ul2 <- NULL
  p.value <- statistic <- std.error <- tbl_text <- NULL

  # Axis labels
  if (is.null(xlab)) xlab <- paste0("Estimates with ", (1 - alpha) * 100, "% CI")
  if (is.null(ylab)) {
    if (statistics == "ci") {
      ylab <- paste0("Estimates and ", (1 - alpha) * 100, "% CI")
    } else if (statistics == "se") {
      ylab <- paste0("Estimates and standard errors")
    } else if (statistics == "t") {
      ylab <- paste0("Estimates and t statistics")
    } else if (statistics == "p") {
      ylab <- paste0("Estimates and p-values")
    }
  }

  # Extract statistics
  temp_df <- tidy(x, conf.int = TRUE, conf.level = 1 - alpha)

  # Drop intercept term if "intercept = TRUE"
  if (!intercept) temp_df <- filter(temp_df, !grepl("Intercept", term))

  # Drop terms if "coef_omit" is not NULL
  if (!is.null(coef_omit)) temp_df <- filter(temp_df, !(term %in% coef_omit))

  if (sig) {
    temp_df <- temp_df |>
      mutate(sig = if_else(conf.low * conf.high > 0, "sig", "insig"))
  } else {
    temp_df <- temp_df |>
      mutate(sig = NA)
  }

  if (!is.null(highlight)) {
    temp_df <- temp_df |>
      mutate(sig = if_else(term %in% highlight, "highlight", sig))
  }

  # Recoding terms
  if (!is.null(coef_rename)) {
    temp_df <- temp_df |>
      mutate(term = recode(term, !!!coef_rename))
  }
  temp_df <- temp_df |>
    mutate(id   = nrow(temp_df):1, .before = term)

  if (add_tbl) {
    temp_df <- temp_df |>
      mutate(est2 = sprintf(paste0("%+.", digits,"f"), estimate),
             ll2  = sprintf(paste0("%+.", digits,"f"), conf.low),
             ul2  = sprintf(paste0("%+.", digits,"f"), conf.high),
             se2  = sprintf(paste0("%.", digits,"f"), std.error),
             t2   = sprintf(paste0("%+.", digits,"f"), statistic),
             p2   = sprintf(paste0("%.", digits,"f"), p.value))

    if (statistics == "ci") {
      temp_df <- temp_df |>
        mutate(tbl_text = paste0(est2, " [", ll2, ", ", ul2, "]"),
               tbl_text = str_replace_all(tbl_text, "\\+", " ")) |>
        select(-c(std.error:p.value, est2:p2))
    } else if (statistics == "se") {
      temp_df <- temp_df |>
        mutate(tbl_text = paste0(est2, " (", se2, ")"),
               tbl_text = str_replace_all(tbl_text, "\\+", " ")) |>
        select(-c(std.error:p.value, est2:p2))
    } else if (statistics == "t") {
      temp_df <- temp_df |>
        mutate(tbl_text = paste0(est2, " (", t2, ")"),
               tbl_text = str_replace_all(tbl_text, "\\+", " ")) |>
        select(-c(std.error:p.value, est2:p2))
    } else if (statistics == "p") {
      temp_df <- temp_df |>
        mutate(tbl_text = paste0(est2, " (", p2, ")"),
               tbl_text = str_replace_all(tbl_text, "\\+", " ")) |>
        select(-c(std.error:p.value, est2:p2))
    }
  }

  #print(temp_df)

  plt <- temp_df |>
    ggplot(aes(x = estimate, xmin = conf.low, xmax = conf.high, y = id)) +
    geom_vline(xintercept = 0, linetype = 2) +
    geom_pointrange(size = size, linewidth = linewidth)

  if (sig | !is.null(highlight)) {
    plt <- plt +
      geom_pointrange(aes(color = sig), size = size, linewidth = linewidth) +
      scale_color_manual(values = c("sig"       = as.vector(colors["sig"]),
                                    "insig"     = as.vector(colors["insig"]),
                                    "highlight" = as.vector(colors["highlight"])),
                         na.value = "black")
  } else {
    plt <- plt +
      geom_pointrange(size = size, linewidth = linewidth)
  }

  if (add_tbl) {
    plt <- plt +
      scale_y_continuous(breaks   = nrow(temp_df):1,
                         labels   = temp_df$term,
                         sec.axis = sec_axis(trans  = ~.,
                                             breaks = nrow(temp_df):1,
                                             labels = temp_df$tbl_text,
                                             name   = ylab))
  } else {
    plt <- plt +
      scale_y_continuous(breaks   = nrow(temp_df):1,
                         labels   = temp_df$term)
  }

  plt <- plt +
    guides(color = "none") +
    labs(x = xlab, title = title) +
    theme_bw(base_size = fontsize) +
    theme(panel.border       = element_blank(),
          axis.title.y.left  = element_blank(),
          axis.text.y.right  = element_text(family = "mono", size = fontsize),
          axis.line.x.bottom = element_line(),
          axis.line.y.left   = element_line(),
          axis.ticks.y.right = element_blank(),
          panel.grid.major.y = element_blank(),
          panel.grid.minor.y = element_blank())

  plt
}
