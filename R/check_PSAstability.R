#' Plot cumulative mean Probabilistic Sensitivity Analysis results
#'
#' This function plots the cumulative mean of incremental net monetary benefit (INMB),
#' incremental cost-effectiveness ratio (ICER), incremental costs, or incremental effects
#' for different strategies compared to a specified comparator.
#'
#' @param m_eff Numeric matrix of effects for different strategies.
#' @param m_cost Numeric matrix of costs for different strategies.
#' @param lambda Numeric value specifying the willingness-to-pay threshold for ICER.
#' @param currency_symbol String specifying the currency symbol for y-axis labels.
#' @param v_strategy_labels Named vector of strategy labels e.g. c("A" = "Strategy A").
#' @param v_strategy_colors Named vector of strategy colors e.g. c("A" = "#665BA6").
#' @param comparator Column name representing the comparator strategy (e.g. "A").
#' @param output String specifying the type of plot, limited to: "inmb", "icer", "costs", or "effects".
#' @param include_reference_line Logical indicating whether to include a reference line.
#' @param log_x Logical indicating whether to use a logarithmic scale on the x-axis.
#'
#' @return A ggplot object representing the cumulative mean PSA stability plot.
#'
#' @export
#'
#'
#' @examples
#' # create example matrices
#' n_psa <- 10000
#'
#' m_eff <- matrix(data = runif(n = n_psa * 4, min = 0, max = 1),
#'                 nrow = n_psa,
#'                 ncol = 4,
#'                 dimnames = list(NULL, paste0("Strategy ", c("A", "B", "C", "D"))))
#'
#' m_cost <- matrix(data = runif(n = n_psa * 4, min = 5000, max = 20000),
#'                  nrow = n_psa,
#'                  ncol = 4,
#'                  dimnames = list(NULL, paste0("Strategy ", c("A", "B", "C", "D"))))
#'
#' v_strategy_colors <- setNames(object = grDevices::palette.colors(n = ncol(m_eff)),
#'                               nm = colnames(m_eff))
#'
#' plot_PSA_stability(m_eff = m_eff,
#'                    m_cost = m_cost,
#'                    lambda = 20000,
#'                    currency_symbol = "\u0024",
#'                    v_strategy_labels = colnames(m_eff),
#'                    v_strategy_colors = v_strategy_colors,
#'                    comparator  = colnames(m_eff)[1],
#'                    output = "inmb",
#'                    include_reference_line = TRUE,
#'                    log_x = FALSE)
#'
plot_PSA_stability <- function(m_eff,
                               m_cost,
                               lambda,
                               currency_symbol = "\u0024",
                               v_strategy_labels = NULL,
                               v_strategy_colors = NULL,
                               comparator = NULL,
                               output = "inmb",
                               include_reference_line = TRUE,
                               log_x = FALSE) {
  assertthat::assert_that(is.matrix(m_eff) &
                            is.matrix(m_cost) & is.numeric(m_eff) & is.numeric(m_cost),
                          msg = "m_eff and m_cost must be numeric matrices")
  assertthat::assert_that(is.numeric(lambda) & length(lambda) == 1,
                          msg = "lambda must be a numeric of length 1")

  # expect the two matrices to have identical dimensions & names
  assertthat::assert_that(all(dim(m_eff) == dim(m_cost)),
                          msg = "m_eff and m_cost dimensions must be equal")
  assertthat::assert_that(all(unlist(dimnames(m_eff)) == unlist(dimnames(m_cost))),
                          msg = "m_eff and m_cost dimension names not equal")
  assertthat::assert_that(is.numeric(lambda),
                          msg = "lambda must be numeric")
  assertthat::assert_that(output %in% c("inmb", "icer", "costs", "effects"),
                          msg = "output must be one of inmb, icer, costs or effects)")

  # if no comparator then use first column
  if (is.null(comparator)) comparator <- colnames(m_eff)[1]
  assertthat::assert_that(comparator  %in% colnames(m_cost),
                          msg = "comparator  must be one of the column names")

  # if no strategy labels
  #if(is.null(v_strategy_labels)) v_strategy_labels <- setNames(object = colnames(m_cost), nm = colnames(m_cost))


  # get incremental costs and effects vs comparator
  m_inc_cost <- m_cost - m_cost[, comparator]
  m_inc_eff  <- m_eff  - m_eff[, comparator]

  # calculate inmb
  cumulative_mean <- function(x) {
    cumsum(x) / seq_along(x)
  }

  # get the cumulative mean for all columns.
  get_cum_mean <- function(m){
    apply(X = m,
          MARGIN = 2,
          FUN = cumulative_mean)
  }

  # get cumulative mean for all columns of effects.
  m_cum_meaneff <- get_cum_mean(m = m_inc_eff)
  m_cum_meancost <- get_cum_mean(m = m_inc_cost)

  #=============#
  # CREATE PLOT #
  #=============#

  label <- switch(
    EXPR  = output,
    "inmb"    = "Incremental Net Monetary Benefit",
    "icer"    = "Incremental Cost-effectiveness Ratio",
    "costs"   = "Incremental Costs",
    "effects" = "Incremental Effects"
  )

  # create a dataframe in long format - the value column depends on the
  df_plot <- switch(
    EXPR  = output,
    "inmb"    = m_cum_meaneff * lambda - m_cum_meancost,
    # calculate inmb at each N
    "icer"    = m_cum_meancost / m_cum_meaneff,
    # calc icer at each N
    "costs"   = m_cum_meancost,
    "effects" = m_cum_meaneff
  ) |> as.data.frame()

  # add an N column
  df_plot$N <- 1:nrow(df_plot)

  # pivot longer format.
  ## Identify the columns to pivot (all except 'N')
  varying_cols <- setdiff(names(df_plot), "N")
  ## Use reshape
  df_plot <- stats::reshape(
    data = df_plot,
    varying = varying_cols,             # Columns that contain the values to stack
    v.names = "value",                  # Name of the new column for the values
    timevar = "strategy",               # Name of the new column for the original column names
    times = varying_cols,               # The actual values for the 'strategy' column
    idvar = "N",                        # Column(s) that identify unique rows in the wide format
    direction = "long"                  # Specify we are going from wide to long format
  )
  ## Reset row names for cleaner output (reshape might create complex ones)
  rownames(df_plot) <- NULL
  ## Reorder columns
  df_plot <- df_plot[, c("N", "strategy", "value")]
  ## Sort rows
  df_plot <- df_plot[order(df_plot$N, df_plot$strategy), ]

  # filter out the comparator and the first 5 in the x axis.
  df_plot <- df_plot[df_plot$N > 5 & df_plot$strategy != comparator, ]

  # the comparator label is defined here to be inserted into the plot.
  comparator_label <-
    ifelse(test = is.null(v_strategy_labels[comparator]),
           yes = comparator,
           no = v_strategy_labels[comparator])

  # Add the following line to satisfy R CMD:
  N <- value <- strategy <- NULL

  # create the baseline plot
  p1 <- ggplot2::ggplot(data = df_plot ,
                        mapping = ggplot2::aes(x = N,
                                               y = value,
                                               col = strategy)) +
    ggplot2::theme_minimal() +
    ggplot2::scale_y_continuous(
      name = label,
      labels = ifelse(
        test = (output == "effects"),
        # If output is "effects", use default numeric labels
        yes = function(x) { x },
        # Otherwise, format as currency using base R
        no = function(x) {
          # Use prettyNum to add thousand separators (big.mark)
          # scientific = FALSE prevents potential scientific notation
          negative_values <- x < 0
          formatted_nums <- prettyNum(abs(x), big.mark = ",", scientific = FALSE)
          # Add the currency symbol prefix
          paste0(
            ifelse(negative_values, "-", ""),
            currency_symbol,
            formatted_nums
          )
        }
      )
    ) +
    ggplot2::geom_line() +
    ggplot2::ggtitle(
      label = paste0("Cumulative Mean ",
                     label,
                     " for Strategy vs ",
                     comparator_label),
      subtitle = "Assessing the stability of PSA outputs"
    ) +
    ggplot2::theme(
      legend.position = "none",
      plot.title = ggplot2::element_text(hjust = 0.5),
      plot.subtitle = ggplot2::element_text(hjust = 0.5)
    ) +
    ggplot2::facet_wrap(
      ~ strategy,
      labeller = ggplot2::labeller(strategy = v_strategy_labels),
      ncol = 2,
      scales = "free_y"
    ) +
    ggplot2::xlab(label = "Number of PSA iterations")

  if (!is.null(v_strategy_colors)) {
    p1 <- p1 + ggplot2::scale_color_manual(name = "Strategy",
                                           values = v_strategy_colors)
  }

  if (log_x) {
    p1 <- p1 + ggplot2::scale_x_log10()
  }

  if (include_reference_line) {
    switch(EXPR = output,
           "icer" = {
             p1 + ggplot2::geom_hline(yintercept = lambda)
           },
           "inmb" = {
             p1 + ggplot2::geom_hline(yintercept = 0)
           },
           p1)
  } else{
    p1
  }

}
