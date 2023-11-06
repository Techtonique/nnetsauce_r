#' Plot multivariate time series forecast or residuals
#'
#' @param x result from \code{basicf}, \code{ridge2f} or \code{varf} (multivariate time series forecast)
#' @param selected_series name of the time series selected for plotting
#' @param type "pi": basic prediction intervals;
#' "dist": a distribution of predictions; "sims": the simulations
#' @param level confidence levels for prediction intervals
#' @param ... additional parameters to be passed to \code{plot} or \code{matplot}
#'
#' @export
#'
#' @examples
#'
plot.MTS <- function(x, selected_series,
                     level = 95, ...)
{
  if (!is.null(x$start) && !is.null(x$frequency))
  {
    y <- ts(x$df_[, selected_series],
            start = x$start,
            frequency = x$frequency)
  } else {
    warning("object has no attributes 'start' and 'frequency'")
    y <- ts(x$df_[, selected_series])
  }

  mean_fcast <- x$mean_[, selected_series]
  upper_fcast <- x$upper_[, selected_series]
  lower_fcast <- x$lower_[, selected_series]

  start_y <- x$start
  frequency_y <- x$frequency

  y_mean <- ts(c(y, mean_fcast), start = start_y,
               frequency = frequency_y)
  y_upper <- ts(c(y, upper_fcast), start = start_y,
                frequency = frequency_y)
  y_lower <- ts(c(y, lower_fcast), start = start_y,
                frequency = frequency_y)

  plot(y_mean, type='l',
       main=paste0("Forecasts for ", selected_series, " (", x$method, ")"),
       ylab="", ylim = c(min(c(y_upper, y_lower)),
                         max(c(y_upper, y_lower))), ...)
  lines(y_upper, col="gray60")
  lines(y_lower, col="gray60")
  lines(y_mean)
}
