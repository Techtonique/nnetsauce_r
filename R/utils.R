


# convert environment to list ---------------------------------------------

env2list <- function(env)
{
  env_names <- rlang::env_names(env)
  n_env_names <- length(env_names)
  res <- vector('list', n_env_names)
  names(res) <- env_names

  for (elt in env_names)
  {
    eval(parse(text=paste("res$", elt, "<- env$", elt)))
  }

  return(res)
}


# calculate end for ts object ---------------------------------------------

calculate_end <- function(start_x, freq_x, n_x) {
  # Calculate the total number of time units
  total_units <- n_x - 1

  # Calculate the number of complete periods and remaining observations
  complete_periods <- total_units %/% freq_x
  remaining_obs <- total_units %% freq_x

  # Calculate the end time
  end_time_year <- start_x[1] + complete_periods
  end_time_period <- start_x[2] + remaining_obs

  # Adjust the end period and year if the end period exceeds the freq_xuency
  while (end_time_period > freq_x) {
    end_time_year <- end_time_year + 1
    end_time_period <- end_time_period - freq_x
  }

  # Return the end time as a vector
  return(c(end_time_year, end_time_period))
}

calculate_next <- function(end_x, freq_x) {

  # Extract the end time and frequency
  end_time <- end(x)
  freq_x <- frequency(x)

  # Calculate the next time point
  next_year <- end_time[1]
  next_period <- end_time[2] + 1

  # Adjust the period and year if the period exceeds the frequency
  if (next_period > freq_x) {
    next_year <- next_year + 1
    next_period <- 1
  }

  # Return the next time point as a vector
  return(c(next_year, next_period))
}


#' Transform list to forecast or mforecast object
#'
#' @param out a list, an object constructed by \code{MTS} or \code{DeepMTS}
#' @param start_input start of the input time series (as in \code{stats::start})
#' @param frequency_input frequency of the input time series (as in \code{stats::frequency})
#'
#' @return an object of class \code{forecast} or \code{mforecast}
#'
#' @export
#'
#' @examples
to_forecast <- function(out,
                        start_input,
                        frequency_input
                        )
{
  n_series <- out$n_series

  res <- vector("list", 9)
  res$model <- out
  res$method <- "[Deep]MTS"

  # returns a time series object
  end_x <- calculate_end(start_input,
                         frequency_input,
                         nrow(out$df_))
  start_preds <- calculate_next(end_x,
                                frequency_input)
  res$mean <- ts(as.matrix(out$mean_),
                 start=start_preds,
                 frequency=frequency_input)
  res$lower <- ts(as.matrix(out$lower_),
                  start=start_preds,
                  frequency=frequency_input)
  res$upper <- ts(as.matrix(out$upper_),
                  start=start_preds,
                  frequency=frequency_input)
  res$level <- out$level
  res$x <- ts(as.matrix(out$df_),
              start=start_input,
              frequency=frequency_input)
  res$residuals <- NULL
  res$fitted <- NULL
  if (n_series <= 1L)
  {
    class(res) <- "forecast"
  } else {
    class(res) <- "mforecast"
  }
  return(res)
}

