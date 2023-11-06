


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

