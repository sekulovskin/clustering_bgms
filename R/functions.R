# Functions to perform the analyses 

# With UI interaction prior --------------------------
estimate_models_UI <- function(rep, data, N, p, cat, 
                               interaction_prior = "UnitInfo", 
                               iter = 1e4,spar, n_cores = 1,  
                               edge_prior = "Beta-Binomial"){
  
  # Register parallel backend
  cl <- makeCluster(n_cores)
  registerDoParallel(cl)
  
  param_grid <- expand.grid(
    rep = 1:rep,
    i = 1:length(N),
    j = 1:length(p),
    k = 1:length(cat),
  )
  
  est <- foreach(idx = seq_len(nrow(param_grid)), .packages='bgms') %dopar% {
    row_params <- param_grid[idx,]
    h <- row_params[["rep"]]
    i <- row_params[["i"]]
    j <- row_params[["j"]]
    k <- row_params[["k"]]
    
    # Estimate
    tryCatch({
      bgm(data[[h]][[i]][[j]][[k]], 
          interaction_prior = interaction_prior,
          iter = iter, 
          edge_prior = edge_prior)
    }, error = function(e) {
      # If an error occurs, store the error message in the corresponding position of the "est" list
      paste0("Error occurred while estimating model ", idx, ": ", conditionMessage(e))
    })
  }
  
  # Stop parallel backend
  stopCluster(cl)
  
  # Return estimated models
  return(est)
}


# With Cauchy/Laplace interaction prior --------------------------
estimate_models_scale <- function(rep, data, N, p, cat, 
                                  interaction_prior = "Cauchy", 
                                  scale = 1, iter = 1e4, 
                                  n_cores = 1,edge_prior = "Beta-Binomial"){
  
  # Initialize parallel backend
  cl <- makeCluster(n_cores)
  registerDoParallel(cl)
  
  # Create parameter grid
  param_grid <- expand.grid(rep = 1:rep,
                            i = 1:length(N),
                            j = 1:length(p),
                            k = 1:length(cat),
                            s = 1:length(scale))
  
  est <- foreach(idx = seq_len(nrow(param_grid)), .packages='bgms') %dopar% {
    row_params <- param_grid[idx,]
    h <- row_params[["rep"]]
    i <- row_params[["i"]]
    j <- row_params[["j"]]
    k <- row_params[["k"]]
    s <- row_params[["s"]]
    
    # Estimate
    tryCatch({
      bgm(data[[h]][[i]][[j]][[k]], 
          interaction_prior = interaction_prior,
          cauchy_scale = scale[s], 
          iter = iter,
          edge_prior = edge_prior)
    }, error = function(e) {
      # If an error occurs, store the error message in the corresponding position of the "est" list
      paste0("Error occurred while estimating model ", idx, ": ", conditionMessage(e))
    })
  }
  
  # Stop parallel backend
  stopCluster(cl)
  
  # Return estimated models
  return(est)
}
