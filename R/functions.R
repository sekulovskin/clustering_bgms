# Functions to perform the analyses 
library(foreach)
library(doParallel)  
#==============================================================================
# Function for the beta-binomial and the bernoulli priors 
estimate_models_no_sbm <- function(rep, data, 
                                   interaction_prior = "Cauchy", 
                                   scale = 2.5, iter = 1e4, 
                                   n_cores = 1, edge_prior = "Beta-Binomial"){
  
  # Initialize parallel backend
  cl <- makeCluster(n_cores)
  registerDoParallel(cl)
  
  # Create parameter grid
  param_grid <- expand.grid(
    rep = 1:rep,
    i = 1:2,   
    j = 1:length(edge_prior))
  
  est <- foreach(idx = seq_len(nrow(param_grid)), .packages='bgms') %dopar% {
    row_params <- param_grid[idx,]
    h <- row_params[["rep"]]
    i <- row_params[["i"]]
    j <- row_params[["j"]]
    
    # Estimate
    tryCatch({
      bgm(data[[i]][[h]], 
          interaction_prior = interaction_prior,
          cauchy_scale = scale, 
          iter = iter,
          edge_prior = edge_prior[[j]])
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




estimate_models_sbm <- function(rep, data, 
                                interaction_prior = "Cauchy", 
                                scale = 2.5, iter = 1e4, 
                                n_cores = 1, edge_prior = "Beta-Binomial"){
  
  # Initialize parallel backend
  cl <- makeCluster(n_cores)
  registerDoParallel(cl)
  
  # Create parameter grid
  param_grid <- expand.grid(
    rep = 1:rep,
    i = 1:length(edge_prior)) 
  
  est <- foreach(idx = seq_len(nrow(param_grid)), .packages='bgms') %dopar% {
    row_params <- param_grid[idx,]
    h <- row_params[["rep"]]
    i <- row_params[["i"]]
    
    # Estimate
    tryCatch({
      bgm(data[[h]], 
          interaction_prior = interaction_prior,
          cauchy_scale = scale, 
          iter = iter,
          edge_prior = edge_prior[[i]])
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


