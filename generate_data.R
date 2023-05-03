# Function that simulates data from the ordinal MRF under different priors for 
# the graph topology with the option for the users to supply specific parameter values


generate_data <- function(rep, N, p, cat, 
                          iter = 1e3, 
                          edge_val, mu_val,
                          edge_prior, alpha, 
                          beta, no.blocks){
  # Storage
  
  data <- vector(mode = "list", length = rep)
  for(h in 1:rep) {
    data[[h]] <- vector(mode = "list", length = length(N))
    for(i in 1:length(N)) {
      data[[h]][[i]] <- vector(mode = "list", length = length(p))
      for(j in 1:length(p)) {
        data[[h]][[i]][[j]] <- vector(mode = "list", length = length(cat))
        for(k in 1:length(cat)) {
          data[[h]][[i]][[j]][[k]] <- list()
  
        }
      }
    }
  }
  
  
  Sigma <- list()
  
  Sigma_true <- list()
  
  mu_true <- vector(mode = "list", length = length(p))
  for (j in seq_along(mu_true)) {
    mu_true[[j]] <- vector(mode = "list", length = length(cat))
    for (l in seq_along(mu_true[[j]])) {
      mu_true[[j]][[k]] <- matrix(0, nrow = p[j], ncol = cat[k])
    }
  }
  
  # Divide the supplied edge and threshold values w.r.t. p
  
  for(j in 1:length(p)){
    Sigma_true[[j]] <- edge_val[1:p[j], 1:p[j]]
  }
  
  
  for(j in 1:length(p)){
    for(k in 1:length(cat)){
      mu_true[[j]][[k]] <- mu_val[1:p[j], 1:cat[k]]
    }
  }
  
  # construct Sigma under different edge priros 

  if (edge_prior == "Bernoulli") {
    for (j in 1:length(p)) {
      Sigma[[j]] <- matrix(0, nrow = p[j], ncol = p[j])
      for (i in 1:(p[j]-1)) {
        for (k in (i+1):p[j]) {
          Sigma[[j]][k, i] <- rbinom(n = 1, size = 1, prob = 0.5)
          Sigma[[j]][i, k] <- Sigma[[j]][k, i]
        }
      }
      for (i in 1:(p[j]-1)) {
        for (k in (i+1):p[j]) {
          if (Sigma[[j]][k, i] != 0) {
            Sigma[[j]][k, i] <- Sigma_true[[j]][k, i]
            Sigma[[j]][i, k] <- Sigma[[j]][k, i]
          } else {
            Sigma[[j]][k, i] <- Sigma[[j]][k, i]
            Sigma[[j]][i, k] <- Sigma[[j]][k, i]
          }
        }
      }   
    }
     
 } else if(edge_prior == "Beta-Binomial"){  
    
    prob <- rbeta(1, alpha, beta)
    
    for (j in 1:length(p)) {
      Sigma[[j]] <- matrix(0, nrow = p[j], ncol = p[j])
      for (i in 1:(p[j]-1)) {
        for (k in (i+1):p[j]) {
          Sigma[[j]][k, i] <- rbinom(n = 1, size = 1, prob = prob)
          Sigma[[j]][i, k] <- Sigma[[j]][k, i]
        }
      }
      for (i in 1:(p[j]-1)) {
        for (k in (i+1):p[j]) {
          if (Sigma[[j]][k, i] != 0) {
            Sigma[[j]][k, i] <- Sigma_true[[j]][k, i]
            Sigma[[j]][i, k] <- Sigma[[j]][k, i]
          } else {
            Sigma[[j]][k, i] <- Sigma[[j]][k, i]
            Sigma[[j]][i, k] <- Sigma[[j]][k, i]
          }
        }
      }   
    }
    
  } else{    # SBM
    
    Q <- matrix(0, nrow = no.blocks, ncol = no.blocks)
    Q[lower.tri(Q)] = rbeta(n = no.blocks * (no.blocks - 1)/2,
                            shape1 = alpha, shape2 = beta)
    Q = Q + t(Q)
    diag(Q) = rbeta(n = no.blocks, shape1 = alpha, shape2 = beta)
    Z = sample(1:no.blocks, size = p[j], replace = TRUE)
    
    for (j in 1:length(p)) {
      Sigma[[j]] <- matrix(0, nrow = p[j], ncol = p[j])
      for (i in 1:(p[j]-1)) {
        for (k in (i+1):p[j]) {
          Sigma[[j]][k, i] <- rbinom(n = 1, size = 1, prob = Q[Z[i], Z[k]])
          Sigma[[j]][i, k] <- Sigma[[j]][k, i]
        }
      }
      for (i in 1:(p[j]-1)) {
        for (k in (i+1):p[j]) {
          if (Sigma[[j]][k, i] != 0) {
            Sigma[[j]][k, i] <- Sigma_true[[j]][k, i]
            Sigma[[j]][i, k] <- Sigma[[j]][k, i]
          } else {
            Sigma[[j]][k, i] <- Sigma[[j]][k, i]
            Sigma[[j]][i, k] <- Sigma[[j]][k, i]
          }
        }
      }   
    }
    

  }
  
  
  # simulate data
  for(h in 1:rep){
    for(i in 1:length(N)){
      for(j in 1:length(p)){
        for(k in 1:length(cat)){
          data[[h]][[i]][[j]][[k]] <-  bgms::sample_omrf_gibbs(no_states = N[i],
                                                               no_nodes = p[j],
                                                               no_categories = rep(cat[k], p[j]),
                                                               interactions = Sigma[[j]],
                                                               thresholds = as.matrix(mu_true[[j]][[k]]),
                                                               iter = iter)
        }
        
      }
      
    }
    
  }
  
  return(list(data, Sigma))
}




# Test

#rep <- 1
#N <- 1000
#p <- c(5, 10)
#cat <- c(2,3)
#alpha <- beta <- 1
#no.blocks <- 2
#edge_prior <-  "Beta-Binomial"
#
#data <- generate_data(rep, N, p, cat, 
#                      iter = 1e3, 
#                      edge_val, mu_val,
#                      edge_prior, alpha, 
#                      beta, no.blocks)
#