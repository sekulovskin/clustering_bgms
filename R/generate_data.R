# Function that simulates data from the ordinal MRF under different priors for 
# the graph topology with the option for the users to supply specific parameter values

generate_data <- function(rep, N, p, cat,
                          iter = 1e3,
                          edge_val, mu_val,
                          edge_prior, theta, alpha,
                          beta, k) {
  
  # Storage
  data <- list()
  Sigma_true <- edge_val
  Sigma <- list()
  
  # Construct Sigma under different edge priors
  if (edge_prior == "Bernoulli") {
    
    for (h in 1:rep) {
      Sigma[[h]] <- matrix(0, nrow = p, ncol = p)
      for (i in 1:(p - 1)) {
        for (j in (i + 1):p) {
          Sigma[[h]][j, i] <- rbinom(n = 1, size = 1, prob = theta)
          Sigma[[h]][i, j] <- Sigma[[h]][j, i]
        }
      }
      for (i in 1:(p - 1)) {
        for (j in (i + 1):p) {
          if (Sigma[[h]][j, i] != 0) {
            Sigma[[h]][j, i] <- Sigma_true[j, i]
            Sigma[[h]][i, j] <- Sigma_true[j, i]
          } else {
            Sigma[[h]][j, i] <- Sigma[[h]][j, i]
            Sigma[[h]][i, j] <- Sigma[[h]][j, i]
          }
        }
      }
      
      data[[h]] <-  bgms::sample_omrf_gibbs(no_states = N,
                                            no_nodes = p,
                                            no_categories = rep(cat, p),
                                            interactions = Sigma[[h]],
                                            thresholds = as.matrix(mu_val),
                                            iter = iter)
    }  
    
  } else if (edge_prior == "Beta-Binomial") {  
    
    for (h in 1:rep) {
      prob <- rbeta(1, alpha, beta)
      Sigma[[h]] <- matrix(0, nrow = p, ncol = p)
      for (i in 1:(p - 1)) {
        for (j in (i + 1):p) {
          Sigma[[h]][j, i] <- rbinom(n = 1, size = 1, prob = prob)
          Sigma[[h]][i, j] <- Sigma[[h]][j, i]
        }
      }
      for (i in 1:(p - 1)) {
        for (j in (i + 1):p) {
          if (Sigma[[h]][j, i] != 0) {
            Sigma[[h]][j, i] <- Sigma_true[j, i]
            Sigma[[h]][i, j] <- Sigma_true[j, i]
          } else {
            Sigma[[h]][j, i] <- Sigma[[h]][j, i]
            Sigma[[h]][i, j] <- Sigma[[h]][j, i]
          }
        }
      }
      
      data[[h]] <-  bgms::sample_omrf_gibbs(no_states = N,
                                            no_nodes = p,
                                            no_categories = rep(cat, p),
                                            interactions = Sigma[[h]],
                                            thresholds = as.matrix(mu_val),
                                            iter = iter)
    }
    
  } else if (edge_prior == "mfm-SBM") {    # SBM
    
    for (h in 1:rep) {
      Q <- matrix(0, nrow = k[h], ncol = k[h])
      Q[lower.tri(Q)] <- rbeta(n = k[h] * (k[h] - 1)/2,
                               shape1 = 2, shape2 = 8)
      Q <- Q + t(Q)
      diag(Q) <- rbeta(n = k[h], shape1 = 8, shape2 = 2)
      Z <- sample(1:k[h], size = p, replace = TRUE)
      
      Sigma[[h]] <- matrix(0, nrow = p, ncol = p)
      for (i in 1:(p - 1)) {
        for (j in (i + 1):p) {
          Sigma[[h]][j, i] <- rbinom(n = 1, size = 1, prob = Q[Z[i], Z[j]])
          Sigma[[h]][i, j] <- Sigma[[h]][j, i]
        }
      }
      for (i in 1:(p - 1)) {
        for (j in (i + 1):p) {
          if (Sigma[[h]][j, i] != 0) {
            Sigma[[h]][j, i] <- Sigma_true[j, i]
            Sigma[[h]][i, j] <- Sigma_true[j, i]
          } else {
            Sigma[[h]][j, i] <- Sigma[[h]][j, i]
            Sigma[[h]][i, j] <- Sigma[[h]][j, i]
          }
        }
      }
      
      data[[h]] <-  bgms::sample_omrf_gibbs(no_states = N,
                                            no_nodes = p,
                                            no_categories = rep(cat, p),
                                            interactions = Sigma[[h]],
                                            thresholds = as.matrix(mu_val),
                                            iter = iter)
    }
  }
  
  return(list(data, Sigma))
}
