# Generate the data-------------------

source("generate_data.R") 

rep <- 100
N <- c(100, 500, 1000)  # ?
p <- c(5, 10, 15, 20) # ?
cat <- c(1, 3)   # ? 
alpha <- 
Beta <- 
n0.blocks <- 

# TODO: Talk about the choices of the abive values 


data_bern <- generate_data(rep, N, p, cat, 
                      iter = 1e3, 
                      edge_val, mu_val,
                      "Bernoulli", alpha, 
                      beta, no.blocks)

saveRDS(data_bern, "data_bern.RDS")


data_bb <- generate_data(rep, N, p, cat, 
                           iter = 1e3, 
                           edge_val, mu_val,
                           "Beta-Binomial", alpha, 
                           beta, no.blocks)

saveRDS(data_bb, "data_bb.RDS")


data_sbm <- generate_data(rep, N, p, cat, 
                         iter = 1e3, 
                         edge_val, mu_val,
                         "mfm-SBM", alpha, 
                         beta, no.blocks)

saveRDS(data_sbm, "data_sbm.RDS")