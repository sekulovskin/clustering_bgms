# !/usr/bin/env Rscript

# Packages 
library(bgms)

source("functions.R") #load the simulation functions 

#load the data
data_bern <- readRDS("data_bern.RDS") 
data_bb <- readRDS("data_bb.RDS") 
data_sbm <- readRDS("data_sbm.RDS") 

#Specify the design 
rep <- 100
N <- c(100, 500, 1000)
p <- c(5, 10, 15, 20)
cat <- c(1, 3)



# Estimate the models 
n_cores <- 124

# For now I just use the Cauchy prior for the interaction effects
# with a scale value of 2.5

#-------------------------------------------------------------------------------
# Estimate models for the data generated under the Bernoulli prior -----------
#-------------------------------------------------------------------------------

estimates_bern_bern <- estimate_models_scale(rep, data_bern, N, p, cat, 
                                           interaction_prior = "Cauchy",
                                           scale = scale,
                                           iter = 1e4, 
                                           n_cores,
                                           edge_prior = "Bernoulli")

# save the output   
saveRDS(estimates_bern_bern, file = "estimates_bern_bern.RDS")


estimates_bern_bb <- estimate_models_scale(rep, data_bern, N, p, cat, 
                                             interaction_prior = "Cauchy",
                                             scale = scale,
                                             iter = 1e4, 
                                             n_cores,
                                             edge_prior = "Beta-Binomial")

# save the output   
saveRDS(estimates_bern_bb, file = "estimates_bern_bb.RDS")

estimates_bern_sbm <- estimate_models_scale(rep, data_bern, N, p, cat, 
                                           interaction_prior = "Cauchy",
                                           scale = scale,
                                           iter = 1e4, 
                                           n_cores,
                                           edge_prior = "mfm-SBM")

# save the output   
saveRDS(estimates_bern_sbm, file = "estimates_bern_sbm.RDS")


#-------------------------------------------------------------------------------
# Estimate models for the data generated under the Bernoulli prior -----------
#-------------------------------------------------------------------------------

estimates_bern_bern <- estimate_models_scale(rep, data_bern, N, p, cat, 
                                             interaction_prior = "Cauchy",
                                             scale = scale,
                                             iter = 1e4, 
                                             n_cores,
                                             edge_prior = "Bernoulli")

# save the output   
saveRDS(estimates_bern_bern, file = "estimates_bern_bern.RDS")


estimates_bern_bb <- estimate_models_scale(rep, data_bern, N, p, cat, 
                                           interaction_prior = "Cauchy",
                                           scale = scale,
                                           iter = 1e4, 
                                           n_cores,
                                           edge_prior = "Beta-Binomial")

# save the output   
saveRDS(estimates_bern_bb, file = "estimates_bern_bb.RDS")

estimates_bern_sbm <- estimate_models_scale(rep, data_bern, N, p, cat, 
                                            interaction_prior = "Cauchy",
                                            scale = scale,
                                            iter = 1e4, 
                                            n_cores,
                                            edge_prior = "mfm-SBM")

# save the output   
saveRDS(estimates_bern_sbm, file = "estimates_bern_sbm.RDS")


#-------------------------------------------------------------------------------
# Estimate models for the data generated under the Beta-Binomial prior ---------
#-------------------------------------------------------------------------------

estimates_bb_bern <- estimate_models_scale(rep, data_bb, N, p, cat, 
                                             interaction_prior = "Cauchy",
                                             scale = scale,
                                             iter = 1e4, 
                                             n_cores,
                                             edge_prior = "Bernoulli")

# save the output   
saveRDS(estimates_bb_bern, file = "estimates_bb_bern.RDS")


estimates_bb_bb <- estimate_models_scale(rep, data_bb, N, p, cat, 
                                           interaction_prior = "Cauchy",
                                           scale = scale,
                                           iter = 1e4, 
                                           n_cores,
                                           edge_prior = "Beta-Binomial")

# save the output   
saveRDS(estimates_bb_bb, file = "estimates_bb_bb.RDS")

estimates_bb_sbm <- estimate_models_scale(rep, data_bb, N, p, cat, 
                                            interaction_prior = "Cauchy",
                                            scale = scale,
                                            iter = 1e4, 
                                            n_cores,
                                            edge_prior = "mfm-SBM")

# save the output   
saveRDS(estimates_bb_sbm, file = "estimates_bb_sbm.RDS")



#-------------------------------------------------------------------------------
# Estimate models for the data generated under the SBM prior -------------------
#-------------------------------------------------------------------------------

estimates_sbm_bern <- estimate_models_scale(rep, data_sbm, N, p, cat, 
                                           interaction_prior = "Cauchy",
                                           scale = scale,
                                           iter = 1e4, 
                                           n_cores,
                                           edge_prior = "Bernoulli")

# save the output   
saveRDS(estimates_sbm_bern, file = "estimates_sbm_bern.RDS")


estimates_sbm_bb <- estimate_models_scale(rep, data_sbm, N, p, cat, 
                                         interaction_prior = "Cauchy",
                                         scale = scale,
                                         iter = 1e4, 
                                         n_cores,
                                         edge_prior = "Beta-Binomial")

# save the output   
saveRDS(estimates_sbm_bb, file = "estimates_sbm_bb.RDS")

estimates_sbm_sbm <- estimate_models_scale(rep, data_sbm, N, p, cat, 
                                          interaction_prior = "Cauchy",
                                          scale = scale,
                                          iter = 1e4, 
                                          n_cores,
                                          edge_prior = "mfm-SBM")

# save the output   
saveRDS(estimates_sbm_sbm, file = "estimates_sbm_sbm.RDS")




