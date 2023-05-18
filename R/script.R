# !/usr/bin/env Rscript

# Packages 
library(bgms)
source("functions.R")

#load the data
data_bern <- readRDS("data_bern.RDS") 
data_bb <- readRDS("data_bb.RDS") 
data_sbm <- readRDS("data_sbm.RDS") 

#Specify the design 
rep <- 25
N <- 1000 
p <- 24
cat <- 4
edge_priors <- c("Bernoulli", "Beta-Binomial", "mfm-SBM")

# Estimate the models 
n_cores <- 124

#-------------------------------------------------------------------------------
# Estimate all models for the data generated under the Bernoulli prior ---------
#-------------------------------------------------------------------------------

estimates_bern <- estimate_models_no_sbm(rep = rep, data = data_bern,
                                         iter = 1e4, 
                                         n_cores = n_cores,
                                         edge_prior = edge_priors)

# save the output   
saveRDS(estimates_bern, file = "estimates_bern.RDS")   # it's probably the edge priors.


#-------------------------------------------------------------------------------
# Estimate all models for the data generated under the Beta-Binomial prior -----
#-------------------------------------------------------------------------------

estimates_bb <- estimate_models_no_sbm(rep = rep, data = data_bb,
                                       iter = 1e4, 
                                       n_cores = n_cores,
                                       edge_prior = edge_priors)

# save the output   
saveRDS(estimates_bb, file = "estimates_bb.RDS")



#-------------------------------------------------------------------------------
# Estimate all models for the data generated under the SBM prior --------------
#-------------------------------------------------------------------------------

estimates_sbm <- estimate_models_sbm(rep = rep, data = data_sbm,
                                     iter = 1e4, 
                                     n_cores = n_cores,
                                     edge_prior = edge_priors)

# save the output   
saveRDS(estimates_sbm, file = "estimates_sbm.RDS")

