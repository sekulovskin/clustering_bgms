setwd("~/Skepter/Part I - Simulation")
# Generate the data-------------------
set.seed(123)
source("generate_data.R") 
library(bgms)
library(readr)

# Obtain MPLE from the original data 
data_full <- read_csv("C:/Users/nsekulo/OneDrive - UvA/Skepter/data/data_paranormal_mma.csv")
data <- as.matrix(data_full[, c(1:24)])
MPLE <- mple(data)

edge_val <- MPLE[[1]]
mu_val <- MPLE[[2]]

# settings 
rep <- 25
N <- 1000  
p <- ncol(edge_val) 
cat <- 4


# Bernoulli --------------------------------
data_bern_1 <- generate_data(rep, N, p, cat, 
                             iter = 1e3, 
                             edge_val, mu_val,
                             "Bernoulli", 
                             theta = 0.2)


data_bern_2 <- generate_data(rep, N, p, cat, 
                             iter = 1e3, 
                             edge_val, mu_val,
                             "Bernoulli", 
                             theta = 0.8)

sigma_bern <- list(data_bern_1[[2]], data_bern_2[[2]])
data_bern <- list(data_bern_1[[1]], data_bern_2[[1]])

saveRDS(data_bern, "data_bern.RDS")
saveRDS(sigma_bern, "sigma_bern.RDS")



# Beta Bionomial --------------------------------
data_bb_1 <- generate_data(rep, N, p, cat, 
                           iter = 1e3, 
                           edge_val, mu_val,
                           "Beta-Binomial", 
                           alpha = 2, 
                           beta = 8)

data_bb_2 <- generate_data(rep, N, p, cat, 
                           iter = 1e3, 
                           edge_val, mu_val,
                           "Beta-Binomial", 
                           alpha = 8, 
                           beta = 2)

sigma_bb <- list(data_bb_1[[2]], data_bb_2[[2]])
data_bb <- list(data_bb_1[[1]], data_bb_2[[1]])

saveRDS(data_bb, "data_bb.RDS")
saveRDS(sigma_bb, "sigma_bb.RDS")

# SBM --------------------------------
k <- round(runif(rep, 2, 5))

data_sbm <- generate_data(rep, N, p, cat, 
                          iter = 1e3, 
                          edge_val, mu_val,
                          "mfm-SBM", k = k)

sigma_sbm <- data_sbm[[2]]
sigma_sbm <- list(sigma_sbm, k)
data_sbm <- data_sbm[[1]]


saveRDS(sigma_sbm, "sigma_bb.RDS")
saveRDS(data_sbm, "data_sbm.RDS")