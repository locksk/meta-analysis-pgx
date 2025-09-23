#!/usr/bin/env

.libPaths(c("scratch/c.c1713552/software/R_library", .libPaths()))

#library(tidyverse)
library(ggplot2)
library(dplyr)
library(tidyr)
library(readr)
library(purrr)
library(tibble)
library(stringr)
library(forcats)
library(lubridate)
library(metafor)
library(clubSandwich)

message("")
message("")
message("Packages loaded")

#source("functions.R")
load("all_smds.RData")
all_smd <- all_smd %>% filter(SMD < 5 & SMD > -5) 

message("")
message("")

# loop for calculating coefficients and variance components at each value of rho in vcalc()
# constant sampling correlation assumption -  it is highly recommended to run several sensitivity analyses for varying values of ρ
rho_list <- seq.int(0.05, 0.95, by = .05)

results <- data.frame()

# test at different levels of correlation
for (i in 1:length(rho_list)){
  
  V <- metafor::vcalc(vi = Var, cluster = Study, obs = es_id, rho = rho_list[i], data = all_smd)
  
  # fit correlated and hierarchical effects model
  che.model <- rma.mv(absmd ~ rating,
                      V = V,
                      random = list(~ rating | Study, ~ 1 | es_id), struct = "UN",
                      data = all_smd,
                      sparse = TRUE)
  
  che.model <- robust(che.model, cluster = Study, adjust = "CR2", clubSandwich = T) 
  
  out <- data.frame(Outcome = c("Proximal vs Distal"),
                      Estimate = c(che.model$b[2]),
                      SE = c(che.model$se[2]),
                      CI_L = c(che.model$ci.lb[2]),
                      CI_U = c(che.model$ci.ub[2]),
                      'p value' = c(che.model$pval[2])
                      )
  
  #s <- data.frame(Component = c("s2", "t21", "t22", "r"),
#                  Value = c(che.model$sigma2, che.model$tau2[1], che.model$tau2[2],  che.model$rho)
#  )
  
   s <- data.frame(Outcome = c("Proximal vs Distal"),
                   s2 = che.model$sigma2,
                   t21 = che.model$tau2[1],
                   t22 = che.model$tau2[2],
                   r = che.model$rho)
  
   out <- left_join(out, s, by = "Outcome")
 # out <- out[,-1]

  out$p <- rho_list[i]
  
  results <- rbind(results, out)
}

df <- results

sink(file="final/vcalc_data.txt")
df
sink(file=NULL)

