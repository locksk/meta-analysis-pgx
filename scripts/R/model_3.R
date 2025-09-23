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

taskid <- Sys.getenv('SLURM_ARRAY_TASK_ID')

message("")
message("")
message("Packages loaded")

#source("functions.R")
load("all_smds.RData")

message("")
message("")
message("Data (all_smds.Rdata) loaded")

all_smd <- all_smd %>% filter(SMD < 5 & SMD > -5) 
all_smd <- dplyr::filter(all_smd, drug_id == "risperidone" & enzyme == "CYP2D6")
all_smd <- all_smd %>% drop_na(mp) %>% drop_na(pdr) %>% drop_na(enzyme)

message("")
message("")
message("Data cleaned - risperidone and CYP2D6")

# model 
V <- metafor::vcalc(vi = Var, cluster = Study, obs = es_id, rho = 0.6, data = all_smd)

mod <- rma.mv(absmd ~ 0 + rating, 
                    V = V,
                    random = list(~ 1 | Study / es_id), 
                    method = "REML", test = "t", dfs = 'contain',
                    data = all_smd,
                    sparse = TRUE)

# make robust and save model
mod <- robust(mod, cluster= Study, adjust = "CR2", clubSandwich = T)
saveRDS(mod, file = paste0("final/meta_", taskid, ".rds"))

# save output
sink(file = paste0("final/meta_", taskid, ".txt"))
print(mod$call)
summary(mod)
sink(file = NULL)

message("")
message("")
message("Model fit")


message("")

outfile1 <- paste0("final/profile_", taskid, ".pdf")
outfile2 <- paste0("final/profile_", taskid, "_log.txt")


pdf(file=outfile1)
sink(file=outfile2)
metafor::profile.rma.mv(mod, parallel = "multicore", ncpus = 5)
sink(file=NULL)
dev.off()


message("")
message("")
message("Plots created")
