rm(list=ls(all.names=TRUE))

#set the directory where the script is saved as the working directory

if (!require("rstudioapi")) install.packages("rstudioapi")
thisdir <- setwd(dirname(rstudioapi::getSourceEditorContext()$path))
thisdir <- setwd(dirname(rstudioapi::getSourceEditorContext()$path))

# load packages

if (!require("data.table")) install.packages("data.table")
library(data.table)
if (!require("lubridate")) install.packages("lubridate")
library(lubridate)
if (!require("truncnorm")) install.packages("truncnorm")
library(truncnorm)


drug_names <- c("SGLT2i","GLP1RA","tirzepatide","DPP4i","DPP4i_SGLT2i",
                "other_combinations")

for (k in seq_along(drug_names)) {
  
  set.seed(123+k)

  # name of the dataset to be generated
  namedataset <- "D3_selezione_coorte"
  
  # set number of persons
  Npersons <- 5000
  # create base 
  data <- data.table::data.table(person_id = 1:Npersons)
  # person_id 
  data[, person_id := paste0("000000",as.character(seq_len(.N)))]
  data[, person_id := paste0("P",substr(person_id, nchar(person_id) - 6, 
                                        nchar(person_id)))]
  
  # covariates at t0: binary
  covariates_binary <- c("sel_data_incomplete", "sel_no_obs_periods", 
                         "sel_no_drug", "sel_no_lookback", "sel_prevalent")
  
  for (i in seq_along(covariates_binary)) {
  
    cov <- seq(0,1)
    probcov = runif(1, min = 0, max = 1)
    totprob = sum(probcov)
    probcov = c(probcov, 1 - totprob)
    data[, cov := sample(cov, Npersons, replace = TRUE, prob = probcov)]
    setnames(data,"cov",covariates_binary[i])
  }
  
  # date first
  start_date <- as.Date("2016-01-01")
  end_date   <- as.Date("2025-12-31")
  
  data[, date_first := sample(seq(start_date, end_date, by = "day"),
                               .N, replace = TRUE)]
  
  # period
  data[, period:=sample(c("pre", "nota", "modifica"), Npersons, replace = TRUE, 
                        prob = c(rep(0.33, 3)))]
  
  # ASL
  data[, ASL:=sample(c("CE", "NO", "SE"), Npersons, replace = TRUE, 
                     prob = c(rep(0.33, 3)))]
  
  # birth date 
  start_date <- as.Date("1920-01-01")
  end_date   <- as.Date("2025-01-01")
  
  data[, birth_date := sample(seq(start_date, end_date, by = "day"),
                              .N, replace = TRUE)]
  
  # gender
  set.seed(1234)
  data[, genere := as.character(sample(1:2, Npersons, replace = TRUE, 
                                       prob = c(.5,.5)))]
  data[, genere := ifelse(genere == "1","M","F")]
  
  
  
  saveRDS(data, file = paste0(thisdir, "/", namedataset, "_", drug_names[k],".rds"))

}
