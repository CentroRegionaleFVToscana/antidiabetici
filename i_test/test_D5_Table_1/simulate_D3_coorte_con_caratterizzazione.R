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



# name of the dataset to be generated
namedataset <- "D3_coorte_con_caratterizzazione"

# set number of persons
Npersons <- 5000
# create base 
data <- data.table::data.table(person_id = 1:Npersons)
# person_id 
data[, person_id := paste0("000000",as.character(seq_len(.N)))]
data[, person_id := paste0("P",substr(person_id, nchar(person_id) - 6, 
                                      nchar(person_id)))]
# gender
set.seed(1234)
data[, genere := as.character(sample(1:2, Npersons, replace = TRUE, 
                                     prob = c(.5,.5)))]
data[, genere := ifelse(genere == "1","M","F")]
# eta
data[, age := round(rtruncnorm(Npersons, a = 18, b = Inf, mean = 50, sd = 15),0)]
# ageband
data[, ageband := ifelse(age >= 18 & age <=44, "18-44",
                         ifelse(age >= 45 & age <= 64, "45-64",
                                ifelse(age >= 65 & age <= 74, "65-74",
                                       ifelse(age>= 75, "75+", NA))))]

# covariates at t0: binary
covariates_binary <- c("met", "antidiabother", "CV", "cerebro", "aop", "Cvrisk",
                        "HF", "renal")

for (i in covariates_binary) {

  cov <- seq(0,1)
  probcov = runif(1, min = 0, max = 1)
  totprob = sum(probcov)
  probcov = c(probcov, 1 - totprob)
  data[, cov := sample(cov, Npersons, replace = TRUE, prob = probcov)]
  setnames(data,"cov",i)
}

drug_names <- c("SGLT2i","GLP1RA","tirzepatide","DPP4i","DPP4i_SGLT2i",
                "other_combinations")

data[, drug:=sample(drug_names, Npersons, replace = T,
                    prob = c(rep(1/length(drug_names), length(drug_names))))]

data[, period:=sample(c("pre", "nota", "modifica"), Npersons, replace = TRUE, 
                      prob = c(rep(0.33, 3)))]

data[, ASL:=sample(c("CE", "NO", "SE"), Npersons, replace = TRUE, 
                   prob = c(rep(0.33, 3)))]


saveRDS(data, file = paste0(thisdir, "/", namedataset,".rds"))
