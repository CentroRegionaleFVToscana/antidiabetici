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
# covariates at t0: binary
covariates_binary <- c("diab_gestaz", 
                       "diab_pregrav")

for (i in covariates_binary) {
  # set.seed(1111)
  cov <- seq(0,1)
  probcov = runif(1, min = 0, max = 1)
  totprob = sum(probcov)
  probcov = c(probcov, 1 - totprob)
  data[, cov := sample(cov, Npersons, replace = TRUE, prob = probcov)]
  setnames(data,"cov",i)
}

# categorie low, medium, high mutuamente esclusive
mutua_excl <- function(data, group_name, categories, Npersons) {
  
  # levels_vec <- categories

  probs <- runif(length(categories))
  probs <- probs / sum(probs)
  
  cat_var <- sample(categories, Npersons, replace = TRUE, prob = probs)
  
  for (i in categories) {
    data[, (paste0(group_name, "_", i)) := as.integer(cat_var == i)]
  }
  
  data
}

data <- mutua_excl(data, "bmi", c("low", "medium", "high"), Npersons)

drug_names <- c("SGLT2i","GLP1RA","tirzepatide","DPP4i","DPP4i_SGLT2i",
                "other_combinations")

data[, drug:=sample(drug_names, Npersons, replace = T,
                    prob = c(rep(1/length(drug_names), length(drug_names))))]

data[, period:=sample(c("pre", "nota", "modifica"), Npersons, replace = TRUE, 
                      prob = c(rep(0.33, 3)))]

data[, ASL:=sample(c("CE", "NO", "SE"), Npersons, replace = TRUE, 
                   prob = c(rep(0.33, 3)))]


saveRDS(data, file = paste0(thisdir, "/", namedataset,".rds"))
