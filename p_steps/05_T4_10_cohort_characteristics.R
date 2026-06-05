# author: Sabrina Giometto

# v 1.0 05 Giu 2026

#########################################

if (TEST){
  testname <- "test_D5_cohort_characteristics"
  thisdirinput <- file.path(dirtest,testname)
  thisdiroutput <- file.path(dirtest,testname,"g_output")
  dir.create(thisdiroutput, showWarnings = F)
}else{
  thisdirinput <- dirinput
  thisdiroutput <- dirtemp
}

# Create simulated D3

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
data[, genere := fifelse(genere == "1","M","F")]
# eta
data[, age := round(rnorm(Npersons, mean = 50, sd = 15),0)]
# covariates at t0: binary
covariates_binary <- c("met", "metass", "antidiabother", "CV", "cerebro", 
                       "Cvrisk", "HF", "renal")

for (i in covariates_binary) {
  set.seed(1111)
  cov <- seq(0,1)
  probcov = runif(1, min = 0, max = 1)
  totprob = sum(probcov)
  probcov = c(probcov, 1 - totprob)
  data[, cov := sample(cov, Npersons, replace = TRUE, prob = probcov)]
  setnames(data,"cov",i)
}

data[, period:=sample(c("pre", "nota", "modifica"), Npersons, replace = TRUE, 
                      prob = c(rep(0.33, 3)))]

data[, AV:=sample(c("CE", "NO", "SE"), Npersons, replace = TRUE, 
                      prob = c(rep(0.33, 3)))]

##################

# Create D5

data[, .(
  N          = .N,
  age_median = median(age),
  age_q1 = quantile(age, probs = 0.25),
  age_q3 = quantile(age, probs = 0.75)),
  colonna]






