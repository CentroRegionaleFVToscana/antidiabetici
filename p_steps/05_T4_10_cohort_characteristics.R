# author: Sabrina Giometto

# v 1.0 05 Giu 2026 Creation of D5 started

# v 1.1 24 Giu 2026 Creation of D5 completed

#########################################

if (TEST){
  testname <- "test_D5_Table_1"
  thisdirinput <- file.path(dirtest,testname)
  thisdiroutput <- file.path(dirtest,testname,"g_output")
  dir.create(thisdiroutput, showWarnings = F)
}else{
  thisdirinput <- dirinput
  thisdiroutput <- dirtemp
}


# load data
data <- readRDS(file = file.path(thisdirinput, "/D3_coorte_con_caratterizzazione.rds"))


# Create D5 with sociodemographic characteristics
D5_nocov <- data[, .(
              N          = .N,
              age_median = median(age),
              age_q1 = quantile(age, probs = 0.25),
              age_q3 = quantile(age, probs = 0.75),
              Age_18_44_N = sum(ageband=="18-44"),
              Age_18_44_p = round(sum(ageband=="18-44")/.N,3)*100,
              Age_45_64_N = sum(ageband=="45-64"),
              Age_45_64_p = round(sum(ageband=="45-64")/.N,3)*100,
              Age_55_74_N = sum(ageband=="65-74"),
              Age_55_74_p = round(sum(ageband=="65-74")/.N,3)*100,
              Age_75over_N = sum(ageband=="75+"),
              Age_75over_p = round(sum(ageband=="75+")/.N,3)*100,
              genere_F_N = sum(genere=="F"),
              genere_F_p = round(sum(genere=="F")/.N,3)*100),
              .(period, ASL)]

# create D5 with binary covariates
D5_cov <- NULL

for (i in covariates_binary) {
  
  tmp <- data[, .(
              N = .N,
              tmp_N = sum(get(i)==1),
              tmp_p = round(sum(get(i)==1)/.N,3)*100),
              .(period, ASL)]
  
  setnames(tmp,"tmp_N",paste0(i, "_N"))
  setnames(tmp,"tmp_p",paste0(i, "_p"))
  
  if (is.null(D5_cov)) {
    
    D5_cov <- tmp
    
  } else {
    
    D5_cov <- merge(D5_cov, tmp, by = c("period", "ASL", "N"))
  }
  
}

# create the final D5 by merging the previous two
D5 <- merge(D5_nocov, D5_cov, by = c("period", "ASL", "N"), all = F)

# save
saveRDS(D5, file = paste0(thisdiroutput, "/D5.rds"))
