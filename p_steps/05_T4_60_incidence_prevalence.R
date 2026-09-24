
# authors: Sabrina Giometto


# v 0.1

# 21 Jul 2026


# assign directories

if (TEST){ 
  testname <- "test_D5_Figure_1"
  thisdirinput <- paste0(file.path(dirtest, testname), "/")
  thisdiroutput <- file.path(dirtest,testname,"g_output")
  dir.create(thisdiroutput, showWarnings = F)
}else{
  thisdirinput <- dirtemp
  thisdiroutput <- direxp
}

D4_pop_ASL <- readRDS(file.path(thisdirinput, "D4_pop_ASL.rds"))


# load
for (i in drug_names_s) {
  
  tab <- readRDS(paste0(thisdirinput, "D4_prevalence_incidence_", i, ".rds"))
  
 
  # create frequency tables

    
  tab <- tab[, .(prevalent = sum(is_prevalent),
                                 incident = sum(is_incident)), .(year, ASL)]
  
  tab <- merge(tab, D4_pop_ASL, by = c("year", "ASL"), all = TRUE)
  
  tab[, `:=`(prevalence=prevalent/pop18,
             incidence=incident/pop18)]
  
 
 
  # save
 
  saveRDS(tab, file = paste0(thisdiroutput, "/D5_Figure_1_prevalence_incidence_", i, ".rds") )
  write.csv(tab, file = paste0(thisdiroutput, "/D5_Figure_1_prevalence_incidence_", i, ".csv"))
  
}

