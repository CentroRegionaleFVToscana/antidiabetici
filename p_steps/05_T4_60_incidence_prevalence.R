
# authors: Sabrina Giometto, Rosa Gini

# v 1.1 28 Sep

# aggiunta Toscana


# v 1.0

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
for (i in drug_names) {
  
  tab <- readRDS(paste0(thisdirinput, "D4_prevalence_incidence_", i, ".rds"))
  
 
  
  # merge con popolazione  
  
  tab <- merge(tab, D4_pop_ASL, by = c("year", "ASL"), all = TRUE)
  
  # aggiunge totali
  
  toadd <- copy(tab)
  toadd[, ASL := "Toscana"]
  tab <- rbind(tab, toadd)
  
  
  # crea frequenze
  
  tab <- tab[, .(prevalent = sum(is_prevalent),
                 incident = sum(is_incident), 
                 sumpop18 = sum(pop18)), .(year, ASL)]
  
  
  tab[, `:=`(prevalence=prevalent/sumpop18,
             incidence=incident/sumpop18)]
  
 
 
  # save
 
  saveRDS(tab, file = paste0(thisdiroutput, "/D5_Figure_1_prevalence_incidence_", i, ".rds") )
  write.csv(tab, file = paste0(thisdiroutput, "/D5_Figure_1_prevalence_incidence_", i, ".csv"))
  
}

