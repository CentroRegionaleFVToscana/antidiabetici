
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

# load
for (k in drug_names_s) {
  
  df <- readRDS(paste0(thisdirinput, "D4_prevalence_incidence_", k, ".rds"))
  
  assign(paste0("D4_prevalence_incidence_", k), df)
  
}

D4_pop_ASL <- readRDS(file.path(thisdirinput, "D4_pop_ASL.rds"))

# bind rows
D4_prevalence_incidence <- rbindlist(mget(ls(pattern = "^D4_prevalence_incidence_")))

# create frequency tables

for (i in drug_names_s) {

tab <- D4_prevalence_incidence[drug==i, .(prevalent = sum(is_prevalent),
                               incident = sum(is_incident)), .(year, ASL)]

tab <- merge(tab, D4_pop_ASL, by = c("year", "ASL"), all = TRUE)

tab[, `:=`(prevalence=prevalent/pop18,
           incidence=incident/pop18)]

assign(paste0("D5_prevalence_incidence_", i), tab)

}

# save

for (i in drug_names_s) {
  
  saveRDS(get(paste0("D5_prevalence_incidence_", i)), file = paste0(thisdiroutput, "/D5_Figure_1_prevalence_incidence_", i, ".rds") )
  write.csv(get(paste0("D5_prevalence_incidence_", i)), file = paste0(thisdiroutput, "/D5_Figure_1_prevalence_incidence_", i, ".csv"))
  
}

