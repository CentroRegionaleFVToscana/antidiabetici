
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
df <- readRDS(file.path(thisdirinput, "D4_prevalence_incidence.rds"))

# create frequency tables

for (i in drug_names_s) {

tab <- df[drug==i, .(prevalent = sum(is_prevalent),
              incident = sum(is_incident)), .(year, ASL)]

assign(paste0("D5_prevalence_incidence_", i), tab)

}

# save

for (i in drug_names_s) {
  
  saveRDS(get(paste0("D5_prevalence_incidence_", i)), file = paste0(thisdiroutput, "/D5_prevalence_incidence_", i, ".rds") )
  write.csv(get(paste0("D5_prevalence_incidence_", i)), file = paste0(thisdiroutput, "/D5_prevalence_incidence_", i, ".csv"))
  
}

