# author: Rosa Gini

# v 1.0 24 Sep 2026

#########################################

if (TEST){
  testname <- "test_D4_prevalence_incidence_nome_farmaco"
  thisdirinput <- file.path(dirtest,testname)
  thisdiroutput <- file.path(dirtest,testname,"g_output")
  dir.create(thisdiroutput, showWarnings = F)
  thisdrug_names <- c("SGLT2i")
}else{
  thisdirinput <- dirtemp
  thisdiroutput <- dirtemp
  thisdrug_names <- drug_names
}
i <- "SGLT2i"

for (i in thisdrug_names) {
  
  print(i)
  
  # load data
  
  ASL <- c("CE", "SE", "NO")

  base <- CJ(year = 2016:2025,ASL = ASL)
  
  processing <- readRDS(file.path(thisdirinput, paste0("D3_pop_", i, ".rds")))

  
  
  processing[, year := year(date_first)]
  processing[, is_incident := 1 - is_prevalent]
  
  # aggregate
  
  processing <- processing[, .(is_prevalent = sum(is_prevalent), is_incident = sum(is_incident)), by = c("year", "ASL")]
  
  #clean
  
  processing <- merge(base,processing, by =  c("year", "ASL"), all = T)
  
  processing[is.na(is_prevalent), is_prevalent := 0]
  processing[is.na(is_incident), is_incident := 0]
  
  setorder(processing, year, ASL)
  
  # clean and save
  
  # tokeep <- c("year",  "ASL", "is_prevalent", "is_incident")
  # 
  # processing <- processing[, ..tokeep]

  nameoutputfile <- paste0("D4_prevalence_incidence_", i, ".rds")

  saveRDS(processing, file = file.path(thisdiroutput, nameoutputfile))
  

}
