# author: Rosa Gini

# v 1.0 25 Sep 2026

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
  
  processing <- readRDS(file.path(thisdirinput, paste0("D3_pop_con_variabili_prevalenza_", i, ".rds")))

  years <- 2016:2025
  
  results <- lapply(years, function(y) {
    asl_col  <- paste0("asl_", y)
    inc_col  <- paste0("is_incident_", y)
    prev_col <- paste0("is_prevalent_", y)
    
    out <- processing[, .(
      is_incident  = sum(get(inc_col), na.rm = TRUE),
      is_prevalent = sum(get(prev_col), na.rm = TRUE)
    ), by = .(asl = get(asl_col))]
    
    out[, year := y]
    out
  })
  
  processing <- rbindlist(results)
  setcolorder(processing, c("asl", "year", "is_incident", "is_prevalent"))
  
  setnames(processing, "asl","ASL")
  
  #clean
  
  processing <- merge(base,processing, by =  c("year", "ASL"), all.x = T)
  
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
