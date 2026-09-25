# author: Rosa Gini

# v 1.0 24 Sep 2026
#########################################

if (TEST){
  testname <- "test_D3_pop_con_variabili_prevalenza"
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

asl <- readRDS(file = file.path(thisdirinput, "D3_ASL.rds"))


for (i in thisdrug_names) {
  
  print(i)
  
  # load data
  
  processing <- readRDS(file.path(thisdirinput, paste0("D3_pop_", i, ".rds")))
  processing <- processing[, .(person_id, start_study_op,end_study_op, is_prevalent, date_first, birth_date)]
  
  processing[,date_18th_birthday := birth_date %m+% years(18)]  
  
  medicines <- as.data.table(get(load(file.path(thisdirinput, paste0(i,".RData")))[[1]]))
  
  medicines[, yearmed := year(DATE)]
  
  setnames(medicines, "ID", "person_id")
  
  medicines <- unique(medicines[,.(person_id, yearmed)])

  
  for (year in 2016:2025) {
    medicines[, in_ := fifelse(yearmed == year, 1 ,0)]
    temp <- copy(medicines)[in_ == 1,.(person_id, in_)]
    processing <- merge(processing, temp, by = "person_id", all.x = T)
    processing[end_study_op < ymd(paste0(year,"0101")), in_ := 0]
    processing[date_18th_birthday >ymd(paste0(year,"1231")), in_ := 0]
    processing[in_ == 1 & is_prevalent == 0 & year(date_first) == year , is_incident_:= 1]
    processing[in_ == 1 & is_prevalent == 1  & year(date_first) == year , is_prevalent_ := 1]
    processing[in_ == 1 &  year(date_first) < year , is_prevalent_ := 1]
    processing[is.na(in_), in_ := 0]
    processing[is.na(is_incident_), is_incident_ := 0]
    processing[is.na(is_prevalent_), is_prevalent_ := 0]
    
    setnames(processing, c("in_", "is_prevalent_", "is_incident_"), paste0(c("in_", "is_prevalent_", "is_incident_"), year))
    
    medicines[, in_ := NULL]
    
    processing[, ref_date := ymd(paste0(year,"1231"))]
    
    processing <- asl[
      processing,
      on = .(
        person_id,
        start_d <= ref_date,
        end_d >= ref_date
      )  
    ]
    
    processing[, c("start_d", "end_d") := NULL]  
    
    setnames(processing, c("ASL"), paste0(c("asl_"), year))
  }
  
  processing[, is_prevalent := NULL]
  
  # clean and save
  
  # tokeep <- c("person_id", "date_first", "period", "ASL", "age", "ageband", "genere", "met", "antidiabother", "IHD", "AMI", "bypass", "angioplastic", "STROKE", "TIA", "carot", "ateros", "organdamage", "age50plus", "dyslipidemia", "obesity", "hypertension", "smoking", "Cvriskfactors", "RENDIS_Alg1_1", "RENDIS_Alg1_2", "RENDIS_Alg1_3", "RENDIS_Alg1", "RENDIS_Alg2", "CV", "cerebro", "aop", "HF", "Cvrisk", "Cvtotal", "renal", "study_drugs","anyantidiab")
  # 
  # processing <- processing[, ..tokeep]

  nameoutputfile <- paste0("D3_pop_con_variabili_prevalenza_", i, ".rds")

  saveRDS(processing, file = file.path(thisdiroutput, nameoutputfile))
  

}
