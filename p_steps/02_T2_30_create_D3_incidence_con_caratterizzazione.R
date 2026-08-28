# author: Rosa Gini

# v 1.0 28 Aug 2026

#########################################

if (TEST){
  testname <- "test_D3_coorte_con_caratterizzazione"
  thisdirinput <- file.path(dirtest,testname)
  thisdiroutput <- file.path(dirtest,testname,"g_output")
  dir.create(thisdiroutput, showWarnings = F)
  thisdrug_names <- c("SGLT2i")
}else{
  thisdirinput <- dirtemp
  thisdiroutput <- dirtemp
  thisdrug_names <- drug_names
}

parameters_this_step <- as.data.table(unique(readxl::read_excel(file.path(dirarchive,"codebooks",paste0("D3_incidence_con_caratterizzazione.xlsx")),1)))

component_variables <- unlist(unique(parameters_this_step[parameter == "component",.(value)]))

i <- "SGLT2i"

for (i in thisdrug_names) {
  
  print(i)
  
  # load data
  
  processing <- readRDS(file.path(thisdirinput, paste0("D3_incidence_", i, ".rds")))

  # age
  
  processing[, age := age_fast(birth_date, date_first)]
  
  # ageband

  processing[, ageband := fcase(
    age >= 18 & age <= 44, "18-45",
    age >= 45 & age <= 64, "45-64",
    age >= 18 & age <= 44, "65-74",
    age >= 18 & age <= 44, "75+"
  )
             ]
  # genere
  
  processing[, genere := gender]
  
  # simple variables and variables that are comonents to more complex variables
    
  for (component in component_variables) {
    processing[, (component) := 0]
    ingredients <- unlist(unique(parameters_this_step[parameter == component,.(value)]))
    for (ingredient in ingredients) {
      temp <- as.data.table(get(load(file.path(thisdirinput, paste0(ingredient,".RData")))[[1]]))
      num <- unlist(unique(parameters_this_step[parameter ==  component & value == ingredient,.(howmany)]))
      setnames(temp, "ID", "person_id")
      temp[, person_id := as.character(person_id)]
      temp <- unique(temp[,.(person_id, DATE)])
      temp <- temp[DATE >= study_start_date - 730,]
      temp <- merge(processing[,.(person_id, date_first)],temp, by = "person_id", all = F)
      temp <- temp[DATE >= date_first - 730 & DATE <= date_first,]
      temp[, n := rowid(person_id)]
      temp <- temp[n == num,]
      temp[, temp := 1]
      tokeep <- c("person_id", "temp")
      temp <- temp[,..tokeep]
      processing <- merge(processing, temp, by = "person_id", all.x = T)
      processing[is.na(temp), temp := 0]
      processing[, (component) := pmax(get(component), temp)]
      processing[, temp := NULL]
    }
    
  }
  
  # age50plus
  
  processing[, age50plus := fifelse(age >= 50, 1 , 0)]
  
  # Cvriskfactors
  
  processing[, Cvriskfactors := fifelse(age50plus + dyslipidemia + obesity + hypertension + smoking >= 3 , 1 , 0)]
  
  # RENDIS_Alg1
  
  processing[, RENDIS_Alg1 := fifelse(RENDIS_Alg1_1 + RENDIS_Alg1_2 + RENDIS_Alg1_3 == 3 , 1 , 0)]
  
  # CV
  
  processing[, CV := fifelse(IHD + AMI + bypass + angioplastic >= 1  , 1 , 0)]
  
  # cerebro

  processing[, cerebro := fifelse(STROKE + TIA + carot  >= 1  , 1 , 0)]
  
  # Cvrisk
  
  processing[, Cvrisk := fifelse((ateros + organdamage + Cvriskfactors) >= 1 & CV == 0 , 1 , 0)]
  
  # renal
  
  processing[, renal := fifelse(RENDIS_Alg1 + RENDIS_Alg2 >= 0 , 1 , 0)]
  
  
  
  
  # clean and save
  
  tokeep <- c("person_id", "date_first", "period", "ASL", "age", "ageband", "genere", "met", "antidiabother", "IHD", "AMI", "bypass", "angioplastic", "STROKE", "TIA", "carot", "ateros", "organdamage", "age50plus", "dyslipidemia", "obesity", "hypertension", "smoking", "Cvriskfactors", "RENDIS_Alg1_1", "RENDIS_Alg1_2", "RENDIS_Alg1_3", "RENDIS_Alg1", "RENDIS_Alg2", "CV", "cerebro", "aop", "Cvrisk", "HF", "renal")

  processing <- processing[, ..tokeep]

  nameoutputfile <- paste0("D3_incidence_con_caratterizzazione_", i, ".rds")

  saveRDS(processing, file = file.path(thisdiroutput, nameoutputfile))
  

}
