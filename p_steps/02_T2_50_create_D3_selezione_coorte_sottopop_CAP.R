# author: Rosa Gini

# v 1.0 1 Oct 2026

#########################################

if (TEST){
  testname <- "test_D3_selezione_coorte_sottopop_CAP_nome_farmaco"
  thisdirinput <- file.path(dirtest,testname)
  thisdircsv <- thisdirinput
  thisdiroutput <- file.path(dirtest,testname,"g_output")
  dir.create(thisdiroutput, showWarnings = F)
  thisdrug_names <- c("SGLT2i")
}else{
  thisdirinput <- dirtemp
  thisdircsv <- dirinput
  thisdiroutput <- dirtemp
  thisdrug_names <- drug_names
}


cap <- fread(file.path(thisdircsv,"SURVEY.csv"))

date_cols <- c("data")

for (datevar in date_cols) {
  print(datevar)
  cap[, (datevar) := ymd(get(datevar))]
}

setnames(cap, c("id", "data"), c("person_id", "date"))

i <- "SGLT2i"


for (i in thisdrug_names) {
  
  print(i)
  
  # load data
  
  input <- readRDS(file.path(thisdirinput, paste0("D3_incidence_con_caratterizzazione_", i, ".rds")))
  
  processing <- copy(input)
  
  # merge with pregnancies
  
  processing[, ref_date := date_first]
  processing[, ref_date_ll := date_first - 1065]
  
  temp <- cap[
    processing,
    on = .(
      person_id,
      date <= ref_date,
      date >= ref_date_ll 
    ),
    allow.cartesian = T
  ]
  
  # keep most recent pregnancy
  
  temp <- temp[!is.na(obs), ]
  
  setorder(temp, person_id, obs, date_first, -date)
  temp[, N := seq_len(.N), by=c("person_id", "obs")]
  temp <- temp[N == 1,]
  tokeep <- c("person_id", "obs", "valore")
  temp <- temp[, ..tokeep]
  
  temp <- dcast(temp,
                person_id ~ obs,
                value.var = "valore"
                )
  # define sel_with_preg
  
    temp[, sel_with_preg := 0]

  processing <- merge(processing, temp, all.x = T)
  processing[is.na(sel_with_preg) , sel_with_preg := 1]
  

  # save selection
  
  outputfile <- copy(processing)
  tokeep <- c("person_id","sel_no_cap")
  nameoutputfile <- paste0("D3_selezione_coorte_sottopop_CAP_", i, ".rds")
  
  saveRDS(outputfile, file = file.path(thisdiroutput, nameoutputfile))
  
  # keep persons with pregnancy
  
  processing <- processing[sel_with_preg == 0,]
  
  # diab_gestaz
  
  processing[DIAB_GEST == 1, diab_gestaz := 1]
  processing[DIAB_GEST == 2, diab_gestaz := 0]
  
  # diab_pregrav
  
  processing[DIAB_PREGR == 1, diab_pregrav := 1]
  processing[DIAB_PREGR == 2, diab_pregrav := 0]
  
  
  # bmi
  
  processing[, bmi := PESO_PRE/(ALTEZZA/100)^2]
  
  # cat bmi
  
  processing[!is.na(bmi), bmi_low := fifelse(bmi < 18.5, 1, 0)]
  processing[!is.na(bmi), bmi_medium := fifelse(bmi >= 18.5 & bmi <= 24.5, 1, 0)]
  processing[!is.na(bmi), bmi_high := fifelse(bmi > 24.5, 1, 0)]
  
  
  # clean and save
  
  var_car <- c("age", "ageband", "genere", "met", "antidiabother", "IHD", "AMI", "bypass", "angioplastic", "STROKE", "TIA", "carot", "ateros", "organdamage", "age50plus", "dyslipidemia", "obesity", "hypertension", "smoking", "Cvriskfactors", "RENDIS_Alg1_1", "RENDIS_Alg1_2", "RENDIS_Alg1_3", "RENDIS_Alg1", "RENDIS_Alg2", "CV", "cerebro", "aop", "HF", "Cvrisk", "Cvtotal", "renal", "study_drugs","anyantidiab")
  
  # tokeep <- c("person_id", var_car)
  # 
  # input <- input[, ..tokeep]
  # 
  # processing <- merge(processing, input, by = "person_id", all.x = T)
  # 
  tokeep <- c("person_id", "date_first", "period", "ASL", "diab_gestaz", "diab_pregrav", "bmi_low", "bmi_medium", "bmi_high", var_car)

  processing <- processing[, ..tokeep]

  nameoutputfile <- paste0("D3_incidence_con_caratterizzazione_sottopop_CAP_", i, ".rds")

  saveRDS(processing, file = file.path(thisdiroutput, nameoutputfile))
  

}
