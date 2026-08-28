# author: Sabrina Giometto

# v 1.0 05 Giu 2026 Creation of D5 started

# v 1.1 24 Giu 2026 Creation of D5 completed

#########################################

if (TEST){
  testname <- "test_D3_selezione_coorte_nome_farmaco"
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
  
  medicines <- as.data.table(get(load(file.path(thisdirinput, paste0(i,".RData")))[[1]]))
  setnames(medicines, "ID", "person_id")
  
  medicines <- medicines[,.(person_id, DATE)]
  
  obsperiods <- readRDS(file = file.path(thisdirinput, "D3_OBSPERIODS.rds"))
  
  asl <- readRDS(file = file.path(thisdirinput, "D3_ASL.rds"))
  
  persons <- readRDS(file = file.path(thisdirinput, "D3_PERSONS.rds"))
  
  ################################
  # start processing
  
  processing <- persons
  
  
  ############################
  # selection variables
  
  processing[, sel := 0]
  
  # sel_data_incomplete
  
  thissel <- "sel_data_incomplete" 
  
  processing[ , (thissel) := fifelse( sel == 1 | birth_date_or_gender_invalid == 1, 1, 0) ]
  
  processing[ , sel := fifelse(get(thissel) == 1 , 1, 0) ]
  
  # sel_no_obs_periods, sel_obs_period_not_overlapped_study_period, sel_never18plus_during_study_period

  obsenriched <- merge(processing, obsperiods, by = "person_id", all.x = T)
  
  obsenriched[, date_18th_birthday := birth_date %m+% years(18)]
  
  thissel <- "sel_no_obs_periods" 
  
  temp <- unique(obsenriched[is.na(start_op),.(person_id)])
  
  temp[, (thissel) := 1]
  
  processing <- merge(processing, temp, by = "person_id", all.x = T)
  
  processing[ , (thissel) := fifelse( sel == 1 | get(thissel) == 1, 1, 0, 0) ]
  
  processing[ , sel := fifelse(sel == 1 |get(thissel) == 1 , 1, 0) ]
  
  thissel <- "sel_obs_period_not_overlapped_study_period"
  
  temp <- unique(obsenriched[start_op <= study_end_date & end_op >= study_start_date,.(person_id)])
  
  temp[, (thissel) := 0]
  
  processing <- merge(processing, temp, by = "person_id", all.x = T)
  
  processing[ , (thissel) := fifelse( sel == 1 | is.na(get(thissel)), 1, 0, 0) ]
  
  processing[ , sel := fifelse(sel == 1 | get(thissel) == 1 , 1, 0) ]  
  
  thissel <- "sel_never18plus_during_study_period"
  
  temp <- obsenriched[start_op <= study_end_date & end_op >= study_start_date & date_18th_birthday <= end_op & date_18th_birthday <= study_end_date ,]
  
  temp <- unique(temp[,.(person_id)])
  
  temp[, (thissel) := 0]
  
  processing <- merge(processing, temp, by = "person_id", all.x = T)
  
  processing[ , (thissel) := fifelse( sel == 1 | is.na(get(thissel)), 1, 0, 0) ]
  
  processing[ , sel := fifelse(sel == 1 | get(thissel) == 1 , 1, 0) ]  
  
  # sel_no_drug

  thissel <- "sel_no_drug"
  
  medenriched <- merge(processing[sel == 0,], medicines, by = "person_id", all = F)
  
  temp <- unique(medenriched[,.(person_id)])

  temp[, (thissel) := 0]
  
  processing <- merge(processing, temp, by = "person_id", all.x = T)
  
  processing[ , (thissel) := fifelse( sel == 1 | is.na(get(thissel)), 1, 0) ]
  
  processing[ , sel := fifelse(sel == 1 | get(thissel) == 1 , 1, 0) ]
  
  # sel_no_drug_during_obs_period_correct_age
  
  thissel <- "sel_no_drug_during_obs_period_correct_age"

  obsenriched <- merge(processing[sel == 0,.(person_id)], obsenriched, by = "person_id", all = F)

  obsenriched <- obsenriched[start_op <= study_end_date & end_op >= study_start_date & date_18th_birthday <= end_op & date_18th_birthday <= study_end_date ,]
  
  temp <- merge(medicines, obsenriched, by = "person_id", allow.cartesian = T, all = F)  

  temp <- temp[DATE >= study_start_date & DATE >= date_18th_birthday & DATE <= study_end_date ,]
  
  setorder(temp, person_id, DATE)
  
  temp[, n := rowid(person_id)]
  
  temp <- temp[ n == 1, .(person_id, start_op, end_op, DATE)]
  
  setnames(temp, c("start_op", "end_op", "DATE"), c("start_study_op", "end_study_op", "date_first"))
  
  temp[, (thissel) := 0]

  processing <- merge(processing, temp, by = "person_id", all.x = T)
  
  processing[ , (thissel) := fifelse( sel == 1 | is.na(get(thissel)), 1, 0, 1) ]
  
  processing[ , sel := fifelse(sel == 1 | get(thissel) == 1 , 1, 0) ]
  
  # sel_no_lookback
  
  thissel <- "sel_no_lookback"
  
  processing[ , (thissel) := fifelse( sel == 1 | date_first < start_study_op + 730, 1, 0, 1) ]
  
  processing[ , sel := fifelse(sel == 1 | get(thissel) == 1 , 1, 0) ]
  
  # sel_no_ASL
  
  thissel <- "sel_no_ASL"
  
  asl <- merge(asl, processing[sel == 0,.(person_id)], all = F)
  
  processing[, ref_date := date_first]
    
  processing <- asl[
    processing,
    on = .(
      person_id,
      start_d <= ref_date,
      end_d >= ref_date
    )  
  ]
  
  processing[, c("start_d", "end_d") := NULL]  

  processing[ , (thissel) := fifelse( sel == 1 | is.na(ASL), 1, 0) ]
  
  processing[ , sel := fifelse(sel == 1 | get(thissel) == 1 , 1, 0) ]
  
  
  ####################
  # final variables

  
  # is_in_study
  
  processing[, is_in_study := fifelse(sel == 0, 1, 0)]

  # is_prevalent
  
  temp <- merge(medicines, processing[is_in_study == 1,.(person_id), all = F])
  
  temp <- temp[, .(min = min(DATE)), by = "person_id" ]
  
  processing <- merge(processing, temp, by = "person_id", all.x = T)
  
  processing[, is_prevalent := fifelse(is_in_study == 1 & min < date_first, 1L, 0L, NA_integer_)]
  
  # period
  
  processing[, period := fcase(
    date_first >= study_start_date & date_first <= end_date_period[["pre"]], "pre",
    date_first >= start_date_period[["nota"]] & date_first <= end_date_period[["nota"]], "nota",
    date_first >= start_date_period[["modi"]], "modi"
  )]
      
  # clean and save
  
  tokeep <- c("person_id", "sel_data_incomplete", "sel_no_obs_periods", "sel_obs_period_not_overlapped_study_period", "sel_never18plus_during_study_period", "sel_no_drug", "sel_no_drug_during_obs_period_correct_age", "date_first", "start_study_op", "end_study_op", "sel_no_lookback", "sel_no_ASL", "is_in_study", "is_prevalent", "period", "ASL", "birth_date", "gender")
  
  processing <- processing[, ..tokeep]
  
  nameoutputfile <- paste0("D3_selezione_coorte_", i, ".rds")
  
  saveRDS(processing, file = file.path(thisdiroutput, nameoutputfile))
  
  # study population
  
  processing <- processing[is_in_study == 1,]
  
  tokeep <- c("person_id", "birth_date", "gender","ASL", "date_first", "start_study_op", "end_study_op", "is_prevalent", "period")
  
  processing <- processing[, ..tokeep]
  
  nameoutputfile <- paste0("D3_pop_", i, ".rds")
  
  saveRDS(processing, file = file.path(thisdiroutput, nameoutputfile))
  
  # incident population
  
  processing <- processing[is_prevalent == 0,]
  
  tokeep <- c("person_id", "birth_date", "gender","ASL", "date_first", "start_study_op", "end_study_op",  "period")
  
  processing <- processing[, ..tokeep]
  
  nameoutputfile <- paste0("D3_incidence_", i, ".rds")
  
  saveRDS(processing, file = file.path(thisdiroutput, nameoutputfile))

}
