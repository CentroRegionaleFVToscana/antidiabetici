############################################################
#                                                          #
####         D6_Table_1                         #
#                                                          #
############################################################

# authors: Rosa Gini, Sabrina Giometto

# v 0.1

# 9 Jun 2026

print('CREATE D6_Table_1')

#########################################
# assign directories

if (TEST){ 
  testname <- "test_D6_Table_1"
  thisdirinput <- paste0(file.path(dirtest, testname), "/")
  thisdiroutput <- file.path(dirtest,testname,"g_output")
  dir.create(thisdiroutput, showWarnings = F)
  thislist_study_medicine <- "DPP4i"
  thisdirsettings <- thisdirinput
  settings <- readRDS(file.path(thisdirsettings,"SETTINGS.rds"))
}else{
    thisdirinput <- dirtemp
    thisdiroutput <- paste0(direxp,"/Formatted tables/")
    thislist_study_medicine <- list_study_medicine
    dir.create(thisdiroutput, showWarnings = F)
    thisdirarchive <- dirarchive
}

#########################################
# load

#########################################

print(paste("Now creating: D6_Table_1 in nice format"))
##################
# HELPERS

add_empty_row <- function(j){
  j <- j + 1
  tab_nice[, cell := ""]
  setnames(tab_nice, "cell", paste0("cell_",j))
  return(j)
}



descriptive_N_perc <- function(j, covar) {
  j <- j + 1
  varN <- paste0(covar,"_N_")
  varP <- paste0(covar,"_perc_")
  tab_nice[, cell := paste0(get(varN), " (",get(varP),"%)")]
  setnames(tab_nice, "cell", paste0("cell_",j))
  return(j)
}

descriptive_lab_values <- function(j, covar){
  j <- j + 1
  varM <- paste0(covar,"_mean_")
  varSD <- paste0(covar,"_sd_")
  tab_nice[, cell := paste0(get(varM), " (",get(varSD),")")]
  setnames(tab_nice, "cell", paste0("cell_",j))
  j <- j + 1
  varM <- paste0(covar,"_median_")
  varQ1 <- paste0(covar,"_q1_")
  varQ3 <- paste0(covar,"_q3_")
  tab_nice[, cell := paste0(get(varM), " (",get(varQ1),"-",get(varQ3),")")]
  setnames(tab_nice, "cell", paste0("cell_",j))ù
  # j <- j + 1
  # varN <- paste0(covar,"_exp_missingN_")
  # varP <- paste0(covar,"_exp_missingperc_")
  # tab_nice[, cell := paste0(get(varN), " (",get(varP),"%)")]
  # setnames(tab_nice, "cell", paste0("cell_exp_",j))  
  return(j)
}


##########################################
# WRITE TABLE(S)


for (nome_farmaco in thislist_study_medicine) {

  
  #########################################
  # ROW HEADERS OF THE TABLE
  
  # N
  # Age (years)
  # Mean (SD)
  # Median (Q1, Q3)
  # Gender
  # Male: N (%)
  # Female: N (%)
  # Other: O (%)
  # 
  # Time since diagnosis
  # Median (IQ range)
  # Min
  # Max
  # N in 1 quintile (years = yrsdia_exp_all_quint1) 
  # N in 2 quintile (years = yrsdia_exp_all_quint2) 
  # N in 3 quintile (years = yrsdia_exp_all_quint3) 
  # N in 4 quintile (years = yrsdia_exp_all_quint4) 
  # N in 5 quintile (years = yrsdia_exp_all_max) 
  # 
  # Number of visits since start of diagnosis
  # Median (IQ range)
  # Min
  # Max
  # 
  # (Co)Morbidities
  # Baseline liver enzyme 
  # AST (IU/L): mean (sd)
  # AST (IU/L): median (IQR)
  # AST: N missing (%)
  # ALT (IU/L): mean (sd)
  # ALT (IU/L): median (IQR)
  # ALT: % missing
  # Total Bilirubin (umol/L): mean (sd)
  # Total Bilirubin (umol/L): median (IQR)
  # %missing
  # Alkaline Phosphatase (IU/L): mean (sd)
  # Alkaline Phosphatase (IU/L): median (IQR)
  # %missing
  # Gamma-glutamyl transpeptidase (IU/L): mean (sd)
  # Gamma-glutamyl transpeptidase (IU/L): median (IQR)
  # %missing
  # 5’nucleotidase (IU/L): mean (sd)
  # 5’nucleotidase (IU/L): median (IQR)
  # %missing
  # Others
  # Alcohol use disorders 
  # Smoking status: never
  # Smoking status: former
  # Smoking status: current 
  # %missing
  # Liver disease: N (%)
  # Hypertension: N (%)
  # Creatinine clearance (mL/min): mean (sd)
  # Creatinine clearance (mL/min): median (IQR)
  # %missing
  
  #########################################
  # POPULATE ROWS
  
  namefile <- paste0("D5_Table_1_", nome_farmaco,".rds")
  tab_nice <- readRDS(file.path(dirinput,namefile))
  
  row_header_1 <- c()
  j <- 0
  
  # row 1
  
  row_header_1 = c(row_header_1,                  
                   "N" 
  )
  
  j <- j + 1
  tab_nice[, cell := N_]
  setnames(tab_nice, "cell", paste0("cell_",j))
  tab_nice[, cell := N_exp_]
  setnames(tab_nice, "cell", paste0("cell_exp_",j))
  
  row_header_1 <- c(  
                    "",
                    "",
                    "",
                    "")

  1 <- 1
  j <- add_empty_row(j)
  
  
  # rows 2-4
  
  row_header_1 <- c(row_header_1,  
                    "Età alla data indice, mediana (IQR)
  )
  
  
  j <- j + 1
  tab_nice[, cell := paste0(age_mean_, " (",age_sd_,")")]
  setnames(tab_nice, "cell", paste0("cell_",j))
  tab_nice[, cell := paste0(age_exp_mean_, " (",age_exp_sd_,")")]
  setnames(tab_nice, "cell", paste0("cell_exp_",j))
  
  j <- j + 1
  tab_nice[, cell := paste0(age_median_, " (",age_q1_, "-",age_q3_,")")]
  setnames(tab_nice, "cell", paste0("cell_",j))
  tab_nice[, cell := paste0(age_exp_median_, " (",age_exp_q1_, "-",age_exp_q3_,")")]
  setnames(tab_nice, "cell", paste0("cell_exp_",j))
  
  # rows 5-8
  
  row_header_1 <- c(row_header_1,  
                    "Gender",
                    "Male: N (%)", 
                    "Female: N (%)",
                    "Other: O (%)"
  )
  
  j <- add_empty_row(j)
  
  j <- descriptive_N_perc(j, "gender_M")
  j <- descriptive_N_perc(j, "gender_F")
  j <- descriptive_N_perc(j, "gender_O")
  
  
  # rows 9-18
  
  row_header_1 <- c(row_header_1,  
                    "",
  "Time since diagnosis",
  "Median (IQ range)",
  "Min",
  "Max",
  paste0("N in 1 quintile (years = ", unique(tab_nice$yrsdia_exp_all_quint1), ")"),
  paste0("N in 2 quintile (years = ", unique(tab_nice$yrsdia_exp_all_quint2), ")"),
  paste0("N in 3 quintile (years = ", unique(tab_nice$yrsdia_exp_all_quint3), ")"),
  paste0("N in 4 quintile (years = ", unique(tab_nice$yrsdia_exp_all_quint4), ")"),
  paste0("N in 5 quintile (years = ", unique(tab_nice$yrsdia_exp_all_max), ")")
  )
  
  j <- add_empty_row(j)
  j <- add_empty_row(j)
  
  j <- j + 1
  varM <- "yrsdia_median_"
  varQ1 <- "yrsdia_q3_"
  varQ3 <- "yrsdia_q3_"
  tab_nice[, cell := paste0(get(varM), " (",get(varQ1),"-",get(varQ3),")")]
  setnames(tab_nice, "cell", paste0("cell_",j))
  varM <- "yrsdia_exp_median_"
  varQ1 <- "yrsdia_exp_q3_"
  varQ3 <- "yrsdia_exp_q3_"
  tab_nice[, cell := paste0(get(varM), " (",get(varQ1),"-",get(varQ3),")")]
  setnames(tab_nice, "cell", paste0("cell_exp_",j))
  
  j <- j + 1
  varM <- "yrsdia_min_"
  tab_nice[, cell := paste0(get(varM))]
  setnames(tab_nice, "cell", paste0("cell_",j))
  varM <- "yrsdia_exp_min_"
  tab_nice[, cell := paste0(get(varM))]
  setnames(tab_nice, "cell", paste0("cell_exp_",j))
  
  j <- j + 1
  varM <- "yrsdia_max_"
  tab_nice[, cell := paste0(get(varM))]
  setnames(tab_nice, "cell", paste0("cell_",j))
  varM <- "yrsdia_exp_max_"
  tab_nice[, cell := paste0(get(varM))]
  setnames(tab_nice, "cell", paste0("cell_exp_",j))
  
  for (s in 1:5) {
    j <- j + 1
    varN <- paste0("quintile_daysdia_",s,"_N_")
    varP <- paste0("quintile_daysdia_",s,"_perc_")
    tab_nice[, cell := paste0(get(varN), " (",get(varP),"%)")]
    setnames(tab_nice, "cell", paste0("cell_",j))
    varN <- paste0("quintile_daysdia_exp_",s,"_N_")
    varP <- paste0("quintile_daysdia_exp_",s,"_perc_")
    tab_nice[, cell := paste0(get(varN), " (",get(varP),"%)")]
    setnames(tab_nice, "cell", paste0("cell_exp_",j))  
  }
  
  
  # rows 19-23
  
  row_header_1 <- c(row_header_1,  
                    "",
                    "Number of visits since start of diagnosis",
                    "Median (IQ range)",
                    "Min",
                    "Max")
  
  
  j <- add_empty_row(j)
  j <- add_empty_row(j)
  
  j <- j + 1
  varM <- "visits_median_"
  varQ1 <- "visits_q3_"
  varQ3 <- "visits_q3_"
  tab_nice[, cell := paste0(get(varM), " (",get(varQ1),"-",get(varQ3),")")]
  setnames(tab_nice, "cell", paste0("cell_",j))
  varM <- "visits_exp_median_"
  varQ1 <- "visits_exp_q3_"
  varQ3 <- "visits_exp_q3_"
  tab_nice[, cell := paste0(get(varM), " (",get(varQ1),"-",get(varQ3),")")]
  setnames(tab_nice, "cell", paste0("cell_exp_",j))
  
  j <- j + 1
  varM <- "visits_min_"
  tab_nice[, cell := paste0(get(varM))]
  setnames(tab_nice, "cell", paste0("cell_",j))
  varM <- "visits_exp_min_"
  tab_nice[, cell := paste0(get(varM))]
  setnames(tab_nice, "cell", paste0("cell_exp_",j))
  
  j <- j + 1
  varM <- "visits_max_"
  tab_nice[, cell := paste0(get(varM))]
  setnames(tab_nice, "cell", paste0("cell_",j))
  varM <- "visits_exp_max_"
  tab_nice[, cell := paste0(get(varM))]
  setnames(tab_nice, "cell", paste0("cell_exp_",j))
  
  
  # rows 24-44
  
  row_header_1 <- c(row_header_1,  
                    "",
                    "(Co)Morbidities",
                    "Baseline liver enzyme",
                    "AST (IU/L): mean (sd)",
                    "AST (IU/L): median (IQR)",
                    "AST: N missing (%)",
                    "ALT (IU/L): mean (sd)",
                    "ALT (IU/L): median (IQR)",
                    "ALT: % missing",
                    "Total Bilirubin (umol/L): mean (sd)",
                    "Total Bilirubin (umol/L): median (IQR)",
                    "Total Bilirubin: % missing",
                    "Alkaline Phosphatase (IU/L): mean (sd)",
                    "Alkaline Phosphatase (IU/L): median (IQR)",
                    "Alkaline Phosphatase: % missing",
                    "Gamma-glutamyl transpeptidase (IU/L): mean (sd)",
                    "Gamma-glutamyl transpeptidase (IU/L): median (IQR)",
                    "Gamma-glutamyl transpeptidase: % missing",
                    "5’nucleotidase (IU/L): mean (sd)",
                    "5’nucleotidase (IU/L): median (IQR)",
                    "5’nucleotidase: % missing")
  
  
  j <- add_empty_row(j)
  j <- add_empty_row(j)
  j <- add_empty_row(j)
  
  
  j <- descriptive_lab_values(j, "level_AST")
  j <- descriptive_lab_values(j, "level_ALT")
  j <- descriptive_lab_values(j, "level_total_bilirubin")
  j <- descriptive_lab_values(j, "level_ALP")
  j <- descriptive_lab_values(j, "level_GGT")
  j <- descriptive_lab_values(j, "level_5_NT")
  
  # rows 45-55
  
  row_header_1 <- c(row_header_1,  
                    "Others",
                    "Alcohol use disorders",
                    "Smoking status: never",
                    "Smoking status: former",
                    "Smoking status: current",
                    "Smoking status: % missing",
                    "Liver disease: N (%)",
                    "Hypertension: N (%)",
  "Creatinine clearance (mL/min): mean (sd)",
  "Creatinine clearance (mL/min): median (IQR)",
  "Creatinine clearance: % missing")
  
  j <- add_empty_row(j)
  
  j <- descriptive_N_perc(j, "alcohol_use_disorders")
  
  j <- descriptive_N_perc(j, "smoking_0")
  j <- descriptive_N_perc(j, "smoking_1")
  j <- descriptive_N_perc(j, "smoking_2")
  
  covar <- "smoking"
  j <- j + 1
  varN <- paste0(covar,"_missingN_")
  varP <- paste0(covar,"_missingperc_")
  tab_nice[, cell := paste0(get(varN), " (",get(varP),"%)")]
  setnames(tab_nice, "cell", paste0("cell_",j))
  varN <- paste0(covar,"_exp_missingN_")
  varP <- paste0(covar,"_exp_missingperc_")
  tab_nice[, cell := paste0(get(varN), " (",get(varP),"%)")]
  setnames(tab_nice, "cell", paste0("cell_exp_",j))  
  
  
  j <- descriptive_N_perc(j, "liver_disease")
  
  j <- descriptive_N_perc(j, "hypertension")
  
  j <- descriptive_lab_values(j, "creatinine_clearance")
  
  # View(tab_nice[, .SD, .SDcols = patterns("^cell_")])
  
  
  #########################################
  # KEEP CELLS
  
  cell_cols <- grep("^cell_", names(tab_nice), value = TRUE)
  tokeep <- c("year", cell_cols)
  tab_nice <- tab_nice[, ..tokeep]
  
  
  #########################################
  # RESHAPE CELLS
  
  # First reshape tab_nice from wide into long format
  
  tab_nice <- melt(
    tab_nice,
    id.vars = "year",
    measure.vars = patterns(
      cell = "^cell_[0-9]+$",
      cell_exp = "^cell_exp_[0-9]+$"
    ),
    variable.name = "rownum"
  )
  
  tab_nice[, rownum := as.integer(rownum)]
  
  setorder(tab_nice, rownum, year)
  
  # add column header
  
  colheader <- copy(tab_nice)[ rownum == 1,]
  colheader[,  rownum := 0]
  colheader[,  cell := paste0("Persons with diagnosis and in the study during ", year)]
  colheader[,  cell_exp := paste0("Including persons with first prescription of tolvaptan in ", year)]
  
  tab_nice <- rbind(colheader, tab_nice)
  setorder(tab_nice, rownum, year)
  
  
  # then reshape from long to wide keeping rownum as the UoO
  
  tab_nice <- dcast(
    tab_nice,
    rownum ~ year,
    value.var = c("cell", "cell_exp")
  )
   
  # Order rows correctly
  setorder(tab_nice, rownum)
  
   
  #########################################
  # ORDER COLUMNS
  # 
  years <- sort(unique(tab_nice_all[[suffix]]$year))

  ordered_cols <- c(
    "rownum",
    as.vector(rbind(
      paste0("cell_", years),
      paste0("cell_exp_", years)
    ))
  )
  
  setcolorder(tab_nice, ordered_cols)
  
  #########################################
  # ADD ROW HEADER
  # 
  
  tab_nice[, row_header := c("",row_header_1)]
  tab_nice[, rownum := NULL]
  
  #########################################
  # FINAL COLUMNS
  
  colorder <- c("row_header",setdiff(names(tab_nice),"row_header"))
  tab_nice <- tab_nice[, ..colorder]
  
  #########################################
  # NAMES
  
  new_names <- names(tab_nice)
  
  # cell_2015 -> 2015, cell_2016 -> 2016, etc.
  new_names <- sub("^cell_([0-9]{4})$", "31 Dec \\1", new_names)
  
  # row_header -> empty
  new_names[new_names == "row_header"] <- ""
  
  # cell_exp_2015, cell_exp_2016, ... -> empty
  new_names[grepl("^cell_exp_[0-9]{4}$", new_names)] <- ""
  
  setnames(tab_nice, new_names)
  
  #########################################
  # SAVE
  
  outputfile <- tab_nice
  nameoutput <- paste0("D6_Table_1_",nome_farmaco)
  assign(nameoutput, outputfile)
  # rds
  saveRDS(outputfile, file = file.path(thisdiroutput, paste0(nameoutput,".rds")))
  # csv
  fwrite(outputfile, file = file.path(thisdiroutput, paste0(nameoutput,".csv")))
  # xls
  write_xlsx(outputfile, file.path(thisdiroutput, paste0(nameoutput,".xlsx")))
  # html
  html_table <- kable(outputfile, format = "html", escape = FALSE) %>% kable_styling(full_width = F, bootstrap_options = c("striped", "hover"))
  writeLines(html_table, file.path(thisdiroutput, paste0(nameoutput,".html")))
  # rtf
  doc <- read_docx() %>% body_add_table(outputfile, style = "table_template") %>% body_end_section_continuous()
  print(doc, target = file.path(thisdiroutput, paste0(nameoutput,".docx")))

}

