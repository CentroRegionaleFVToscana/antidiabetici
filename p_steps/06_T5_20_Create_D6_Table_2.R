############################################################
#                                                          #
####         D6_Table_2_baseline_cohort                         #
#                                                          #
############################################################

# authors: Rosa Gini

# v 1.1

# added name of data sources

# v 1.0 

# 11 Jun 2026

# v 0.1

# 8 Jun 2026

print('CREATE D6_Table_2_baseline_cohort')

#########################################
# assign directories

if (TEST){ 
  testname <- "test_18_D6_Table_2"
  thisdirinput <- paste0(file.path(dirtest, testname), "/")
  thisdiroutput <- file.path(dirtest,testname,"g_output")
  dir.create(thisdiroutput, showWarnings = F)
  thisdirsettings <- thisdirinput
  settings <- readRDS(file.path(thisdirsettings,"SETTINGS.rds"))
  thisdatasources_for_postprocessing <- settings$ds
  thisdatasource <- settings$ds
  thisdirarchive <- thisdirinput
  thisfolder_submission <- list()
  thisfolder_submission[[thisdatasource]] <- thisdirinput
}else{
    thisdirinput <- direxp
    thisdiroutput <- direxp
    thisdirsettings <- dirpargen
    settings <- readRDS(file.path(thisdirsettings,"SETTINGS.rds"))
    thisdatasources_for_postprocessing <- settings$ds
    thisdirarchive <- dirarchive
    thisfolder_submission <- list()
    thisfolder_submission[[thisdatasource]] <- thisdirinput
}

#########################################
# load

# from settings: create the list of subpopulations
settings <- readRDS(file.path(thisdirsettings,"SETTINGS.rds"))
thissubpopulations <- unique(settings[,.(subpop)])
thissubpopulations <- unlist(strsplit(trimws(thissubpopulations), " +"))


#########################################

print(paste("Now creating: D6_Table_2_baseline_cohort"))

# this header works both locally and on DRE
# create the list list_ds_analysed of all datasources including their subpopulations (ds_subpop)
# store all the data in a list of datasets indexed by ds_subpop

 filetoread <- file.path(
      thisfolder_submission[[thisds]],
      paste0("D5_Table_2_XXX.csv")
    )
    
    if (file.exists(filetoread)) {
      
      temp <- fread(filetoread, colClasses = "character")
      
      suffix <- paste0("_",thisds,thissuffix)
      name_suffix[[suffix]] <- fifelse(unlist(PARPOP[ds == thisds & subpop == thissubpop,.(name_in_tables)]) == "", suffix, unlist(PARPOP[ds == thisds & subpop == thissubpop,.(name_in_tables)]))
      tab_nice_all[[suffix]] <- temp
      list_ds_analysed <- c(list_ds_analysed, suffix)
    }else{
      errmess <- paste("File",filetoread,"does not exist in the export folder of", thisds)
      stop(errmess)
    }

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
  varN <- paste0(covar,"N_")
  varP <- paste0(covar,"perc_")
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
  setnames(tab_nice, "cell", paste0("cell_",j))
  j <- j + 1
  varN <- paste0(covar,"_missingN_")
  varP <- paste0(covar,"_missingperc_")
  tab_nice[, cell := paste0(get(varN), " (",get(varP),"%)")]
  setnames(tab_nice, "cell", paste0("cell_",j))
  return(j)
}


##########################################
# WRITE TABLE(S)

#########################################
# POPULATE ROWS

tab_nice <- data.table()
for (suffix in list_ds_analysed) {
  temp <- copy(tab_nice_all[[suffix]])
  temp[, name_ds_subpop := name_suffix[[suffix]]]
  tab_nice  <- rbind(tab_nice, temp, fill = T)
}

# row 0

row_header_1 <- c("Datasource")
j <- 0
tab_nice[, cell := name_ds_subpop]
setnames(tab_nice, "cell", paste0("cell_",j))


# row 1

row_header_1 <- c(row_header_1,                  
                  "" 
)

j <- j + 1
tab_nice[stratum == "overall", cell := "Overall"]
tab_nice[stratum == "exp0", cell := "Dpp4-i"]
tab_nice[stratum == "exp1", cell := "Dapagliflozin"]
setnames(tab_nice, "cell", paste0("cell_",j))


# row 2

row_header_1 = c(row_header_1,                  
                 "N" 
)

j <- j + 1
tab_nice[, cell := N_]
setnames(tab_nice, "cell", paste0("cell_",j))

# row 2

row_header_1 <- c(row_header_1,  
                  "Characteristics")

j <- add_empty_row(j)

# rows 4-6

row_header_1 <- c(row_header_1,  
                  "Age (years)", 
                  "Mean (SD)", 
                  "Median (Q1 - Q3)"
)

j <- add_empty_row(j)

j <- j + 1
tab_nice[, cell := paste0(age_at_t0_mean_, " (",age_at_t0_sd_,")")]
setnames(tab_nice, "cell", paste0("cell_",j))

j <- j + 1
tab_nice[, cell := paste0(age_at_t0_median_, " (",age_at_t0_q1_, "-",age_at_t0_q3_,")")]
setnames(tab_nice, "cell", paste0("cell_",j))

# rows 7-10

row_header_1 <- c(row_header_1,  
                  "Gender",
                  "Male: N (%)", 
                  "Female: N (%)",
                  "Other: O (%)"
)

j <- add_empty_row(j)

j <- descriptive_N_perc(j, "gender_M_")
j <- descriptive_N_perc(j, "gender_F_")
j <- descriptive_N_perc(j, "gender_O_")


# rows 11-13

row_header_1 <- c(row_header_1,  
                  "Duration of diabetes",
                  "Mean (SD)",
                  "Median (IQ range)"
)

j <- add_empty_row(j)

j <- j + 1
tab_nice[, cell := paste0(yrsdiab_mean_, " (",yrsdiab_sd_,")")]
setnames(tab_nice, "cell", paste0("cell_",j))

j <- j + 1
varM <- "yrsdiab_median_"
varQ1 <- "yrsdiab_q3_"
varQ3 <- "yrsdiab_q3_"
tab_nice[, cell := paste0(get(varM), " (",get(varQ1),"-",get(varQ3),")")]
setnames(tab_nice, "cell", paste0("cell_",j))


# rows 14-22

row_header_1 <- c(row_header_1,  
                  "Missing data pre-imputation",
                  "HbA1c",
                  "Mean (SD)",
                  "Median (IQ range)",
                  "Missing N (%)",
                  "eGFR",
                  "Mean (SD)",
                  "Median (IQ range)",
                  "Missing N (%)"  )

j <- add_empty_row(j)

j <- add_empty_row(j)
j <- descriptive_lab_values(j, "HbA1c")

j <- add_empty_row(j)
j <- descriptive_lab_values(j, "EGFR")


# rows 23-27

row_header_1 <- c(row_header_1, 
                  "Tobacco use",
                  "None",
                  "Past",
                  "Current",
                  "missing N (%)" )

j <- add_empty_row(j)

j <- descriptive_N_perc(j, "Tobacco_use_0_")
j <- descriptive_N_perc(j, "Tobacco_use_1_")
j <- descriptive_N_perc(j, "Tobacco_use_2_")
j <- descriptive_N_perc(j, "Tobacco_use_missing")


# rows 28-48

row_header_1 <- c(row_header_1,  
                  "(Co)Morbidities")

j <- add_empty_row(j)

for (cov in covar_comorb) {
  name_cov <- unlist(parameters_this_step[values == cov,.(get("name in protocol"))])
  row_header_1 <- c(row_header_1,  
                    name_cov)
  j <- descriptive_N_perc(j, paste0(cov,"_"))
  
}

# rows 45-68

row_header_1 <- c(row_header_1,  
                  "Comedications")

j <- add_empty_row(j)

for (cov in covar_comed) {
  name_cov <- unlist(parameters_this_step[values == cov,.(get("name in protocol"))])
  row_header_1 <- c(row_header_1,  
                    name_cov)
  j <- descriptive_N_perc(j, paste0(cov,"_"))
}


# rows 69-72

row_header_1 <- c(row_header_1,  
                  "Established ACVD")

j <- add_empty_row(j)

for (cov in covar_est_ACVD) {
  name_cov <- unlist(parameters_this_step[values == cov,.(get("name in protocol"))])
  row_header_1 <- c(row_header_1,  
                    name_cov)
  j <- descriptive_N_perc(j, paste0(cov,"_"))
}

# rows 73-75

row_header_1 <- c(row_header_1,  
                  "ACVD Risk factors")

j <- add_empty_row(j)

for (cov in covar_rf_ACVD) {
  name_cov <- unlist(parameters_this_step[values == cov,.(get("name in protocol"))])
  row_header_1 <- c(row_header_1,  
                    name_cov)
  j <- descriptive_N_perc(j, paste0(cov,"_"))
}


# View(tab_nice[, .SD, .SDcols = patterns("^cell_")])



#########################################
# KEEP CELLS

cell_cols <- grep("^cell_", names(tab_nice), value = TRUE)
tokeep <- c("ds", "stratum", cell_cols)
tab_nice <- tab_nice[, ..tokeep]


#########################################
# RESHAPE CELLS

# First reshape tab_nice from wide into long format

tab_nice <- melt(
  tab_nice,
  id.vars = c("ds","stratum"),
  measure.vars = patterns(
    cell = "^cell_[0-9]+$"
  ),
  variable.name = "rownum"
)

tab_nice[, rownum := as.integer(rownum)]

setorder(tab_nice, rownum, ds, stratum)


# then reshape from long to wide keeping rownum as the UoO

tab_nice <- dcast(
  tab_nice,
  rownum ~ ds + stratum,
  value.var = "value"
)


# Order rows correctly
setorder(tab_nice, rownum)


#########################################
# ORDER COLUMNS
# 

ordered_ds <- substring(list_ds_analysed, 2)

ordered_cols <- c(
  "rownum",
  as.vector(rbind(paste0(ordered_ds, c("_overall", "_exp1", "_exp0"))
  ))
)

setcolorder(tab_nice, ordered_cols)

#########################################
# ADD ROW HEADER
# 

tab_nice[, row_header := row_header_1]
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

suffix <- "_all_datasources"
outputfile <- tab_nice
nameoutput <- paste0("D6_Table_2_baseline_cohort",suffix)
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
