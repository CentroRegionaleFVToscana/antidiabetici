
#### D6_Table_1_cohort_characteristics ----


# authors: Rosa Gini, Sabrina Giometto

# v 1.2

# adapting to study on antidiabetics

# v 1.1

# added name of data sources

# v 1.0 

# 11 Jun 2026

# v 0.1

# 8 Jun 2026


print('CREATE D6_Table_2_cohort_characteristics_subpopulation_CAP')


# assign directories

if (TEST){ 
  testname <- "test_D6_Table_2"
  thisdirinput <- paste0(file.path(dirtest, testname), "/")
  thisdiroutput <- file.path(dirtest,testname,"g_output")
  dir.create(thisdiroutput, showWarnings = F)
}else{
  thisdirinput <- direxp
  thisdiroutput <- direxp
}

# load
D5 <- read.csv(file.path(
                  thisdirinput,
                  "D5_Table1_from_simulation.csv"))

D5 <- as.data.table(D5)

    
# helpers

add_empty_row <- function(j){
  
  j <- j + 1
  tab_nice[, cell := ""]
  setnames(tab_nice, "cell", paste0("cell_",j))
  return(j)
}

descriptive_N_perc <- function(j, covar) {
  
  j <- j + 1
  varN <- paste0(covar,"N")
  varP <- paste0(covar,"p")
  tab_nice[, cell := paste0(
    formatC(get(varN), format = "f", digits = 0, big.mark = ".", 
            decimal.mark = ","), " (",
    formatC(get(varP), format = "f", digits = 1, big.mark = ".", 
            decimal.mark = ","),"%)"
  )]
  setnames(tab_nice, "cell", paste0("cell_",j))
  return(j)
}

descriptive_median_q1q3 <- function(j, covar) {
  
  j <- j + 1
  varM <- paste0(covar,"_median")
  varQ1 <- paste0(covar,"_q1")
  varQ3 <- paste0(covar,"_q3")
  tab_nice[, cell := paste0(get(varM), " (", get(varQ1), " - ", 
                            get(varQ3), ")")]
  setnames(tab_nice, "cell", paste0("cell_", j))
  return(j)
}



#########################################
# POPULATE ROWS

tab_nice <- copy(D5)
 
# row 0
row_header_1 <- c()
j            <- -1

# row 1
row_header_1 <- c(row_header_1, "Periodo")
j <- j + 1
tab_nice[, cell := period]
setnames(tab_nice, "cell", paste0("cell_", j))

# row 2
row_header_1 <- c(row_header_1, "ASL")
j <- j + 1
tab_nice[, cell := ASL]
setnames(tab_nice, "cell", paste0("cell_", j))

# row 3
row_header_1 <- c(row_header_1, "Totale sottopopolazione CAP")
j <- j + 1
tab_nice[, cell := as.character(N)]
setnames(tab_nice, "cell", paste0("cell_", j))

# row 4
row_header_1 <- c(row_header_1, "BMI pregravidico basso (<18,5), n (%)")
j <- descriptive_N_perc(j, "bmi_low_")

# row 5
row_header_1 <- c(row_header_1, "BMI pregravidico medio (18,5-24,9), n (%)")
j <- descriptive_N_perc(j, "bmi_medium_")

# row 6
row_header_1 <- c(row_header_1, "BMI pregravidico alto (>24,9), n (%)")
j <- descriptive_N_perc(j, "bmi_high_")

# row 7
row_header_1 <- c(row_header_1, "Diabete gestazionale, n (%)")
j <- descriptive_N_perc(j, "diab_gestaz_")

# row 8
row_header_1 <- c(row_header_1, "Diabete pregravidico, n (%)")
j <- descriptive_N_perc(j, "diab_pregrav_")


#########################################
# KEEP CELLS

cell_cols <- grep("^cell_", names(tab_nice), value = TRUE)
tokeep <- c("period", "ASL", cell_cols)
tab_nice <- tab_nice[, ..tokeep]


#########################################
# RESHAPE CELLS

# First reshape tab_nice from wide into long format

tab_nice <- melt(
  tab_nice,
  id.vars = c("period", "ASL"),
  measure.vars = patterns(
    cell = "^cell_[0-9]+$"
  ),
  variable.name = "rownum"
)

tab_nice[, rownum := as.integer(rownum)]

setorder(tab_nice, rownum, period, ASL)


# then reshape from long to wide keeping rownum as the UoO

tab_nice <- dcast(
  tab_nice,
  rownum ~ period + ASL,
  value.var = "value"
)


# Order rows correctly
setorder(tab_nice, rownum)


#########################################
# ADD ROW HEADER
# 

tab_nice[, row_header := row_header_1]
tab_nice[, rownum := NULL]

#########################################
# FINAL COLUMNS

data_cols <- setdiff(names(tab_nice), "row_header")
pre_cols   <- grep("^pre_", data_cols, value = TRUE)
nota_cols   <- grep("^nota_", data_cols, value = TRUE)
other_cols <- setdiff(data_cols, c(pre_cols, nota_cols))
setcolorder(tab_nice, c("row_header", pre_cols, nota_cols, other_cols))

#########################################
# NAMES

newnames <- c(
  pre = "Pre- Nota 100 AIFA (1ge2016-25gen2022)",
  nota = "Nota 100 AIFA (26gen2022-31lug2025)",
  modifica = "Modifica Nota 100 AIFA(1ago2025-31dic2025)"
)


tab_nice[1] <- lapply(tab_nice[1], function(x) {
  
  idx <- x %in% names(newnames)
  x[idx] <- unname(newnames[x[idx]])
  x
  
})

#########################################
# SAVE

outputfile <- tab_nice
nameoutput <- "D6_Table_2_cohort_characteristics_subpopulation_CAP"
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
doc <- read_docx() %>% body_add_table(outputfile, style = "table_template", header = F) %>% body_end_section_continuous()
print(doc, target = file.path(thisdiroutput, paste0(nameoutput,".docx")))
