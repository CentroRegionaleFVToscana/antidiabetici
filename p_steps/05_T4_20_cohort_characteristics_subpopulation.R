# author: Sabrina Giometto

# v 1.0 09 Lug 2026 Creation of D5 started

# v 1.1 14 Lug 2026 Creation of D5 completed

#########################################

if (TEST){
  testname <- "test_D5_Table_2"
  thisdirinput <- file.path(dirtest,testname)
  thisdiroutput <- file.path(dirtest,testname,"g_output")
  dir.create(thisdiroutput, showWarnings = F)
}else{
  thisdirinput <- dirinput
  thisdiroutput <- dirtemp
}

# load data

if (TEST & type_data_test=="simulation") {
  
  data <- readRDS(file = file.path(thisdirinput, "/D3_coorte_con_caratterizzazione.rds"))
  
} else if (TEST & type_data_test=="dummy") {
  
  data <- read_csv2(file.path(thisdirinput, "D3_coorte_con_caratterizzazione_dummy_data.csv"))
  
}

data <- as.data.table(data)

# create D5 with binary covariates
covariates_binary <- c("diab_gestaz", "diab_pregrav", "bmi_low", "bmi_medium", 
                       "bmi_high")

D5 <- NULL

for (i in covariates_binary) {
  
  tmp <- data[, .(
    N = .N,
    tmp_N = sum(get(i)==1),
    tmp_p = round(sum(get(i)==1)/.N,3)*100),
    .(period, ASL)]
  
  setnames(tmp,"tmp_N",paste0(i, "_N"))
  setnames(tmp,"tmp_p",paste0(i, "_p"))
    
  if (is.null(D5)) {
    
    D5 <- tmp
    
  } else {
    
    D5 <- merge(D5, tmp, by = c("period", "ASL", "N"))
  }
    
}


# save

if (TEST & type_data_test=="simulation") {
  
  saveRDS(D5, file = paste0(thisdiroutput, "/D5_Table1_from_simulation.rds"))
  write.csv(D5, file = paste0(thisdiroutput, "/D5_Table1_from_simulation.csv"))

} else if (TEST & type_data_test=="dummy") {
  
  saveRDS(D5, file = paste0(thisdiroutput, "/D5_Table1_from_dummy_data.rds"))
  write.csv(D5, file = paste0(thisdiroutput, "/D5_Table1_from_dummy_data.csv"))
  
}
