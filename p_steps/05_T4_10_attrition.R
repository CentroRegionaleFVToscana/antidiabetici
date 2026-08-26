# author: Sabrina Giometto

# v 0.1 28 Lug 2026

#########################################

if (TEST){
  testname <- "test_D5_Table_S1"
  thisdirinput <- file.path(dirtest,testname)
  thisdiroutput <- file.path(dirtest,testname,"g_output")
  dir.create(thisdiroutput, showWarnings = F)
}else{
  thisdirinput <- dirinput
  thisdiroutput <- dirtemp
}


# load data
for (i in drug_names) {
  
  data <- readRDS(file = paste0(thisdirinput, "/D3_selezione_coorte_", i, ".rds"))
  
  data <- as.data.table(data)
  
  assign(paste0("D3_selezione_coorte_", i), data)
  
}

# create D5 with binary selection variables

for (j in drug_names) {

D5 <- NULL

for (i in var_selection) {
  
  if (i != "is_in_study") {
  
  tmp <- get(paste0("D3_selezione_coorte_", j))[, .(
    N = .N,
    tmp_N = sum(get(i)==0, na.rm = T),
    tmp_p = round(sum(get(i)==0, na.rm = T)/.N,3)*100)]
  
  setnames(tmp,"tmp_N",paste0(i, "_N"))
  setnames(tmp,"tmp_p",paste0(i, "_p"))
  
  } else {
    
    tmp <- get(paste0("D3_selezione_coorte_", j))[, .(
      N = .N,
      tmp_N = sum(get(i)==1, na.rm = T),
      tmp_p = round(sum(get(i)==1, na.rm = T)/.N,3)*100)]
    
    setnames(tmp,"tmp_N",paste0(i, "_N"))
    setnames(tmp,"tmp_p",paste0(i, "_p"))
    
  }
  
  if (is.null(D5)) {
    
    D5 <- tmp
    
  } else {
    
    D5 <- merge(D5, tmp)
  }
  
  assign(paste0("D5_attrition_",j), D5)
  
 }
  
}

# save
for (j in drug_names) {
  
  saveRDS(get(paste0("D5_attrition_", j)), file = paste0(thisdiroutput, "/D5_Table_S1_attrition_", j, ".rds"))
  write.csv(get(paste0("D5_attrition_", j)), file = paste0(thisdiroutput, "/D5_Table_S1_attrition_", j, ".csv"))
  
}
