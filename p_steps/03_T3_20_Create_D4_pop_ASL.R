# create D4_pop_ASL

# author: Rosa Gini

# v 1.0

# 28 Aug 2026

#########################################

if (TEST){
  testname <- "test_D4_pop_ASL"
  thisdirinput <- file.path(dirtest,testname)
  thisdiroutput <- file.path(dirtest,testname,"g_output")
  dir.create(thisdiroutput, showWarnings = F)
}else{
  thisdirinput <- dirtemp
  thisdiroutput <- dirtemp
}

# load data

processing <- readRDS(file.path(thisdirinput,"D3_ASL.rds"))

# pop_year

for (year in 2016:2025) {
  processing[, in_pop := fifelse(start_d <= ymd(paste0(year,"0101")) & end_d >= ymd(paste0(year,"0101")),1,0)]
  processing <- processing[,]
  setnames(processing, "in_pop", paste0(pop18,year))
}


# # clean and save
# 
# tokeep <- c("year", "ASL", "pop18")
# 
# processing <- processing[, ..tokeep]
# 
# nameoutputfile <- paste0("D4_pop_ASL.rds")
# 
# saveRDS(processing, file = file.path(thisdiroutput, nameoutputfile))
# 
