rm(list=ls(all.names=TRUE))

#set the directory where the script is saved as the working directory
if (!require("rstudioapi")) install.packages("rstudioapi")
thisdir <- setwd(dirname(rstudioapi::getSourceEditorContext()$path))
thisdir <- setwd(dirname(rstudioapi::getSourceEditorContext()$path))

# load packages
if (!require("data.table")) install.packages("data.table")
library(data.table)
if (!require("lubridate")) install.packages("lubridate")
library(lubridate)

baselinedate <- 20151231

# list of datasets

conceptsets_med <- c("antidiabother","met","med_IHD", "med_dyslipidemia", "med_hypertension", "med_RENDIS_Alg1_1", "med_RENDIS_Alg1_2", "med_RENDIS_Alg1_3", "med_RENDIS_Alg2")

conceptsets_dia <- c("dia_IHD", "dia_AMI", "dia_STROKE", "dia_TIA", "dia_aop", "dia_ateros", "dia_organdamage", "dia_dyslipidemia", "dia_obesity", "dia_hypertension", "dia_smoking", "dia_HF", "dia_RENDIS")

conceptsets_proc <- c("proc_bypass", "proc_angioplasty", "proc_carot")

listdatasetsRData <- c(conceptsets_med,conceptsets_dia,conceptsets_proc)

listdatasets <- c("D3_incidence_SGLT2i",listdatasetsRData)


# dates variables 

listdates <- list()

listdates[["D3_incidence_SGLT2i"]] <- c("birth_date", "date_first")

for (dataset in conceptsets_dia) {
  listdates[[dataset]] <- c("DATE")
}

for (dataset in conceptsets_med) {
  listdates[[dataset]] <- c("DATE")
}

for (dataset in conceptsets_proc) {
  listdates[[dataset]] <- c("DATE")
}


# date baseline

baseline <- vector(mode="list")
for (namedataset in listdatasets) {
  for (datevar in listdates[[namedataset]]) {
    baseline[[namedataset]][[datevar]] <- as.Date(lubridate::ymd(baselinedate))
  }
}


# load datasets


for (namedataset in listdatasets){
  print(namedataset)
  # data <- fread(paste0(thisdir, "/", namedataset, ".csv") )
  # data[, (listdates[[namedataset]]) := lapply(.SD, lubridate::ymd), .SDcols = listdates[[namedataset]]]
  data <- as.data.table(readxl::read_excel((paste0(thisdir, "/", namedataset, ".xlsx") )))
  for (datevar in listdates[[namedataset]]) {
    if (!is.na(baseline[[namedataset]][[datevar]])){
      #  data[, (datevar) := as.Date(get(datevar), origin = "1970-01-01") + as.numeric(baseline[[namedataset]][[datevar]])]
      data[, (datevar) := as.Date(get(datevar) + baseline[[namedataset]][[datevar]])]
    }else{
      data <- data[, (datevar) := lubridate::ymd(get(datevar))]
      
    }
  }
  
  assign(namedataset,data)
  if (namedataset %in% listdatasetsRData){
    save(data, file = file.path(thisdir, paste0(namedataset,".RData")), list = namedataset)
  }else{
    saveRDS(data, file = file.path(thisdir, paste0(namedataset,".rds")))
  }
}

