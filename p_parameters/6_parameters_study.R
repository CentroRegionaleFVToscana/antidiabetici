# #set dates

study_start_date <-  ymd(20160101)
study_end_date <- ymd(20251231)

# baselinedate_components <- ymd(20190101)

# 
# instance_creation <- ymd(CDM_SOURCE[1,"date_creation"])
# recommended_end_date <- ymd(CDM_SOURCE[1,"recommended_end_date"])
# study_end_date <- min(study_end_date, instance_creation, recommended_end_date, na.rm = T)


# periods

start_date_period <- list()
end_date_period <- list()

end_date_period <- list()
end_date_period[["pre"]] <- ymd(20220125)
end_date_period[["nota"]] <- ymd(20250731)
end_date_period[["modi"]] <- study_end_date

start_date_period[["pre"]] <- study_start_date
start_date_period[["nota"]] <- end_date_period[["pre"]] + 1
start_date_period[["modi"]] <- end_date_period[["nota"]] + 1


# days for CreateSpells

# days <- 365

# agebands

Agebands_countpersontime = c(18, 44, 64, 74, 120)
Agebands_labels <- c("18-44", "45-64", "65-74", "75+")
names(Agebands_countpersontime) <- Agebands_labels

# Agebands_large = c(0, 18, 60)
# Agebands_large_labels = c("0-17","18-59","60+")
# names(Agebands_large) <- Agebands_large_labels


# duration of apixaban and rivaroxaban

# duration_MoI <- as.data.table(readxl::read_excel(file.path(thisdir,"p_parameters","archive_parameters","duration_MoI.xlsx")))

# components

# list_outcomesER_severe <- c(2,3,4,8,9)


# baselinedate_components <- ymd(20190101)

