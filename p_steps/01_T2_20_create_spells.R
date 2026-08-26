# author: Rosa Gini

# v 1.0 24 Nov 2024

#########################################
# assign input and output directories

if (TEST){
  testname <- "test_D3_OBSPERIODS"
  thisdirinput <- file.path(dirtest,testname)
  thisdiroutput <- file.path(dirtest,testname,"g_output")
  dir.create(thisdiroutput, showWarnings = F)
}else{
  thisdirinput <- dirinput
  thisdiroutput <- dirtemp
}

ana <- fread(file.path(thisdirinput,"ANAGRAFE_ASSISTITI.csv"))


processing <- CreateSpells(
  dataset = ana,
  id = "id" ,
  start_date = "data_inizioass",
  end_date = "data_fineass",
  gap_allowed = 365
)

rm(ana)

# keep only the spells overlapping the study period
processing <- processing[study_start_date <= exit_spell_category & study_end_date >= entry_spell_category ,]


setorderv(processing, c("id", "entry_spell_category"))


setnames(processing, old = c("id","entry_spell_category","exit_spell_category"),new = c("person_id","start_op", "end_op"))


################################
# clean

tokeep <- c("person_id", "start_op", "end_op")

processing <- processing[, ..tokeep]

setorderv(
  processing,
   c("person_id")
)


#########################################
# save

outputfile <- processing

nameoutput <- "D3_OBSPERIODS"
nameoutputext <- paste0(nameoutput,".rds")
assign(nameoutput, outputfile)
saveRDS(outputfile, file = file.path(thisdiroutput, nameoutputext))
