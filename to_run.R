# CRFV TOSCANA
# studio antidiabetici

# authors: Rosa Gini, Sabrina Giometto

# v 0.1 15 Mag 2026

# skeleton 

rm(list=ls(all.names=TRUE))

#set the directory where the file is saved as the working directory
if (!require("rstudioapi")) install.packages("rstudioapi")
hisdir <- setwd(dirname(rstudioapi::getSourceEditorContext()$path))
thisdir <- setwd(dirname(rstudioapi::getSourceEditorContext()$path))

TEST <- T


####################
# load parameters
source(paste0(thisdir,"/p_parameters/1_parameters_program.R"))
source(paste0(thisdir,"/p_parameters/2_parameters_CDM.R"))
source(paste0(thisdir,"/p_parameters/3_concept_sets.R"))
source(paste0(thisdir,"/p_parameters/5_variable_lists.R"))
source(paste0(thisdir,"/p_parameters/6_parameters_study.R"))
source(paste0(thisdir,"/p_parameters/7_parameters_postprocessing.R"))


#

# ######################################
# # run scripts
# source(paste0(thisdir,"/p_steps/01_T2_10_create_conceptsets.R"))
# source(paste0(thisdir,"/p_steps/01_T2_20_create_spells.R"))
# source(paste0(thisdir,"/p_steps/01_T2_30_create_persons.R"))
