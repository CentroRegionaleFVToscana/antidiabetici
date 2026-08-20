###################################################################
# DESCRIBE THE CONCEPT SETS
###################################################################
concept_sets_of_our_study_drugs <- c("SGLT2i","GLP1RA","tirzepatide","DPP4i","DPP4i_SGLT2i",
                                     "other_combinations","met","antidiabother")
# 
concept_sets_of_our_study_diagnosis <- c(  "CV", "cerebro", "aop", "Cvrisk", "HF", "renal")

drug_names <- c("SGLT2i","GLP1RA","tirzepatide","DPP4i","DPP4i_SGLT2i",
                "other_combinations")

drug_names_s <- c("SGLT2i","GLP1RA","tirzepatide","DPP4i")

# names of the concept sets

name_codelist <- list()

name_codelist[["SGLT2i"]] = "Inibitori SGLT2"
name_codelist[["GLP1RA"]] = "Antagonisti recettoriali GLP-1"
name_codelist[["tirzepatide"]] = "Doppi antagonisti GIP/GLP-1b"
name_codelist[["DPP4i"]] = "Inibitori DPP4"
name_codelist[["DPP4i_SGLT2i"]] = "Associazioni con limiti alla prescrivibilità"
name_codelist[["other_combinations"]] = "Altre associazioni precostituite"

name_codelist[["met"]] = "Metformina"
name_codelist[["antidiabother"]] = "Altri antidiabetici"
name_codelist[["CV"]] = "Malattia cardiovascolare"
name_codelist[["cerebro"]] = "Malattia cerebrovascolare"
name_codelist[["aop"]] = "Arteriopatia periferica"
name_codelist[["Cvrisk"]] = "Rischio CV elevato"
name_codelist[["HF"]] = "Scompenso cardiaco"
name_codelist[["renal"]] = "Malattia renale cronica"


# -concept_set_domains- is a 2-level list encoding for each concept set the corresponding data domain

concept_set_domains <- vector(mode="list")

for (concept_id in concept_sets_of_our_study_drugs) {
  concept_set_domains[[concept_id]] = "Medicines"
}

for (concept_id in concept_sets_of_our_study_diagnosis) {
  concept_set_domains[[concept_id]]="Diagnosis"
}


# -concept_set_codes_our_study- is a nested list, with 3 levels: foreach concept set, for each coding system of its data domain, the list of codes is recorded

concept_set_codes_our_study <- vector(mode="list")
concept_set_codes_our_study_excl <- vector(mode="list")

concept_set_codes_our_study[["SGLT2i"]][["ATC"]] = c("A10BK") # Inibitori SGLT2, A10BK*
concept_set_codes_our_study[["GLP1RA"]][["ATC"]] = c("A10BJ") # Antagonisti recettoriali GLP-1, A10BJ*
concept_set_codes_our_study[["tirzepatide"]][["ATC"]] = c("A10BX16") # Doppi antagonisti GIP/GLP-1b
concept_set_codes_our_study[["DPP4i"]][["ATC"]] = c("A10BH") # Inibitori DPP4, A10BH*
concept_set_codes_our_study[["DPP4i_SGLT2i"]][["ATC"]] = c("A10BD24", "A10BD21", "A10BD19") # Associazioni con limiti alla prescrivibilità precostituite ed estemporanee
concept_set_codes_our_study[["other_combinations"]][["ATC"]] = c("A10BD13", "A10BD11", "A10BD10", "A10BD07", "A10BD08",
                                                                 "A10BD09",
                                                                 "A10AE56", "A10AE54",
                                                                 "A10BD16", "A10BD15", "A10BD20", "A10BD23") # Altre associazioni precostituite: DPP4i e metformina (A10BD13, A10BD11, A10BD10, A10BD07, A10BD08), DPP4i e tiazolidinedioni (A10BD09), GLP1-RA e insulina (A10AE56 e A10AE54), SGLT2i e metformina (A10BD16, A10BD15, A10BD20, A10BD23)


# protocol version https://docs.google.com/document/d/1iDsfua3dngmNHm3Xy-y9wdqGxIvjGNpy/edit?tab=t.0
# Table 3.7.1

concept_set_codes_our_study[["CV"]][["ICD9"]] = c("410", "411", "412", "413", "414", "36.10", "36.11", "36.12", "36.13", "36.14", "36.15", "36.16", "36.17", "36.18", "36.19", "00.66", "36.06", "36.07") # Malattia cardiovascolare
concept_set_codes_our_study[["cerebro"]][["ICD9"]] = c("430", "431", "432", "433.01", "433.11", "433.21", "433.31", "433.41", "433.51", "433.61", "433.71", "433.81", "433.91", "434.01", "434.11", "434.21", "434.31", "434.41", "434.51", "434.61", "434.71", "434.81", "434.91", "436", "435", "00.61", "00.63") # Malattia cerebrovascolare
concept_set_codes_our_study[["aop"]][["ICD9"]] = c("440.1", "440.2", "440.3", "440.8", "440.9") # Arteriopatia periferica
concept_set_codes_our_study[["Cvrisk"]][["ICD9"]] = c("440.0", "433.00", "433.10", "433.20", "433.30", "433.40", "433.50", "433.60", "433.70", "433.80", "433.90", "250.4", "362.0", "429.3") # Rischio CV elevato
concept_set_codes_our_study[["Cvrisk_combined_b"]][["ICD9"]] = c("272.0", "272.1", "272.3", "272.4") # Rischio CV elevato (fattore di rischio b.)
concept_set_codes_our_study[["Cvrisk_combined_c"]][["ICD9"]] = c("278.0", "278.1", "278.8", "649.1", "649.2", "V45.86") # Rischio CV elevato (fattore di rischio c.)
concept_set_codes_our_study[["Cvrisk_combined_d"]][["ICD9"]] = c("401", "402", "403", "404", "405", "362.11", "000") # Rischio CV elevato (fattore di rischio d.)
concept_set_codes_our_study[["Cvrisk_combined_e"]][["ICD9"]] = c("305.1") # Rischio CV elevato (fattore di rischio e.)
concept_set_codes_our_study[["HF"]][["ICD9"]] = c("428", "398.91", "402.01", "402.11", "402.91", "404.01", "404.03", "404.11", "404.13", "404.91", "404.93") # Scompenso cardiaco
concept_set_codes_our_study[["renal_alg2"]][["ICD9"]] = c("582.0", "582.1", "582.2", "582.3", "582.4", "582.5", "582.6", "582.7", "582.8", "582.9", "581", "753.1", "590.00", "590.01", "589.0", "585", "586") # Malattia renale cronica

concept_set_codes_our_study[["met"]][["ATC"]] = "A10BA02" # metformina
concept_set_codes_our_study[["antidiabother"]][["ATC"]] = c("A10") # altri farmaci antidiabetici
concept_set_codes_our_study[["CV"]][["ATC"]] = c("C01DA") # Malattia cardiovascolare
concept_set_codes_our_study[["Cvrisk_combined_b"]][["ATC"]] = c("C10") # Rischio CV elevato (fattore di rischio b.)
concept_set_codes_our_study[["Cvrisk_combined_d"]][["ATC"]] = c("C09", "C02", "C07", "C08C") # Rischio CV elevato (fattore di rischio d.)
concept_set_codes_our_study[["renal_alg1"]][["ATC"]] = c("C09C", "C09B", "M04AA01") # Malattia renale cronica (alg.1)
concept_set_codes_our_study[["renal_alg2"]][["ATC"]] = c("B03XA01", "V03AE03", "V03AE02", "V03AE01", "H05BX02", "B03XA02", "H05BX01") # Malattia renale cronica (alg.2)


concept_set_codes_our_study[["Cvrisk_combined_b"]][["CMN"]] = c("025") # Rischio CV elevato (fattore di rischio b.)

concept_set_codes_our_study_excl[["antidiabother"]][["ATC"]] = c("A10BA02", "A10BH", "A10BJ", "A10BK", "A10BX16", "A10BD24", "A10BD21", "A10BD19", "A10BD13", "A10BD11", "A10BD10", "A10BD07", "A10BD08",
                                                                 "A10BD09",
                                                                 "A10AE56", "A10AE54",
                                                                 "A10BD16", "A10BD15", "A10BD20", "A10BD23") # altri farmaci antidiabetici

