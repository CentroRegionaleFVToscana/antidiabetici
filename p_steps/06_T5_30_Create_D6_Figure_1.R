#### D6_Figure_1_incident_prevalent_users ----


# authors: Sabrina Giometto


# v 0.1

# 21 Jul 2026


print('CREATE D6_Figure_1_incident_prevalent_users')


# assign directories

if (TEST){ 
  testname <- "test_D6_Figure_1"
  thisdirinput <- paste0(file.path(dirtest, testname), "/")
  thisdiroutput <- file.path(dirtest,testname,"g_output")
  dir.create(thisdiroutput, showWarnings = F)
}else{
  thisdirinput <- dirtemp
  thisdiroutput <- direxp
}


# load
for (j in drug_names_s) {

  D5 <- read.csv(paste0(thisdirinput, "D5_prevalence_incidence_", j, ".csv"))
  D5 <- as.data.table(D5)
  assign(paste0("D5_prevalence_incidence_",j), D5)

}

base_colors <- c("CE" = "#1f4e79",
                 "NO" = "#2e7d32",
                 "SE" = "#e07b00")

drug_labels <- c("Inibitori SGLT2", "Agonisti recettoriali GLP-1",
                 "Doppi antagonisti GIP/GLP-1", "Inibitori DPP-4")

# create plots
plot_list <- list()

for (i in seq_along(drug_names_s)) {

  p <- ggplot(get(paste0("D5_prevalence_incidence_", drug_names_s[i])), aes(x = factor(year), group = ASL)) +
          # barra chiara = prevalent (sotto, più alta)
          geom_col(aes(y = prevalent, fill = ASL),
                   position = position_dodge(width = 0.9),
                   width = 0.85, alpha = 0.55, color = NA) +
          # barra scura = incident (sopra, più bassa) - stessa dodge, stesso width
          geom_col(aes(y = incident, fill = ASL),
                   position = position_dodge(width = 0.9),
                   width = 0.85, alpha = 1, color = NA, show.legend = FALSE) +
          scale_fill_manual(values = base_colors, name = NULL) +
          labs(x = NULL, y = "Numero Totale di Utilizzatori",
               title = paste0(LETTERS[i], ") ", drug_labels[i])) +
          theme_minimal() 
  
  plot_list[[i]] <- p
  
}

# save
png(paste0(thisdiroutput, "/D6_Figure_1.png"), width = 15, height = 12, units = "in", res = 300)

ggarrange(plotlist = plot_list, ncol = 2, nrow = 2,
          common.legend = TRUE, legend = "bottom")

dev.off()

