library(haven)
library(MASS)
library(modelsummary)
library(sqldf)
library(fixest)
CHES2019_means <- read_dta("Z:/Predict Voter Position/CHES2019_means.dta")

subset(CHES2019_means, party_id == 2010)

EVS_predicted_data <- read.csv("Z:/Predict Voter Position/EVS_predicted_data.csv")
voter_table <- EVS_predicted_data


View(voter_table)
voter_table$iso_a2 <- "AT"
voter_table$east <- 0
voter_table[voter_table$hCountry == 2, ]$iso_a2 <- "BE"
voter_table[voter_table$hCountry == 3, ]$iso_a2 <- "BG"
voter_table[voter_table$hCountry == 3, ]$east <- 1
voter_table[voter_table$hCountry == 4, ]$iso_a2 <- "HR"
voter_table[voter_table$hCountry == 4, ]$east <- 1
voter_table[voter_table$hCountry == 5, ]$iso_a2 <- "CY"
voter_table[voter_table$hCountry == 6, ]$iso_a2 <- "CZ"
voter_table[voter_table$hCountry == 6, ]$east <- 1
voter_table[voter_table$hCountry == 7, ]$iso_a2 <- "DE"
voter_table[voter_table$hCountry == 8, ]$iso_a2 <- "DK"
voter_table[voter_table$hCountry == 9, ]$iso_a2 <- "EE"
voter_table[voter_table$hCountry == 9, ]$east <- 1
voter_table[voter_table$hCountry == 10, ]$iso_a2 <- "FI"
voter_table[voter_table$hCountry == 11, ]$iso_a2 <- "FR"
voter_table[voter_table$hCountry == 12, ]$iso_a2 <- "GR"
voter_table[voter_table$hCountry == 13, ]$iso_a2 <- "HU"
voter_table[voter_table$hCountry == 13, ]$east <- 1
voter_table[voter_table$hCountry == 14, ]$iso_a2 <- "IE"
voter_table[voter_table$hCountry == 15, ]$iso_a2 <- "IT"
voter_table[voter_table$hCountry == 16, ]$iso_a2 <- "LV"
voter_table[voter_table$hCountry == 16, ]$east <- 1
voter_table[voter_table$hCountry == 17, ]$iso_a2 <- "LT"
voter_table[voter_table$hCountry == 17, ]$east <- 1
voter_table[voter_table$hCountry == 18, ]$iso_a2 <- "LU"
voter_table[voter_table$hCountry == 19, ]$iso_a2 <- "MT"
voter_table[voter_table$hCountry == 20, ]$iso_a2 <- "NL"
voter_table[voter_table$hCountry == 21, ]$iso_a2 <- "PL"
voter_table[voter_table$hCountry == 21, ]$east <- 1
voter_table[voter_table$hCountry == 22, ]$iso_a2 <- "PT"
voter_table[voter_table$hCountry == 23, ]$iso_a2 <- "RO"
voter_table[voter_table$hCountry == 23, ]$east <- 1
voter_table[voter_table$hCountry == 24, ]$iso_a2 <- "SI"
voter_table[voter_table$hCountry == 24, ]$east <- 1
voter_table[voter_table$hCountry == 25, ]$iso_a2 <- "SK"
voter_table[voter_table$hCountry == 25, ]$east <- 1
voter_table[voter_table$hCountry == 26, ]$iso_a2 <- "ES"
voter_table[voter_table$hCountry == 27, ]$iso_a2 <- "SE"
voter_table[voter_table$hCountry == 28, ]$iso_a2 <- "GB"


party_table <- read.csv("Z:/Predict Voter Position/party_table_new.csv", sep = ";")
CHES2019_means_to_be_linked <- sqldf("select t2.*, t1.lrgen, t1.galtan, t1.lrecon from CHES2019_means t1 LEFT OUTER JOIN party_table t2 ON t1.party_id = t2.chess")

voter_table_long <- read.csv("Z:/Predict Voter Position/ZA7890_v1-0-0.csv", sep = ";")


summary(voter_table_long$q13_1)

library(readr)
codeplan <- read_csv("Z:/Predict Voter Position/codeplan.csv")

voter_table_long_linked <- sqldf("SELECT t1.*, t2.Q9_Q25_EES as partyid_EES FROM voter_table_long t1 INNER JOIN codeplan t2 ON t2.Q9 = t1.party")
voter_table_long_linked <- sqldf("SELECT t1.*, t2.lrgen AS lrgen_this_party, t2.galtan AS galtan_this_party, t2.lrecon AS lrecon_this_party FROM voter_table_long_linked t1 LEFT OUTER JOIN CHES2019_means_to_be_linked t2 ON t2.ees = t1.partyid_EES")

voter_table_long_linked_imputed2 <- sqldf("SELECT t1.*, t2.predicted_values_lrgen, t2.predicted_values_lrecon, t2.predicted_values_galtan, t2.lrgen_selfdescription  FROM voter_table_long_linked t1 INNER JOIN EVS_predicted_data t2 ON t1.respid = t2.respid")
voter_table_long_linked_imputed2$distance_self_description <- abs(voter_table_long_linked_imputed2$lrgen_selfdescription - voter_table_long_linked_imputed2$lrgen_this_party)
voter_table_long_linked_imputed2$distance_lrgen_predicted <- abs(voter_table_long_linked_imputed2$predicted_values_lrgen - voter_table_long_linked_imputed2$lrgen_this_party)
voter_table_long_linked_imputed2$distance_lrecon_predicted <- abs(voter_table_long_linked_imputed2$predicted_values_lrecon - voter_table_long_linked_imputed2$lrecon_this_party)
voter_table_long_linked_imputed2$distance_galtan_predicted <- abs(voter_table_long_linked_imputed2$predicted_values_galtan - voter_table_long_linked_imputed2$galtan_this_party)
voter_table_long_linked_imputed2$Q10_new <- scan(text=voter_table_long_linked_imputed2$Q10_gen, dec=",", sep=".")
voter_table_long_linked_imputed2$voting_propensity <- ifelse(voter_table_long_linked_imputed2$Q10_new <= 1, voter_table_long_linked_imputed2$Q10_new, NA)
voter_table_long_linked_imputed2$Q11_Q13_new <- scan(text=voter_table_long_linked_imputed2$Q11_Q13_gen, dec=",", sep=".")
voter_table_long_linked_imputed2$perceived_distance<- ifelse(voter_table_long_linked_imputed2$Q11_Q13_new <= 1, voter_table_long_linked_imputed2$Q11_Q13_new * 10, NA)


voter_table_long_linked_imputed2$euclidean_distance_predicted <- sqrt((voter_table_long_linked_imputed2$predicted_values_lrecon - voter_table_long_linked_imputed2$lrecon_this_party) ^ 2 + (voter_table_long_linked_imputed2$predicted_values_galtan - voter_table_long_linked_imputed2$galtan_this_party) ^ 2)

summary(voter_table_long_linked_imputed2$lrgen_selfdescription)

###### without objective

options(scipen=999)


unique_parties <- unique(voter_table_long_linked_imputed2$party)
results_table <- data.frame(party_id=character(),
                            country=character(), 
                            objective_distance_lrgen=numeric(), 
                            objective_distance_lrgen_lower=numeric(), 
                            objective_distance_lrgen_upper=numeric(), 
                            self_description_distance_lrgen=numeric(), 
                            self_description_distance_lrgen_lower=numeric(), 
                            self_description_distance_lrgen_upper=numeric(), 
                            
                            objective_distance_lrecon=numeric(), 
                            objective_distance_lrecon_lower=numeric(), 
                            objective_distance_lrecon_upper=numeric(), 
                            
                            objective_distance_galtan=numeric(), 
                            objective_distance_galtan_lower=numeric(), 
                            objective_distance_galtan_upper=numeric(), 
                            
                            euclidean_distance=numeric(), 
                            euclidean_distance_lower=numeric(), 
                            euclidean_distance_upper=numeric(), 
                            
                            stringsAsFactors=FALSE) 

while (!is.na(unique_parties[1]))
{
  if (!is.na(unique(subset(voter_table_long_linked_imputed2, party == unique_parties[1])$distance_lrgen_predicted)[1]))
  {
    lm_results_objective_lrgen <- lm(data = subset(voter_table_long_linked_imputed2, party == unique_parties[1]) , voting_propensity ~ distance_lrgen_predicted)
    lm_results_self_description_lrgen <- lm(data = subset(voter_table_long_linked_imputed2, party == unique_parties[1]) , voting_propensity ~ distance_self_description)
    lm_results_objective_lrecon <- lm(data = subset(voter_table_long_linked_imputed2, party == unique_parties[1]) , voting_propensity ~ distance_lrecon_predicted)
    lm_results_objective_galtan <- lm(data = subset(voter_table_long_linked_imputed2, party == unique_parties[1]) , voting_propensity ~ distance_galtan_predicted)
    lm_results_euclidean_distance <- lm(data = subset(voter_table_long_linked_imputed2, party == unique_parties[1]) , voting_propensity ~ euclidean_distance_predicted)
    results_table_appended <- data.frame(party_id=unique_parties[1],
                                         country=unique(subset(voter_table_long_linked_imputed2, party == unique_parties[1])$countryname)[1], 
                                         #objective_distance_lrecon=numeric(), 
                                         #objective_distance_galtan=numeric(), 
                                         objective_distance_lrgen=as.numeric(lm_results_objective_lrgen$coefficients[2]),
                                         objective_distance_lrgen_lower=as.numeric(confint(lm_results_objective_lrgen)[2,1]),
                                         objective_distance_lrgen_upper=as.numeric(confint(lm_results_objective_lrgen)[2,2]), 
                                         self_description_distance_lrgen=as.numeric(lm_results_self_description_lrgen$coefficients[2]),
                                         self_description_distance_lrgen_lower=as.numeric(confint(lm_results_self_description_lrgen)[2,1]),
                                         self_description_distance_lrgen_upper=as.numeric(confint(lm_results_self_description_lrgen)[2,2]), 
                                         
                                         objective_distance_lrecon=as.numeric(lm_results_objective_lrecon$coefficients[2]),
                                         objective_distance_lrecon_lower=as.numeric(confint(lm_results_objective_lrecon)[2,1]),
                                         objective_distance_lrecon_upper=as.numeric(confint(lm_results_objective_lrecon)[2,2]), 
                                         
                                         objective_distance_galtan=as.numeric(lm_results_objective_galtan$coefficients[2]),
                                         objective_distance_galtan_lower=as.numeric(confint(lm_results_objective_galtan)[2,1]),
                                         objective_distance_galtan_upper=as.numeric(confint(lm_results_objective_galtan)[2,2]),
                                         
                                         euclidean_distance=as.numeric(lm_results_euclidean_distance$coefficients[2]),
                                         euclidean_distance_lower=as.numeric(confint(lm_results_euclidean_distance)[2,1]),
                                         euclidean_distance_upper=as.numeric(confint(lm_results_euclidean_distance)[2,2]),
                                         #objective_distance_lrgen=as.numeric(lm_results_subjective_vs_objective_lrgen$coefficients[3]), 
                                         #objective_distance_lrgen_lower=as.numeric(confint(lm_results_subjective_vs_objective_lrgen)[3,1]), 
                                         #objective_distance_lrgen_upper=as.numeric(confint(lm_results_subjective_vs_objective_lrgen)[3,2]), 
                                         #distance_self_description=numeric(), 
                                         #objective_distance_lrecon_upper=numeric(), 
                                         #objective_distance_galtan_upper=numeric(), 
                                         #distance_self_description_upper=numeric(), 
                                         #objective_distance_lrecon_lower=numeric(), 
                                         #objective_distance_galtan_lower=numeric(), 
                                         #distance_self_description_lower=numeric(), 
                                         stringsAsFactors=FALSE) 
    results_table <- rbind(results_table, results_table_appended)
  }
  unique_parties <- unique_parties[-1]
  
}
results_table_objective_lrgen <- sqldf("SELECT ROW_NUMBER() OVER(ORDER BY objective_distance_lrgen) RowNumber, * FROM results_table ORDER BY objective_distance_lrgen")
results_table_self_description_lrgen <- sqldf("SELECT ROW_NUMBER() OVER(ORDER BY self_description_distance_lrgen) RowNumber, * FROM results_table ORDER BY self_description_distance_lrgen")
results_table_objective_lrecon <- sqldf("SELECT ROW_NUMBER() OVER(ORDER BY objective_distance_lrecon) RowNumber, * FROM results_table ORDER BY objective_distance_lrecon")
results_table_objective_galtan <- sqldf("SELECT ROW_NUMBER() OVER(ORDER BY objective_distance_galtan) RowNumber, * FROM results_table ORDER BY objective_distance_galtan")
results_table_euclidean <- sqldf("SELECT ROW_NUMBER() OVER(ORDER BY euclidean_distance) RowNumber, * FROM results_table ORDER BY euclidean_distance")


#results_table3 <- sqldf("SELECT ROW_NUMBER() OVER(ORDER BY subjective_distance_lrgen) RowNumber, * FROM results_table ORDER BY subjective_distance_lrgen")

library(ggplot2)


# install.packages(c("dplyr","ggplot2","tidyr","tibble"))
library(dplyr)
library(tidyr)
library(tibble)
library(ggplot2)

vars <- c(
  "objective_distance_lrgen",
  "objective_distance_lrecon",
  "objective_distance_galtan",
  "euclidean_distance"
)

# Nice labels (edit as you like)
var_labels <- c(
  objective_distance_lrgen        = "Objective distance (LR general)",
  objective_distance_lrecon       = "Objective distance (LR economy)",
  objective_distance_galtan       = "Objective distance (GAL-TAN)",
  euclidean_distance              = "Euclidean distance"
)

corr_mat <- results_table_objective_galtan %>%
  select(all_of(vars)) %>%
  cor(use = "pairwise.complete.obs", method = "pearson")

corr_long <- as.data.frame(corr_mat) %>%
  rownames_to_column("Var1_raw") %>%
  pivot_longer(-Var1_raw, names_to = "Var2_raw", values_to = "r") %>%
  mutate(
    i = match(Var1_raw, vars),
    j = match(Var2_raw, vars),
    
    # mask lower triangle (i > j) -> set to NA
    r_plot = ifelse(i > j, NA_real_, r),
    lab    = ifelse(is.na(r_plot), "", sprintf("%.2f", r_plot)),
    
    # apply nice labels + ordering
    Var1 = factor(Var1_raw, levels = vars, labels = unname(var_labels[vars])),
    Var2 = factor(Var2_raw, levels = vars, labels = unname(var_labels[vars]))
  )

corr_plot <- ggplot(corr_long, aes(Var1, Var2, fill = r_plot)) +
  geom_tile(color = "white", linewidth = 0.5) +
  geom_text(aes(label = lab), size = 3) +
  coord_equal() +
  scale_fill_gradient2(limits = c(-1, 1), midpoint = 0, na.value = "white") +
  theme_minimal(base_size = 12) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    axis.title  = element_blank()
  ) +
  labs(fill = "r", title = "Correlation matrix of regression coefficients") + theme(plot.title = element_text(hjust = 0.5))+ theme(legend.position = "none")

ggsave(filename = "correlation_matrix.PNG", width=7, height=4)


m <- mean(results_table_objective_lrgen$objective_distance_lrgen, na.rm = TRUE)
p <- t.test(results_table_objective_lrgen$objective_distance_lrgen, mu = 0)$p.value

sig_neg_share <- with(
  results_table_objective_lrgen,
  mean(objective_distance_lrgen_upper < 0, na.rm = TRUE)
) * 100

sig_neg_share

sig_pos_share <- with(
  results_table_objective_lrgen,
  mean(objective_distance_lrgen_lower > 0, na.rm = TRUE)
) * 100

sig_pos_share

results_table_objective_lrgen$statistically_different <- "not"
results_table_objective_lrgen[results_table_objective_lrgen$objective_distance_lrgen_upper < 0, ]$statistically_different <- "negative"
results_table_objective_lrgen[results_table_objective_lrgen$objective_distance_lrgen_lower > 0, ]$statistically_different <- "positive"

cols <- c("negative"="#CC79A7", "positive"="#E69F00", "not"="#7F7F7F")

 lab_txt <- ifelse(p<0.001,sprintf("Mean estimate: %.3f*** (p < 0.001)\nShare significantly negative: %.2f%%\nShare significantly positive: %.2f%%", m, sig_neg_share, sig_pos_share), sprintf("Mean estimate: %.3f*** (p = %3f)\nShare significantly negative: %.2f%%\nShare significantly positive: %.2f%%", m, p, sig_neg_share, sig_pos_share))

p_objective_distance_lrgen <- ggplot(data = results_table_objective_lrgen) + 
  geom_linerange(aes(x=RowNumber, ymax = objective_distance_lrgen_upper, ymin = objective_distance_lrgen_lower, col=statistically_different)) +
  geom_point(aes(x=RowNumber, y = objective_distance_lrgen)) + theme_bw() + 
  annotate(
    "text",
    x = -Inf, y = Inf,
    label = lab_txt,
    hjust = -0.05, vjust = 1.5,  # nudges it slightly inward
    size = 3
  ) +
  geom_hline(aes(yintercept=0, linetype = "zero effect"))+ 
  # geom_hline(aes(yintercept = mean(results_table_objective_galtan$objective_distance_lrgen), color = "mean\nestimate")) + 
  labs(linetype="", color="", y="Regression coefficient", x = "Party (ordered by estimated coefficient)", title = "Objective distance (LR general)") + theme(plot.title = element_text(hjust = 0.5))+
  scale_linetype_manual(values = c("dashed"))+ theme(legend.position = "none") + scale_color_manual(values = cols)


m <- mean(results_table_self_description_lrgen$self_description_distance_lrgen, na.rm = TRUE)
p <- t.test(results_table_self_description_lrgen$self_description_distance_lrgen, mu = 0)$p.value

sig_neg_share <- with(
  results_table_self_description_lrgen,
  mean(self_description_distance_lrgen_upper < 0, na.rm = TRUE)
) * 100

sig_neg_share

sig_pos_share <- with(
  results_table_self_description_lrgen,
  mean(self_description_distance_lrgen_lower > 0, na.rm = TRUE)
) * 100

sig_pos_share


  lab_txt <- ifelse(p<0.001,sprintf("Mean estimate: %.3f*** (p < 0.001)\nShare significantly negative: %.2f%%\nShare significantly positive: %.2f%%", m, sig_neg_share, sig_pos_share), sprintf("Mean estimate: %.3f*** (p = %3f)\nShare significantly negative: %.2f%%\nShare significantly positive: %.2f%%", m, p, sig_neg_share, sig_pos_share))

results_table_self_description_lrgen$statistically_different <- "not"
results_table_self_description_lrgen[results_table_self_description_lrgen$self_description_distance_lrgen_upper < 0, ]$statistically_different <- "negative"
results_table_self_description_lrgen[results_table_self_description_lrgen$self_description_distance_lrgen_lower > 0, ]$statistically_different <- "positive"


p_self_description_distance_lrgen <- ggplot(data = results_table_self_description_lrgen) + 
  geom_linerange(aes(x=RowNumber, ymax = self_description_distance_lrgen_upper, ymin = self_description_distance_lrgen_lower, col=statistically_different)) +
  geom_point(aes(x=RowNumber, y = self_description_distance_lrgen)) + theme_bw() + 
  annotate(
    "text",
    x = -Inf, y = Inf,
    label = lab_txt,
    hjust = -0.05, vjust = 1.5,  # nudges it slightly inward
    size = 3
  ) +
  geom_hline(aes(yintercept=0, linetype = "zero effect"))+ 
  #geom_hline(aes(yintercept = mean(results_table_self_description_lrgen$self_description_distance_lrgen), color = "average effect")) +
  labs(linetype="", color="", y="Regression coefficient", x = "Party (ordered by estimated coefficient)", title = "Subjective distance (left-right)") + theme(plot.title = element_text(hjust = 0.5))+
  scale_linetype_manual(values = c("dashed")) + theme(legend.position = "none") + scale_color_manual(values = cols)


results_table_objective_lrecon$statistically_different <- "not"
results_table_objective_lrecon[results_table_objective_lrecon$objective_distance_lrecon_upper < 0, ]$statistically_different <- "negative"
results_table_objective_lrecon[results_table_objective_lrecon$objective_distance_lrecon_lower > 0, ]$statistically_different <- "positive"

m <- mean(results_table_objective_lrecon$objective_distance_lrecon, na.rm = TRUE)
p <- t.test(results_table_objective_lrecon$objective_distance_lrecon, mu = 0)$p.value

sig_neg_share <- with(
  results_table_objective_lrecon,
  mean(objective_distance_lrecon_upper < 0, na.rm = TRUE)
) * 100

sig_neg_share

sig_pos_share <- with(
  results_table_objective_lrecon,
  mean(objective_distance_lrecon_lower > 0, na.rm = TRUE)
) * 100

sig_pos_share


  lab_txt <- ifelse(p<0.001,sprintf("Mean estimate: %.3f*** (p < 0.001)\nShare significantly negative: %.2f%%\nShare significantly positive: %.2f%%", m, sig_neg_share, sig_pos_share), sprintf("Mean estimate: %.3f*** (p = %3f)\nShare significantly negative: %.2f%%\nShare significantly positive: %.2f%%", m, p, sig_neg_share, sig_pos_share))


p_objective_distance_lrecon <- ggplot(data = results_table_objective_lrecon) + 
  geom_linerange(aes(x=RowNumber, ymax = objective_distance_lrecon_upper, ymin = objective_distance_lrecon_lower, col=statistically_different)) +
  geom_point(aes(x=RowNumber, y = objective_distance_lrecon)) + theme_bw() + 
  annotate(
    "text",
    x = -Inf, y = Inf,
    label = lab_txt,
    hjust = -0.05, vjust = 1.5,  # nudges it slightly inward
    size = 3
  ) +
  geom_hline(aes(yintercept=0, linetype = "zero effect"))+ 
  #geom_hline(aes(yintercept = mean(results_table_objective_galtan$objective_distance_lrecon), color = "mean\nestimate")) + 
  labs(linetype="", color="", y="Regression coefficient", x = "Party (ordered by estimated coefficient)", title = "Objective distance (LR economy)") + theme(plot.title = element_text(hjust = 0.5))+
  scale_linetype_manual(values = c("dashed"))+ theme(legend.position = "none") + scale_color_manual(values = cols)


results_table_objective_galtan$statistically_different <- "not"
results_table_objective_galtan[results_table_objective_galtan$objective_distance_galtan_upper < 0, ]$statistically_different <- "negative"
results_table_objective_galtan[results_table_objective_galtan$objective_distance_galtan_lower > 0, ]$statistically_different <- "positive"


m <- mean(results_table_objective_galtan$objective_distance_galtan, na.rm = TRUE)
p <- t.test(results_table_objective_galtan$objective_distance_galtan, mu = 0)$p.value

sig_neg_share <- with(
  results_table_objective_galtan,
  mean(objective_distance_galtan_upper < 0, na.rm = TRUE)
) * 100

sig_neg_share

sig_pos_share <- with(
  results_table_objective_galtan,
  mean(objective_distance_galtan_lower > 0, na.rm = TRUE)
) * 100

sig_pos_share


  lab_txt <- ifelse(p<0.001,sprintf("Mean estimate: %.3f*** (p < 0.001)\nShare significantly negative: %.2f%%\nShare significantly positive: %.2f%%", m, sig_neg_share, sig_pos_share), sprintf("Mean estimate: %.3f*** (p = %3f)\nShare significantly negative: %.2f%%\nShare significantly positive: %.2f%%", m, p, sig_neg_share, sig_pos_share))



p_objective_distance_galtan <- ggplot(data = results_table_objective_galtan) + 
  geom_linerange(aes(x=RowNumber, ymax = objective_distance_galtan_upper, ymin = objective_distance_galtan_lower, col=statistically_different)) +
  geom_point(aes(x=RowNumber, y = objective_distance_galtan)) + theme_bw() + 
  geom_hline(aes(yintercept=0, linetype = "zero effect"))+ 
  annotate(
    "text",
    x = -Inf, y = Inf,
    label = lab_txt,
    hjust = -0.05, vjust = 1.5,  # nudges it slightly inward
    size = 3
  ) +
  #  geom_hline(aes(yintercept = mean(results_table_objective_galtan$objective_distance_galtan), color = "mean\nestimate")) + 
  labs(linetype="", color="", y="Regression coefficient", x = "Party (ordered by estimated coefficient)", title = "Objective distance (GAL-TAN)") + theme(plot.title = element_text(hjust = 0.5))+
  scale_linetype_manual(values = c("dashed")) + theme(legend.position = "none") + scale_color_manual(values = cols)


results_table_euclidean$statistically_different <- "not"
results_table_euclidean[results_table_euclidean$euclidean_distance_upper < 0, ]$statistically_different <- "negative"
results_table_euclidean[results_table_euclidean$euclidean_distance_lower > 0, ]$statistically_different <- "positive"

m <- mean(results_table_euclidean$euclidean_distance, na.rm = TRUE)
p <- t.test(results_table_euclidean$euclidean_distance, mu = 0)$p.value

sig_neg_share <- with(
  results_table_euclidean,
  mean(euclidean_distance_upper < 0, na.rm = TRUE)
) * 100

sig_neg_share

sig_pos_share <- with(
  results_table_euclidean,
  mean(euclidean_distance_lower > 0, na.rm = TRUE)
) * 100

sig_pos_share


  lab_txt <- ifelse(p<0.001,sprintf("Mean estimate: %.3f*** (p < 0.001)\nShare significantly negative: %.2f%%\nShare significantly positive: %.2f%%", m, sig_neg_share, sig_pos_share), sprintf("Mean estimate: %.3f*** (p = %3f)\nShare significantly negative: %.2f%%\nShare significantly positive: %.2f%%", m, p, sig_neg_share, sig_pos_share))


p_euclidean_distance <- ggplot(data = results_table_euclidean) + 
  geom_linerange(aes(x=RowNumber, ymax = euclidean_distance_upper, ymin = euclidean_distance_lower, col=statistically_different)) +
  geom_point(aes(x=RowNumber, y = euclidean_distance)) + theme_bw() + 
  geom_hline(aes(yintercept=0, linetype = "zero effect"))+ 
  annotate(
    "text",
    x = -Inf, y = Inf,
    label = lab_txt,
    hjust = -0.05, vjust = 1.5,  # nudges it slightly inward
    size = 3
  ) +
  #  geom_hline(aes(yintercept = mean(results_table_euclidean$euclidean_distance), color = "mean\nestimate")) + 
  labs(linetype="", color="", y="Regression coefficient", x = "Party (ordered by estimated coefficient)", title = "Euclidean distance") + theme(plot.title = element_text(hjust = 0.5))+
  scale_linetype_manual(values = c("dashed"))+ theme(legend.position = "none") + scale_color_manual(values = cols)




library(ggpubr)

combined <- ggarrange(
  p_self_description_distance_lrgen,
  p_objective_distance_lrgen,
  p_objective_distance_galtan,
  p_objective_distance_lrecon,
  p_euclidean_distance,
  ncol = 2, nrow = 3,
  labels = c("A", "B", "C", "D", "E"),
  common.legend = FALSE,
  align = "v"
)

combined_titled <- annotate_figure(
  combined,
  top = text_grob("", face = "bold", size = 8)
)
combined_titled <- annotate_figure(
  combined_titled,
  top = text_grob("Esimtated coefficients and 95% confidence intervals\nof the impact of ideology on voting propensity across models\n(univariate regressions)", face = "bold", size = 18)
)
combined_titled

ggsave(filename="combined_plot_univariate.PNG", width = 8, height = 10)









###### with objective

options(scipen=999)


unique_parties <- unique(voter_table_long_linked_imputed2$party)
results_table <- data.frame(party_id=character(),
                            country=character(), 
                            objective_distance_lrgen=numeric(), 
                            objective_distance_lrgen_lower=numeric(), 
                            objective_distance_lrgen_upper=numeric(), 
                            self_description_distance_lrgen=numeric(), 
                            self_description_distance_lrgen_lower=numeric(), 
                            self_description_distance_lrgen_upper=numeric(), 
                            
                            objective_distance_lrecon=numeric(), 
                            objective_distance_lrecon_lower=numeric(), 
                            objective_distance_lrecon_upper=numeric(), 
                            
                            objective_distance_galtan=numeric(), 
                            objective_distance_galtan_lower=numeric(), 
                            objective_distance_galtan_upper=numeric(), 
                            
                            euclidean_distance=numeric(), 
                            euclidean_distance_lower=numeric(), 
                            euclidean_distance_upper=numeric(), 
                            
                            stringsAsFactors=FALSE) 

while (!is.na(unique_parties[1]))
{
  if (!is.na(unique(subset(voter_table_long_linked_imputed2, party == unique_parties[1])$distance_lrgen_predicted)[1]))
  {
    lm_results_objective_lrgen <- lm(data = subset(voter_table_long_linked_imputed2, party == unique_parties[1]) , voting_propensity ~ distance_lrgen_predicted + distance_self_description)
    lm_results_self_description_lrgen <- lm(data = subset(voter_table_long_linked_imputed2, party == unique_parties[1]) , voting_propensity ~ distance_self_description)
    lm_results_objective_lrecon <- lm(data = subset(voter_table_long_linked_imputed2, party == unique_parties[1]) , voting_propensity ~ distance_lrecon_predicted + distance_self_description)
    lm_results_objective_galtan <- lm(data = subset(voter_table_long_linked_imputed2, party == unique_parties[1]) , voting_propensity ~ distance_galtan_predicted + distance_self_description)
    lm_results_euclidean_distance <- lm(data = subset(voter_table_long_linked_imputed2, party == unique_parties[1]) , voting_propensity ~ euclidean_distance_predicted + distance_self_description)
    results_table_appended <- data.frame(party_id=unique_parties[1],
                                         country=unique(subset(voter_table_long_linked_imputed2, party == unique_parties[1])$countryname)[1], 
                                         #objective_distance_lrecon=numeric(), 
                                         #objective_distance_galtan=numeric(), 
                                         objective_distance_lrgen=as.numeric(lm_results_objective_lrgen$coefficients[2]),
                                         objective_distance_lrgen_lower=as.numeric(confint(lm_results_objective_lrgen)[2,1]),
                                         objective_distance_lrgen_upper=as.numeric(confint(lm_results_objective_lrgen)[2,2]), 
                                         self_description_distance_lrgen=as.numeric(lm_results_self_description_lrgen$coefficients[2]),
                                         self_description_distance_lrgen_lower=as.numeric(confint(lm_results_self_description_lrgen)[2,1]),
                                         self_description_distance_lrgen_upper=as.numeric(confint(lm_results_self_description_lrgen)[2,2]), 
                                         
                                         objective_distance_lrecon=as.numeric(lm_results_objective_lrecon$coefficients[2]),
                                         objective_distance_lrecon_lower=as.numeric(confint(lm_results_objective_lrecon)[2,1]),
                                         objective_distance_lrecon_upper=as.numeric(confint(lm_results_objective_lrecon)[2,2]), 
                                         
                                         objective_distance_galtan=as.numeric(lm_results_objective_galtan$coefficients[2]),
                                         objective_distance_galtan_lower=as.numeric(confint(lm_results_objective_galtan)[2,1]),
                                         objective_distance_galtan_upper=as.numeric(confint(lm_results_objective_galtan)[2,2]),
                                         
                                         euclidean_distance=as.numeric(lm_results_euclidean_distance$coefficients[2]),
                                         euclidean_distance_lower=as.numeric(confint(lm_results_euclidean_distance)[2,1]),
                                         euclidean_distance_upper=as.numeric(confint(lm_results_euclidean_distance)[2,2]),
                                         #objective_distance_lrgen=as.numeric(lm_results_subjective_vs_objective_lrgen$coefficients[3]), 
                                         #objective_distance_lrgen_lower=as.numeric(confint(lm_results_subjective_vs_objective_lrgen)[3,1]), 
                                         #objective_distance_lrgen_upper=as.numeric(confint(lm_results_subjective_vs_objective_lrgen)[3,2]), 
                                         #distance_self_description=numeric(), 
                                         #objective_distance_lrecon_upper=numeric(), 
                                         #objective_distance_galtan_upper=numeric(), 
                                         #distance_self_description_upper=numeric(), 
                                         #objective_distance_lrecon_lower=numeric(), 
                                         #objective_distance_galtan_lower=numeric(), 
                                         #distance_self_description_lower=numeric(), 
                                         stringsAsFactors=FALSE) 
    results_table <- rbind(results_table, results_table_appended)
  }
  unique_parties <- unique_parties[-1]
  
}
results_table_objective_lrgen <- sqldf("SELECT ROW_NUMBER() OVER(ORDER BY objective_distance_lrgen) RowNumber, * FROM results_table ORDER BY objective_distance_lrgen")
results_table_self_description_lrgen <- sqldf("SELECT ROW_NUMBER() OVER(ORDER BY self_description_distance_lrgen) RowNumber, * FROM results_table ORDER BY self_description_distance_lrgen")
results_table_objective_lrecon <- sqldf("SELECT ROW_NUMBER() OVER(ORDER BY objective_distance_lrecon) RowNumber, * FROM results_table ORDER BY objective_distance_lrecon")
results_table_objective_galtan <- sqldf("SELECT ROW_NUMBER() OVER(ORDER BY objective_distance_galtan) RowNumber, * FROM results_table ORDER BY objective_distance_galtan")
results_table_euclidean <- sqldf("SELECT ROW_NUMBER() OVER(ORDER BY euclidean_distance) RowNumber, * FROM results_table ORDER BY euclidean_distance")


#results_table3 <- sqldf("SELECT ROW_NUMBER() OVER(ORDER BY subjective_distance_lrgen) RowNumber, * FROM results_table ORDER BY subjective_distance_lrgen")

library(ggplot2)


# install.packages(c("dplyr","ggplot2","tidyr","tibble"))
library(dplyr)
library(tidyr)
library(tibble)
library(ggplot2)

vars <- c(
  "objective_distance_lrgen",
  "objective_distance_lrecon",
  "objective_distance_galtan",
  "euclidean_distance"
)

# Nice labels (edit as you like)
var_labels <- c(
  objective_distance_lrgen        = "Objective distance (LR general)",
  objective_distance_lrecon       = "Objective distance (LR economy)",
  objective_distance_galtan       = "Objective distance (GAL-TAN)",
  euclidean_distance              = "Euclidean distance"
)

corr_mat <- results_table_objective_galtan %>%
  select(all_of(vars)) %>%
  cor(use = "pairwise.complete.obs", method = "pearson")

corr_long <- as.data.frame(corr_mat) %>%
  rownames_to_column("Var1_raw") %>%
  pivot_longer(-Var1_raw, names_to = "Var2_raw", values_to = "r") %>%
  mutate(
    i = match(Var1_raw, vars),
    j = match(Var2_raw, vars),
    
    # mask lower triangle (i > j) -> set to NA
    r_plot = ifelse(i > j, NA_real_, r),
    lab    = ifelse(is.na(r_plot), "", sprintf("%.2f", r_plot)),
    
    # apply nice labels + ordering
    Var1 = factor(Var1_raw, levels = vars, labels = unname(var_labels[vars])),
    Var2 = factor(Var2_raw, levels = vars, labels = unname(var_labels[vars]))
  )

corr_plot <- ggplot(corr_long, aes(Var1, Var2, fill = r_plot)) +
  geom_tile(color = "white", linewidth = 0.5) +
  geom_text(aes(label = lab), size = 3) +
  coord_equal() +
  scale_fill_gradient2(limits = c(-1, 1), midpoint = 0, na.value = "white") +
  theme_minimal(base_size = 12) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    axis.title  = element_blank()
  ) +
  labs(fill = "r", title = "Correlation matrix of regression coefficients") + theme(plot.title = element_text(hjust = 0.5))+ theme(legend.position = "none")

ggsave(filename = "correlation_matrix.PNG", width=7, height=4)


m <- mean(results_table_objective_lrgen$objective_distance_lrgen, na.rm = TRUE)
p <- t.test(results_table_objective_lrgen$objective_distance_lrgen, mu = 0)$p.value

sig_neg_share <- with(
  results_table_objective_lrgen,
  mean(objective_distance_lrgen_upper < 0, na.rm = TRUE)
) * 100

sig_neg_share

sig_pos_share <- with(
  results_table_objective_lrgen,
  mean(objective_distance_lrgen_lower > 0, na.rm = TRUE)
) * 100

sig_pos_share

results_table_objective_lrgen$statistically_different <- "not"
results_table_objective_lrgen[results_table_objective_lrgen$objective_distance_lrgen_upper < 0, ]$statistically_different <- "negative"
results_table_objective_lrgen[results_table_objective_lrgen$objective_distance_lrgen_lower > 0, ]$statistically_different <- "positive"

cols <- c("negative"="#CC79A7", "positive"="#E69F00", "not"="#7F7F7F")

  lab_txt <- ifelse(p<0.001,sprintf("Mean estimate: %.3f*** (p < 0.001)\nShare significantly negative: %.2f%%\nShare significantly positive: %.2f%%", m, sig_neg_share, sig_pos_share), sprintf("Mean estimate: %.3f*** (p = %3f)\nShare significantly negative: %.2f%%\nShare significantly positive: %.2f%%", m, p, sig_neg_share, sig_pos_share))

p_objective_distance_lrgen <- ggplot(data = results_table_objective_lrgen) + 
  geom_linerange(aes(x=RowNumber, ymax = objective_distance_lrgen_upper, ymin = objective_distance_lrgen_lower, col=statistically_different)) +
  geom_point(aes(x=RowNumber, y = objective_distance_lrgen)) + theme_bw() + 
  annotate(
    "text",
    x = -Inf, y = Inf,
    label = lab_txt,
    hjust = -0.05, vjust = 1.5,  # nudges it slightly inward
    size = 3
  ) +
  geom_hline(aes(yintercept=0, linetype = "zero effect"))+ 
 # geom_hline(aes(yintercept = mean(results_table_objective_galtan$objective_distance_lrgen), color = "mean\nestimate")) + 
  labs(linetype="", color="", y="Regression coefficient", x = "Party (ordered by estimated coefficient)", title = "Objective distance (LR general)") + theme(plot.title = element_text(hjust = 0.5))+
  scale_linetype_manual(values = c("dashed"))+ theme(legend.position = "none") + scale_color_manual(values = cols)


m <- mean(results_table_self_description_lrgen$self_description_distance_lrgen, na.rm = TRUE)
p <- t.test(results_table_self_description_lrgen$self_description_distance_lrgen, mu = 0)$p.value

sig_neg_share <- with(
  results_table_self_description_lrgen,
  mean(self_description_distance_lrgen_upper < 0, na.rm = TRUE)
) * 100

sig_neg_share

sig_pos_share <- with(
  results_table_self_description_lrgen,
  mean(self_description_distance_lrgen_lower > 0, na.rm = TRUE)
) * 100

sig_pos_share


  lab_txt <- ifelse(p<0.001,sprintf("Mean estimate: %.3f*** (p < 0.001)\nShare significantly negative: %.2f%%\nShare significantly positive: %.2f%%", m, sig_neg_share, sig_pos_share), sprintf("Mean estimate: %.3f*** (p = %3f)\nShare significantly negative: %.2f%%\nShare significantly positive: %.2f%%", m, p, sig_neg_share, sig_pos_share))

results_table_self_description_lrgen$statistically_different <- "not"
results_table_self_description_lrgen[results_table_self_description_lrgen$self_description_distance_lrgen_upper < 0, ]$statistically_different <- "negative"
results_table_self_description_lrgen[results_table_self_description_lrgen$self_description_distance_lrgen_lower > 0, ]$statistically_different <- "positive"


p_self_description_distance_lrgen <- ggplot(data = results_table_self_description_lrgen) + 
  geom_linerange(aes(x=RowNumber, ymax = self_description_distance_lrgen_upper, ymin = self_description_distance_lrgen_lower, col=statistically_different)) +
  geom_point(aes(x=RowNumber, y = self_description_distance_lrgen)) + theme_bw() + 
  annotate(
    "text",
    x = -Inf, y = Inf,
    label = lab_txt,
    hjust = -0.05, vjust = 1.5,  # nudges it slightly inward
    size = 3
  ) +
  geom_hline(aes(yintercept=0, linetype = "zero effect"))+ 
  #geom_hline(aes(yintercept = mean(results_table_self_description_lrgen$self_description_distance_lrgen), color = "average effect")) +
    labs(linetype="", color="", y="Regression coefficient", x = "Party (ordered by estimated coefficient)", title = "Subjective distance (left-right)") + theme(plot.title = element_text(hjust = 0.5))+
    scale_linetype_manual(values = c("dashed")) + theme(legend.position = "none") + scale_color_manual(values = cols)


m <- mean(results_table_objective_lrecon$objective_distance_lrecon, na.rm = TRUE)
p <- t.test(results_table_objective_lrecon$objective_distance_lrecon, mu = 0)$p.value

sig_neg_share <- with(
  results_table_objective_lrecon,
  mean(objective_distance_lrecon_upper < 0, na.rm = TRUE)
) * 100

sig_neg_share

sig_pos_share <- with(
  results_table_objective_lrecon,
  mean(objective_distance_lrecon_lower > 0, na.rm = TRUE)
) * 100

sig_pos_share


results_table_objective_lrecon$statistically_different <- "not"
results_table_objective_lrecon[results_table_objective_lrecon$objective_distance_lrecon_upper < 0, ]$statistically_different <- "negative"
results_table_objective_lrecon[results_table_objective_lrecon$objective_distance_lrecon_lower > 0, ]$statistically_different <- "positive"

  lab_txt <- ifelse(p<0.001,sprintf("Mean estimate: %.3f*** (p < 0.001)\nShare significantly negative: %.2f%%\nShare significantly positive: %.2f%%", m, sig_neg_share, sig_pos_share), sprintf("Mean estimate: %.3f*** (p = %3f)\nShare significantly negative: %.2f%%\nShare significantly positive: %.2f%%", m, p, sig_neg_share, sig_pos_share))


p_objective_distance_lrecon <- ggplot(data = results_table_objective_lrecon) + 
  geom_linerange(aes(x=RowNumber, ymax = objective_distance_lrecon_upper, ymin = objective_distance_lrecon_lower, col=statistically_different)) +
  geom_point(aes(x=RowNumber, y = objective_distance_lrecon)) + theme_bw() + 
  annotate(
    "text",
    x = -Inf, y = Inf,
    label = lab_txt,
    hjust = -0.05, vjust = 1.5,  # nudges it slightly inward
    size = 3
  ) +
  geom_hline(aes(yintercept=0, linetype = "zero effect"))+ 
  #geom_hline(aes(yintercept = mean(results_table_objective_galtan$objective_distance_lrecon), color = "mean\nestimate")) + 
  labs(linetype="", color="", y="Regression coefficient", x = "Party (ordered by estimated coefficient)", title = "Objective distance (LR economy)") + theme(plot.title = element_text(hjust = 0.5))+
  scale_linetype_manual(values = c("dashed"))+ theme(legend.position = "none") + scale_color_manual(values = cols)


m <- mean(results_table_objective_galtan$objective_distance_galtan, na.rm = TRUE)
p <- t.test(results_table_objective_galtan$objective_distance_galtan, mu = 0)$p.value

sig_neg_share <- with(
  results_table_objective_galtan,
  mean(objective_distance_galtan_upper < 0, na.rm = TRUE)
) * 100

mean(results_table_objective_galtan$objective_distance_galtan_upper < 0, na.rm = TRUE)

mean(results_table_objective_lrgen$objective_distance_lrgen_lower > 0, na.rm = TRUE)

mean(results_table_objective_galtan$objective_distance_galtan_lower > 0, na.rm = TRUE)

sig_neg_share

sig_pos_share <- with(
  results_table_objective_galtan,
  mean(objective_distance_galtan_lower > 0, na.rm = TRUE)
) * 100

sig_pos_share

results_table_objective_galtan$statistically_different <- "not"
results_table_objective_galtan[results_table_objective_galtan$objective_distance_galtan_upper < 0, ]$statistically_different <- "negative"
results_table_objective_galtan[results_table_objective_galtan$objective_distance_galtan_lower > 0, ]$statistically_different <- "positive"



  lab_txt <- ifelse(p<0.001,sprintf("Mean estimate: %.3f*** (p < 0.001)\nShare significantly negative: %.2f%%\nShare significantly positive: %.2f%%", m, sig_neg_share, sig_pos_share), sprintf("Mean estimate: %.3f*** (p = %3f)\nShare significantly negative: %.2f%%\nShare significantly positive: %.2f%%", m, p, sig_neg_share, sig_pos_share))



p_objective_distance_galtan <- ggplot(data = results_table_objective_galtan) + 
  geom_linerange(aes(x=RowNumber, ymax = objective_distance_galtan_upper, ymin = objective_distance_galtan_lower, col=statistically_different)) +
  geom_point(aes(x=RowNumber, y = objective_distance_galtan)) + theme_bw() + 
  geom_hline(aes(yintercept=0, linetype = "zero effect"))+ 
  annotate(
    "text",
    x = -Inf, y = Inf,
    label = lab_txt,
    hjust = -0.05, vjust = 1.5,  # nudges it slightly inward
    size = 3
  ) +
#  geom_hline(aes(yintercept = mean(results_table_objective_galtan$objective_distance_galtan), color = "mean\nestimate")) + 
  labs(linetype="", color="", y="Regression coefficient", x = "Party (ordered by estimated coefficient)", title = "Objective distance (GAL-TAN)") + theme(plot.title = element_text(hjust = 0.5))+
  scale_linetype_manual(values = c("dashed")) + theme(legend.position = "none") + scale_color_manual(values = cols)


results_table_euclidean$statistically_different <- "not"
results_table_euclidean[results_table_euclidean$euclidean_distance_upper < 0, ]$statistically_different <- "negative"
results_table_euclidean[results_table_euclidean$euclidean_distance_lower > 0, ]$statistically_different <- "positive"

m <- mean(results_table_euclidean$euclidean_distance, na.rm = TRUE)
p <- t.test(results_table_euclidean$euclidean_distance, mu = 0)$p.value

sig_neg_share <- with(
  results_table_euclidean,
  mean(euclidean_distance_upper < 0, na.rm = TRUE)
) * 100

sig_neg_share

sig_pos_share <- with(
  results_table_euclidean,
  mean(euclidean_distance_lower > 0, na.rm = TRUE)
) * 100

sig_pos_share

  
  lab_txt <- ifelse(p<0.001,sprintf("Mean estimate: %.3f*** (p < 0.001)\nShare significantly negative: %.2f%%\nShare significantly positive: %.2f%%", m, sig_neg_share, sig_pos_share), sprintf("Mean estimate: %.3f*** (p = %3f)\nShare significantly negative: %.2f%%\nShare significantly positive: %.2f%%", m, p, sig_neg_share, sig_pos_share))


p_euclidean_distance <- ggplot(data = results_table_euclidean) + 
  geom_linerange(aes(x=RowNumber, ymax = euclidean_distance_upper, ymin = euclidean_distance_lower, col=statistically_different)) +
  geom_point(aes(x=RowNumber, y = euclidean_distance)) + theme_bw() + 
  geom_hline(aes(yintercept=0, linetype = "zero effect"))+ 
  annotate(
    "text",
    x = -Inf, y = Inf,
    label = lab_txt,
    hjust = -0.05, vjust = 1.5,  # nudges it slightly inward
    size = 3
  ) +
#  geom_hline(aes(yintercept = mean(results_table_euclidean$euclidean_distance), color = "mean\nestimate")) + 
  labs(linetype="", color="", y="Regression coefficient", x = "Party (ordered by estimated coefficient)", title = "Euclidean distance") + theme(plot.title = element_text(hjust = 0.5))+
  scale_linetype_manual(values = c("dashed"))+ theme(legend.position = "none") + scale_color_manual(values = cols)


ggsave(filename="euclidean_distance.PNG", width = 6, height = 4)



library(ggpubr)

combined <- ggarrange(
  p_self_description_distance_lrgen,
  p_objective_distance_lrgen,
  p_objective_distance_galtan,
  p_objective_distance_lrecon,
  p_euclidean_distance,
  ncol = 2, nrow = 3,
  labels = c("A", "B", "C", "D", "E"),
  common.legend = FALSE,
  align = "v"
)

combined_titled <- annotate_figure(
  combined,
  top = text_grob("", face = "bold", size = 8)
)
combined_titled <- annotate_figure(
  combined_titled,
  top = text_grob("Esimtated coefficients and 95% confidence intervals\nof the impact of ideology on voting propensity across models", face = "bold", size = 18)
)
combined_titled

ggsave(filename="combined_plot.PNG", width = 8, height = 10)


ggplot(data = results_table_objective_galtan) + 
  geom_linerange(aes(x=RowNumber, ymax = objective_distance_galtan_upper, ymin = objective_distance_galtan_lower, color = "GAL-TAN")) +
  geom_point(aes(x=RowNumber, y = objective_distance_galtan, color = "GAL-TAN")) + 
  geom_linerange(aes(x=RowNumber, ymax = objective_distance_lrecon_upper, ymin = objective_distance_lrecon_lower, color = "LRECON")) +
  geom_point(aes(x=RowNumber, y = objective_distance_lrecon, color = "LRECON")) + 
  theme_bw() + 
  geom_hline(yintercept=0, color = "red") 

ggplot(data = results_table_self_description_lrgen) + 
  geom_linerange(aes(x=RowNumber, ymax = objective_distance_lrgen_upper, ymin = objective_distance_lrgen_lower, color = "objective")) +
  geom_point(aes(x=RowNumber, y = objective_distance_lrgen, color = "objective")) + 
  geom_linerange(aes(x=RowNumber, ymax = objective_distance_galtan_upper, ymin = objective_distance_galtan_lower, color = "GAL-TAN")) +
  geom_point(aes(x=RowNumber, y = objective_distance_galtan, color = "GAL-TAN")) + 
  geom_linerange(aes(x=RowNumber, ymax = objective_distance_lrecon_upper, ymin = objective_distance_lrecon_lower, color = "LRECON")) +
  geom_point(aes(x=RowNumber, y = objective_distance_lrecon, color = "LRECON")) + 
  theme_bw() + 
  geom_hline(yintercept=0, color = "red") 

cor(results_table_objective_galtan$self_description_distance_lrgen, results_table_objective_galtan$objective_distance_lrgen,  method = "pearson", use = "complete.obs")
cor(results_table_objective_galtan$self_description_distance_lrgen, results_table_objective_galtan$objective_distance_lrecon,  method = "pearson", use = "complete.obs")
cor(results_table_objective_galtan$self_description_distance_lrgen, results_table_objective_galtan$objective_distance_galtan,  method = "pearson", use = "complete.obs")








## voting propensity

unique_parties <- unique(voter_table_long_linked_imputed2$party)
results_table <- data.frame(party_id=character(),
                country=character(), 
                #objective_distance_lrecon=numeric(), 
                #objective_distance_galtan=numeric(), 
                subjective_distance_lrgen=numeric(), 
                subjective_distance_lrgen_lower=numeric(), 
                subjective_distance_lrgen_upper=numeric(), 
                objective_distance_lrgen=numeric(), 
                objective_distance_lrgen_lower=numeric(), 
                objective_distance_lrgen_upper=numeric(), 
                #distance_self_description=numeric(), 
                #objective_distance_lrecon_upper=numeric(), 
                #objective_distance_galtan_upper=numeric(), 
                #distance_self_description_upper=numeric(), 
                #objective_distance_lrecon_lower=numeric(), 
                #objective_distance_galtan_lower=numeric(), 
                #distance_self_description_lower=numeric(), 
                stringsAsFactors=FALSE) 

while (!is.na(unique_parties[1]))
{
  if (!is.na(unique(subset(voter_table_long_linked_imputed2, party == unique_parties[1])$distance_lrgen_predicted)[1]))
  {
  lm_results_subjective_vs_objective_lrgen <- lm(data = subset(voter_table_long_linked_imputed2, party == unique_parties[1]) , voting_propensity ~ perceived_distance + distance_lrgen_predicted)
  
  results_table_appended <- data.frame(party_id=unique_parties[1],
                              country=unique(subset(voter_table_long_linked_imputed2, party == unique_parties[1])$countryname)[1], 
                              #objective_distance_lrecon=numeric(), 
                              #objective_distance_galtan=numeric(), 
                              subjective_distance_lrgen=as.numeric(lm_results_subjective_vs_objective_lrgen$coefficients[2]),
                              subjective_distance_lrgen_lower=as.numeric(confint(lm_results_subjective_vs_objective_lrgen)[2,1]),
                              subjective_distance_lrgen_upper=as.numeric(confint(lm_results_subjective_vs_objective_lrgen)[2,2]), 
                              objective_distance_lrgen=as.numeric(lm_results_subjective_vs_objective_lrgen$coefficients[3]), 
                              objective_distance_lrgen_lower=as.numeric(confint(lm_results_subjective_vs_objective_lrgen)[3,1]), 
                              objective_distance_lrgen_upper=as.numeric(confint(lm_results_subjective_vs_objective_lrgen)[3,2]), 
                              #distance_self_description=numeric(), 
                              #objective_distance_lrecon_upper=numeric(), 
                              #objective_distance_galtan_upper=numeric(), 
                              #distance_self_description_upper=numeric(), 
                              #objective_distance_lrecon_lower=numeric(), 
                              #objective_distance_galtan_lower=numeric(), 
                              #distance_self_description_lower=numeric(), 
                              stringsAsFactors=FALSE) 
  results_table <- rbind(results_table, results_table_appended)
  }
  unique_parties <- unique_parties[-1]
         
}
results_table2 <- sqldf("SELECT ROW_NUMBER() OVER(ORDER BY objective_distance_lrgen) RowNumber, * FROM results_table ORDER BY objective_distance_lrgen")
results_table3 <- sqldf("SELECT ROW_NUMBER() OVER(ORDER BY subjective_distance_lrgen) RowNumber, * FROM results_table ORDER BY subjective_distance_lrgen")

library(ggplot2)

ggplot(data = results_table2) + 
  geom_linerange(aes(x=RowNumber, ymax = objective_distance_lrgen_upper, ymin = objective_distance_lrgen_lower)) +
  geom_point(aes(x=RowNumber, y = objective_distance_lrgen)) + theme_bw()

ggplot(data = results_table3) + 
  geom_linerange(aes(x=RowNumber, ymax = subjective_distance_lrgen_upper, ymin = subjective_distance_lrgen_lower)) +
  geom_point(aes(x=RowNumber, y = subjective_distance_lrgen)) + theme_bw()




unique_parties <- unique(voter_table_long_linked_imputed2$party)
results_table_self <- data.frame(party_id=character(),
                            country=character(), 
                            #objective_distance_lrecon=numeric(), 
                            #objective_distance_galtan=numeric(), 
                            subjective_distance_lrgen=numeric(), 
                            subjective_distance_lrgen_lower=numeric(), 
                            subjective_distance_lrgen_upper=numeric(), 
                            #objective_distance_lrgen=numeric(), 
                            #objective_distance_lrgen_lower=numeric(), 
                            #objective_distance_lrgen_upper=numeric(), 
                            distance_self_description=numeric(), 
                            distance_self_description_lower=numeric(), 
                            distance_self_description_upper=numeric(), 
                            #objective_distance_lrecon_upper=numeric(), 
                            #objective_distance_galtan_upper=numeric(), 
                            #objective_distance_lrecon_lower=numeric(), 
                            #objective_distance_galtan_lower=numeric(), 
                            stringsAsFactors=FALSE) 

while (!is.na(unique_parties[1]))
{
  if (!is.na(unique(subset(voter_table_long_linked_imputed2, party == unique_parties[1])$distance_lrgen_predicted)[1]))
  {
    lm_results_subjective_vs_objective_lrgen <- lm(data = subset(voter_table_long_linked_imputed2, party == unique_parties[1]) , voting_propensity ~ perceived_distance + distance_self_description)
    
    results_table_appended2 <- data.frame(party_id=unique_parties[1],
                                         country=unique(subset(voter_table_long_linked_imputed2, party == unique_parties[1])$countryname)[1], 
                                         #objective_distance_lrecon=numeric(), 
                                         #objective_distance_galtan=numeric(), 
                                         subjective_distance_lrgen=as.numeric(lm_results_subjective_vs_objective_lrgen$coefficients[2]),
                                         subjective_distance_lrgen_lower=as.numeric(confint(lm_results_subjective_vs_objective_lrgen)[2,1]),
                                         subjective_distance_lrgen_upper=as.numeric(confint(lm_results_subjective_vs_objective_lrgen)[2,2]), 
                                         #objective_distance_lrgen=as.numeric(lm_results_subjective_vs_objective_lrgen$coefficients[3]), 
                                         #objective_distance_lrgen_lower=as.numeric(confint(lm_results_subjective_vs_objective_lrgen)[3,1]), 
                                         #objective_distance_lrgen_upper=as.numeric(confint(lm_results_subjective_vs_objective_lrgen)[3,2]), 
                                         distance_self_description=as.numeric(lm_results_subjective_vs_objective_lrgen$coefficients[3]), 
                                         distance_self_description_lower=as.numeric(confint(lm_results_subjective_vs_objective_lrgen)[3,1]), 
                                         distance_self_description_upper=as.numeric(confint(lm_results_subjective_vs_objective_lrgen)[3,2]), 
                                         #objective_distance_lrecon_upper=numeric(), 
                                         #objective_distance_galtan_upper=numeric(), 
                                         #objective_distance_lrecon_lower=numeric(), 
                                         #objective_distance_galtan_lower=numeric(), 
                                         stringsAsFactors=FALSE) 
    results_table_self <- rbind(results_table_self, results_table_appended2)
  }
  unique_parties <- unique_parties[-1]
  
}
results_table_self2 <- sqldf("SELECT ROW_NUMBER() OVER(ORDER BY distance_self_description) RowNumber, * FROM results_table_self ORDER BY distance_self_description")
results_table_self3 <- sqldf("SELECT ROW_NUMBER() OVER(ORDER BY subjective_distance_lrgen) RowNumber, * FROM results_table_self ORDER BY subjective_distance_lrgen")

library(ggplot2)

ggplot(data = results_table_self2) + 
  geom_linerange(aes(x=RowNumber, ymax = distance_self_description_upper, ymin = distance_self_description_lower)) +
  geom_point(aes(x=RowNumber, y = distance_self_description)) + theme_bw()

ggplot(data = results_table_self3) + 
  geom_linerange(aes(x=RowNumber, ymax = subjective_distance_lrgen_upper, ymin = subjective_distance_lrgen_lower)) +
  geom_point(aes(x=RowNumber, y = subjective_distance_lrgen)) + theme_bw()



library(forestplot)

?forestplot

voting_propensity_self_FE <- feols(data = voter_table_long_linked_imputed2, voting_propensity ~ perceived_distance + distance_self_description | party)
summary(voting_propensity_self_FE)

voting_propensity_RF_FE <- feols(data = voter_table_long_linked_imputed2, voting_propensity ~ perceived_distance + distance_lrgen_predicted  | party)
summary(voting_propensity_RF_FE)

voting_propensity_2D_FE <- feols(data = voter_table_long_linked_imputed2, voting_propensity ~ perceived_distance + distance_lrecon_predicted + distance_galtan_predicted  | party)
summary(voting_propensity_2D_FE)

voting_propensity_2D_FE_2 <- feols(data = voter_table_long_linked_imputed2, voting_propensity ~ perceived_distance + distance_lrgen_predicted + distance_lrecon_predicted + distance_galtan_predicted | party)
summary(voting_propensity_2D_FE_2)

voting_propensity_subjective_objective <- feols(data = voter_table_long_linked_imputed2, voting_propensity ~ perceived_distance + distance_self_description + distance_lrgen_predicted  | party)
summary(voting_propensity_subjective_objective)


#European elections

voter_table_long_linked_imputed2$voted_for_this_party <- ifelse(voter_table_long_linked_imputed2$Q7_gen < 2, voter_table_long_linked_imputed2$Q7_gen, NA)

voted_party_probit_self <- feglm(data = voter_table_long_linked_imputed2, voted_for_this_party ~ perceived_distance + distance_self_description)
summary(voted_party_probit_self)

voted_party_probit_self_FE <- feglm(data = voter_table_long_linked_imputed2, voted_for_this_party ~ perceived_distance + distance_self_description | party)
summary(voted_party_probit_self_FE)

voted_party_probit_RF <- feglm(data = voter_table_long_linked_imputed2, voted_for_this_party ~ perceived_distance + distance_lrgen_predicted )
summary(voted_party_probit_RF)

voted_party_probit_RF_FE <- feglm(data = voter_table_long_linked_imputed2, voted_for_this_party ~ perceived_distance + distance_lrgen_predicted  | party)
summary(voted_party_probit_RF_FE)

voted_party_probit_2D_FE <- feglm(data = voter_table_long_linked_imputed2, voted_for_this_party ~ perceived_distance + distance_lrecon_predicted + distance_galtan_predicted  | party)
summary(voted_party_probit_2D_FE)

voted_party_probit_2D_FE_2 <- feglm(data = voter_table_long_linked_imputed2, voted_for_this_party ~ perceived_distance + distance_lrgen_predicted + distance_lrecon_predicted + distance_galtan_predicted | party)
summary(voted_party_probit_2D_FE_2)

voted_party_probit_subjective_objective <- feglm(data = voter_table_long_linked_imputed2, voted_for_this_party ~ perceived_distance + distance_self_description + distance_lrgen_predicted  | party)
summary(voted_party_probit_subjective_objective)

#nationl elections

voter_table_long_linked_imputed2$voted_for_this_party_national <- ifelse(voter_table_long_linked_imputed2$Q9_gen < 2, voter_table_long_linked_imputed2$Q9_gen, NA)

voted_party_national_probit_self <- feglm(data = voter_table_long_linked_imputed2, voted_for_this_party_national ~ perceived_distance + distance_self_description)
summary(voted_party_national_probit_self)

voted_party_national_probit_self_FE <- feglm(data = voter_table_long_linked_imputed2, voted_for_this_party_national ~ perceived_distance + distance_self_description | party)
summary(voted_party_national_probit_self_FE)

voted_party_national_probit_RF <- feglm(data = voter_table_long_linked_imputed2, voted_for_this_party_national ~ perceived_distance + distance_lrgen_predicted )
summary(voted_party_national_probit_RF)

voted_party_national_probit_RF_FE <- feglm(data = voter_table_long_linked_imputed2, voted_for_this_party_national ~ perceived_distance + distance_lrgen_predicted  | party)
summary(voted_party_national_probit_RF_FE)

voted_party_national_probit_2D_FE <- feglm(data = voter_table_long_linked_imputed2, voted_for_this_party_national ~ perceived_distance + distance_lrecon_predicted + distance_galtan_predicted  | party)
summary(voted_party_national_probit_2D_FE)

voted_party_national_probit_2D_FE_2 <- feglm(data = voter_table_long_linked_imputed2, voted_for_this_party_national ~ perceived_distance + distance_lrgen_predicted + distance_lrecon_predicted + distance_galtan_predicted  | party)
summary(voted_party_national_probit_2D_FE_2)

voted_party_national_probit_subjective_objective <- feglm(data = voter_table_long_linked_imputed2, voted_for_this_party_national ~ perceived_distance + distance_self_description + distance_lrgen_predicted  | party)
summary(voted_party_national_probit_subjective_objective)




library("modelsummary")



cm <- c(
  'perceived_distance' = 'Distance between\\\\self-placement and\\\\subjective party\\\\placement',
  'distance_self_description' = 'Distance between\\\\self-placement and\\\\party placement\\\\by mean expert',
  'distance_lrgen_predicted' = 'Distance between\\\\predicted ideology\\\\and party placement\\\\by mean expert\\\\(left-right general)',
  'distance_lrecon_predicted' = 'Distance between\\\\predicted ideology\\\\and party placement\\\\by mean expert\\\\(left right economy)',
  'distance_galtan_predicted' = 'Distance between\\\\predicted ideology\\\\and party placement\\\\by mean expert\\\\(GAL-TAN)',
  'party' = 'party')

get_gof(voted_party_national_probit_self_FE)
go <- modelsummary::gof_map
go[go$raw == "r2", ]$clean <- "McFadden's R2"
go[go$raw == "r.squared", ]$clean <- "McFadden's R2"
go[go$raw == "adj.r.squared", ]$clean <- "McFadden's R2 Adj."
go[go$raw == "r2.adjusted", ]$clean <- "McFadden's R2 Adj."
go[go$raw == "r2.within", ]$clean <- "McFadden's R2 Within"
go[go$raw == "r2.within.adjusted", ]$clean <- "McFadden's R2 Within Adj."
go[go$raw == "tli", ]$omit <- FALSE
go[go$raw == "tli", ]$clean <- "FE: party"
go[go$raw == "tli", ]$raw <- "FE: party"

##OLS voting propensity
tab <- modelsummary(list('(1)' = voting_propensity_self_FE, '(2)' = voting_propensity_RF_FE, '(3)' = voting_propensity_subjective_objective,'(4)' =  voting_propensity_2D_FE, '(5)' =  voting_propensity_2D_FE_2), title = "OLS estimates for the propensity to ever vote for a specific party", stars =  c('*' = .05, '**' = .01, '***' = .001), coef_map = cm,
                    ,output = "latex", escape = FALSE)

##Probit national elections
tab2 <- modelsummary(list('(1)' = voted_party_national_probit_self_FE, '(2)' = voted_party_national_probit_RF_FE, '(3)' = voted_party_probit_subjective_objective,'(4)' =  voted_party_national_probit_2D_FE, '(5)' =  voted_party_national_probit_2D_FE_2), title = "Probit regression estimates for the probability to have voted for a specific party at the last national elections", stars =  c('*' = .05, '**' = .01, '***' = .001), coef_map = cm, gof_map = go,
                    ,output = "latex", escape = FALSE)
##Probit European elections
tab3 <- modelsummary(list('(1)' = voted_party_probit_self_FE, '(2)' = voted_party_probit_RF_FE, '(3)' = voted_party_probit_subjective_objective,'(4)' =  voted_party_probit_2D_FE, '(5)' =  voted_party_probit_2D_FE_2), title = "Probit regression estimates for the probability to have voted for a specific party at the elections to the European parliament 2019", stars =  c('*' = .05, '**' = .01, '***' = .001), coef_map = cm, gof_map = go,
                    ,output = "latex", escape = FALSE)


##############robustness check: incl respid fixed effects

## voting propensity

voting_propensity_self_FE_voter <- feols(data = voter_table_long_linked_imputed2, voting_propensity ~ perceived_distance + distance_self_description | party + respid)
summary(voting_propensity_self_FE_voter, vcov = "twoway")

voting_propensity_RF_FE_voter <- feols(data = voter_table_long_linked_imputed2, voting_propensity ~ perceived_distance + distance_lrgen_predicted  | party + respid)
summary(voting_propensity_RF_FE_voter, vcov = "twoway")

voting_propensity_2D_FE_voter <- feols(data = voter_table_long_linked_imputed2, voting_propensity ~ perceived_distance + distance_lrecon_predicted + distance_galtan_predicted  | party + respid)
summary(voting_propensity_2D_FE_voter, vcov = "twoway")

voting_propensity_2D_FE_2_voter <- feols(data = voter_table_long_linked_imputed2, voting_propensity ~ perceived_distance + distance_lrgen_predicted + distance_lrecon_predicted + distance_galtan_predicted | party + respid)
summary(voting_propensity_2D_FE_2_voter, vcov = "twoway")

voting_propensity_subjective_objective_voter <- feols(data = voter_table_long_linked_imputed2, voting_propensity ~ perceived_distance + distance_self_description + distance_lrgen_predicted  | party + respid)
summary(voting_propensity_subjective_objective_voter, vcov = "twoway")


#European elections

voter_table_long_linked_imputed2$voted_for_this_party <- ifelse(voter_table_long_linked_imputed2$Q7_gen < 2, voter_table_long_linked_imputed2$Q7_gen, NA)

voted_party_probit_self_FE_voter <- feglm(data = voter_table_long_linked_imputed2, voted_for_this_party ~ perceived_distance + distance_self_description | party + respid)
summary(voted_party_probit_self_FE_voter, vcov = "twoway")

voted_party_probit_RF_FE_voter <- feglm(data = voter_table_long_linked_imputed2, voted_for_this_party ~ perceived_distance + distance_lrgen_predicted  | party + respid)
summary(voted_party_probit_RF_FE_voter, vcov = "twoway")

voted_party_probit_2D_FE_voter <- feglm(data = voter_table_long_linked_imputed2, voted_for_this_party ~ perceived_distance + distance_lrecon_predicted + distance_galtan_predicted  | party + respid)
summary(voted_party_probit_2D_FE_voter)

voted_party_probit_2D_FE_2_voter <- feglm(data = voter_table_long_linked_imputed2, voted_for_this_party ~ perceived_distance + distance_lrgen_predicted + distance_lrecon_predicted + distance_galtan_predicted | party + respid)
summary(voted_party_probit_2D_FE_2_voter)

voted_party_probit_subjective_objective_voter <- feglm(data = voter_table_long_linked_imputed2, voted_for_this_party ~ perceived_distance + distance_self_description + distance_lrgen_predicted  | party + respid)
summary(voted_party_probit_subjective_objective_voter)


#nationl elections

voter_table_long_linked_imputed2$voted_for_this_party_national <- ifelse(voter_table_long_linked_imputed2$Q9_gen < 2, voter_table_long_linked_imputed2$Q9_gen, NA)

voted_party_national_probit_self_FE_voter <- feglm(data = voter_table_long_linked_imputed2, voted_for_this_party_national ~ perceived_distance + distance_self_description | party + respid)
summary(voted_party_national_probit_self_FE_voter, vcov = "twoway")

voted_party_national_probit_RF_FE_voter <- feglm(data = voter_table_long_linked_imputed2, voted_for_this_party_national ~ perceived_distance + distance_lrgen_predicted  | party + respid)
summary(voted_party_national_probit_RF_FE_voter)

voted_party_national_probit_2D_FE_voter <- feglm(data = voter_table_long_linked_imputed2, voted_for_this_party_national ~ perceived_distance + distance_lrecon_predicted + distance_galtan_predicted  | party + respid)
summary(voted_party_national_probit_2D_FE_voter)

voted_party_national_probit_2D_FE_2_voter <- feglm(data = voter_table_long_linked_imputed2, voted_for_this_party_national ~ perceived_distance + distance_lrgen_predicted + distance_lrecon_predicted + distance_galtan_predicted  | party + respid)
summary(voted_party_national_probit_2D_FE_2_voter)

voted_party_national_probit_subjective_objective_voter <- feglm(data = voter_table_long_linked_imputed2, voted_for_this_party_national ~ perceived_distance + distance_self_description + distance_lrgen_predicted  | party + respid)
summary(voted_party_national_probit_subjective_objective_voter)





cm <- c(
  'perceived_distance' = 'Distance between\\\\self-placement and\\\\subjective party\\\\placement',
  'distance_self_description' = 'Distance between\\\\self-placement and\\\\party placement\\\\by mean expert',
  'distance_lrgen_predicted' = 'Distance between\\\\predicted ideology\\\\and party placement\\\\by mean expert\\\\(left-right general)',
  'distance_lrecon_predicted' = 'Distance between\\\\predicted ideology\\\\and party placement\\\\by mean expert\\\\(left right economy)',
  'distance_galtan_predicted' = 'Distance between\\\\predicted ideology\\\\and party placement\\\\by mean expert\\\\(GAL-TAN)',
  'party' = 'party')


go2a <- modelsummary::gof_map
go2a[go2a$raw == "vcov.type", ]$omit <- "TRUE"
go2a[go2a$raw == "std.error.type", ]$omit <- "TRUE"
go2a[go2a$raw == "se_type", ]$omit <- "TRUE"
go2a[go2a$raw == "tli", ]$omit <- FALSE
go2a[go2a$raw == "tli", ]$clean <- "FE: party"
go2a[go2a$raw == "tli", ]$raw <- "FE: party"
go2a[go2a$raw == "srmr", ]$omit <- FALSE
go2a[go2a$raw == "srmr", ]$clean <- "FE: respondent"
go2a[go2a$raw == "srmr", ]$raw <- "FE: respid"



go2 <- modelsummary::gof_map
go2[go2$raw == "r2", ]$clean <- "McFadden's R2"
go2[go2$raw == "r.squared", ]$clean <- "McFadden's R2"
go2[go2$raw == "adj.r.squared", ]$clean <- "McFadden's R2 Adj."
go2[go2$raw == "r2.adjusted", ]$clean <- "McFadden's R2 Adj."
go2[go2$raw == "r2.within", ]$clean <- "McFadden's R2 Within"
go2[go2$raw == "r2.within.adjusted", ]$clean <- "McFadden's R2 Within Adj."
go2[go2$raw == "vcov.type", ]$omit <- "TRUE"
go2[go2$raw == "tli", ]$omit <- FALSE
go2[go2$raw == "tli", ]$clean <- "FE: party"
go2[go2$raw == "tli", ]$raw <- "FE: party"
go2[go2$raw == "srmr", ]$omit <- FALSE
go2[go2$raw == "srmr", ]$clean <- "FE: respondent"
go2[go2$raw == "srmr", ]$raw <- "FE: respid"


##OLS voting propensity
tab_robustness <- modelsummary(list('(1)' = voting_propensity_self_FE_voter, '(2)' = voting_propensity_RF_FE_voter, '(3)' = voting_propensity_subjective_objective_voter,'(4)' =  voting_propensity_2D_FE_voter, '(5)' =  voting_propensity_2D_FE_2_voter), title = "OLS estimates for the propensity to ever vote for a specific party (two-way fixed effects)", stars =  c('*' = .05, '**' = .01, '***' = .001), coef_map = cm,
                    ,output = "latex", escape = FALSE)
##need to replace the
##Probit national elections
tab2_robustness <- modelsummary(list('(1)' = voted_party_national_probit_self_FE_voter, '(2)' = voted_party_national_probit_RF_FE_voter, '(3)' = voted_party_national_probit_subjective_objective_voter,'(4)' =  voted_party_national_probit_2D_FE_voter, '(5)' =  voted_party_national_probit_2D_FE_2_voter), title = "Probit regression estimates for the probability to have voted for a specific party at the last national elections (two-way fixed effects)", stars =  c('*' = .05, '**' = .01, '***' = .001), coef_map = cm, gof_map = go2,
                     ,output = "latex", escape = FALSE)
##Probit European elections
tab3_robustness <- modelsummary(list('(1)' = voted_party_probit_self_FE_voter, '(2)' = voted_party_probit_RF_FE_voter, '(3)' = voted_party_probit_subjective_objective_voter,'(4)' =  voted_party_probit_2D_FE_voter, '(5)' =  voted_party_probit_2D_FE_2_voter), title = "Probit regression estimates for the probability to have voted for a specific party at the elections to the European parliament 2019 (two-way fixed effects)", stars =  c('*' = .05, '**' = .01, '***' = .001), coef_map = cm, gof_map = go2,
                     ,output = "latex", escape = FALSE)
