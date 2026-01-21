library(haven)
library(MASS)
library(modelsummary)
library(sqldf)
library(fixest)
CHES2019_means <- read_dta("Z:/Predict Voter Position/CHES2019_means.dta")

subset(CHES2019_means, party_id == 2010)

EVS_predicted_data <- read.csv("Z:/Predict Voter Position/EVS_predicted_data.csv")
voter_table <- EVS_predicted_data


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

library(readr)
codeplan <- read_csv("Z:/Predict Voter Position/codeplan.csv")

voter_table_long_linked <- sqldf("SELECT t1.*, t2.Q9_Q25_EES as partyid_EES FROM voter_table_long t1 INNER JOIN codeplan t2 ON t2.Q9 = t1.party")
voter_table_long_linked <- sqldf("SELECT t1.*, t2.lrgen AS lrgen_this_party, t2.galtan AS galtan_this_party, t2.lrecon AS lrecon_this_party FROM voter_table_long_linked t1 LEFT OUTER JOIN CHES2019_means_to_be_linked t2 ON t2.ees = t1.partyid_EES")