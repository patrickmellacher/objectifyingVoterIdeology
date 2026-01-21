getwd()

library(MASS)
library(modelsummary)
library(sqldf)

setwd("Z:/Predict Voter Position")

#EVS_predicted_data_old <- read.csv("Z:/Predict Voter Position/EVS_predicted_data_old.csv")

EVS_predicted_data <- read.csv("Z:/Predict Voter Position/EVS_predicted_data.csv")


library("rnaturalearth")
library(e1071)
library(rockchalk)
library(stats)

library(psych)


library (car)

library(gt)
library(gtExtras)

voter_table <- EVS_predicted_data
#View(world)
world <- ne_countries(scale = "medium", returnclass = "sf")
Europe <- world[which(world$continent == "Europe"  | world$sovereignt == "Malta" | world$sovereignt == "Cyprus" | world$sovereignt == "Turkey"),]


voter_table$iso_a2 <- "AT"
voter_table$countryname <- "Austria"
voter_table$east <- 0
voter_table[voter_table$hCountry == 2, ]$iso_a2 <- "BE"
voter_table[voter_table$hCountry == 2, ]$countryname <- "Belgium"
voter_table[voter_table$hCountry == 3, ]$iso_a2 <- "BG"
voter_table[voter_table$hCountry == 3, ]$countryname <- "Bulgaria"
voter_table[voter_table$hCountry == 3, ]$east <- 1
voter_table[voter_table$hCountry == 4, ]$iso_a2 <- "HR"
voter_table[voter_table$hCountry == 4, ]$countryname <- "Croatia"
voter_table[voter_table$hCountry == 4, ]$east <- 1
voter_table[voter_table$hCountry == 5, ]$iso_a2 <- "CY"
voter_table[voter_table$hCountry == 5, ]$countryname <- "Cyprus"
voter_table[voter_table$hCountry == 6, ]$iso_a2 <- "CZ"
voter_table[voter_table$hCountry == 6, ]$countryname <- "Czech Republic"
voter_table[voter_table$hCountry == 6, ]$east <- 1
voter_table[voter_table$hCountry == 7, ]$iso_a2 <- "DE"
voter_table[voter_table$hCountry == 7, ]$countryname <- "Germany"
voter_table[voter_table$hCountry == 8, ]$iso_a2 <- "DK"
voter_table[voter_table$hCountry == 8, ]$countryname <- "Denmark"
voter_table[voter_table$hCountry == 9, ]$iso_a2 <- "EE"
voter_table[voter_table$hCountry == 9, ]$countryname <- "Estonia"
voter_table[voter_table$hCountry == 9, ]$east <- 1
voter_table[voter_table$hCountry == 10, ]$iso_a2 <- "FI"
voter_table[voter_table$hCountry == 10, ]$countryname <- "Finland"
voter_table[voter_table$hCountry == 11, ]$iso_a2 <- "FR"
voter_table[voter_table$hCountry == 11, ]$countryname <- "France"
voter_table[voter_table$hCountry == 12, ]$iso_a2 <- "GR"
voter_table[voter_table$hCountry == 12, ]$countryname <- "Greece"
voter_table[voter_table$hCountry == 13, ]$iso_a2 <- "HU"
voter_table[voter_table$hCountry == 13, ]$countryname <- "Hungary"
voter_table[voter_table$hCountry == 13, ]$east <- 1
voter_table[voter_table$hCountry == 14, ]$iso_a2 <- "IE"
voter_table[voter_table$hCountry == 14, ]$countryname <- "Ireland"
voter_table[voter_table$hCountry == 15, ]$iso_a2 <- "IT"
voter_table[voter_table$hCountry == 15, ]$countryname <- "Italy"
voter_table[voter_table$hCountry == 16, ]$iso_a2 <- "LV"
voter_table[voter_table$hCountry == 16, ]$countryname <- "Latvia"
voter_table[voter_table$hCountry == 16, ]$east <- 1
voter_table[voter_table$hCountry == 17, ]$iso_a2 <- "LT"
voter_table[voter_table$hCountry == 17, ]$countryname <- "Lithuania"
voter_table[voter_table$hCountry == 17, ]$east <- 1
voter_table[voter_table$hCountry == 18, ]$iso_a2 <- "LU"
voter_table[voter_table$hCountry == 18, ]$countryname <- "Luxemburg"
voter_table[voter_table$hCountry == 19, ]$iso_a2 <- "MT"
voter_table[voter_table$hCountry == 19, ]$countryname <- "Malta"
voter_table[voter_table$hCountry == 20, ]$iso_a2 <- "NL"
voter_table[voter_table$hCountry == 20, ]$countryname <- "Netherlands"
voter_table[voter_table$hCountry == 21, ]$iso_a2 <- "PL"
voter_table[voter_table$hCountry == 21, ]$countryname <- "Poland"
voter_table[voter_table$hCountry == 21, ]$east <- 1
voter_table[voter_table$hCountry == 22, ]$iso_a2 <- "PT"
voter_table[voter_table$hCountry == 22, ]$countryname <- "Portugal"
voter_table[voter_table$hCountry == 23, ]$iso_a2 <- "RO"
voter_table[voter_table$hCountry == 23, ]$countryname <- "Romania"
voter_table[voter_table$hCountry == 23, ]$east <- 1
voter_table[voter_table$hCountry == 24, ]$iso_a2 <- "SI"
voter_table[voter_table$hCountry == 24, ]$countryname <- "Slovenia"
voter_table[voter_table$hCountry == 24, ]$east <- 1
voter_table[voter_table$hCountry == 25, ]$iso_a2 <- "SK"
voter_table[voter_table$hCountry == 25, ]$countryname <- "Slovakia"
voter_table[voter_table$hCountry == 25, ]$east <- 1
voter_table[voter_table$hCountry == 26, ]$iso_a2 <- "ES"
voter_table[voter_table$hCountry == 26, ]$countryname <- "Spain"
voter_table[voter_table$hCountry == 27, ]$iso_a2 <- "SE"
voter_table[voter_table$hCountry == 27, ]$countryname <- "Sweden"
voter_table[voter_table$hCountry == 28, ]$iso_a2 <- "GB"
voter_table[voter_table$hCountry == 28, ]$countryname <- "United Kingdom"


voter_table$skewness_lrgen_self <- 0
voter_table$skewness_lrgen_predicted <- 0
voter_table$skewness_lrecon_predicted <- 0
voter_table$skewness_galtan_predicted <- 0

voter_table$excesskurtosis_lrgen_self <- 0
voter_table$excesskurtosis_lrgen_predicted <- 0
voter_table$excesskurtosis_lrecon_predicted <- 0
voter_table$excesskurtosis_galtan_predicted <- 0

voter_table$p_value_fligner_predictedlrgen <- 0
voter_table$p_value_fligner_predictedlrecon <- 0
voter_table$p_value_fligner_predictedgaltan <- 0

for (i in 1:28)
{
  voter_table[voter_table$hCountry == i, ]$skewness_lrgen_predicted <- skewness(voter_table[voter_table$hCountry == i, ]$predicted_values_lrgen)
  voter_table[voter_table$hCountry == i, ]$skewness_lrecon_predicted <- skewness(voter_table[voter_table$hCountry == i, ]$predicted_values_lrecon)
  voter_table[voter_table$hCountry == i, ]$skewness_galtan_predicted <- skewness(voter_table[voter_table$hCountry == i, ]$predicted_values_galtan)
  voter_table[voter_table$hCountry == i, ]$skewness_lrgen_self <- skewness(voter_table[voter_table$hCountry == i, ]$lrgen_selfdescription)

  voter_table[voter_table$hCountry == i, ]$excesskurtosis_lrgen_predicted <- kurtosis(voter_table[voter_table$hCountry == i, ]$predicted_values_lrgen)
  voter_table[voter_table$hCountry == i, ]$excesskurtosis_lrecon_predicted <- kurtosis(voter_table[voter_table$hCountry == i, ]$predicted_values_lrecon)
  voter_table[voter_table$hCountry == i, ]$excesskurtosis_galtan_predicted <- kurtosis(voter_table[voter_table$hCountry == i, ]$predicted_values_galtan)
  voter_table[voter_table$hCountry == i, ]$excesskurtosis_lrgen_self <- kurtosis(voter_table[voter_table$hCountry == i, ]$lrgen_selfdescription)
  
  voter_table[voter_table$hCountry == i, ]$p_value_fligner_predictedlrgen <- fligner.test(voter_table[voter_table$hCountry == i, ]$predicted_values_lrgen , voter_table[voter_table$hCountry == i, ]$lrgen_selfdescription)$p.value
}




#country_means <- sqldf("SELECT iso_a2, MEDIAN(prediction_rf_lrgen2_unrounded) prediction_rf_lrgen2, STDEV(prediction_rf_lrgen2_unrounded) AS stdev_prediction_rf_lrgen2, MEDIAN(prediction_rf_galtan2_unrounded) AS prediction_rf_galtan2, STDEV(prediction_rf_galtan2_unrounded) AS stdev_prediction_rf_galtan2, MEDIAN(prediction_rf_lrecon2_unrounded) AS prediction_rf_lrecon2, STDEV(prediction_rf_lrecon2_unrounded) AS stdev_prediction_rf_lrecon2, MEDIAN(lrgen_selfdescription) AS lrgen_selfdescription, STDEV(lrgen_selfdescription) AS stdev_lrgen_selfdescription FROM voter_table group by iso_a2")

options(scipen=999)
#########add excess kurtosis and skewness, then calculate index
country_means <- sqldf("SELECT iso_a2, countryname, AVG(lrgen_selfdescription) AS lrgen_selfdescription, AVG(predicted_values_lrgen) predicted_values_lrgen, AVG(predicted_values_lrecon) AS predicted_values_lrecon, AVG(predicted_values_galtan) AS predicted_values_galtan, STDEV(lrgen_selfdescription) AS stdev_lrgen_selfdescription, STDEV(predicted_values_lrgen) AS stdev_predicted_values_lrgen, STDEV(predicted_values_lrecon) AS stdev_predicted_values_lrecon, STDEV(predicted_values_galtan) AS stdev_predicted_values_galtan, MEDIAN(skewness_lrgen_predicted) AS skewness_lrgen_predicted, MEDIAN(skewness_lrecon_predicted) AS skewness_lrecon_predicted, MEDIAN(skewness_galtan_predicted) AS skewness_galtan_predicted, MEDIAN(skewness_lrgen_self) AS skewness_lrgen_self, MEDIAN(excesskurtosis_lrgen_predicted) AS excesskurtosis_lrgen_predicted, MEDIAN(excesskurtosis_lrecon_predicted) AS excesskurtosis_lrecon_predicted, MEDIAN(excesskurtosis_galtan_predicted) AS excesskurtosis_galtan_predicted, MEDIAN(excesskurtosis_lrgen_self) AS excesskurtosis_lrgen_self, MEDIAN(p_value_fligner_predictedlrgen) AS p_value_fligner, COUNT(*) AS participants FROM voter_table group by iso_a2")

#acc. https://academic.oup.com/poq/article/80/S1/392/2223374 , Equation 1:
country_means$bimodality_coefficient_lrgen_self <- ((country_means$skewness_lrgen_self ^ 2) + 1 ) / (country_means$excesskurtosis_lrgen_self + 3 * (((country_means$participants-1)^2)/((country_means$participants-2)*(country_means$participants-3))))
country_means$bimodality_coefficient_lrgen_predicted <- ((country_means$skewness_lrgen_predicted ^ 2) + 1 ) / (country_means$excesskurtosis_lrgen_predicted + 3 * (((country_means$participants-1)^2)/((country_means$participants-2)*(country_means$participants-3))))
country_means$bimodality_coefficient_galtan_predicted <- ((country_means$skewness_galtan_predicted ^ 2) + 1 ) / (country_means$excesskurtosis_galtan_predicted + 3 * (((country_means$participants-1)^2)/((country_means$participants-2)*(country_means$participants-3))))
country_means$bimodality_coefficient_lrecon_predicted <- ((country_means$skewness_lrecon_predicted ^ 2) + 1 ) / (country_means$excesskurtosis_lrecon_predicted + 3 * (((country_means$participants-1)^2)/((country_means$participants-2)*(country_means$participants-3))))



country_means |>
  dplyr::select(countryname, participants, lrgen_selfdescription, predicted_values_lrgen, predicted_values_lrecon, predicted_values_galtan, stdev_lrgen_selfdescription, stdev_predicted_values_lrgen, stdev_predicted_values_lrecon, stdev_predicted_values_galtan, bimodality_coefficient_lrgen_self, bimodality_coefficient_lrgen_predicted, bimodality_coefficient_lrecon_predicted, bimodality_coefficient_galtan_predicted) |>
  gt() %>%
  cols_label(
    countryname = "Country",
    participants = "N",
    lrgen_selfdescription = "left-right general (self-placement)",
    predicted_values_lrgen = "left-right general (predicted)",
    predicted_values_lrecon = "left-right economy (predicted)",
    predicted_values_galtan = "GAL-TAN (predicted)",
    stdev_lrgen_selfdescription = "left-right general (self-placement)",
    stdev_predicted_values_lrgen = "left-right general (predicted)",
    stdev_predicted_values_lrecon = "left-right economy (predicted)",
    stdev_predicted_values_galtan = "GAL-TAN (predicted)",
    bimodality_coefficient_lrgen_self = "left-right general (self-placement)",
    bimodality_coefficient_lrgen_predicted = "left-right general (predicted)",
    bimodality_coefficient_lrecon_predicted = "left-right economy (predicted)",
    bimodality_coefficient_galtan_predicted = "GAL-TAN (predicted)"
    
  )  %>%
  tab_spanner(
    label = "means",
    columns = c(
      lrgen_selfdescription, predicted_values_lrgen, predicted_values_lrecon, predicted_values_galtan
    )) %>%
  tab_spanner(
    label = "standard deviation",
    columns = c(
      stdev_lrgen_selfdescription, stdev_predicted_values_lrgen, stdev_predicted_values_lrecon, stdev_predicted_values_galtan
    )) %>%
  tab_spanner(
    label = "bimodality coefficient",
    columns = c(
      bimodality_coefficient_lrgen_self, bimodality_coefficient_lrgen_predicted, bimodality_coefficient_lrecon_predicted, bimodality_coefficient_galtan_predicted
    )) 


|> gtsave("tab_38.tex")





country_means |>
  dplyr::select(countryname, participants, bimodality_coefficient_lrgen_self, bimodality_coefficient_lrgen_predicted, bimodality_coefficient_lrecon_predicted, bimodality_coefficient_galtan_predicted) |>
  gt()  |>
  cols_label(
    countryname = "Country",
    participants = "N",
    bimodality_coefficient_lrgen_self = "l-r general",
    bimodality_coefficient_lrgen_predicted = "l-r general",
    bimodality_coefficient_lrecon_predicted = "l-r economy",
    bimodality_coefficient_galtan_predicted = "GAL-TAN"
    
  )  %>%
  tab_spanner(
    label = "self-placement",
    columns = c(
      bimodality_coefficient_lrgen_self
    )) %>%
  tab_spanner(
    label = "prediction",
    columns = c(
      bimodality_coefficient_lrgen_predicted, bimodality_coefficient_lrecon_predicted, bimodality_coefficient_galtan_predicted
    )) |>
  tab_header(
    title = "Bimodality Coefficient"
  )|> gtsave("tab_38.tex")


country_means_rounded <- country_means
library(dplyr)

country_means   %>% mutate_if(is.numeric, round, digits=3) |>
  dplyr::select(countryname, participants, lrgen_selfdescription, stdev_lrgen_selfdescription, bimodality_coefficient_lrgen_self, predicted_values_lrgen, stdev_predicted_values_lrgen, bimodality_coefficient_lrgen_predicted, predicted_values_lrecon, stdev_predicted_values_lrecon, bimodality_coefficient_lrecon_predicted, predicted_values_galtan, stdev_predicted_values_galtan, bimodality_coefficient_galtan_predicted) |>
  gt()  |>
  cols_merge(
    columns = c(lrgen_selfdescription,stdev_lrgen_selfdescription),
    pattern = "{1}<< ({2})>>"
  ) |>
  cols_merge(
    columns = c(predicted_values_lrgen,stdev_predicted_values_lrgen),
    pattern = "{1}<< ({2})>>"
  )|>
  cols_merge(
    columns = c(predicted_values_lrecon,stdev_predicted_values_lrecon),
    pattern = "{1}<< ({2})>>"
  )|>
  cols_merge(
    columns = c(predicted_values_galtan,stdev_predicted_values_galtan),
    pattern = "{1}<< ({2})>>"
  )%>%
  cols_label(
    countryname = "Country",
    participants = "N",
    lrgen_selfdescription = "mean (sd)",
    bimodality_coefficient_lrgen_self = "BC",
    predicted_values_lrgen = "mean (sd)",
    bimodality_coefficient_lrgen_predicted = "BC",
    predicted_values_lrecon = "mean (sd)",
    bimodality_coefficient_lrecon_predicted = "BC",
    predicted_values_galtan = "mean (sd)",
    bimodality_coefficient_galtan_predicted = "BC"
    
  )  %>%

  tab_spanner(
    label = "left-right general",
    columns = c(
      lrgen_selfdescription, stdev_lrgen_selfdescription, bimodality_coefficient_lrgen_self, predicted_values_lrgen, stdev_predicted_values_lrgen, bimodality_coefficient_lrgen_predicted
    )) %>%
  tab_spanner(
    label = "left-right economy",
    columns = c(
      predicted_values_lrecon, stdev_predicted_values_lrecon, bimodality_coefficient_lrecon_predicted
    )) %>%
  tab_spanner(
    label = "GAL-TAN",
    columns = c(
      predicted_values_galtan, stdev_predicted_values_galtan, bimodality_coefficient_galtan_predicted
    )) %>%
tab_spanner(
  label = "self-placement",
  columns = c(
    lrgen_selfdescription, stdev_lrgen_selfdescription, bimodality_coefficient_lrgen_self
  )) %>%
  tab_spanner(
    label = "predictions",
    columns = c(
      predicted_values_lrgen, stdev_predicted_values_lrgen, bimodality_coefficient_lrgen_predicted, predicted_values_lrecon, stdev_predicted_values_lrecon, bimodality_coefficient_lrecon_predicted, predicted_values_galtan, stdev_predicted_values_galtan, bimodality_coefficient_galtan_predicted
    )) %>%
  gt_add_divider(columns = "countryname")%>%
  gt_add_divider(columns = "participants")%>%
  gt_add_divider(columns = "bimodality_coefficient_lrgen_self")%>%
  gt_add_divider(columns = "bimodality_coefficient_lrgen_predicted")%>%
  gt_add_divider(columns = "bimodality_coefficient_lrecon_predicted")|> gtsave("tab_38b.tex")





head(voter_table)
voter_table$absolute_prediction_difference <- abs(voter_table$predicted_values_lrgen-voter_table$lrgen_selfdescription)

test_difference <- lm(data=voter_table, absolute_prediction_difference~east)

summary(test_difference)

Europe$iso_a2
library("dplyr")
europe_full <- left_join(Europe, country_means, by = "iso_a2")

europe_full$prediction_rf_lrgen2_truncated <- ifelse(europe_full$predicted_values_lrgen < 4, 4, ifelse(europe_full$predicted_values_lrgen > 6, 6, europe_full$predicted_values_lrgen))

europe_full$prediction_rf_lrecon2_truncated <- ifelse(europe_full$predicted_values_lrecon < 4, 4, ifelse(europe_full$predicted_values_lrecon > 6, 6, europe_full$predicted_values_lrecon))

europe_full$prediction_rf_galtan2_truncated <- ifelse(europe_full$predicted_values_galtan < 4, 4, ifelse(europe_full$predicted_values_galtan > 6, 6, europe_full$predicted_values_galtan))

europe_full$lrgen_selfdescription_truncated <- ifelse(europe_full$lrgen_selfdescription > 6, 6, europe_full$lrgen_selfdescription)


europe_full$stdev_lrgen_selfdescription_truncated <- ifelse(europe_full$stdev_lrgen_selfdescription > 4, 4, ifelse(europe_full$stdev_lrgen_selfdescription < 2, 2, europe_full$stdev_lrgen_selfdescription))
europe_full$stdev_predicted_values_lrgen_truncated <- ifelse(europe_full$stdev_predicted_values_lrgen > 4, 4, ifelse(europe_full$stdev_predicted_values_lrgen < 2, 2, europe_full$stdev_predicted_values_lrgen))
europe_full$stdev_predicted_values_lrecon_truncated <- ifelse(europe_full$stdev_predicted_values_lrecon > 4, 4, ifelse(europe_full$stdev_predicted_values_lrecon < 2, 2, europe_full$stdev_predicted_values_lrecon))
europe_full$stdev_predicted_values_galtan_truncated <- ifelse(europe_full$stdev_predicted_values_galtan > 4, 4, ifelse(europe_full$stdev_predicted_values_galtan < 2, 2, europe_full$stdev_predicted_values_galtan))

library("sf")
library("ggplot2")

#480x429
###LRGEN

(Malta <- ggplot(data = europe_full) +
    geom_sf(aes(fill=prediction_rf_lrgen2_truncated)) +
    scale_fill_distiller(palette = "RdYlBu", direction = 1, limits = c(4,6))+ 
    coord_sf(xlim = c(14,14.8), ylim = c(35.8,36.2), datum = NA) + labs(title = "Malta")  + theme_bw() + theme(plot.title = element_text(color = "black", size = 10, hjust = 0.5, vjust=-6)) + theme(legend.position = "none") )

#limits 4.8-6.0
ggplot(europe_full) +
  geom_sf(aes(fill=prediction_rf_lrgen2_truncated)) +
  coord_sf(xlim = c(-8,35), ylim = c(35,69))  +
  scale_fill_distiller(palette = "RdYlBu", direction = 1, limits = c(4,6))+ 
  labs (fill = "left-right general \n(RF estimation)") +
  annotation_custom(
    grob = ggplotGrob(Malta),
    xmin = 0,
    xmax = 10,
    ymin = 30,
    ymax = 40
  ) + theme_bw() + 
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
  )+ theme(panel.border = element_blank(), panel.grid.major = element_blank(),
           panel.grid.minor = element_blank())+ 
  theme(legend.title=element_text(size=15), legend.text=element_text(size=15))

ggsave(filename="lrgen predicted means Europe.png", width=1900, height=1900, units="px")


###LRECON

(Malta <- ggplot(data = europe_full) +
    geom_sf(aes(fill=prediction_rf_lrecon2_truncated)) +
    scale_fill_distiller(palette = "RdYlBu", direction = 1, limits = c(4,6))+ 
    coord_sf(xlim = c(14,14.8), ylim = c(35.8,36.2), datum = NA) + labs(title = "Malta")  + theme_bw() + theme(plot.title = element_text(color = "black", size = 10, hjust = 0.5, vjust=-6)) + theme(legend.position = "none") )

#limits: 4-5.5
ggplot(europe_full) +
  geom_sf(aes(fill=prediction_rf_lrecon2_truncated)) +
  coord_sf(xlim = c(-8,35), ylim = c(35,69)) + 
  scale_fill_distiller(palette = "RdYlBu", direction = 1, limits = c(4,6)) + 
  labs (fill = "left-right economy \n(RF estimation)") +
  annotation_custom(
    grob = ggplotGrob(Malta),
    xmin = 0,
    xmax = 10,
    ymin = 30,
    ymax = 40
  ) + theme_bw() + 
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
  )+ theme(panel.border = element_blank(), panel.grid.major = element_blank(),
           panel.grid.minor = element_blank())+ 
  theme(legend.title=element_text(size=15), legend.text=element_text(size=15))

ggsave(filename="lrecon predicted means Europe.png", width=1900, height=1900, units="px")


####GALTAN

(Malta <- ggplot(data = europe_full) +
    geom_sf(aes(fill=prediction_rf_galtan2_truncated)) +
    scale_fill_distiller(palette = "RdYlBu", direction = 1, limits = c(4,6))+ 
    coord_sf(xlim = c(14,14.8), ylim = c(35.8,36.2), datum = NA) + labs(title = "Malta")  + theme_bw() + theme(plot.title = element_text(color = "black", size = 10, hjust = 0.5, vjust=-6)) + theme(legend.position = "none") )

#limits: 4-6
ggplot(europe_full) +
  geom_sf(aes(fill=prediction_rf_galtan2_truncated)) +
  coord_sf(xlim = c(-8,35), ylim = c(35,69))  + 
  scale_fill_distiller(palette = "RdYlBu", direction = 1, limits = c(4,6))+ 
  labs (fill = "GAL-TAN \n(RF estimation)") +
  annotation_custom(
    grob = ggplotGrob(Malta),
    xmin = 0,
    xmax = 10,
    ymin = 30,
    ymax = 40
  ) + theme_bw() + 
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
  )+ theme(panel.border = element_blank(), panel.grid.major = element_blank(),
           panel.grid.minor = element_blank())+ 
  theme(legend.title=element_text(size=15), legend.text=element_text(size=15))

ggsave(filename="gal-tan predicted means Europe.png", width=1900, height=1900, units="px")


###self description

(Malta <- ggplot(data = europe_full) +
    geom_sf(aes(fill=lrgen_selfdescription_truncated)) +
    scale_fill_distiller(palette = "RdYlBu", direction = 1, limits = c(4,6))+ 
    coord_sf(xlim = c(14,14.8), ylim = c(35.8,36.2), datum = NA) + labs(title = "Malta")  + theme_bw() + theme(plot.title = element_text(color = "black", size = 10, hjust = 0.5, vjust=-6)) + theme(legend.position = "none") )

#limits: 4-6
ggplot(europe_full) +
  geom_sf(aes(fill=lrgen_selfdescription_truncated)) +
  coord_sf(xlim = c(-8,35), ylim = c(35,69))  + 
  scale_fill_distiller(palette = "RdYlBu", direction = 1, limits = c(4,6))+ 
  labs (fill = "left-right general \n(self description)") +
  annotation_custom(
    grob = ggplotGrob(Malta),
    xmin = 0,
    xmax = 10,
    ymin = 30,
    ymax = 40
  ) + theme_bw() + 
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
  )+ theme(panel.border = element_blank(), panel.grid.major = element_blank(),
           panel.grid.minor = element_blank())+ 
  theme(legend.title=element_text(size=15), legend.text=element_text(size=15))


ggsave(filename="lrgen self-description means Europe.png", width=1900, height=1900, units="px")





###Fragmentation:

###self description
(Malta <- ggplot(data = europe_full) +
    geom_sf(aes(fill=stdev_lrgen_selfdescription_truncated)) +
    scale_fill_distiller(palette = "Purples", direction = 1, limits = c(2,4))+ 
    coord_sf(xlim = c(14,14.8), ylim = c(35.8,36.2), datum = NA) + labs(title = "Malta")  + theme_bw() + theme(plot.title = element_text(color = "black", size = 10, hjust = 0.5, vjust=-6)) + theme(legend.position = "none") )

ggplot(europe_full) +
  geom_sf(aes(fill=stdev_lrgen_selfdescription_truncated)) +
  coord_sf(xlim = c(-8,35), ylim = c(35,69))  +
  scale_fill_distiller(palette = "Purples", direction = 1, limits = c(2,4))+ 
  labs (fill = "standard deviation\nleft-right general \n(self description)") +
  annotation_custom(
    grob = ggplotGrob(Malta),
    xmin = 0,
    xmax = 10,
    ymin = 30,
    ymax = 40
  ) + theme_bw() + 
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
  )+ theme(panel.border = element_blank(), panel.grid.major = element_blank(),
           panel.grid.minor = element_blank())+ 
  theme(legend.title=element_text(size=15), legend.text=element_text(size=15))

ggsave(filename="stddev lrgen self-description Europe.png", width=1900, height=1900, units="px")

###lrgen predicted

(Malta <- ggplot(data = europe_full) +
    geom_sf(aes(fill=stdev_predicted_values_lrgen_truncated)) +
    scale_fill_distiller(palette = "Purples", direction = 1, limits = c(2,4))+ 
    coord_sf(xlim = c(14,14.8), ylim = c(35.8,36.2), datum = NA) + labs(title = "Malta")  + theme_bw() + theme(plot.title = element_text(color = "black", size = 10, hjust = 0.5, vjust=-6)) + theme(legend.position = "none") )

ggplot(europe_full) +
  geom_sf(aes(fill=stdev_predicted_values_lrgen_truncated)) +
  coord_sf(xlim = c(-8,35), ylim = c(35,69))  +
  scale_fill_distiller(palette = "Purples", direction = 1, limits = c(2,4))+ 
  labs (fill = "standard deviation\nleft-right general \n(RF estimation)") +
  annotation_custom(
    grob = ggplotGrob(Malta),
    xmin = 0,
    xmax = 10,
    ymin = 30,
    ymax = 40
  ) + theme_bw() + 
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
  )+ theme(panel.border = element_blank(), panel.grid.major = element_blank(),
           panel.grid.minor = element_blank())+ 
  theme(legend.title=element_text(size=15), legend.text=element_text(size=15))



ggsave(filename="stddev lrgen predicted Europe.png", width=1900, height=1900, units="px")




###lrecon
(Malta <- ggplot(data = europe_full) +
    geom_sf(aes(fill=stdev_predicted_values_lrecon_truncated)) +
    scale_fill_distiller(palette = "Purples", direction = 1, limits = c(2,4))+ 
    coord_sf(xlim = c(14,14.8), ylim = c(35.8,36.2), datum = NA) + labs(title = "Malta")  + theme_bw() + theme(plot.title = element_text(color = "black", size = 10, hjust = 0.5, vjust=-6)) + theme(legend.position = "none") )

ggplot(europe_full) +
  geom_sf(aes(fill=stdev_predicted_values_lrecon_truncated)) +
  coord_sf(xlim = c(-8,35), ylim = c(35,69))  +
  scale_fill_distiller(palette = "Purples", direction = 1, limits = c(2,4))+ 
  labs (fill = "standard deviation\nleft-right economy \n(RF estimation)") +
  annotation_custom(
    grob = ggplotGrob(Malta),
    xmin = 0,
    xmax = 10,
    ymin = 30,
    ymax = 40
  ) + theme_bw() + 
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
  )+ theme(panel.border = element_blank(), panel.grid.major = element_blank(),
           panel.grid.minor = element_blank()) + 
  theme(legend.title=element_text(size=15), legend.text=element_text(size=15))


ggsave(filename="stddev lrecon predicted Europe.png", width=1900, height=1900, units="px")


###galtan
(Malta <- ggplot(data = europe_full) +
    geom_sf(aes(fill=stdev_predicted_values_galtan_truncated)) +
    scale_fill_distiller(palette = "Purples", direction = 1, limits = c(2,4))+ 
    coord_sf(xlim = c(14,14.8), ylim = c(35.8,36.2), datum = NA) + labs(title = "Malta")  + theme_bw() + theme(plot.title = element_text(color = "black", size = 10, hjust = 0.5, vjust=-6)) + theme(legend.position = "none") )

ggplot(europe_full) +
  geom_sf(aes(fill=stdev_predicted_values_galtan_truncated)) +
  coord_sf(xlim = c(-8,35), ylim = c(35,69))  +
  scale_fill_distiller(palette = "Purples", direction = 1, limits = c(2,4))+ 
  labs (fill = "standard deviation\nGAL-TAN \n(RF estimation)") +
  annotation_custom(
    grob = ggplotGrob(Malta),
    xmin = 0,
    xmax = 10,
    ymin = 30,
    ymax = 40
  ) + theme_bw() + 
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
  )+ theme(panel.border = element_blank(), panel.grid.major = element_blank(),
           panel.grid.minor = element_blank()) + 
  theme(legend.title=element_text(size=15), legend.text=element_text(size=15))



ggsave(filename="stddev galtan predicted Europe.png", width=1900, height=1900, units="px")


###### create heatmap
heatmap_countries <- sqldf("SELECT iso_a2, predicted_values_galtan_reduced, predicted_values_lrecon_reduced, COUNT (*) as count FROM voter_table WHERE predicted_values_lrecon_reduced != 'NA' AND predicted_values_galtan_reduced != 'NA' GROUP BY predicted_values_galtan_reduced, predicted_values_lrecon_reduced, iso_a2")
country_totals <- sqldf("SELECT iso_a2, COUNT (*) as count_total FROM voter_table WHERE predicted_values_lrecon_reduced != 'NA' AND predicted_values_galtan_reduced != 'NA' GROUP BY iso_a2")
heatmap_complete <- sqldf("SELECT t1.*, t2.count_total FROM heatmap_countries t1 INNER JOIN country_totals t2 ON t1.iso_a2 = t2.iso_a2")
heatmap_complete$relative_frequency <- heatmap_complete$count / heatmap_complete$count_total

heatmap_complete$predicted_values_lrecon_reduced <- factor(heatmap_complete$predicted_values_lrecon_reduced, levels = c("l", "c", "r"))

heatmap_complete$predicted_values_galtan_reduced <- factor(heatmap_complete$predicted_values_galtan_reduced, levels = c("l", "c", "r"))

ggplot(data=heatmap_complete) + 
  geom_tile(aes(x=predicted_values_lrecon_reduced, y=predicted_values_galtan_reduced, fill = relative_frequency)) + 
  geom_text(aes(x=predicted_values_lrecon_reduced, y=predicted_values_galtan_reduced, label = round(relative_frequency, 2)), size=2.5, col = "white") + 
  scale_fill_continuous( low = "#56B1F7",
                         high = "#132B43") + 
  facet_wrap(~iso_a2) + 
  labs(x="left-right economy", y = "Green/Alternative/Libertarian - Traditional/Authoritarian/Nationalist (GAL-TAN)", fill = "relative\nfrequency")

ggsave(filename="galtan lrecon Europe.png")
