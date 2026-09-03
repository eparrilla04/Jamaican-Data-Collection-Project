library(tidyverse)
library(dplyr)

APS_2015 <- read_csv('Raw Data/UK Data/APS Jan-Dec 2015.csv')

APS_2016 <- read_csv('Raw Data/UK Data/APS Jan-Dec 2016.csv')

APS_2017 <- read_csv('Raw Data/UK Data/APS Jan-Dec 2017.csv')

APS_2018 <- read_csv('Raw Data/UK Data/APS Jan-Dec 2018.csv')

APS_2019 <- read_csv('Raw Data/UK Data/APS Jan-Dec 2019.csv')

#Filtering data for migrants of Black Caribbean ethnicity between ages 18-65 with positive incomes

APS_2015_filtered <- APS_2015 %>%
  filter(CAMEYR != -9.0 & CAMEYR != -8.0) %>%
  filter(ETHBL11 == 2.0) %>%
  filter(GRSSWK != -9 & GRSSWK != -8) %>%
  filter(AGE >= 18 & AGE <= 65) 

APS_2016_filtered <- APS_2016 %>%
  filter(CAMEYR != -9.0 & CAMEYR != -8.0) %>%
  filter(ETHBL11 == 2.0) %>%
  filter(GRSSWK != -9 & GRSSWK != -8) %>%
  filter(AGE >= 18 & AGE <= 65)

APS_2017_filtered <- APS_2017 %>%
  filter(CAMEYR != -9.0 & CAMEYR != -8.0) %>%
  filter(ETHBL11 == 2.0) %>%
  filter(GRSSWK != -9 & GRSSWK != -8) %>%
  filter(AGE >= 18 & AGE <= 65)

APS_2018_filtered <- APS_2018 %>%
  filter(CAMEYR != -9.0 & CAMEYR != -8.0) %>%
  filter(ETHBL11 == 2.0) %>%
  filter(GRSSWK != -9 & GRSSWK != -8) %>%
  filter(AGE >= 18 & AGE <= 65)

APS_2019_filtered <- APS_2019 %>%
  filter(CAMEYR != -9.0 & CAMEYR != -8.0) %>%
  filter(ETHBL11 == 2.0) %>%
  filter(GRSSWK != -9 & GRSSWK != -8) %>%
  filter(AGE >= 18 & AGE <= 65)

#Creating a function to create harmonized education variable that can be applied to APS 2015-2019

harmonized_education_APS <- function(data) {
  data |>
    dplyr::mutate(
      HIQUL15D_numeric = as.numeric(HIQUL15D),
      EDUCATION_LEVEL = dplyr::case_when(
        HIQUL15D_numeric == 6 ~
          "Low/no qualifications",
        HIQUL15D_numeric %in% c(3, 4) ~
          "Secondary",
        HIQUL15D_numeric == 2 ~
          "Postsecondary below degree",
        HIQUL15D_numeric == 1 ~
          "Degree/equivalent",
        HIQUL15D_numeric %in% c(5, 7, -8, -9) ~ NA_character_,
        TRUE ~ NA_character_
      ),
      EDUCATION_LEVEL = factor(
        EDUCATION_LEVEL,
        levels = c(
          "Low/no qualifications",
          "Secondary",
          "Postsecondary below degree",
          "Degree/equivalent"
        )
      ))}

#Merging dataframes from 2015 through 2019 and applying the education across them
APS_2015_2019 <- bind_rows(
  `2015` = APS_2015_filtered,
  `2016` = APS_2016_filtered,
  `2017` = APS_2017_filtered,
  `2018` = APS_2018_filtered,
  `2019` = APS_2019_filtered,
  .id = "YEAR"
) %>%
  mutate(YEAR = as.integer(YEAR)) %>%
  harmonized_education_APS()

#Creating harmonized duration of stay variables

APS_2015_2019 <- APS_2015_2019 |>
  dplyr::mutate(
    DURATION_LEVEL = dplyr::if_else(
      CAMEYR > 0,
      YEAR - as.numeric(CAMEYR),
      NA_real_
    ),
    DURATION_LEVEL = cut(
      DURATION_LEVEL,
      breaks = c(-Inf, 5, 10, 20, Inf),
      labels = c("0–5 years", "6–10 years", "11–20 years", "21+ years"),
      right = TRUE
    )
  )

#Preparing Sex indicator variable

APS_2015_2019 <- APS_2015_2019 %>%
  dplyr::mutate(
    SEX = factor(
      as.numeric(SEX),
      levels = c(1, 2),
      labels = c("Male", "Female")
    )
  )

qp_model_APS <- glm(GRSSWK ~ SEX + AGE + I(AGE^2) + EDUCATION_LEVEL + DURATION_LEVEL + factor(YEAR), 
                     family = quasipoisson(link = 'log'), weight = PIWTA18, data = APS_2015_2019)

summary(qp_model_APS)

#Creating harmonized marital status variables

APS_2015_2019 <- APS_2015_2019 %>%
  dplyr::mutate(
    MARITAL_STATUS = dplyr::case_when(
      MARSTA == 1 ~
        "Never married",
      MARSTA == 2 ~
        "Married",
      MARSTA %in% c(3, 4, 7, 8) ~
        "Separated/Divorced",
      MARSTA %in% c(5, 9) ~
        "Widowed"),
      MARITAL_STATUS = factor(
      MARITAL_STATUS,
      levels = c(
        "Never married",
        "Married",
        "Separated/Divorced",
        "Widowed"
      )
    ))

#Model including marital status 

qp_model_APS_marital <- glm(GRSSWK ~ + factor(YEAR) + SEX + AGE + I(AGE^2) + EDUCATION_LEVEL + 
                                  DURATION_LEVEL + MARITAL_STATUS, family = quasipoisson(link = 'log'), 
                                  weight = PIWTA18, data = APS_2015_2019)

summary(qp_model_APS_marital)

#Saving regression outputs into the Output folder

saveRDS(qp_model_APS, "TMP/qp_model_APS.rds")

saveRDS(qp_model_APS_marital, "TMP/qp_model_APS_marital.rds")



