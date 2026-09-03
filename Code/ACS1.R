library(tidyverse)
library(ggplot2)

#From the IPUMS data extract, the dataset only includes those of Jamaica origin in the ages 18-6

ACS_Data <- read_csv('Raw Data/5_year_ACS_Data.csv')

#Filtering for those with positive income
ACS_Data_filtered <- ACS_Data %>%
  filter(INCWAGE > 0, 
         INCWAGE < 999998) 

#Creating a function that creates a harmonized education variable and apply it to the ACS data

harmonized_education_ACS <- function(data) {
  data |>
    dplyr::mutate(
      EDUCD_numeric = as.numeric(EDUCD),
      EDUCATION_LEVEL = dplyr::case_when(
        dplyr::between(EDUCD_numeric, 2, 61)   ~
          "Low/no qualifications",
        dplyr::between(EDUCD_numeric, 62, 64)  ~
          "Secondary",
        dplyr::between(EDUCD_numeric, 65, 100) ~
          "Postsecondary below degree",
        dplyr::between(EDUCD_numeric, 101, 116) ~
          "Degree/equivalent",
        EDUCD_numeric %in% c(0, 1, 999) ~ NA_character_,
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

ACS_Data_filtered <- harmonized_education_ACS(ACS_Data_filtered)

#Creating harmonized duration of stay variables

ACS_Data_filtered <- ACS_Data_filtered %>%
  dplyr::mutate(
    years_destination = as.numeric(YRSUSA1),
    DURATION_LEVEL = dplyr::case_when(
      years_destination >= 0 & years_destination <= 5  ~ "0-5 years",
      years_destination >= 6 & years_destination <= 10 ~ "6-10 years",
      years_destination >= 11 & years_destination <= 20 ~ "11-20 years",
      years_destination >= 21 ~ "21+ years",
      TRUE ~ NA_character_
    ),
    DURATION_LEVEL = factor(
      DURATION_LEVEL,
      levels = c(
        "0-5 years",
        "6-10 years",
        "11-20 years",
        "21+ years"
      )
    ))

#Preparing Sex indicator variable

ACS_Data_filtered <- ACS_Data_filtered %>%
  dplyr::mutate(
    SEX = factor(
      as.numeric(SEX),
      levels = c(1, 2),
      labels = c("Male", "Female")
    )
  )

#Quasi-poisson Model
qp_model_ACS <- glm(INCWAGE ~ factor(MULTYEAR) + SEX + AGE + I(AGE^2) + EDUCATION_LEVEL + DURATION_LEVEL
                    , family = quasipoisson(link = 'log'), weights = PERWT, data = ACS_Data_filtered)

summary(qp_model_ACS)

#Creating harmonized marital status variables

ACS_Data_filtered <- ACS_Data_filtered %>%
  dplyr::mutate(
    MARITAL_STATUS = dplyr::case_when(
      MARST == 6 ~
        "Never married",
      MARST %in% c(1, 2) ~
        "Married",
      MARST %in% c(3, 4) ~
        "Separated/Divorced",
      MARST == 5 ~
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

qp_model_ACS_marital <- glm(INCWAGE ~ factor(MULTYEAR) + SEX + AGE + I(AGE^2) + 
                              EDUCATION_LEVEL + DURATION_LEVEL + MARITAL_STATUS,
                            family = quasipoisson(link = 'log'), weights = PERWT, data = ACS_Data_filtered)

summary(qp_model_ACS_marital)

#Saving regression outputs into the Output folder

saveRDS(qp_model_ACS, "TMP/qp_model_ACS.rds")

saveRDS(qp_model_ACS_marital, "TMP/qp_model_ACS_marital.rds")





