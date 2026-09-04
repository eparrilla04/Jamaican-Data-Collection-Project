library(tidyverse)

Can_data <- read_csv('Raw Data/2016 Canadian Survey.csv')

#Filtering data for individuals of Black Caribbean ethnicity between ages 18-65 with positive incomes

Canada_data_filtered <- Can_data %>%
  filter(POB == 4) %>%
  filter(EMPIN > 0 & EMPIN < 88888888) 

#Creating harmonized education variable and apply it data

harmonized_education_Canada <- function(data) {
  data |>
    dplyr::mutate(
      HDGREE_numeric = as.numeric(HDGREE),
      EDUCATION_LEVEL = dplyr::case_when(
        HDGREE_numeric == 1 ~
          "Low/no qualifications",
        HDGREE_numeric == 2 ~
          "Secondary",
        HDGREE_numeric %in% 3:8 ~
          "Postsecondary below degree",
        HDGREE_numeric %in% 9:13 ~
          "Degree/equivalent",
        HDGREE_numeric %in% c(88, 99) ~ NA_character_,
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

Canada_data <- harmonized_education_Canada(Canada_data_filtered)

#Creating harmonized duration of stay variables

Canada_data <- Canada_data  |>
  dplyr::mutate(
    DURATION_LEVEL = dplyr::case_when(
      YRIMM >= 2011 & YRIMM <= 2015 ~ "0–5 years",
      YRIMM >= 2006 & YRIMM <= 2010 ~ "6–10 years",
      YRIMM >= 1996 & YRIMM <= 2005 ~ "11–20 years",
      YRIMM %in% 1:8                ~ "21+ years",
      YRIMM >= 1990 & YRIMM <= 1995 ~ "21+",
      TRUE                          ~ NA_character_
    ),
    DURATION_LEVEL = factor(
      DURATION_LEVEL,
      levels = c("0–5 years", "6–10 years", "11–20 years", "21+ years")
    )
  )

#Creating age groups with a baseline group of 18-24

Canada_data <- Canada_data  %>%
  dplyr::mutate(
    AGEGRP = as.integer(AGEGRP),
    AGEGRP = dplyr::case_when(
      AGEGRP %in% c(7, 8)   ~ "18–24",
      AGEGRP %in% c(9, 10)  ~ "25–34",
      AGEGRP %in% c(11, 12) ~ "35–44",
      AGEGRP %in% c(13, 14) ~ "45–54",
      AGEGRP %in% c(15, 16) ~ "55–64",
      TRUE                  ~ NA_character_
    ),
    AGEGRP = factor(
      AGEGRP,
      levels = c("18–24", "25–34", "35–44", "45–54", "55–64")
    )
  ) %>%
  dplyr::filter(!is.na(AGEGRP))

#Preparing Sex indicator variable

Canada_data  <- Canada_data %>%
  dplyr::mutate(
    SEX = factor(
      as.numeric(SEX),
      levels = c(2, 1),
      labels = c("Male", "Female")
    )
  )

#Quasi-poisson regression model

qp_model_Canada <- glm(EMPIN ~ SEX + factor(AGEGRP) + EDUCATION_LEVEL + DURATION_LEVEL, 
                       family = quasipoisson(link = "log"), weights = WEIGHT, data = Canada_data)

summary(qp_model_Canada)

#Creating harmonized marital status variables

Canada_data  <- Canada_data  %>%
  dplyr::mutate(
    MARITAL_STATUS = dplyr::case_when(
      MARSTH == 1 ~
        "Never married",
      MARSTH %in% c(2, 3) ~
        "Married",
      MARSTH %in% c(4, 5) ~
        "Separated/Divorced",
      MARSTH == 6 ~
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

qp_model_Canada_marital <- glm(EMPIN ~ SEX + factor(AGEGRP) + EDUCATION_LEVEL + 
                                 MARITAL_STATUS + DURATION_LEVEL, 
                       family = quasipoisson(link = "log"), weights = WEIGHT, data = Canada_data)

summary(qp_model_Canada_marital)

#Saving regression outputs into the Output folder

saveRDS(qp_model_Canada, "TMP/qp_model_Canada.rds")

saveRDS(qp_model_Canada_marital, "TMP/qp_model_Canada_marital.rds")



