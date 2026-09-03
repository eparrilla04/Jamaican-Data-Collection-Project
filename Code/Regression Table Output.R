library(modelsummary)

qp_model_ACS_results <- readRDS("TMP/qp_model_ACS.rds")

qp_model_ACS_marital_results <- readRDS("TMP/qp_model_ACS_marital.rds")

qp_model_Canada_results <- readRDS("TMP/qp_model_Canada.rds")

qp_model_Canada_marital_results <- readRDS("TMP/qp_model_Canada_marital.rds")

qp_model_APS_results <- readRDS("TMP/qp_model_APS.rds")

qp_model_APS_marital_results <- readRDS("TMP/qp_model_APS_marital.rds")

model_results <- list(
  "ACS 2015-2019" = qp_model_ACS_results,
  "Canada 2016" = qp_model_Canada_results,
  "APS 2015-2019" =  qp_model_APS_results
)

model_marital_results <- list(
  "ACS 2015-2019 Marital" = qp_model_ACS_marital_results,
  "Canada 2016 Marital" = qp_model_Canada_marital_results,
  "APS 2015-2019 Marital" = qp_model_APS_marital_results
)

#Renaming coefficients to make tables more presentable

coefficient_labels <- c(
  "(Intercept)" = "Intercept",
  
  "SEXFemale" = "Sex: Female",
  
  "AGE" = "Age",
  "I(AGE^2)" = "Age^2",
  
  "factor(AGEGRP)25–34" = "Age: 25–34",
  "factor(AGEGRP)35–44" = "Age: 35–44",
  "factor(AGEGRP)45–54" = "Age: 45–54",
  "factor(AGEGRP)55–64" = "Age: 55–64",
  
  "EDUCATION_LEVELSecondary" =
    "Education: Secondary",
  
  "EDUCATION_LEVELPostsecondary below degree" =
    "Education: Postsecondary below degree",
  
  "EDUCATION_LEVELDegree/equivalent" =
    "Education: Degree/equivalent",
  
  # ACS hyphenated labels
  "DURATION_LEVEL6-10 years" =
    "Duration in destination: 6–10 years",
  
  "DURATION_LEVEL11-20 years" =
    "Duration in destination: 11–20 years",
  
  "DURATION_LEVEL21+ years" =
    "Duration in destination: 21+ years",
  
  # APS labels using an en dash
  "DURATION_LEVEL6–10 years" =
    "Duration in destination: 6–10 years",
  
  "DURATION_LEVEL11–20 years" =
    "Duration in destination: 11–20 years",
  
  # Canadian labels
  "DURATION_LEVEL6–10" =
    "Duration in destination: 6–10 years",
  
  "DURATION_LEVEL11–20" =
    "Duration in destination: 11–20 years",
  
  "DURATION_LEVEL21+" =
    "Duration in destination: 21+ years",
  
  "MARITAL_STATUSMarried" =
    "Marital status: Married/partnered",
  
  "MARITAL_STATUSSeparated/Divorced" =
    "Marital status: Separated/divorced",
  
  "MARITAL_STATUSWidowed" =
    "Marital status: Widowed",
  
  "factor(MULTYEAR)2016" = "Survey year: 2016",
  "factor(MULTYEAR)2017" = "Survey year: 2017",
  "factor(MULTYEAR)2018" = "Survey year: 2018",
  "factor(MULTYEAR)2019" = "Survey year: 2019",
  
  "factor(YEAR)2016" = "Survey year: 2016",
  "factor(YEAR)2017" = "Survey year: 2017",
  "factor(YEAR)2018" = "Survey year: 2018",
  "factor(YEAR)2019" = "Survey year: 2019"
)

#Creating final regression output tables

modelsummary::modelsummary(
  model_results,
  coef_map = coefficient_labels,
  estimate = "{estimate}{stars}",
  statistic = "({std.error})",
  stars = TRUE,
  fmt = 4,
  output = "Output/final_regression_output.md"
)

modelsummary::modelsummary(
  model_marital_results,
  coef_map = coefficient_labels,
  estimate = "{estimate}{stars}",
  statistic = "({std.error})",
  stars = TRUE,
  fmt = 4,
  output = "Output/final_regression_marital_output.md"
)

github_markdown <- function(path) {
  
  temporary_file <- tempfile(fileext = ".md")
  
  rmarkdown::pandoc_convert(
    input = normalizePath(path),
    from = "markdown",
    to = "gfm",
    output = temporary_file
  )
  
  file.copy(
    temporary_file,
    path,
    overwrite = TRUE
  )
  
  unlink(temporary_file)
}

github_markdown(
  "Output/final_regression_output.md"
)

github_markdown(
  "Output/final_regression_marital_output.md"
)