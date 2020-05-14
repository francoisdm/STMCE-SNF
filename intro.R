# intro.R
# This file loads all the necessary files and tables to run the app.
# This code must be run before starting the app.

# Load all the necessary functions
source("helpers.R")

# ====== Packages ======
required_packages <- c("dplyr",
                       "DT",
                       "data.table",
                       "ggplot2",
                       "readr",
                       "shiny",
                       "shinyjs",
                       "tidyr",
                       "readxl")

uninstalled_packages <- required_packages[!(required_packages %in% installed.packages()[,'Package'])]
if (length(uninstalled_packages) > 0) {
  install.packages(uninstalled_packages, dependencies = TRUE, repos = 'http://cran.us.r-project.org')
}

lapply(required_packages, require, character.only = TRUE)

rm(required_packages)
rm(uninstalled_packages)


# ====== Multi-criteria analysis ======
# Parameters
parameters <- readr::read_csv(file="parameters.txt",
                              col_names=c("filename", "n_sample", "tabname"))
parameters$range <- sapply(parameters$filename,
                           function(filename) read_excel(filename, range="D1:D1", col_names=F)[[1]])
N <- nrow(parameters)

# Cell range
parsedRange <- lapply(1:N, parseRange)

# Alternatives
alternatives <- readxl::read_excel(
  path = parameters$filename[1],
  range = paste0(parsedRange[[1]]$letters[1], '2:', parsedRange[[1]]$letters[2], '2'),
  col_names = FALSE
)
alternatives <- as.character(alternatives[1,])
alternatives <- data.frame(Option = alternatives[alternatives != "NA"])
alternatives <- alternatives %>%
  dplyr::mutate(OptionID = 1:n(),
                Option = as.character(Option)) %>%
  dplyr::select(OptionID, Option)

# Criteria
criteria <- lapply(1:N, getCriteria)

# Impact matrix
# Used to identify the cell columns to read
START_IDX <- sapply(1:N, function(i) ncol(criteria[[i]]) - 4)
CELL_RANGE <- c(LETTERS, paste0("A", LETTERS))
impactMatrix <- lapply(1:N, getImpactMatrix)

# Simulations
simulations <- NULL
# Check for existing MC results.
tryCatch(
  {
    filenames <- c(paste0("Technical_view_MC_results", parameters$n_sample[1], ".csv"),
                   paste0("Societal_view_MC_results", parameters$n_sample[2], ".csv"),
                   paste0("Combined_MC_results", parameters$n_sample[3], ".csv"))
    simulations <- lapply(filenames, function(i) list(ranking=readr::read_csv(i)))
  },
  error=function(cond) {}
)
# Cannot load existing MC results; generate new results and write to CSV.
if (is.null(simulations)) {
  simulations <- lapply(1:N, function(i) simulateMC(parameters$n_sample[i], 
                                                    impactMatrix[[i]],
                                                    criteria[[i]]))
  for (i in 1:length(simulations)) {
    readr::write_csv(
      simulations[[i]]$ranking,
      paste0(parameters$tabname[i], "_MC_results", parameters$n_sample[i], ".csv")
    )
  }
}


# ====== Stakeholders ======
stakeholders <- readxl::read_excel(path = 'stakeholders-v2.xlsx',
                                   range = 'A3:D23') %>%
  dplyr::rename(ID = `Stakeholder Abbreviation`)

equityImpact <- readxl::read_excel(path = 'equity-impact-v2.xlsx',
                                 range = 'A3:J23') %>%
  dplyr::rename(Stakeholder = `Socio-economic actor`)

# Social impact matrix
socialImpact <- NULL
tryCatch(
  {socialImpact <- read_csv("social-impact.csv", col_names=F)},
  error = function(cond) {}
)
if (!is.null(socialImpact)) {
  socialImpact <- as.matrix(socialImpact)
  colnames(socialImpact) <- as.character(1:nrow(socialImpact))
}

# Constants to set up analysis on linguistic variables
rankOptions <- c("Very bad", "Bad", "More or less bad", "Moderate",
                 "More or less good", "Good", "Very good")
linguisticOrder <- list(
  "Very bad" = 1,
  "Bad" = 2,
  "More or less bad" = 3,
  "Moderate" = 4,
  "More or less good" = 5,
  "Good" = 6,
  "Very good" = 7
)
# Data frame containing all relevant information for the membership curves
# for each linguistic variable.
lvParams <- data.frame(name = rankOptions,
                       stdDev = c(1/10, 1/7, 1/5, 1/7, 1/5, 1/7, 1/10),
                       med = c(0, 0.2, 0.3, 0.5, 0.7, 0.8, 1)) %>%
  mutate(lower = med - stdDev,
         lower = ifelse(lower < 0, 0, lower),
         upper = med + stdDev,
         upper = ifelse(upper > 1, 1, upper))
lvParams$k <- sapply(1:nrow(lvParams), 
                     function(i) with(lvParams, 
                                      1/integrate(mu, stdDev[i], med[i],
                                                  lower = lower[i], 
                                                  upper = upper[i])$value))