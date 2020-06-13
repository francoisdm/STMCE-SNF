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
# Constants to set up analysis on linguistic variables
lv_opt <- c("Very bad", "Bad", "More or less bad", "Moderate",
            "More or less good", "Good", "Very good",
            "Very low", "Low", "Moderately low", "Average",
            "Moderately high", "High", "Very high")
lv_order <- list(
  "Very bad" = 1,
  "Bad" = 2,
  "More or less bad" = 3,
  "Moderate" = 4,
  "More or less good" = 5,
  "Good" = 6,
  "Very good" = 7,
  "Very low" = 1,
  "Low" = 2,
  "Moderately low" = 3,
  "Average" = 4,
  "Moderately high" = 5,
  "High" = 6,
  "Very high" = 7
)
# Data frame containing all relevant information for the membership curves
# for each linguistic variable.
lv_params <- data.frame(name = lv_opt,
                        stdDev = rep(c(1/10, 1/7, 1/5, 1/7, 1/5, 1/7, 1/10), 2),
                        med = rep(c(0, 0.2, 0.3, 0.5, 0.7, 0.8, 1), 2)) %>%
  mutate(lower = med - stdDev,
         lower = ifelse(lower < 0, 0, lower),
         upper = med + stdDev,
         upper = ifelse(upper > 1, 1, upper))
lv_params$k <- sapply(1:nrow(lv_params), 
                      function(i) with(lv_params, 
                                       1/integrate(mu, stdDev[i], med[i],
                                                   lower = lower[i], 
                                                   upper = upper[i])$value))

# Parameters
params <- readr::read_csv(file="parameters.txt",
                          col_names=c("filename", "range", "tabname", "n_sample"))
params_sia <- params[nrow(params),]
params <- params[-nrow(params),]
stopifnot(params$n_sample > 0 & params$n_sample <= 10000)
# Number of multi-criteria analyses
N <- nrow(params)
# Cell range
parsed_ranges <- lapply(1:N, parseRange)
CELL_RANGE <- c(LETTERS, paste0("A", LETTERS))

# Alternatives
alternatives <- readxl::read_excel(
  path = params$filename[1],
  range = paste0(parsed_ranges[[1]]$letters[1], '2:', parsed_ranges[[1]]$letters[2], '2'),
  col_names = F
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
START_IDX <- sapply(1:N, function(i) ncol(criteria[[i]]) - 1)
impact_mat <- lapply(1:N, getImpactMatrix)

# Simulations
simulations <- NULL
# Check for existing MC results.
tryCatch(
  {
    filenames <- c(paste0("MCE_technical_MC_results", params$n_sample[1], ".csv"),
                   paste0("MCE_societal_MC_results", params$n_sample[2], ".csv"),
                   paste0("MCE_combined_MC_results", params$n_sample[3], ".csv"))
    simulations <- lapply(filenames, function(i) list(ranking=readr::read_csv(i)))
  },
  error=function(cond) {}
)
# Cannot load existing MC results; generate new results and write to CSV.
if (is.null(simulations)) {
  simulations <- suppressWarnings(lapply(1:N, function(i) simulateMC(params$n_sample[i], 
                                                    impact_mat[[i]],
                                                    criteria[[i]])))
  for (i in 1:length(simulations)) {
    readr::write_csv(
      simulations[[i]]$ranking,
      paste0(params$tabname[i], "_MC_results", params$n_sample[i], ".csv")
    )
  }
}


# ====== Stakeholders ======
stakeholders <- readxl::read_excel(path = 'stakeholders.xlsx',
                                   range = 'A3:D23') %>%
  dplyr::rename(ID = `Stakeholder Abbreviation`)

socialImpact <- readxl::read_excel(path = params_sia$filename,
                                   range = params_sia$range) %>%
  dplyr::rename(Stakeholder = `Socio-economic actor`)


# Evaluation matrix for stakeholders
eval_mat_soc <- NULL
tryCatch(
  {eval_mat_soc <- read_csv("eval-mat-soc.csv", col_names=F)},
  error = function(cond) {}
)
if (!is.null(eval_mat_soc)) {
  eval_mat_soc <- as.matrix(eval_mat_soc)
  colnames(eval_mat_soc) <- as.character(1:nrow(eval_mat_soc))
}
