# TO-DO:
# 1. account for tertiary variables
# 2. ranking with no weights considered (all)
# 3. ranking with weights considered (all)
# 4. ranking by dendrogram groups (filtered by stake)

# ====== Packages ======
required_packages <- c("crosstalk",
                       "dplyr",
                       "DT",
                       "data.table",
                       "ggplot2",
                       "plotly",
                       "purrr",
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

# ====== Read-in Data ======

# Read-in parameters
parameters <- readr::read_csv("parameters.txt",
                              col_names=c("filename", "n_sample", "tabname"))
parameters$range <- sapply(parameters$filename,
                           function(filename) read_excel(filename, range="D1:D1", col_names=F)[[1]])

N <- nrow(parameters)
CELL_RANGE <- c(LETTERS, paste0("A", LETTERS))

# Parse given sheet ranges
parse_range <- function(i) {
  # Extracts a sheet range's letters and numbers.
  # e.g "A3:AG14" -> list(c("A", "AG"), c(3, 14))
  # Arguments:
  # i (numeric): row number of parameter data frame
  
  letterRange <- gsub("[^a-zA-Z]", " ", parameters$range[i]) %>%
    strsplit(split=" ") %>%
    unlist
  numRange <- gsub("[^0-9]", " ", parameters$range[i]) %>%
    strsplit(split=" ") %>%
    unlist
  
  list(letters=letterRange[nchar(letterRange) > 0],
       numbers=numRange[numRange > 0])
}
parsedRange <- lapply(1:N, parse_range)

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
getCriteria <- function(i) {
  # Gets the criteria for a given sheet.
  # Arguments:
  # i (numeric): row number of parameters data frame.
  
  crit <- readxl::read_excel(parameters$filename[i], 
                             range = parameters$range[i]) %>%
    dplyr::select(Category:Direction, 
                  Threshold=`Indifference threshold`, 
                  Correlation,
                  Binary) %>%
    tidyr::fill(Category) %>%
    dplyr::mutate(weight = 1/n()) %>%
    dplyr::rename(CriterionID=ID)
  crit <- crit %>%
    dplyr::mutate(CategoryID = dplyr::group_indices(crit, Category)) %>%
    dplyr::select(CategoryID, everything())
  
  # Check for odd representations of cells when reading into R
  crit$Correlation <- sapply(
    crit$Correlation,
    function(j) ifelse(is.na(as.numeric(j)), j, round(as.numeric(j), 3))
  )
  crit
}
criteria <- lapply(1:N, getCriteria)

# Used for identifying cell columns to read
START_IDX <- sapply(1:N, function(i) ncol(criteria[[i]]) - 4)

# Impact matrix
getImpactMatrix <- function(i) {
  # Gets the impact matrix for a given sheet.
  # Arguments:
  # i (numeric): row number of parameters data frame.
  
  curLetter <- START_IDX[i]
  curRange <- parsedRange[[i]]
  
  impact_mat <- list()
  for (a in 1:nrow(alternatives)) {
    curCells <- CELL_RANGE[c(curLetter, curLetter+2)]
    range <- paste0(curCells, curRange$numbers) %>%
      paste(collapse=":")
    
    impact_mat[[a]] <- data.frame(OptionID = alternatives$OptionID[a]) %>%
      cbind(criteria[[i]]) %>%
      cbind(readxl::read_excel(path = parameters$filename[i],
                               range = range))
    curLetter <- curLetter + 3
  }
  # # In the absence of data, standard deviation is estimated using the equation from:
  # # https://www.rocscience.com/help/rocfall/rocfall/Normal_Distribution.htm  
  do.call("rbind", impact_mat) %>%
    dplyr::mutate(stdDev = max(0, (max - min)/6)) %>%
    dplyr::select(-CategoryID, -Category)
}
impactMatrix <- lapply(1:N, getImpactMatrix)

stakeholders <- readxl::read_excel(path = 'stakeholders-v2.xlsx',
                                   range = 'A3:D23') %>%
  dplyr::rename(ID = `Stakeholder Abbreviation`)

actorTable <- readxl::read_excel(path = 'equity-impact-v2.xlsx',
                                 range = 'A3:J23') %>%
  dplyr::rename(Stakeholder = `Socio-economic actor`)


# ====== Technical ======
createOutrankingMatrix <- function(impact_mat) {
  # Computes the outranking matrix E given the current impact matrix. E is an N x N 
  # matrix (where N is the number of alternatives) representing the degree of preference
  # for each pairwise comparison between two alternatives. For example, the element of 
  # E at index (j, k) represents how much j is preferred to k (ranging from [0, 1]).
  # 
  # Paramaters:
  # - impact_mat (data.frame): impact matrix being considered.
  # Returns:
  # - E: N x N outranking matrix
  
  computeOutrankingEntry <- function(comps, j, k) {
    # Helper function to compute the entry of the outranking matrix E at index (j, k)
    # by aggregating pairwise comparisons of criterion scores.
    #
    # Parameters:
    # - comps: data frame containing all pairwise comparisons for the given criterion measures.
    # - j: ID of alternative j
    # - k: ID of alternative k
    # Returns:
    # A number representing the entry of E at index (j, k).
    
    if (j == k) return(0)
    
    comps %>%
      dplyr::filter(OptionID == j,
                    preference != "N") %>%
      dplyr::mutate(weight = ifelse(preference == "I", weight / 2, weight)) %>%
      dplyr::summarise(tot = sum(weight)) %>%
      .$tot
  }
  
  n_alternatives <- nrow(alternatives)
  E <- data.frame(matrix(nrow = n_alternatives, 
                         ncol = n_alternatives,
                         dimnames = list(alternatives$Option, alternatives$Option)))
  for (alt in rownames(E)) {
    alt_id <- alternatives %>%
      dplyr::filter(Option == alt) %>%
      .$OptionID
    
    comparisons <- impact_mat %>%
      dplyr::mutate(value_diff = Value - impact_mat %>%
                      dplyr::rename(temp_id = CriterionID) %>%
                      dplyr::filter(OptionID == alt_id, temp_id == CriterionID) %>%
                      .$Value,
                    preference = ifelse(Direction == "minimize",
                                        ifelse(abs(value_diff) > Threshold & value_diff < 0, "P",
                                               ifelse(abs(value_diff) <= Threshold, "I", "N")),
                                        ifelse(abs(value_diff) > Threshold & value_diff > 0, "P",
                                               ifelse(abs(value_diff) <= Threshold, "I", "N"))))
    
    for (j in alternatives$OptionID) E[j, alt_id] <- computeOutrankingEntry(comparisons, j, alt_id)
  }
  
  E
}

smceRankings <- function(impact_mat) {
  # Generates a ranking of options according to a technical impact matrix.
  # This is done by generating an outranking matrix E that contains the degree
  # of preference across all pairwise comparisons.
  #
  # Parameters:
  # - impact_mat (data.frame): technical impact matrix.
  # Returns:
  # A list with entries from 1 to n, where n is the number of options, and whose
  # names are the alternatives. The number associated with a name is the ranking
  # that that alternative is given with the impact matrix.
  
  alternativeRank <- createOutrankingMatrix(impact_mat) %>%
    rowSums() %>%
    sort(decreasing = T) %>%
    names()

  data.frame(Option = alternativeRank, ranking = 1:length(alternativeRank),
             stringsAsFactors = FALSE) %>%
    dplyr::left_join(alternatives, by = "Option") %>%
    dplyr::arrange(OptionID) %>%
    .$ranking
}

adjustValues <- function(values, impact_mat) {
  # Adjusts the sampled values based on the criterion measures a variable is
  # dependent on. The adjustment is made by taking the net distance from the mean
  # the sampled value of a correlated measure is (from a scale of [min, max]).
  #
  # Parameters:
  # - values (vector): vector of sampled values for each criterion
  # - impact_mat (data.frame): technical impact matrix
  # Returns:
  # Vector with appropriate adjustments to initial sampled values
  
  getDist <- function(val, mean, min, max) {
    # Helper function to determine how much a sampled value deviates from 
    # its given mean value.
    #
    # Parameters:
    # - val (numeric): the sampled value
    # - mean, min, max (numeric): the associated mean, min, and max of the criterion
    # Returns:
    # A numeric value representing total deviation from the mean.
    
    ifelse(val - mean < 0,
           -abs(val - mean) / abs(mean - min),
           abs(val - mean) / abs(mean - max))
  }
  
  for (i in 1:length(values)) {
    if (!is.na(impact_mat$Correlation[i])) {
      corrIds <- strsplit(as.character(impact_mat$Correlation[i]), split = '; ')[[1]]
      
      # Check for possible inverse correlations
      inverseCorr <- sapply(corrIds, function(i) substring(as.character(i), 1, 1) == "-")
      corrIds <- ifelse(inverseCorr, substring(corrIds, 2), corrIds)
      corrDF <- data.frame(
        CriterionID = corrIds,
        inverse = ifelse(inverseCorr, -1, 1)
      )
      correlations <- suppressWarnings(cbind(impact_mat, values) %>%
        dplyr::filter(CriterionID %in% corrIds & min != max) %>%
        dplyr::inner_join(corrDF, by="CriterionID"))
      
      netChange <- correlations %>%
        dplyr::mutate(change = getDist(values, mean, min, max)) %>%
        .$change
      netChange <- sum(netChange * correlations$inverse)
      values[i] <- ifelse(netChange < 0,
                          values[i] - netChange * abs(impact_mat$mean[i] - impact_mat$min[i]),
                          values[i] + netChange * abs(impact_mat$mean[i] - impact_mat$max[i]))
    }
  }
  
  values
}

simulateMC <- function(n, impact_mat, crit, mean = F, corr = T) {
  # Runs a Monte Carlo simulation given an impact matrix. This is done
  # by modeling each criterion as an unknown quantity drawn from a
  # distribution and repeatedly sampling from those distributions.
  # 
  # Parameters:
  # - n (numeric): number of simulations to run
  # - impact_mat (data.frame): impact matrix
  # - mean (boolean): decide whether to sample only the mean value (if T, n=1)
  # Returns:
  # a list of two data frames, one containing the historic sampled values and
  # one containing the historic rankings.
  
  set.seed(2020)
  
  criterionID <- crit %>%
    .$CriterionID
  colnamesHV <- c()
  for (option in alternatives$Option) {
    colnamesHV <- c(colnamesHV, paste0(option, criterionID))
  }
  historicRankings <- matrix(nrow=n, ncol=nrow(alternatives),
                             dimnames=list(NULL, alternatives$Option)) %>%
    data.frame()
  historicValues <- matrix(nrow=n, ncol = nrow(impact_mat),
                           dimnames=list(NULL, colnamesHV))
  
  for (i in 1:n) {
    if (mean) {
      Value <- impact_mat$mean
    } else {
      Value <- sapply(1:nrow(impact_mat), 
                       function(i) rnorm(1, mean = impact_mat$mean[i], sd = impact_mat$stdDev[i]))
      Value <- ifelse(impact_mat$Binary, 
                      ifelse(abs(Value - impact_mat$min) < abs(Value - impact_mat$max),
                             impact_mat$min,
                             impact_mat$max),
                      Value)
      if (corr) {
        Value <- adjustValues(Value, impact_mat)
      }
    }
    historicValues[i,] <- Value
    historicRankings[i,] <- smceRankings(cbind(impact_mat, Value))
  }
  
  colnames(historicRankings) <- sapply(colnames(historicRankings), 
                                       function(i) strsplit(i, split="\\.|\\.\\.\\.") %>% 
                                         unlist %>% 
                                         paste(collapse=" "))
  
  list(values = historicValues,
       ranking = historicRankings)
}

simulations <- NA
tryCatch(
  {
    filenames <- c(paste0("Technical_view_MC_results", parameters$n_sample[1], ".csv"),
                   paste0("Societal_view_MC_results", parameters$n_sample[2], ".csv"),
                   paste0("Combined_MC_results", parameters$n_sample[3], ".csv"))

    simulations <- lapply(filenames, function(i) list(ranking=readr::read_csv(i)))
  },
  error=function(cond) {
    
  }
)

if (is.na(simulations)) {
  simulations <- lapply(1:N, function(i) simulateMC(parameters$n_sample[i], 
                                                    impactMatrix[[i]],
                                                    criteria[[i]]))
  # Store simulation results in CSV
  for (i in 1:length(simulations)) {
    readr::write_csv(
      simulations[[i]]$ranking,
      paste0(parameters$tabname[i], "_MC_results", parameters$n_sample[i], ".csv")
    )
  }
}

# simulations <- lapply(1:N, function(i) simulateMC(10, # Number of samples (before: paramaters$n_sample[i])
#                                                   impactMatrix[[i]],
#                                                   criteria[[i]]))



# ====== Social ======
rankOptions <- c("Very bad", "Bad", "More or less bad", "Moderate",
                 "More or less good", "Good", "Very good")
# nActors <- nrow(actorTable)
# actorPrefs <- actorTable %>%
#   dplyr::select(-ID, -Stakeholder) %>%
#   split(seq(nActors)) %>%
#   setNames(stakeholders$ID)
# 
# impactSocial <- matrix(nrow = nActors, ncol = nActors)
linguisticOrder <- list(
  "Very bad" = 1,
  "Bad" = 2,
  "More or less bad" = 3,
  "Moderate" = 4,
  "More or less good" = 5,
  "Good" = 6,
  "Very good" = 7
)

mu <- function(x, stdDev, med) {
  # Computes the membership function for a linguistic varible.
  
  -1/stdDev^2 * (x - med)^2 + 1
}

sampleX <- function(df) {
  # Helper function in obtaining the semantic distance between
  # two linguistic variables. See section 6.3 of Munda (1995).
  
  while(T) {
    r <- runif(1)
    x <- r*df$lower + (1 - r)*df$upper
    z <- runif(1, max = df$k)
    if (z > df$k * mu(x, df$stdDev, df$med)) {
      return(x)
    }
  }
}

semanticDist <- function(a, b) {
  # Semantic distance between two linguistic variables.
  
  aDF <- lvParams %>%
    filter(name == a)
  bDF <- lvParams %>%
    filter(name == b)
  
  # Check for overlap in domains
  if (aDF$lower < bDF$upper & aDF$upper > bDF$upper | 
      aDF$lower < bDF$lower & aDF$upper > bDF$lower) {
    n <- 1000
    dist <- sapply(1:n, function(i) abs(sampleX(aDF) - sampleX(bDF)))
    mean(dist)
  } else {
    ex <- integrate(function(x) x*aDF$k*mu(x, aDF$stdDev, aDF$med), 
                    lower=aDF$lower, upper=aDF$upper)$value
    ey <- integrate(function(x) x*bDF$k*mu(x, bDF$stdDev, bDF$med), 
                    lower=bDF$lower, upper=bDF$upper)$value
    abs(ex - ey)
  }
}

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
                     function(i) with(lvParams, 1/integrate(mu, stdDev[i], med[i],
                                                            lower = lower[i], 
                                                            upper = upper[i])$value))

# # Make pairwise comparisons
# for (i in 1:nActors) {
#   for (j in i:nActors) {
#     a <- actorPrefs[[i]]
#     b <- actorPrefs[[j]]
#     d <- sum(sapply(1:nrow(alternatives), function(i) semanticDist(a[[i]], b[[i]])))
#     s <- 1/(1 + d)
#     impactSocial[i, j] <- s
#     impactSocial[j, i] <- s
#   }
# }

# actorTable <- data.frame(Actor = stakeholders$ID) %>%
#   cbind(data.frame(do.call('rbind', actorPrefs)))
# colnames(actorTable) <- c("Actor", alternatives$Option)

add <- function(x) Reduce("+", x)

# Dendrogram
# hc <- hclust(as.dist(impactSocial), method="complete")
# plot(hc, xlab="Actor ID")

# ggplot(data = data.frame(x = c(0, 1)), mapping = aes(x = x)) +
#   stat_function(fun = mu, args = list(stdDev = lvParams$stdDev[1], med = lvParams$med[1])) +
#   stat_function(fun = mu, args = list(stdDev = lvParams$stdDev[2], med = lvParams$med[2])) +
#   stat_function(fun = mu, args = list(stdDev = lvParams$stdDev[3], med = lvParams$med[3])) +
#   stat_function(fun = mu, args = list(stdDev = lvParams$stdDev[4], med = lvParams$med[4])) +
#   stat_function(fun = mu, args = list(stdDev = lvParams$stdDev[5], med = lvParams$med[5])) +
#   stat_function(fun = mu, args = list(stdDev = lvParams$stdDev[6], med = lvParams$med[6])) +
#   stat_function(fun = mu, args = list(stdDev = lvParams$stdDev[7], med = lvParams$med[7])) +
#   ylim(0, 1)