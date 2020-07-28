# ------------------------------------------------------------------------------
# Notes
# ------------------------------------------------------------------------------

# This script finds the 'best' pairs of dates where the growth rate of COVID-19 cases changes,
# from a list of candidate pairs of dates.

# For each candidate pair, an ARIMA spline model is fit with the pair of dates as knot points.
# The growth factor for each of the spline segments is estimated from this model,
# and the growth of cases is simulated using the estimated growth factors.

# The 'best' pairs of dates are selected according to how well their estimated growth factors
# fit the observed growth of COVID-19 cases (both incident and cumulative cases).

# ------------------------------------------------------------------------------
# Set up
# ------------------------------------------------------------------------------

# Load required packages
packrat::restore()
library(tidyverse); library(lspline); library(forecast)

# Run source code to import and format data
source("./Code/Import and format data.R")

# Set storage directory for outputs
out <- paste0("./Results/")

## Functions -------------------------------------------------------------------

# Function to calculate Poisson deviance between two vectors
# from: https://en.wikipedia.org/wiki/Deviance_(statistics)
# Arguments: obs = vector of observed values, sim = vector of simulated/predicted values
Calc_Pois_Dev <- function(obs, sim) {
  
  D <- 2 * sum(obs * log(obs / sim) - (obs - sim))
  return(D)
  
}

# ------------------------------------------------------------------------------
# Estimate when exponential growth changed 
# ------------------------------------------------------------------------------

## Set simulation parameters --------------------------------------------------- 

# Define potential knot dates
possible_knot_dates_1 <- seq(from = date_sd, to = date_sd + 21, by = 1)
possible_knot_dates_2 <- seq(from = date_lockdown, to = date_lockdown + 21, by = 1)

# Set dates to simulate
dates <- seq.Date(from = date_100, to = date_T, by = 1)

# Create matrices for simulated data (daily and cumulative cases)
# (1 row per simulation run, 1 col per date)
daily_cases_sim <- cumulative_cases_end_sim <- 
  matrix(nrow = 1, ncol = length(dates) + 1,
         dimnames = list(1, as.character(seq.Date(from = date_100 - 1, to = date_T, by = 1))))
# Initialise matrices with data at date_100 - 1
daily_cases_sim[, 1] <- filter(cases_eng, Date == (date_100 - 1))$Daily_cases
cumulative_cases_end_sim[, 1] <- filter(cases_eng, Date == (date_100 - 1))$Cumulative_cases_end

## Create dataframe to store simulation outputs --------------------------------

# Create dataframe of all possible combinations of 2 knot dates
knots <- tibble(Knot_date_1 = as.Date(character()),
                Knot_date_2 = as.Date(character()),
                Growth_factor_1 = as.numeric(),
                Growth_factor_2 = as.numeric(),
                Growth_factor_3 = as.numeric(),
                Growth_factor_1_sd = as.numeric(),
                Growth_factor_2_sd = as.numeric(),
                Growth_factor_3_sd = as.numeric(),
                Pois_dev_inc = as.numeric(),
                Pois_dev_cum = as.numeric(),
                Diff_cum_end = as.numeric())
grid <- tibble(expand.grid(possible_knot_dates_2, possible_knot_dates_1))
names(grid) <- c("Knot_date_2", "Knot_date_1")
grid <- grid %>% select("Knot_date_1", "Knot_date_2") %>% filter(Knot_date_1 < Knot_date_2)
knots <- bind_rows(knots, grid)

## Estimate growth of cases for each pair of knot dates ------------------------

# Fit spline of incidence ~ cumulative for each set of potential knot points
# Evaluate: spline fit statistics (AIC, BIC), and how well model parameters fit observed growth of data
# (RMSE of both incident and cumulative cases,  and difference between cumulative cases at end of simulation)

# (1) Iterate through pairs of knot points
for (i in 1:nrow(knots)) {
  
  # Set knot dates
  knot_date_1 <- knots[[i, "Knot_date_1"]]
  knot_date_2 <- knots[[i, "Knot_date_2"]]
  
  # Set knot point
  knot_1 <- subset(cases_eng_100, Date == knot_date_1)$Cumulative_cases_beg
  knot_2 <- subset(cases_eng_100, Date == knot_date_2)$Cumulative_cases_beg
  
  # Create time series object for daily cases
  cases_ts <- ts(data = cases_eng_100$Daily_cases, frequency = 7)
  
  # Create dataframe of covariates (cumulative cases) for fitting manual Arima splines
  # (using data where cumulative cases > 100, up to max date)
  covariates <- data.frame(lspline(cases_eng_100$Cumulative_cases_beg, knots = c(knot_1, knot_2)))
  names(covariates) <- c( "Cumulative_cases_beg_1", "Cumulative_cases_beg_2", "Cumulative_cases_beg_3")  
  
  # Fit ARIMA model w/ specified knot points (no intercept)
  spline <- Arima(cases_ts, order = c(1, 0, 0), 
                  seasonal = list(order = c(1, 0, 0), period = 7),
                  xreg = as.matrix(covariates[, 1:3]), include.constant = FALSE,
                  method = "ML")
  
  # Record model parameters
  spline_slope_1 <- as.numeric(coef(spline)["Cumulative_cases_beg_1"])  # slope of segment 1
  spline_slope_2 <- as.numeric(coef(spline)["Cumulative_cases_beg_2"])  # slope of segment 2
  spline_slope_3 <- as.numeric(coef(spline)["Cumulative_cases_beg_3"])  # slope of segment 3
  spline_slope_1_sd <- as.numeric(sqrt(diag(spline$var.coef))[["Cumulative_cases_beg_1"]])  # SD of slope of segment 1
  spline_slope_2_sd <- as.numeric(sqrt(diag(spline$var.coef))[["Cumulative_cases_beg_2"]])  # SD of slope of segment 2
  spline_slope_3_sd <- as.numeric(sqrt(diag(spline$var.coef))[["Cumulative_cases_beg_3"]])  # SD of slope of segment 3
  
  # Record growth factors and SDs in knots summary table
  knots[[i, "Growth_factor_1"]] <- growth_factor_1 <- spline_slope_1 + 1
  knots[[i, "Growth_factor_2"]] <- growth_factor_2 <- spline_slope_2 + 1
  knots[[i, "Growth_factor_3"]] <- growth_factor_3 <- spline_slope_3 + 1
  knots[[i, "Growth_factor_1_sd"]] <- spline_slope_1_sd
  knots[[i, "Growth_factor_2_sd"]] <- spline_slope_2_sd
  knots[[i, "Growth_factor_3_sd"]] <- spline_slope_3_sd
  
  # (2) Estimate growth of cases using knot points
  for (t in as.list(dates)) {
    
    # Get daily and cumulative cases from time t-1
    inc_tminus1 <- daily_cases_sim[, as.character(t-1)]
    cum_tminus1 <- cumulative_cases_end_sim[, as.character(t-1)]
    
    # Define growth parameters
    if (t <= knot_date_1) {
      growth <- growth_factor_1 
    } else if (t <= knot_date_2) {
      growth <- growth_factor_2
    } else {
      growth <- growth_factor_3
    }
    
    # Calculate daily cases at time t and record
    inc_t <- round(growth*inc_tminus1)
    daily_cases_sim[, as.character(t)] <- inc_t
    
    # Calculate cumulative cases at end of time t and record
    cum_t <- cum_tminus1 + inc_t
    cumulative_cases_end_sim[, as.character(t)] <- cum_t
    
  }  # close loop (2)
  
  # Calculate and record Poisson deviance
  ## (1) For predicted vs true (7-day moving average) incident cases
  true_inc <- cases_eng_100$Daily_cases_MA7
  pred_inc <- daily_cases_sim[1, -1]
  knots[i, "Pois_dev_inc"] <- Calc_Pois_Dev(obs = true_inc, sim = pred_inc)
  ## (2) For predicted vs true (7-day moving average) cumulative cases
  true_cum <- cases_eng_100$Cumulative_cases_end_MA7
  pred_cum <- cumulative_cases_end_sim[1, -1]
  knots[i, "Pois_dev_cum"] <- Calc_Pois_Dev(obs = true_cum, sim = pred_cum)
  
  # Calculate absolute difference between cumulative cases at end of simulation vs true
  true_cum_end <- filter(cases_eng_100, Date == date_T)$Cumulative_cases_end
  pred_cum_end <- cumulative_cases_end_sim[1, ncol(cumulative_cases_end_sim)]
  knots[i, "Diff_cum_end"] <- true_cum_end - pred_cum_end
  
  # Display progress 
  cat('\r', paste(round((i / nrow(knots) * 100), 0), 
                  "% done    ", sep = " "))
  
}  # close loop (1)

# Calculate 10 knots with lowest Pois_dev_inc
knots1 <- knots %>% arrange(Pois_dev_inc) %>% head(10)

# Calculate 10 knots with lowest Pois_dev_cum
knots2 <- knots %>% arrange(Pois_dev_cum) %>% head(10)

# Keep matches between two datsets
knots_best <- intersect(knots1, knots2)
knots_best

# Construct probability for selecting each pair of knot points
## (1) Equal probability of each knot point pair
knots_best <- knots_best %>% mutate(Prob_equal = 1 / nrow(knots_best))
## (2) Unequal probabilities of each knot point pair according to Pois_dev_cum
### Create inverse of Pois_dev_cum values so that lower values are ranked higher,
### calculate normaliser for rescaling Pois_dev_cum inverse values, and
### calculate probability by multiplying Pois_dev_cum inverse values by normaliser
knots_best <- knots_best %>% mutate(Pois_dev_cum_inv = 1 / Pois_dev_cum,
                                    Norm = 1 / sum(Pois_dev_cum_inv), 
                                    Prob_unequal = Pois_dev_cum_inv * Norm) %>%
  select(-c(Pois_dev_cum_inv, Norm))

# Export
filename <- paste0("Best knot points.csv")
write_csv(knots_best, path = paste0(out, filename))
