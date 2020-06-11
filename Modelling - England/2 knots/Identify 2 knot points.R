# ------------------------------------------------------------------------------
# Notes
# ------------------------------------------------------------------------------

# This script finds points where growth rate of Covid-19 cases changes 
# by fitting ARIMA spline to incident ~ cumulative case 
# and selecting knot points according to a range of different fit criteria

# ------------------------------------------------------------------------------
# Set up
# ------------------------------------------------------------------------------

# Load required packages
packrat::restore()
library(tidyverse); library(lspline); library(forecast); library(Metrics)

# Run source code to import and format data
source("./Modelling - England/Import and format data.R")

# Set simulation-specific directory
path <- paste0("./Modelling - England/2 knots/")

# Set storage directory for outputs
# (default is simulation directory)
out <- path

# ------------------------------------------------------------------------------
# Estimate when exponential growth changed 
# ------------------------------------------------------------------------------

# Define potential knot dates:
possible_knot_dates_1 <- seq(from = date_sd, to = date_sd + 21, by = 1)
possible_knot_dates_2 <- seq(from = date_lockdown, to = date_lockdown + 21, by = 1)

# Create dataframe of all possible combinations of 2 knot dates
knots <- tibble(Knot_date_1 = as.Date(character()),
                Knot_date_2 = as.Date(character()),
                Growth_factor_1 = as.numeric(),
                Growth_factor_2 = as.numeric(),
                Growth_factor_3 = as.numeric(),
                AIC = as.numeric(),
                BIC = as.numeric(),
                RMSE_inc = as.numeric(),
                RMSE_cum = as.numeric(),
                Abs_diff_cum_end = as.numeric())
grid <- tibble(expand.grid(possible_knot_dates_2, possible_knot_dates_1))
names(grid) <- c("Knot_date_2", "Knot_date_1")
grid <- grid %>% select("Knot_date_1", "Knot_date_2") %>% filter(Knot_date_1 < Knot_date_2)
knots <- bind_rows(knots, grid)

# Fit spline of incidence ~ cumulative for each set of potential knot points
# Evaluate: spline fit statistics (AIC, BIC), and how well model parameters fit observed growth of data
# (RMSE of both incident and cumulative cases,  and absolute difference between cumulative cases at end of simulation)
for (i in 1:nrow(knots)) {
  
  # Set knot dates
  knot_date_1 <- knots[[i, "Knot_date_1"]]
  knot_date_2 <- knots[[i, "Knot_date_2"]]
  
  # Set knot point
  knot_1 <- subset(cases_eng_100, Date == knot_date_1)$Cumulative_cases_beg
  knot_2 <- subset(cases_eng_100, Date == knot_date_2)$Cumulative_cases_beg
  
  # Create dataframe for fitting manual splines
  # (using data where cumulative cases > 100, up to max date)
  data <- data.frame(lspline(cases_eng_100$Cumulative_cases_beg, knots = c(knot_1, knot_2)))
  names(data) <- c( "Cumulative_cases_beg_1", "Cumulative_cases_beg_2", "Cumulative_cases_beg_3")  
  data <- bind_cols(Daily_cases = cases_eng_100$Daily_cases, data)
  
  # Fit ARIMA model w/ specified knot points (no intercept)
  spline <- Arima(data$Daily_cases, order = c(2, 0, 0), 
                 seasonal = list(order = c(1, 0, 0), period = 7),
                 xreg = as.matrix(data[, 2:4]), include.constant = FALSE)
  
  # Record model parameters
  spline_slope_1 <- as.numeric(coef(spline)["Cumulative_cases_beg_1"])  # slope of segement 1
  spline_slope_2 <- as.numeric(coef(spline)["Cumulative_cases_beg_2"])  # slope of segement 2
  spline_slope_3 <- as.numeric(coef(spline)["Cumulative_cases_beg_3"])  # slope of segement 3
  
  # Calculate and record growth factors
  knots[[i, "Growth_factor_1"]] <- growth_factor_1 <- spline_slope_1 + 1
  knots[[i, "Growth_factor_2"]] <- growth_factor_2 <- spline_slope_2 + 1
  knots[[i, "Growth_factor_3"]] <- growth_factor_3 <- spline_slope_3 + 1
  
  # Record model summaries
  knots[[i, "AIC"]] <- AIC(spline)
  knots[[i, "BIC"]] <- BIC(spline)

  # Create dataframe for simulated model data
  # Set all cases to NA
  sim_data <- cases_eng %>% filter(Date >= (date_100 - 1) & Date <= date_T) %>% 
    select(c(Date, Cumulative_cases_beg, Daily_cases, Cumulative_cases_end))
  sim_data[sim_data$Date >= date_100, 
           c("Cumulative_cases_beg", 
             "Daily_cases", 
             "Cumulative_cases_end")] <- NA
  
  # Estimate growth of cases using knot point
  dates <- seq.Date(from = date_100, to = date_T, by = 1)
  for (t in as.list(dates)) {
    
    # Get cumulative cases at beginning of time t
    cum_t_beg <- as.numeric(sim_data[sim_data$Date == (t-1), "Cumulative_cases_end"])
    sim_data[sim_data$Date == t, "Cumulative_cases_beg"] <- cum_t_beg
    
    # Define growth parameters
    if (t <= knot_date_1) {
      growth <- growth_factor_1 
    } else if (t <= knot_date_2) {
      growth <- growth_factor_2
    } else {
      growth <- growth_factor_3
    }
    
    # Calculate daily cases at time t and record
    inc_tminus1 <- as.numeric(sim_data[sim_data$Date == (t-1), "Daily_cases"])
    inc_t <- round(growth*inc_tminus1)
    sim_data[sim_data$Date == t, "Daily_cases"] <- inc_t
    
    # Calculate cumulative cases at end of time t and record
    cum_t_end <- as.numeric(cum_t_beg + inc_t)
    sim_data[sim_data$Date == t, "Cumulative_cases_end"] <- cum_t_end
    
  }
  
  # Calculate and record RMSE 
  ## (1) For true vs predicted incident cases
  true_inc <- cases_eng_100$Daily_cases
  pred_inc <- filter(sim_data, Date >= date_100)$Daily_cases
  knots[i, "RMSE_inc"] <- rmse(true_inc, pred_inc)
  ## (2) For true vs predicted cumulative cases
  true_cum <- cases_eng_100$Cumulative_cases_end
  pred_cum <- filter(sim_data, Date >= date_100)$Cumulative_cases_end
  knots[i, "RMSE_cum"] <- rmse(true_cum, pred_cum)
  
  # Calculate absolute difference between cumulative cases at end of simulation vs true
  true_cum_end <- filter(cases_eng_100, Date == date_T)$Cumulative_cases_end
  pred_cum_end <- filter(sim_data, Date == date_T)$Cumulative_cases_end
  knots[i, "Abs_diff_cum_end"] <- abs(true_cum_end - pred_cum_end)
  
  # Display progress 
  cat('\r', paste(round((i / nrow(knots) * 100), 0), 
                  "% done    ", sep = " "))
  
}

# Calculate knots with lowest RMSE_inc
knots1 <- knots %>% arrange(RMSE_inc) %>% head(10)

# Calculate knots with lowest RMSE_cum
knots2 <- knots %>% arrange(RMSE_cum) %>% head(10)

# Calculate knots with lowest absolute difference in cumulative cases at end
knots3 <- knots %>% arrange(Abs_diff_cum_end) %>% head(10)

# Keep matches between three datsets
knots_best <- knots1[(knots1$Knot_date_1 %in% knots2$Knot_date_1 & knots1$Knot_date_1 %in% knots3$Knot_date_1) & 
         (knots1$Knot_date_2 %in% knots2$Knot_date_2 & knots1$Knot_date_2 %in% knots3$Knot_date_2), ]
knots_best

# Export
filename <- paste0("Best knot points.csv")
write_csv(knots_best, path = paste0(out, filename))
