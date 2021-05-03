# ------------------------------------------------------------------------------
# Notes
# ------------------------------------------------------------------------------

# This script simulates the natural and counterfactual histories of COVID-19 growth,
# using of the 'best' pairs of knot points identified in script 'Identify 2 knot points.R'.

# It also produces plots comparing simulated histories with true (observed) data.

# ------------------------------------------------------------------------------
# Set up
# ------------------------------------------------------------------------------

# Load required packages
packrat::restore()
library(tidyverse); library(lspline); library(forecast); library(ggpubr); 
library(scales); library(shadowtext)

# Run source code to import and format data
source("./Code/Import and format data.R")

# Set storage directory for outputs
out <- paste0("./Results/")

# Import file containing best knot point pairs
knots_best <- read_csv(paste0(out, "Best knot points.csv"))

## Functions -------------------------------------------------------------------

# Functions to calculate mean and SD of growth factor distributions on log scale
# (equations from https://en.wikipedia.org/wiki/Log-normal_distribution#Arithmetic_moments)
# Arguments: mean, sd of variable on normal scale
## (1) Mean
Calculate_mean_log <- function(mean, sd) {
  mean_log <- log(mean^2 / sqrt(sd^2 + mean^2))
  return(mean_log)
}
## (2) SD
Calculate_sd_log <- function(mean, sd) {
  sd_log <- sqrt(log(1 + (sd^2 / mean^2)))
  return(sd_log)
}

# Function to summarise mean, 2.5 and 97.5 centiles
# Argument: vector of values (x)
Summarise_centiles <- function(x) {
  x <- x[!is.na(x) & !is.infinite(x)]
  c(Mean = mean(x), 
    C_025 = quantile(x, 0.025, names = FALSE), 
    C_975 = quantile(x, 0.975, names = FALSE))
}

# Function to preserve rounded sum
# (from https://www.r-bloggers.com/round-values-while-preserve-their-rounded-sum-in-r/)
# Arguments: vector of values (x), number of digits (default = 0)
Round_preserve_sum <- function(x, digits = 0) {
  up <- 10 ^ digits
  x <- x * up
  y <- floor(x)
  indices <- tail(order(x-y), round(sum(x)) - sum(y))
  y[indices] <- y[indices] + 1
  y / up
}

# Function to calculate percent change in a number between two time periods
# Arguments: initial value (initial), final value (final)
Percent_Change <- function(initial, final) {
  (final - initial) / initial * 100
}

# Function to calculate Poisson deviance between two vectors
# from: https://en.wikipedia.org/wiki/Deviance_(statistics)
# Arguments: obs = vector of observed values, sim = vector of simulated/predicted values
Calc_Pois_Dev <- function(obs, sim) {
  
  D <- 2 * sum(obs * log(obs / sim) - (obs - sim))
  return(D)
  
}

# ------------------------------------------------------------------------------
# Define simulation parameters
# ------------------------------------------------------------------------------

# Define number of best knot point pairs
n_knots_best <- nrow(knots_best)

# Define parameters associated with best knot point pairs
## Possible dates of first and second knots
knot_date_1 <- knots_best %>% pull(Knot_date_1)
knot_date_2 <- knots_best %>% pull(Knot_date_2)
## Growth factors associated with possible knot point pairs
growth_factor_1 <- knots_best %>% pull(Growth_factor_1)
growth_factor_2 <- knots_best %>% pull(Growth_factor_2)
growth_factor_3 <- knots_best %>% pull(Growth_factor_3)
## Standard deviations of growth factors associated with possible knot point pairs
growth_factor_1_sd <- knots_best %>% pull(Growth_factor_1_sd)
growth_factor_2_sd <- knots_best %>% pull(Growth_factor_2_sd)
growth_factor_3_sd <- knots_best %>% pull(Growth_factor_3_sd)
## Probabilities (equal and unequal) of possible knot point pairs
prob_equal <- knots_best %>% pull(Prob_equal)
prob_unequal <- knots_best %>% pull(Prob_unequal)

# Define number of simulation runs per simulated scenario
n_runs <- 100000

# Calculate number of simulation runs for each pair of knot dates 
n_equal <- Round_preserve_sum(prob_equal * n_runs)
n_unequal <- Round_preserve_sum(prob_unequal * n_runs)

# Define true sd/lockdown dates
scenarios_true <- tibble(Simulation = "True",
                         Description = "True",
                         Date_sd = date_sd, Date_lockdown = date_lockdown)

# Define scenarios, sd/lockdown dates, and knot dates to be simulated
knots_sim <- bind_rows(tibble(Simulation = "Natural history",
                              Description = "Social distancing and lockdown as implemented",
                              Date_sd = date_sd, Date_lockdown = date_lockdown,
                              Knot_date_1 = knot_date_1, Knot_date_2 = knot_date_2,
                              Growth_factor_1 = growth_factor_1, Growth_factor_1_sd = growth_factor_1_sd,
                              Growth_factor_2 = growth_factor_2, Growth_factor_2_sd = growth_factor_2_sd,
                              Growth_factor_3 = growth_factor_3, Growth_factor_3_sd = growth_factor_3_sd,
                              Prob = prob_unequal, N = n_unequal),
                       tibble(Simulation = "Counterfactual history",
                              Description = "Social distancing and lockdown 1 week earlier",
                              Date_sd = date_sd - 7, Date_lockdown = date_lockdown - 7,
                              Knot_date_1 = knot_date_1 - 7, Knot_date_2 = knot_date_2 - 7,
                              Growth_factor_1 = growth_factor_1, Growth_factor_1_sd = growth_factor_1_sd,
                              Growth_factor_2 = growth_factor_2, Growth_factor_2_sd = growth_factor_2_sd,
                              Growth_factor_3 = growth_factor_3, Growth_factor_3_sd = growth_factor_3_sd,
                              Prob = prob_unequal, N = n_unequal),
                       tibble(Simulation = "Counterfactual history",
                              Description = "Social distancing and lockdown 2 weeks earlier",
                              Date_sd = date_sd - 14, Date_lockdown = date_lockdown - 14,
                              Knot_date_1 = knot_date_1 - 14, Knot_date_2 = knot_date_2 - 14,
                              Growth_factor_1 = growth_factor_1, Growth_factor_1_sd = growth_factor_1_sd,
                              Growth_factor_2 = growth_factor_2, Growth_factor_2_sd = growth_factor_2_sd,
                              Growth_factor_3 = growth_factor_3, Growth_factor_3_sd = growth_factor_3_sd,
                              Prob = prob_unequal, N = n_unequal))

# Create succinct dataframe containing simulation, description, and sd/lockdown dates
scenarios_sim <- knots_sim %>% select(Simulation, Description, Date_sd, Date_lockdown) %>% unique

# Define number of scenarios to simulate
n_sim <- nrow(scenarios_sim)

# Set dates to simulate
dates <- seq.Date(from = date_100, to = date_T, by = 1)

## Create lists to store simulation outputs ------------------------------------

# Create lists to store ALL simulation outputs, if desired
#data_sim <- list()  # daily cases, cumulative cases, growth factors
#deaths_sim <- list()  # estimated deaths

# Create lists to store SUMMARY outputs from all simulations
summary_daily_cases_sim <- list()  # daily cases
summary_cumulative_cases_end_sim <- list()  # cumulative cases
summary_growth_factor_sim <- list()  # growth factor
summary_deaths_sim <- list()  # deaths

# ------------------------------------------------------------------------------
# Simulate natural and counterfactual histories
# ------------------------------------------------------------------------------

# Set seed
set.seed(23)

# (1) Iterate through hypothetical interventions
start <- Sys.time()
for (k in 1:n_sim) {
  
  # Filter scenario dataframe by k
  scenarios_sim_k <- scenarios_sim[k, ]
  
  # Define simulation, description, social distancing and lockdown dates
  #simulation_k <- scenarios_sim_k[["Simulation"]]
  description_k <- scenarios_sim_k[["Description"]]
  #date_sd_k <- scenarios_sim_k[["Date_sd"]]
  #date_lockdown_k <- scenarios_sim_k[["Date_lockdown"]]
  
  # Filter knots dataframe for defined scenario
  knots_sim_k <- knots_sim %>% filter(Description == description_k)
  
  # Create empty matrices for simulated data
  # (1 row per simulation run, 1 col per date)
  daily_cases_sim_k <- cumulative_cases_end_sim_k <- growth_factor_sim_k <-
    matrix(nrow = 0, ncol = length(dates) + 1,
           dimnames = list(NULL, as.character(seq.Date(from = date_100 - 1, to = date_T, by = 1))))
  
  # Create matrix for estimated deaths
  # (1 row per simulation run, 1 col per CFR)
  deaths_sim_k <- matrix(nrow = n_runs, ncol = 2,
                         dimnames = list(NULL, c("cfr_hosp_dod", "cfr_all_dod")))
  
  # (2) Iterate through possible knot date pairs
  for (i in 1:n_knots_best) {
    
    # Filter scenario-specific dataframe by i
    knots_sim_i <- knots_sim_k[i, ]
    
    # Define knot dates
    knot_date_1_i <- knots_sim_i[["Knot_date_1"]]
    knot_date_2_i <- knots_sim_i[["Knot_date_2"]]
    
    # Calculate cumulative cases at dates of knot
    knot_1_i <- cases_eng_100 %>% filter(Date == knot_date_1_i) %>% pull(Cumulative_cases_beg)
    knot_2_i <- cases_eng_100 %>% filter(Date == knot_date_2_i) %>% pull(Cumulative_cases_beg)
    
    # Define growth parameters - means and SDs
    growth_factor_1_i <- knots_sim_i[["Growth_factor_1"]]
    growth_factor_2_i <- knots_sim_i[["Growth_factor_2"]]
    growth_factor_3_i <- knots_sim_i[["Growth_factor_3"]]
    growth_factor_1_sd_i <- knots_sim_i[["Growth_factor_1_sd"]]
    growth_factor_2_sd_i <- knots_sim_i[["Growth_factor_2_sd"]]
    growth_factor_3_sd_i <- knots_sim_i[["Growth_factor_3_sd"]]
    
    # Define number of simulation runs for specified knot dates
    n_runs_i <- knots_sim_i[["N"]]
    
    # Create matrices for simulated data
    # (1 row per simulation run, 1 col per date)
    daily_cases_sim_i <- cumulative_cases_end_sim_i <- growth_factor_sim_i <-
      matrix(nrow = n_runs_i, ncol = length(dates) + 1,
             dimnames = list(NULL, as.character(seq.Date(from = date_100 - 1, to = date_T, by = 1))))
    # Initialise matrices with data at date_100 - 1
    daily_cases_sim_i[, 1] <- filter(cases_eng, Date == (date_100 - 1))$Daily_cases
    cumulative_cases_end_sim_i[, 1] <- filter(cases_eng, Date == (date_100 - 1))$Cumulative_cases_end
    growth_factor_sim_i[, 1] <- filter(cases_eng, Date == (date_100 - 1))$Growth_factor
    
    # (3) Iterate through dates
    for (t in as.list(dates)) {
      
      # Get daily and cumulative cases from time t-1
      inc_tminus1 <- daily_cases_sim_i[, as.character(t-1)]
      cum_tminus1 <- cumulative_cases_end_sim_i[, as.character(t-1)]
      
      # Define growth parameters
      if (t <= knot_date_1_i) {
        growth <- rlnorm(n = n_runs_i,
                         meanlog = Calculate_mean_log(mean = growth_factor_1_i, sd = growth_factor_1_sd_i),
                         sdlog = Calculate_sd_log(mean = growth_factor_1_i, sd = growth_factor_1_sd_i))
      } else if (t <= knot_date_2_i) {
        growth <- rlnorm(n = n_runs_i,
                         meanlog = Calculate_mean_log(mean = growth_factor_2_i, sd = growth_factor_2_sd_i),
                         sdlog = Calculate_sd_log(mean = growth_factor_2_i, sd = growth_factor_2_sd_i))
      } else {
        growth <- rlnorm(n = n_runs_i,
                         meanlog = Calculate_mean_log(mean = growth_factor_3_i, sd = growth_factor_3_sd_i),
                         sdlog = Calculate_sd_log(mean = growth_factor_3_i, sd = growth_factor_3_sd_i))
      }
      
      # Calculate daily cases at time t and record
      inc_t <- round(growth*inc_tminus1)
      daily_cases_sim_i[, as.character(t)] <- inc_t
      
      # Calculate cumulative cases at end of time t and record
      cum_t <- cum_tminus1 + inc_t
      cumulative_cases_end_sim_i[, as.character(t)] <- cum_t
      
      # Record growth factor
      growth_factor <- inc_t / inc_tminus1
      growth_factor_sim_i[, as.character(t)] <- growth_factor
      
      # Display progress of simulation
      #cat('\r', paste(round((match(t, dates) / length(dates) * 100), 0), 
      #                "% done of simulation", k, "of", n_sim, "                 ", sep = " "))
      
    }  # close loop 3 (t)
    
    # Bind knot-specific dataframes to full scenario dataframe
    daily_cases_sim_k <- rbind(daily_cases_sim_k, daily_cases_sim_i)
    cumulative_cases_end_sim_k <- rbind(cumulative_cases_end_sim_k, cumulative_cases_end_sim_i)
    growth_factor_sim_k <- rbind(growth_factor_sim_k, growth_factor_sim_i)
    
  }  # close loop 2 (i)
  
  # Estimate number of deaths at date_T in scenario k
  for (j in 1:ncol(deaths_sim_k)) {
    cfr <- eval(parse(text = colnames(deaths_sim_k)[j]))
    deaths_sim_k[, j] <- round(cumulative_cases_end_sim_k[, as.character(date_T)] * cfr)
  }
  
  # Print status update
  cat('\r', paste("Summarising data from simulation", k, "of", n_sim, "...    ", sep = " "))
  
  # Record simulated data, if desired
  ## Daily cases, cumulative cases, and growth factors:
  #daily_cases_sim <- daily_cases_sim_k %>% as_tibble(rownames = "Run") %>% 
  #  gather(Date, Daily_cases, dimnames(daily_cases_sim_k)[[2]]) %>% 
  #  mutate(Date = as.Date(Date)) %>% mutate(Run = as.numeric(Run))  # daily cases
  #cumulative_cases_end_sim <- cumulative_cases_end_sim_k %>% as_tibble(rownames = "Run") %>% 
  #  gather(Date, Cumulative_cases_end, dimnames(daily_cases_sim_k)[[2]]) %>% 
  #  mutate(Date = as.Date(Date)) %>% mutate(Run = as.numeric(Run))  # cumulative cases
  #growth_factor_sim <- growth_factor_sim_k %>% as_tibble(rownames = "Run") %>% 
  #  gather(Date, Growth_factor, dimnames(daily_cases_sim_k)[[2]]) %>% 
  #  mutate(Date = as.Date(Date)) %>% mutate(Run = as.numeric(Run))  # growth factor
  #data_sim[[k]] <- full_join(daily_cases_sim, cumulative_cases_end_sim) %>% 
  #  full_join(., growth_factor_sim) %>% arrange(Run) %>% 
  #  mutate(scenarios_sim_k) %>% relocate(names(scenarios_sim_k))
  ### Estimated deaths:
  #deaths_sim[[k]] <- deaths_sim_k %>% as_tibble(rownames = "Run") %>%
  #  gather(CFR, Deaths, dimnames(deaths_sim_k)[[2]]) %>% 
  #  mutate(Date = date_T) %>% mutate(Run = as.numeric(Run)) %>%
  #  arrange(Run) %>% mutate(scenarios_sim_k) %>% relocate(names(scenarios_sim_k))
  
  # Record summaries (mean, 2.5 and 97.5 centiles)
  ## Daily cases:
  summary <- apply(X = daily_cases_sim_k, MARGIN = 2, FUN = Summarise_centiles) %>%
    round %>% t %>% as_tibble(rownames = "Date") %>% mutate(Date = as.Date(Date))
  summary_daily_cases_sim[[k]] <- summary %>% mutate(scenarios_sim_k) %>%
    relocate(names(scenarios_sim_k))
  ## Cumulative cases:
  summary <- apply(X = cumulative_cases_end_sim_k, MARGIN = 2, FUN = Summarise_centiles) %>% 
    round %>% t %>% as_tibble(rownames = "Date") %>% mutate(Date = as.Date(Date))
  summary_cumulative_cases_end_sim[[k]] <- summary %>% mutate(scenarios_sim_k) %>%
    relocate(names(scenarios_sim_k))
  ## Growth factors:
  summary <- apply(X = growth_factor_sim_k, MARGIN = 2, FUN = Summarise_centiles) %>%
    t %>% as_tibble(rownames = "Date") %>% mutate(Date = as.Date(Date))
  summary_growth_factor_sim[[k]] <- summary %>% mutate(scenarios_sim_k) %>%
    relocate(names(scenarios_sim_k))
  ## Deaths:
  summary <- apply(X = deaths_sim_k, MARGIN = 2, FUN = Summarise_centiles) %>% 
    round %>% t %>% as_tibble(rownames = "CFR") %>% 
    mutate(CFR_value = sapply(CFR, function(x) eval(parse(text = x)))) %>%
    relocate(CFR_value, .after = CFR)
  summary_deaths_sim[[k]] <- summary %>% mutate(scenarios_sim_k) %>%
    mutate(Date = date_T) %>%
    relocate(names(scenarios_sim_k), Date)
  
}  # close loop 1 (k)
end <- Sys.time()
end - start

# Combine simulated data from all natural and counterfactual histories, if desired
#data_sim <- bind_rows(data_sim)  # daily cases, cumulative cases, growth factors
#deaths_sim <- bind_rows(deaths_sim)  # deaths

# Combine summary results from all natural and counterfactual histories 
summary_daily_cases_sim <- bind_rows(summary_daily_cases_sim) 
summary_cumulative_cases_end_sim <- bind_rows(summary_cumulative_cases_end_sim) 
summary_growth_factor_sim <- bind_rows(summary_growth_factor_sim) 
summary_deaths_sim <- bind_rows(summary_deaths_sim) 

# Combine summary data with true (observed) data up to time_T
summary_daily_cases_sim <- bind_rows(tibble(scenarios_true, 
                                            Date = filter(cases_eng, Date <= date_T)$Date,
                                            Mean = filter(cases_eng, Date <= date_T)$Daily_cases), 
                                     summary_daily_cases_sim)
summary_cumulative_cases_end_sim <- bind_rows(tibble(scenarios_true,
                                                     Date = filter(cases_eng, Date <= date_T)$Date,
                                                     Mean = filter(cases_eng, Date <= date_T)$Cumulative_cases_end),
                                              summary_cumulative_cases_end_sim)
summary_growth_factor_sim <- bind_rows(tibble(scenarios_true,
                                              Date = filter(cases_eng, Date <= date_T)$Date,
                                              Mean = filter(cases_eng, Date <= date_T)$Growth_factor),
                                       summary_growth_factor_sim)
summary_deaths_sim <- bind_rows(tibble(scenarios_true,
                                       Date = date_T,
                                       CFR = colnames(deaths_sim_k)) %>%
                                  mutate(CFR_value = sapply(CFR, function(x) eval(parse(text = x)))) %>%
                                  mutate(Mean = round(c(filter(deaths_hosp_dod_eng, Date == date_T)$Cumulative_deaths, 
                                                        filter(deaths_all_dod_eng, Date == date_T)$Cumulative_deaths))),
                                summary_deaths_sim)

# Create summary table of cases and deaths at time_T
cases_natural_history_T <- summary_cumulative_cases_end_sim %>% 
  filter(Date == date_T, Simulation == "Natural history") %>% pull(Mean)  # cases under natural history at time_T
cases_T <- summary_cumulative_cases_end_sim %>% filter(Date == date_T) %>% 
  mutate(Percent_change_cases = ifelse(Simulation == "Counterfactual history", 
                                 Percent_Change(initial = cases_natural_history_T, final = Mean), NA)) %>%
  rename_at(vars(Mean, C_025, C_975), function(x) { paste0(x, "_cases") })
deaths_T <- summary_deaths_sim %>% rename_at(vars(Mean, C_025, C_975), function(x) { paste0(x, "_deaths") })
summary_T <- full_join(cases_T, deaths_T)
rm(cases_natural_history_T, cases_T, deaths_T)

# Calculate Poisson deviance between mean predicted and true (7-day moving average) natural history
## Incident cases
true_inc <- cases_eng_100$Daily_cases_MA7
pred_inc <- summary_daily_cases_sim %>% filter(Simulation == "Natural history", Date >= date_100) %>% pull(Mean)
pois_dev_inc <- Calc_Pois_Dev(obs = true_inc, sim = pred_inc)
pois_dev_inc
## Cumulative cases
true_cum <- cases_eng_100$Cumulative_cases_end_MA7
pred_cum <- summary_cumulative_cases_end_sim %>% filter(Simulation == "Natural history", Date >= date_100) %>% pull(Mean)
pois_dev_cum <- Calc_Pois_Dev(obs = true_cum, sim = pred_cum)
pois_dev_cum
rm(true_inc, pred_inc, true_cum, pred_cum)

# Export all summary data
write_csv(summary_daily_cases_sim, path = paste0(out, "Summary - daily cases.csv"))
write_csv(summary_cumulative_cases_end_sim, path = paste0(out, "Summary - cumulative cases.csv"))
write_csv(summary_growth_factor_sim, path = paste0(out, "Summary - growth factors.csv"))
write_csv(summary_deaths_sim, path = paste0(out, "Summary - deaths.csv"))
write_csv(summary_T, path = paste0(out, "Final summary - cases and deaths at end of simulation.csv"))

# Export descriptions of simulated interventions
write_csv(scenarios_sim, path = paste0(out, "Hypothetical interventions.csv"))

# Remove loop variables
rm(k, i, t, j, scenarios_sim_k, description_k, knots_sim_k,
   daily_cases_sim_k, cumulative_cases_end_sim_k, growth_factor_sim_k, deaths_sim_k,
   knots_sim_i, knot_date_1_i, knot_date_2_i, knot_1_i, knot_2_i,
   growth_factor_1_i, growth_factor_2_i, growth_factor_3_i,
   growth_factor_1_sd_i, growth_factor_2_sd_i, growth_factor_3_sd_i, n_runs_i, 
   daily_cases_sim_i, cumulative_cases_end_sim_i, growth_factor_sim_i, 
   inc_tminus1, cum_tminus1, inc_t, cum_t, growth, growth_factor,
   cfr, summary, start, end)

# ------------------------------------------------------------------------------
# Figures
# ------------------------------------------------------------------------------

# Create figures of incident and cumulative cases under natural vs counterfactual histories

# Import files required for figures, if necessary
#scenarios_sim <- read_csv(paste0(out, "Hypothetical interventions.csv"))
#summary_daily_cases_sim <- read_csv(paste0(out, "Summary - daily cases.csv"))
#summary_cumulative_cases_end_sim <- read_csv(paste0(out, "Summary - cumulative cases.csv"))
#knots_best <- read_csv(paste0(out, "Best knot points.csv"))

## Incident cases --------------------------------------------------------------

# Create list for storing figures
plot_cases_inc_sim <- list()

# Figures
for (i in 1:nrow(scenarios_sim)) {
  
  # Define as natural or counterfactual history
  simulation_i <- scenarios_sim[[i, "Simulation"]]
  description_i <- scenarios_sim[[i, "Description"]]
  
  # Calculate dates of social distancing and lockdown
  date_sd_i <- scenarios_sim[[i, "Date_sd"]]
  date_lockdown_i <- scenarios_sim[[i, "Date_lockdown"]]
  
  # Filter data
  summary_data_i <- filter(summary_daily_cases_sim, 
                           Simulation == simulation_i & Description == description_i)
  
  # Create plot
  p <- ggplot(data = summary_data_i, aes(x = Date, y = Mean)) +
    theme_minimal() +
    theme(plot.margin = unit(c(0.5, 1, 0.5, 1), "cm")) +
    labs(title = simulation_i,
         subtitle = description_i) +
    geom_col(data = filter(summary_daily_cases_sim, 
                           Simulation == "True"), aes(x = Date, y = Mean), alpha = 0.4) +
    geom_vline(xintercept = date_sd_i, col = "red4") +
    geom_text(aes_(x = date_sd_i - 1, y = 5000, 
                   label = paste0("Date of\nsocial distancing:\n", 
                                  as.character(date_sd_i, format = "%d %b %C")), 
                   hjust = 1),
              color = "red4", size = 3, check_overlap = TRUE, show.legend = FALSE) +
    geom_vline(xintercept = date_lockdown_i, col = "red4") +
    geom_text(aes_(x = date_lockdown_i + 1, y = 8000, 
                   label = paste0("Date of\nlockdown:\n", 
                                  as.character(date_lockdown_i, format = "%d %b %C")), 
                   hjust = 0),
              color = "red4", size = 3, check_overlap = TRUE, show.legend = FALSE) +
    geom_line(color = "navyblue", size = 1) +
    geom_ribbon(aes(ymin = C_025, ymax = C_975),
                fill = "navyblue", alpha = 0.25) +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
    scale_x_date(name = "Date", 
                 limits = c(date_0, date_T + 7), 
                 date_breaks = "1 week", 
                 date_labels = "%d %b %C",
                 expand = expansion(mult = c(0, 0))) +
    scale_y_continuous(name = "Number of lab-confirmed cases",
                       limits = c(0, 10000), 
                       breaks = seq(0, 10000, 1000),
                       expand = expansion(mult = c(0.01, 0.01)))
  #p
  
  # Add plot to list
  plot_cases_inc_sim[[i]] <- p
  
}

# Save plots
p <- ggarrange(plotlist = plot_cases_inc_sim, nrow = length(plot_cases_inc_sim))
g <- annotate_figure(p, top = text_grob("Incident lab-confirmed cases of COVID-19 in England\n(Pillar 1)", size = 15),
                     bottom = text_grob("Data from https://www.gov.uk/guidance/coronavirus-covid-19-information-for-the-public.", size = 10))
ggsave(paste0(out, "Plot - true vs counterfactual - incident cases.png"),
       plot = g, width = 6, height = 4*length(plot_cases_inc_sim))

# Remove plotting objects
rm(i, simulation_i, description_i, date_sd_i, date_lockdown_i, summary_data_i, p, g)

## Cumulative cases ------------------------------------------------------------

# Create list for storing figures
plot_cases_cum_sim <- list()

# Figures
for (i in 1:nrow(scenarios_sim)) {
  
  # Define as natural or counterfactual history
  simulation_i <- scenarios_sim[[i, "Simulation"]]
  description_i <- scenarios_sim[[i, "Description"]]
  
  # Calculate dates of social distancing and lockdown
  date_sd_i <- scenarios_sim[[i, "Date_sd"]]
  date_lockdown_i <- scenarios_sim[[i, "Date_lockdown"]]
  
  # Filter data
  summary_data_i <- filter(summary_cumulative_cases_end_sim, 
                           Simulation == simulation_i & Description == description_i)
  
  # Create plot
  p <- ggplot(data = summary_data_i, aes(x = Date, y = Mean)) +
    theme_minimal() +
    theme(plot.margin = unit(c(0.5, 1, 0.5, 1), "cm")) +
    labs(title = simulation_i,
         subtitle = description_i) +
    geom_col(data = filter(summary_cumulative_cases_end_sim, 
                           Simulation == "True"), aes(x = Date, y = Mean), alpha = 0.4) +
    geom_vline(xintercept = date_sd_i, col = "red4") +
    geom_text(aes_(x = date_sd_i - 1, y = 120000, 
                   label = paste0("Date of\nsocial distancing:\n", 
                                  as.character(date_sd_i, format = "%d %b %C")), 
                   hjust = 1),
              color = "red4", size = 3, check_overlap = TRUE, show.legend = FALSE) +
    geom_vline(xintercept = date_lockdown_i, col = "red4") +
    geom_text(aes_(x = date_lockdown_i + 1, y = 190000, 
                   label = paste0("Date of\nlockdown:\n", 
                                  as.character(date_lockdown_i, format = "%d %b %C")), 
                   hjust = 0),
              color = "red4", size = 3, check_overlap = TRUE, show.legend = FALSE) +
    geom_line(data = summary_data_i, 
              aes(x = Date, y = Mean),
              color = "navyblue", size = 1) +
    geom_ribbon(aes(ymin = C_025, ymax = C_975),
                fill = "navyblue", alpha = 0.25) +
    geom_text(data = summary_data_i, 
              aes(x = Date, y = Mean,
                  label = ifelse(Date == date_T, formatC(Mean, 
                                                         format = "f", big.mark = ",", digits = 0), "")),
              vjust = -1, size = 3, color = "navyblue", inherit.aes = FALSE) +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
    scale_x_date(name = "Date", 
                 limits = c(date_0, date_T + 7), 
                 date_breaks = "1 week", 
                 date_labels = "%d %b %C",
                 expand = expansion(mult = c(0, 0))) +
    scale_y_continuous(name = "Number of lab-confirmed cases",
                       limits = c(0, 240000), 
                       breaks = seq(0, 240000, 30000),
                       labels = comma_format(accuracy = 1),
                       expand = expansion(mult = c(0, 0)))
  #p
  
  # Add plot to list
  plot_cases_cum_sim[[i]] <- p
  
}

# Save plots
p <- ggarrange(plotlist = plot_cases_cum_sim, nrow = length(plot_cases_cum_sim))
g <- annotate_figure(p, top = text_grob("Cumulative lab-confirmed cases of COVID-19 in England\n(Pillar 1)", size = 15),
                     bottom = text_grob("Data from https://www.gov.uk/guidance/coronavirus-covid-19-information-for-the-public.", size = 10))
ggsave(paste0(out, "Plot - true vs counterfactual - cumulative cases.png"),
       plot = g, width = 6, height = 4*length(plot_cases_cum_sim))

# Remove plotting objects
rm(i, simulation_i, description_i, date_sd_i, date_lockdown_i, summary_data_i, p, g)

## Plot exponential growth -----------------------------------------------------

# Cumulative vs incident cases - base plot
plot_exp_growth_cases <- ggplot(data = filter(cases_eng, Date <= date_T),
                                aes(x = Cumulative_cases_beg, 
                                    y = Daily_cases)) +
  theme_minimal() +
  theme(plot.margin = unit(c(1, 1, 1, 1), "cm")) +
  labs(title = "Exponential growth of COVID-19 cases in England (Pillar 1)",
       subtitle = "Cumulative versus incident cases",
       caption = paste0("Data from https://www.gov.uk/guidance/coronavirus-covid-19-information-for-the-public,\n up to ", 
                        as.character(date_T, format = "%d %b %C"), ".")) +
  geom_path() +
  geom_point(size = 1) +
  geom_point(data = subset(cases_eng, Date == date_sd),
             size = 3, color = "blue", shape = 17) +
  geom_text(data = subset(cases_eng, Date == date_sd), color = "blue",
            label = paste0("Date of social distancing:\n", as.character(date_sd, format = "%d %b")),
            hjust = 0, vjust = 1, position = position_nudge(x = 2000), size = 3) +
  geom_point(data = subset(cases_eng, Date == date_lockdown), 
             size = 3, color = "blue", shape = 17) +
  geom_text(data = subset(cases_eng, Date == date_lockdown), color = "blue",
            label = paste0("Date of lockdown:\n", as.character(date_lockdown, format = "%d %b")),
            hjust = 0, vjust = 1, position = position_nudge(x = 2000), size = 3) +
  scale_x_continuous(name = "Cumulative number of lab-confirmed cases",
                     limits = c(0, 160000),
                     breaks = seq(0, 160000, 20000),
                     labels = comma_format(accuracy = 1),
                     expand = expansion(mult = c(0, 0))) + 
  scale_y_continuous(name = "New daily number of lab-confirmed cases",
                     limits = c(0, 6000),
                     breaks = seq(0, 6000, 1000),
                     labels = comma_format(accuracy = 1),
                     expand = expansion(mult = c(0, 0)))

# Add fitted lines corresponding to best knot dates onto base plot
for (i in 1:nrow(knots_best)) {
  
  # Filter best knots dataset
  knots_best_i <- knots_best[i, ]
  
  # Define knot date pair
  knot_date_1 <- knots_best_i %>% pull(Knot_date_1)
  knot_date_2 <- knots_best_i %>% pull(Knot_date_2)
  
  # Calculate cumulative cases at dates of knot
  knot_1 <- cases_eng_100 %>% filter(Date == knot_date_1) %>% pull(Cumulative_cases_beg)
  knot_2 <- cases_eng_100 %>% filter(Date == knot_date_2) %>% pull(Cumulative_cases_beg)
  
  # Define slopes associated with knot date pair
  slope_1 <- knots_best_i %>% pull(Growth_factor_1) - 1
  slope_2 <- knots_best_i %>% pull(Growth_factor_2) - 1
  slope_3 <- knots_best_i %>% pull(Growth_factor_3) - 1
  
  # Define intercepts associated with knot date pair
  int_1 <- 0  # segment 1 (slope fixed at 0)
  int_2 <- (int_1 + slope_1*knot_1) - slope_2*knot_1  # segment 2
  int_3 <- (int_2 + slope_2*knot_2) - slope_3*knot_2  # segment 3
  
  # Add fitted lines to plot
  plot_exp_growth_cases <- plot_exp_growth_cases +
    geom_segment(aes_(x = min(cases_eng_100$Cumulative_cases_beg), xend = knot_1,
                     y = int_1 + slope_1*min(cases_eng_100$Cumulative_cases_beg), 
                     yend = int_1 + slope_1*knot_1),
                 color = "red", size = 0.25, linetype = "dashed") + 
    geom_segment(aes_(x = knot_1, xend = knot_2,
                     y = int_2 + slope_2*knot_1, 
                     yend = int_2 + slope_2*knot_2),
                 color = "orange", size = 0.25, linetype = "dashed") +
    geom_segment(aes_(x = knot_2, xend = max(cases_eng_100$Cumulative_cases_beg),
                     y = int_3 + slope_3*knot_2, 
                     yend = int_3 + slope_3*max(cases_eng_100$Cumulative_cases_beg)),
                 color = "green", size = 0.25, linetype = "dashed")
  
}
#plot_exp_growth_cases

# Save plot
ggsave(paste0(out, "Plot - Cumulative vs incident cases - normal scale.png"),
       plot = plot_exp_growth_cases, width = 6, height = 6)

# Remove plotting objects
rm(i, knots_best_i, knot_date_1, knot_date_2, knot_1, knot_2, 
   slope_1, slope_2, slope_3, int_1, int_2, int_3)

## Plot combined figures -------------------------------------------------------

# Combine Simulation and Description columns into 1 factor
summary_daily_cases_sim_factor <- summary_daily_cases_sim %>% 
  filter(Simulation != "True") %>%
  unite(Sim_Desc, c(Simulation, Description), sep = ": ", remove = FALSE) %>%
  mutate(Sim_Desc = factor(Sim_Desc,
                           levels = c("Natural history: Social distancing and lockdown as implemented",
                                      "Counterfactual history: Social distancing and lockdown 1 week earlier",
                                      "Counterfactual history: Social distancing and lockdown 2 weeks earlier"))) 
summary_cumulative_cases_end_sim_factor <- summary_cumulative_cases_end_sim %>%
  filter(Simulation != "True") %>%
  unite(Sim_Desc, c(Simulation, Description), sep = ": ", remove = FALSE) %>%
  mutate(Sim_Desc = factor(Sim_Desc,
                           levels = c("Natural history: Social distancing and lockdown as implemented",
                                      "Counterfactual history: Social distancing and lockdown 1 week earlier",
                                      "Counterfactual history: Social distancing and lockdown 2 weeks earlier"))) 

# (1) Plot incident cases
## Observed
plot_cases_inc_sim_all <- ggplot(data = filter(cases_eng, Date <= date_T),
       aes(x = Date, y = Daily_cases)) +
  theme_minimal() +
  theme(plot.margin = unit(c(0.5, 1, 0.5, 1), "cm"),
        axis.text.x = element_text(angle = 90, vjust = 0.5), 
        legend.position = "bottom") +
  labs(title = "Incident lab-confirmed cases",
       subtitle = "(Pillar 1)") +
  geom_col(alpha = 0.4) +
  scale_x_date(name = "Date", 
               limits = c(date_0, date_T + 7), 
               date_breaks = "1 week", 
               date_labels = "%d %b %C",
               expand = expansion(mult = c(0, 0))) +
  scale_y_continuous(name = "Number of lab-confirmed cases",
                     limits = c(0, 6000), 
                     breaks = seq(0, 6000, 1000),
                     labels = comma_format(accuracy = 1),
                     expand = expansion(mult = c(0.01, 0.01)))
## Add simulated lines
plot_cases_inc_sim_all <- plot_cases_inc_sim_all +
  geom_line(data = summary_daily_cases_sim_factor,
            aes(x = Date, y = Mean, color = Sim_Desc),
            size = 1) +
  geom_ribbon(data = summary_daily_cases_sim_factor,
              aes(x = Date, y = Mean, fill = Sim_Desc, ymin = C_025, ymax = C_975),
              alpha = 0.2) +
  guides(colour = guide_legend(nrow = 3),
         fill = guide_legend(nrow = 3)) +
  scale_color_manual(name = "Simulation:",
                     values = c("navyblue", "darkorchid", "mediumseagreen")) +
  scale_fill_manual(name = "Simulation:",
                    values = c("navyblue", "darkorchid", "mediumseagreen"))


# (1) Plot cumulative cases
## Observed
plot_cases_cum_sim_all <- ggplot(data = filter(cases_eng, Date <= date_T),
                                 aes(x = Date, y = Cumulative_cases_end)) +
  theme_minimal() +
  theme(plot.margin = unit(c(0.5, 1, 0.5, 1), "cm"),
        axis.text.x = element_text(angle = 90, vjust = 0.5),
        legend.position = "bottom") +
  labs(title = "Cumulative lab-confirmed cases",
       subtitle = "(Pillar 1)") +
  geom_col(alpha = 0.4) +
  scale_x_date(name = "Date", 
               limits = c(date_0, date_T + 7), 
               date_breaks = "1 week", 
               date_labels = "%d %b %C",
               expand = expansion(mult = c(0, 0))) +
  scale_y_continuous(name = "Number of lab-confirmed cases",
                     limits = c(0, 240000), 
                     breaks = seq(0, 240000, 30000),
                     labels = comma_format(accuracy = 1),
                     expand = expansion(mult = c(0, 0)))
## Add simulated lines
plot_cases_cum_sim_all <- plot_cases_cum_sim_all +
  geom_line(data = summary_cumulative_cases_end_sim_factor,
            aes(x = Date, y = Mean, color = Sim_Desc),
            size = 1) +
  geom_ribbon(data = summary_cumulative_cases_end_sim_factor,
              aes(x = Date, y = Mean, fill = Sim_Desc, ymin = C_025, ymax = C_975),
              alpha = 0.2) +
  geom_shadowtext(data = summary_cumulative_cases_end_sim_factor, 
                  aes(x = Date, y = Mean,
                      label = ifelse(Date == date_T, formatC(Mean, 
                                                             format = "f", big.mark = ",", digits = 0), ""),
                      color = Sim_Desc),
                  vjust = -1, size = 3, bg.color = "white", bg.r = 0.25, 
                  inherit.aes = FALSE, show.legend = FALSE) +
  guides(colour = guide_legend(nrow = 3),
         fill = guide_legend(nrow = 3)) +
  scale_color_manual(name = "Simulation:",
                     values = c("navyblue", "darkorchid", "mediumseagreen")) +
  scale_fill_manual(name = "Simulation:",
                    values = c("navyblue", "darkorchid", "mediumseagreen"))

# Combind plots and save
plot_cases_sim_all <- ggarrange(plotlist = list(plot_cases_inc_sim_all, plot_cases_cum_sim_all), align = "h",
                                legend.grob = get_legend(plot_cases_inc_sim_all), legend = "bottom", 
                                nrow = 1, ncol = 2)
ggsave(paste0(out, "Plot - true vs counterfactual - all cases combined.png"),
       plot = plot_cases_sim_all, width = 6*2, height = 6)

# Remove plotting objects
rm(summary_daily_cases_sim_factor, summary_cumulative_cases_end_sim_factor)

