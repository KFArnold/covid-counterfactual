# ------------------------------------------------------------------------------
# Notes
# ------------------------------------------------------------------------------

# This script simulates the natural and counterfactual histories of Covid-19 growth

# ------------------------------------------------------------------------------
# Set up
# ------------------------------------------------------------------------------

# Load required packages
library(tidyverse); library(lspline); library(forecast); library(ggpubr); library(scales)

# Run source code to import and format data
source("./Modelling - England/Import and format data.R")

# Set simulation-specific directory
path <- paste0("./Modelling - England/2 knots/")

# Set storage directory for outputs
# (default is simulation directory)
out <- path

# Import file containing best knot points
knots_best <- read_csv(paste0(path, "Best knot points.csv"))

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

# ------------------------------------------------------------------------------
# Estimate simulation parameters
# ------------------------------------------------------------------------------

# Estimate simulation parameters for spline with specified knot points

# View dataframe containing best knot points
knots_best

# Choose knot dates which result in best fit to overall cumulative cases
knot_date_1 <- filter(knots_best, RMSE_cum == min(RMSE_cum))$Knot_date_1  
knot_date_2 <- filter(knots_best, RMSE_cum == min(RMSE_cum))$Knot_date_2  

# Create folder for storing knot-specific outputs if none exists
# and set as storage directory for outputs
folder <- paste(knot_date_1, "and", knot_date_2, sep = " ")
if (!dir.exists(paste0(path, folder))) {
  dir.create(paste0(path, folder))
} else {
  print("Folder already exists")
}
out <- paste0(path, folder, "/")

# Calculate cumulative cases at date of knot
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

# Calculate number of time points in each spline segment
n_1 <- nrow(filter(cases_eng_100, Date <= knot_date_1))
n_2 <- nrow(filter(cases_eng_100, Date > knot_date_1 & Date <= knot_date_2))
n_3 <- nrow(filter(cases_eng_100, Date > knot_date_2))

# Record model parameters
## Slope and intercept
spline_int_1 <- 0
spline_slope_1 <- coef(spline)[["Cumulative_cases_beg_1"]]  # slope of segement 1
spline_slope_2 <- coef(spline)[["Cumulative_cases_beg_2"]]  # slope of segement 2
spline_slope_3 <- coef(spline)[["Cumulative_cases_beg_3"]]  # slope of segement 3
spline_int_2 <- (spline_int_1 + spline_slope_1*knot_1) - spline_slope_2*knot_1
spline_int_3 <- (spline_int_2 + spline_slope_2*knot_2) - spline_slope_3*knot_2

# Calculate growth factors pre- and post-knot
growth_factor_1 <- spline_slope_1 + 1; growth_factor_1
growth_factor_2 <- spline_slope_2 + 1; growth_factor_2
growth_factor_3 <- spline_slope_3 + 1; growth_factor_3

# Estimate SD of growth factor pre- and post-knot
growth_factor_sd_1 <- cases_eng_100 %>% filter(Date <= knot_date_1) %>% 
  summarise(sd(Growth_factor)) %>% as.numeric
growth_factor_sd_2 <- cases_eng_100 %>% filter(Date > knot_date_1 & Date <= knot_date_2) %>% 
  summarise(sd(Growth_factor)) %>% as.numeric
growth_factor_sd_3 <- cases_eng_100 %>% filter(Date > knot_date_2) %>%
  summarise(sd(Growth_factor)) %>% as.numeric

## Plot exponential growth -----------------------------------------------------

# Untransformed axes
plot_exp_growth_cases <- ggplot(data = filter(cases_eng, Date <= date_T),
                                aes(x = Cumulative_cases_beg, 
                                    y = Daily_cases)) +
  theme_minimal() +
  theme(plot.margin = unit(c(1, 1, 1, 1), "cm")) +
  labs(title = "Exponential growth of Covid-19 cases in England",
       subtitle = "Cumulative versus incident cases",
       caption = paste0("Data from https://coronavirus.data.gov.uk,\n up to ", 
                        as.character(date_T, format = "%d %b %C"), ".")) +
  geom_path() +
  geom_point(size = 1) +
  geom_segment(aes(x = min(cases_eng_100$Cumulative_cases_beg), xend = knot_1,
                   y = spline_int_1 + spline_slope_1*min(cases_eng_100$Cumulative_cases_beg), 
                   yend = spline_int_1 + spline_slope_1*knot_1),
               color = "red") + 
  geom_segment(aes(x = knot_1, xend = knot_2,
                   y = spline_int_2 + spline_slope_2*knot_1, 
                   yend = spline_int_2 + spline_slope_2*knot_2),
               color = "orange") +
  geom_segment(aes(x = knot_2, xend = max(cases_eng_100$Cumulative_cases_beg),
                   y = spline_int_3 + spline_slope_3*knot_2, 
                   yend = spline_int_3 + spline_slope_3*max(cases_eng_100$Cumulative_cases_beg)),
               color = "green") +
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
  geom_point(data = subset(cases_eng, Date == knot_date_1), 
             size = 3, shape = 15) +
  geom_text(data = subset(cases_eng, Date == knot_date_1), 
            label = paste0(as.character(knot_date_1, format = "%d %b")),
            hjust = 0.5, vjust = 0, position = position_nudge(y = 200), size = 3) +
  geom_point(data = subset(cases_eng, Date == knot_date_2), 
             size = 3, shape = 15) +
  geom_text(data = subset(cases_eng, Date == knot_date_2), 
            label = paste0(as.character(knot_date_2, format = "%d %b")),
            hjust = 0.5, vjust = 1, position = position_nudge(y = -200), size = 3) +
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
#plot_exp_growth_cases

plot_exp_growth_cases_log <- ggplot(data = filter(cases_eng, Date <= date_T),
                                    aes(x = Cumulative_cases_beg, y = Daily_cases)) +
  theme_minimal() +
  theme(plot.margin = unit(c(1, 1, 1, 1), "cm")) +
  labs(title = "Exponential growth of Covid-19 cases in England",
       subtitle = "Cumulative versus incident cases",
       caption = paste0("Data from https://coronavirus.data.gov.uk,\n up to ", 
                        as.character(date_T, format = "%d %b %C"), ".")) +
  geom_path() +
  geom_point(size = 1) +
  geom_segment(aes(x = min(cases_eng_100$Cumulative_cases_beg), xend = knot_1,
                   y = spline_int_1 + spline_slope_1*min(cases_eng_100$Cumulative_cases_beg), 
                   yend = spline_int_1 + spline_slope_1*knot_1),
               color = "red") + 
  geom_segment(aes(x = knot_1, xend = knot_2,
                   y = spline_int_2 + spline_slope_2*knot_1, 
                   yend = spline_int_2 + spline_slope_2*knot_2),
               color = "orange") +
  geom_segment(aes(x = knot_2, xend = max(cases_eng_100$Cumulative_cases_beg),
                   y = spline_int_3 + spline_slope_3*knot_2, 
                   yend = spline_int_3 + spline_slope_3*max(cases_eng_100$Cumulative_cases_beg)),
               color = "green") +
  geom_point(data = subset(cases_eng, Date == date_sd),
             size = 3, color = "blue", shape = 17) +
  geom_text(data = subset(cases_eng, Date == date_sd), color = "blue",
            label = paste0("Date of social distancing:\n", as.character(date_sd, format = "%d %b")),
            hjust = 0, vjust = 1, position = position_nudge(x = 0.1), size = 3) +
  geom_point(data = subset(cases_eng, Date == date_lockdown), 
             size = 3, color = "blue", shape = 17) +
  geom_text(data = subset(cases_eng, Date == date_lockdown), color = "blue",
            label = paste0("Date of lockdown:\n", as.character(date_lockdown, format = "%d %b")),
            hjust = 0, vjust = 1, position = position_nudge(x = 0.1), size = 3) +
  geom_point(data = subset(cases_eng, Date == knot_date_1), 
             size = 3, shape = 15) +
  geom_text(data = subset(cases_eng, Date == knot_date_1), 
            label = paste0(as.character(knot_date_1, format = "%d %b")),
            hjust = 1, vjust = 0, position = position_nudge(x = -0.1, y = 0.1), size = 3) +
  geom_point(data = subset(cases_eng, Date == knot_date_2), 
             size = 3, shape = 15) +
  geom_text(data = subset(cases_eng, Date == knot_date_2), 
            label = paste0(as.character(knot_date_2, format = "%d %b")),
            position = position_nudge(y = 0.2), size = 3) +
  scale_x_continuous(name = "Cumulative number of lab-confirmed cases (logarithmic scale)", 
                     trans = "log10", 
                     limits = c(1, 10^6), 
                     breaks = trans_breaks("log10", function(x) 10^x),
                     labels = comma_format(accuracy = 1),
                     expand = expansion(mult = c(0, 0))) + 
  scale_y_continuous(name = "New daily number of lab-confirmed cases (logarithmic scale)",
                     trans = "log10",
                     limits = c(1, 10^5), 
                     breaks = trans_breaks("log10", function(x) 10^x),
                     labels = comma_format(accuracy = 1),
                     expand = expansion(mult = c(0, 0))) 
#plot_exp_growth_cases_log

# Save plots
ggsave(paste0(out, "Plot - Cumulative vs incident cases - normal scale.png"),
       plot = plot_exp_growth_cases, width = 6, height = 6)
#ggsave(paste0(out, "Plot - Cumulative vs incident cases - log scale.png"),
#       plot = plot_exp_growth_cases_log, width = 6, height = 6)

# ------------------------------------------------------------------------------
# Estimate natural and counterfactual histories
# ------------------------------------------------------------------------------

## Set simulation parameters ---------------------------------------------------

# Define knots to be simulated
knots_simulation <- bind_rows(tibble(Date_sd = date_sd, Date_lockdown = date_lockdown,
                                     Knot_date_1 = knot_date_1, Knot_date_2 = knot_date_2,
                                     Simulation = "Natural history",
                                     Description = "Social distancing and lockdown as implemented"),
                              tibble(Date_sd = date_sd - 7, Date_lockdown = date_lockdown - 7,
                                     Knot_date_1 = knot_date_1 - 7, Knot_date_2 = knot_date_2 - 7,
                                     Simulation = "Counterfactual history",
                                     Description = "Social distancing and lockdown 1 week earlier"),
                              tibble(Date_sd = date_sd - 14, Date_lockdown = date_lockdown - 14,
                                     Knot_date_1 = knot_date_1 - 14, Knot_date_2 = knot_date_2 - 14,
                                     Simulation = "Counterfactual history",
                                     Description = "Social distancing and lockdown 2 weeks earlier"))

# Define number of scenarios to simulate
n_sim <- nrow(knots_simulation)

# Define number of simulation runs per scenario
n_runs <- 20000

# Set dates to simulate
dates <- seq.Date(from = date_100, to = date_T, by = 1)

## Create dataframes to store simulation outputs -------------------------------

# Create dataframe to store all modelled data
cases_eng_sim <- tibble(Simulation = character(),
                        Date_sd = as.Date(character()),
                        Date_lockdown = as.Date(character()),
                        Knot_date_1 = as.Date(character()),
                        Knot_date_2 = as.Date(character()), 
                        Run = numeric(),
                        Date = as.Date(character()),
                        Cumulative_cases_beg = numeric(),
                        Daily_cases = numeric(),
                        Cumulative_cases_end = numeric(),
                        Growth_factor = numeric())

# Create dataframe for 1 round of simulated data
# Set all cases to NA
sim_data <- cases_eng %>% filter(Date >= (date_100 - 1) & Date <= date_T) %>% 
  mutate(Simulation = as.character(NA)) %>% 
  mutate(Date_sd = as.Date(NA)) %>% mutate(Date_lockdown = as.Date(NA)) %>%
  mutate(Knot_date_1 = as.Date(NA)) %>% mutate(Knot_date_2 = as.Date(NA)) %>% 
  mutate(Run = as.numeric(NA)) %>% select(c(names(cases_eng_sim)))
sim_data[sim_data$Date >= date_100, 
         c("Cumulative_cases_beg", 
           "Daily_cases", 
           "Cumulative_cases_end",
           "Growth_factor")] <- NA

# Create dataframe to store estimated deaths
deaths_eng_sim <- tibble(Simulation = character(),
                         Date_sd = as.Date(character()),
                         Date_lockdown = as.Date(character()),
                         Knot_date_1 = as.Date(character()),
                         Knot_date_2 = as.Date(character()), 
                         Run = numeric(), 
                         CFR = numeric(),
                         Cumulative_cases = numeric(),
                         Estimated_deaths = numeric())

# Create dataframes to store summaries of cases, deaths, and growth factors
# Mean, 2.5 and 97.5 centiles
summary_cases_eng_sim <- tibble(Simulation = character(),
                                Date_sd = as.Date(character()),
                                Date_lockdown = as.Date(character()),
                                Knot_date_1 = as.Date(character()),
                                Knot_date_2 = as.Date(character()),
                                Date = as.Date(character()),
                                Mean_daily_cases = as.numeric(),
                                C025_daily_cases = as.numeric(),
                                C975_daily_cases = as.numeric(),
                                Mean_cumulative_cases_end = as.numeric(),
                                C025_cumulative_cases_end = as.numeric(),
                                C975_cumulative_cases_end = as.numeric())
summary_growth_eng_sim <- tibble(Simulation = character(),
                                 Date_sd = as.Date(character()),
                                 Date_lockdown = as.Date(character()),
                                 Knot_date_1 = as.Date(character()),
                                 Knot_date_2 = as.Date(character()),
                                 Date = as.Date(character()),
                                 Mean_growth = as.numeric(),
                                 C025_growth = as.numeric(),
                                 C975_growth = as.numeric())
summary_deaths_eng_sim <- tibble(Simulation = character(),
                                 Date_sd = as.Date(character()),
                                 Date_lockdown = as.Date(character()),
                                 Knot_date_1 = as.Date(character()),
                                 Knot_date_2 = as.Date(character()),
                                 Date = as.Date(character()),
                                 CFR = as.numeric(),
                                 Mean_estimated_deaths = as.numeric(),
                                 C025_estimated_deaths = as.numeric(),
                                 C975_estimated_deaths = as.numeric())

## Simulate natural and counterfactual histories -------------------------------

# Set seed
set.seed(20)

# Iterate through hypothetical interventions
start <- Sys.time()
for (k in 1:n_sim) {
  
  # Define simulation as natural or counterfactual history
  simulation_k <- knots_simulation[[k, "Simulation"]]
  
  # Define social distancing and lockdown dates
  date_sd_k <- knots_simulation[[k, "Date_sd"]]
  date_lockdown_k <- knots_simulation[[k, "Date_lockdown"]]
  
  # Define knot dates
  knot_date_1_k <- knots_simulation[[k, "Knot_date_1"]]
  knot_date_2_k <- knots_simulation[[k, "Knot_date_2"]]
  
  # Calculate cumulative cases at date of knot
  knot_1_k <- subset(cases_eng_100, Date == knot_date_1_k)$Cumulative_cases_beg
  knot_2_k <- subset(cases_eng_100, Date == knot_date_2_k)$Cumulative_cases_beg
  
  # Create dataframe for simulated counterfactual data 
  sim_data_k <- sim_data[0, ]
  deaths_k <- deaths_eng_sim[0, ]
  
  # Iterate through simulation runs
  for (i in 1:n_runs) {
  
    # Create dataframe for 1 run
    sim_data_i <- sim_data %>% mutate(Simulation = simulation_k) %>%
      mutate(Date_sd = date_sd_k) %>% mutate(Date_lockdown = date_lockdown_k) %>%
      mutate(Knot_date_1 = knot_date_1_k) %>% mutate(Knot_date_2 = knot_date_2_k) %>% mutate(Run = i)
    
    for (t in as.list(dates)) {
      
      # Get cumulative cases at beginning of time t
      cum_t_beg <- as.numeric(sim_data_i[sim_data_i$Date == (t-1), "Cumulative_cases_end"])
      sim_data_i[sim_data_i$Date == t, "Cumulative_cases_beg"] <- cum_t_beg
      
      # Get incident cases at time t-1
      inc_tminus1 <- as.numeric(sim_data_i[sim_data_i$Date == (t-1), "Daily_cases"])
      
      # Calculate daily cases at time t
      inc_t <- -1  #  cases must be positive
      while (inc_t < 0) {
        
        # Define growth parameters
        if (t <= knot_date_1_k) {
          growth <- rlnorm(n = 1,
                           meanlog = Calculate_mean_log(mean = growth_factor_1, sd = growth_factor_sd_1),
                           sdlog = Calculate_sd_log(mean = growth_factor_1, sd = growth_factor_sd_1))
          #growth <- growth_factor_1
        } else if (t <= knot_date_2_k) {
          growth <- rlnorm(n = 1,
                           meanlog = Calculate_mean_log(mean = growth_factor_2, sd = growth_factor_sd_2),
                           sdlog = Calculate_sd_log(mean = growth_factor_2, sd = growth_factor_sd_2))
          #growth <- growth_factor_2
        } else {
          growth <- rlnorm(n = 1,
                           meanlog = Calculate_mean_log(mean = growth_factor_3, sd = growth_factor_sd_3),
                           sdlog = Calculate_sd_log(mean = growth_factor_3, sd = growth_factor_sd_3))
          #growth <- growth_factor_3
        }
        
        # Calculate daily cases at time t and record
        inc_t <- round(growth*inc_tminus1)

      }
      
      # Record daily cases at time t
      sim_data_i[sim_data_i$Date == t, "Daily_cases"] <- inc_t
      
      # Calculate cumulative cases at end of time t and record
      cum_t_end <- as.numeric(cum_t_beg + inc_t)
      sim_data_i[sim_data_i$Date == t, "Cumulative_cases_end"] <- cum_t_end
      
      # Record growth factor
      growth_factor <- as.numeric(inc_t / inc_tminus1)
      sim_data_i[sim_data_i$Date == t, "Growth_factor"] <- growth_factor
      
    }
    
    # Add simulated data to counterfactual dataframe
    sim_data_k <- bind_rows(sim_data_k, sim_data_i)
    
    # Estimate number of deaths in simulation run and add to counterfactual
    deaths_i <- tibble(Simulation = simulation_k,
                       Date_sd = date_sd_k,
                       Date_lockdown = date_lockdown_k,
                       Knot_date_1 = knot_date_1_k,
                       Knot_date_2 = knot_date_2_k,
                       Run = c(rep(i, 2)), 
                       CFR = c(cfr_hosp_dod, cfr_hosp_dor),
                       Cumulative_cases = tail(sim_data_i$Cumulative_cases_end, n = 1)) %>%
      mutate(Estimated_deaths = round(CFR * Cumulative_cases))
    deaths_k <- bind_rows(deaths_k, deaths_i)
    
    # Display progress of simulation
    cat('\r', paste(round((i / n_runs * 100), 0), 
                    "% done of simulation", k, "of", n_sim, "     ", sep = " "))
    
  }
  
  # Record summaries of model output (mean, 2.5 and 97.5 centiles)
  ## (1) Cases
  cases_mean <- sim_data_k %>% filter() %>% group_by(Date) %>% 
    summarise(Mean_daily_cases = round(mean(Daily_cases)),
              Mean_cumulative_cases_end = round(mean(Cumulative_cases_end)))
  cases_c025 <- sim_data_k %>% group_by(Date) %>% 
    summarise(C025_daily_cases = round(quantile(Daily_cases, 0.025)),
              C025_cumulative_cases_end = round(quantile(Cumulative_cases_end, 0.025)))
  cases_c975 <- sim_data_k %>% group_by(Date) %>% 
    summarise(C975_daily_cases = round(quantile(Daily_cases, 0.975)),
              C975_cumulative_cases_end = round(quantile(Cumulative_cases_end, 0.975)))
  cases_summary <- full_join(cases_mean, cases_c025, by = "Date") %>% 
    full_join(., cases_c975, by = "Date") %>% mutate(Simulation = simulation_k) %>%
    mutate(Date_sd = date_sd_k) %>% mutate(Date_lockdown = date_lockdown_k) %>%
    mutate(Knot_date_1 = knot_date_1_k) %>% mutate(Knot_date_2 = knot_date_2_k) %>%
    select(names(summary_cases_eng_sim))
  summary_cases_eng_sim <- bind_rows(summary_cases_eng_sim, cases_summary)
  ## (2) Growth factors
  growth_mean <- sim_data_k %>% group_by(Date) %>% filter(Growth_factor != -Inf) %>% filter(Growth_factor != Inf) %>%
    summarise(Mean_growth = round(mean(Growth_factor), 2))
  growth_c025 <- sim_data_k %>% group_by(Date) %>% filter(Growth_factor != -Inf) %>% filter(Growth_factor != Inf) %>%
    summarise(C025_growth = round(quantile(Growth_factor, 0.025), 2))
  growth_c975 <- sim_data_k %>% group_by(Date) %>% filter(Growth_factor != -Inf) %>% filter(Growth_factor != Inf) %>%
    summarise(C975_growth = round(quantile(Growth_factor, 0.975), 2))
  growth_summary <- full_join(growth_mean, growth_c025, by = "Date") %>% 
    full_join(., growth_c975, by = "Date") %>% mutate(Simulation = simulation_k) %>%
    mutate(Date_sd = date_sd_k) %>% mutate(Date_lockdown = date_lockdown_k) %>%
    mutate(Knot_date_1 = knot_date_1_k) %>% mutate(Knot_date_2 = knot_date_2_k) %>%
    select(names(summary_growth_eng_sim))
  summary_growth_eng_sim <- bind_rows(summary_growth_eng_sim, growth_summary)
  ## (3) Deaths
  deaths_mean <- deaths_k %>% group_by(CFR) %>% 
    summarise(Mean_estimated_deaths = round(mean(Estimated_deaths)))
  deaths_c025 <- deaths_k %>% group_by(CFR) %>% 
    summarise(C025_estimated_deaths = round(quantile(Estimated_deaths, 0.025)))
  deaths_c975 <- deaths_k %>% group_by(CFR) %>% 
    summarise(C975_estimated_deaths = round(quantile(Estimated_deaths, 0.975)))
  deaths_summary <- full_join(deaths_mean, deaths_c025, by = "CFR") %>% 
    full_join(., deaths_c975, by = "CFR") %>% mutate(Simulation = simulation_k) %>%
    mutate(Date_sd = date_sd_k) %>% mutate(Date_lockdown = date_lockdown_k) %>%
    mutate(Knot_date_1 = knot_date_1_k) %>% mutate(Knot_date_2 = knot_date_2_k) %>% 
    mutate(Date = max(sim_data_k$Date)) %>% select(names(summary_deaths_eng_sim))
  summary_deaths_eng_sim <- bind_rows(summary_deaths_eng_sim, deaths_summary)
  
  # Bind simulated counterfactual data to full dataframes
  cases_eng_sim <- bind_rows(cases_eng_sim, sim_data_k)
  deaths_eng_sim <- bind_rows(deaths_eng_sim, deaths_k)
  
}
end <- Sys.time()
end - start

# Select random sample of 500 simulation runs to save
set.seed(51)
select <- as.factor(sample(x = seq(1, n_runs), size = 500, replace = FALSE))
cases_eng_sim_sample <- cases_eng_sim[cases_eng_sim$Run %in% select, ]

# Export simulated data
write_csv(cases_eng_sim_sample, path = paste0(out, "Simulated data sample.csv"))
write_csv(deaths_eng_sim, path = paste0(out, "Estimated deaths.csv"))
write_csv(summary_cases_eng_sim, path = paste0(out, "Summary - cases.csv"))
write_csv(summary_growth_eng_sim, path = paste0(out, "Summary - growth factors.csv"))
write_csv(summary_deaths_eng_sim, path = paste0(out, "Summary - deaths.csv"))

# Export descriptions of simulated knots
write_csv(knots_simulation, path = paste0(out, "Hypothetical interventions.csv"))
