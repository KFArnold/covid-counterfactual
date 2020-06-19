# ------------------------------------------------------------------------------
# Notes
# ------------------------------------------------------------------------------

# This script simulates the natural and counterfactual histories of COVID-19 growth,
# using one of the 'best' pairs of knot points identified from 'Identify 2 knot points.R'.

# It also produces plots comparing simulated histories with true (observed) data.

# ------------------------------------------------------------------------------
# Set up
# ------------------------------------------------------------------------------

# Load required packages
packrat::restore()
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

# Function to summarise mean, 2.5 and 97.5 centiles
# Argument: vector of values (x)
Summarise_centiles <- function(x) {
  x <- x[!is.na(x) & !is.infinite(x)]
  c(Mean = mean(x), 
    C_025 = quantile(x, 0.025, names = FALSE), 
    C_975 = quantile(x, 0.975, names = FALSE))
}

# ------------------------------------------------------------------------------
# Estimate simulation parameters
# ------------------------------------------------------------------------------

# Estimate simulation parameters for ARIMA spline with specified knot points

# View dataframe containing best knot points
knots_best

# Choose knot dates which result in best fit to overall incident cases
knot_date_1 <- filter(knots_best, RMSE_inc == min(RMSE_inc))$Knot_date_1  
knot_date_2 <- filter(knots_best, RMSE_inc == min(RMSE_inc))$Knot_date_2  

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

# ------------------------------------------------------------------------------
# Estimate natural and counterfactual histories
# ------------------------------------------------------------------------------

## Set simulation parameters ---------------------------------------------------

# Define true knot points
knots_true <- tibble(Simulation = "True",
                     Description = "True",
                     Date_sd = date_sd, Date_lockdown = date_lockdown,
                     Knot_date_1 = knot_date_1, Knot_date_2 = knot_date_2)

# Define knots to be simulated
knots_simulation <- bind_rows(tibble(Simulation = "Natural history",
                                     Description = "Social distancing and lockdown as implemented",
                                     Date_sd = date_sd, Date_lockdown = date_lockdown,
                                     Knot_date_1 = knot_date_1, Knot_date_2 = knot_date_2),
                              tibble(Simulation = "Counterfactual history",
                                     Description = "Social distancing and lockdown 1 week earlier",
                                     Date_sd = date_sd - 7, Date_lockdown = date_lockdown - 7,
                                     Knot_date_1 = knot_date_1 - 7, Knot_date_2 = knot_date_2 - 7),
                              tibble(Simulation = "Counterfactual history",
                                     Description = "Social distancing and lockdown 2 weeks earlier",
                                     Date_sd = date_sd - 14, Date_lockdown = date_lockdown - 14,
                                     Knot_date_1 = knot_date_1 - 14, Knot_date_2 = knot_date_2 - 14))

# Define number of scenarios to simulate
n_sim <- nrow(knots_simulation)

# Define number of simulation runs per scenario
n_runs <- 50000

# Set dates to simulate
dates <- seq.Date(from = date_100, to = date_T, by = 1)

# Export descriptions of simulated knots
write_csv(knots_simulation, path = paste0(out, "Hypothetical interventions.csv"))

## Create dataframes to store simulation outputs -------------------------------

# Create matrices for simulated data
# (1 row per simulation run, 1 col per date)
daily_cases_sim_k <- cumulative_cases_end_sim_k <- growth_factor_sim_k <-
  matrix(nrow = n_runs, ncol = length(dates) + 1,
         dimnames = list(seq(from = 1, to = n_runs, by = 1),
                         as.character(seq.Date(from = date_100 - 1, to = date_T, by = 1))))
# Initialise matrices with data at date_100 - 1
daily_cases_sim_k[, 1] <- filter(cases_eng, Date == (date_100 - 1))$Daily_cases
cumulative_cases_end_sim_k[, 1] <- filter(cases_eng, Date == (date_100 - 1))$Cumulative_cases_end
growth_factor_sim_k[, 1] <- filter(cases_eng, Date == (date_100 - 1))$Growth_factor

# Create matrix for estimated deaths
# (1 row per simulation run, 1 col per CFR)
deaths_sim_k <- matrix(nrow = n_runs, ncol = 2,
                       dimnames = list(seq(from = 1, to = n_runs, by = 1),
                                       c("cfr_hosp_dod", "cfr_hosp_dor")))

# Create lists to store simulation outputs
data_sim <- list()  # daily cases, cumulative cases, growth factors
#deaths_sim <- list()  # estimated deaths

# Create lists to store summary outputs from all simulations
summary_daily_cases_sim <- list()  # daily cases
summary_cumulative_cases_end_sim <- list()  # cumulative cases
summary_growth_factor_sim <- list()  # growth factor
summary_deaths_sim <- list()  # deaths

## Simulate natural and counterfactual histories -------------------------------

# Set seed
set.seed(20)

# (1) Iterate through hypothetical interventions
start <- Sys.time()
for (k in 1:n_sim) {
  
  # Define simulation
  knots_simulation_k <- knots_simulation[k, ]
  
  # Define social distancing and lockdown dates
  date_sd_k <- knots_simulation_k[["Date_sd"]]
  date_lockdown_k <- knots_simulation_k[["Date_lockdown"]]
  
  # Define knot dates
  knot_date_1_k <- knots_simulation_k[["Knot_date_1"]]
  knot_date_2_k <- knots_simulation_k[["Knot_date_2"]]
  
  # Calculate cumulative cases at date of knot
  knot_1_k <- subset(cases_eng_100, Date == knot_date_1_k)$Cumulative_cases_beg
  knot_2_k <- subset(cases_eng_100, Date == knot_date_2_k)$Cumulative_cases_beg
  
  # (2) Iterate through dates
  for (t in as.list(dates)) {
    
    # Get daily and cumulative cases from time t-1
    inc_tminus1 <- daily_cases_sim_k[, as.character(t-1)]
    cum_tminus1 <- cumulative_cases_end_sim_k[, as.character(t-1)]
    
    # Define growth parameters
    if (t <= knot_date_1_k) {
      growth <- rlnorm(n = n_runs,
                       meanlog = Calculate_mean_log(mean = growth_factor_1, sd = growth_factor_sd_1),
                       sdlog = Calculate_sd_log(mean = growth_factor_1, sd = growth_factor_sd_1))
    } else if (t <= knot_date_2_k) {
      growth <- rlnorm(n = n_runs,
                       meanlog = Calculate_mean_log(mean = growth_factor_2, sd = growth_factor_sd_2),
                       sdlog = Calculate_sd_log(mean = growth_factor_2, sd = growth_factor_sd_2))
    } else {
      growth <- rlnorm(n = n_runs,
                       meanlog = Calculate_mean_log(mean = growth_factor_3, sd = growth_factor_sd_3),
                       sdlog = Calculate_sd_log(mean = growth_factor_3, sd = growth_factor_sd_3))
    }
    
    # Calculate daily cases at time t and record
    inc_t <- round(growth*inc_tminus1)
    daily_cases_sim_k[, as.character(t)] <- inc_t
    
    # Calculate cumulative cases at end of time t and record
    cum_t <- cum_tminus1 + inc_t
    cumulative_cases_end_sim_k[, as.character(t)] <- cum_t
    
    # Record growth factor
    growth_factor <- inc_t / inc_tminus1
    growth_factor_sim_k[, as.character(t)] <- growth_factor
    
    # Display progress of simulation
    cat('\r', paste(round((match(t, dates) / length(dates) * 100), 0), 
                    "% done of simulation", k, "of", n_sim, "                 ", sep = " "))
    
  }  # close loop (2)
  
  # Estimate number of deaths at date_T in simulation run
  for (i in 1:ncol(deaths_sim_k)) {
    cfr <- eval(parse(text = colnames(deaths_sim_k)[i]))
    deaths_sim_k[, i] <- round(cumulative_cases_end_sim_k[, as.character(date_T)] * cfr)
  }
  
  # Print status update
  cat('\r', paste("Summarising data from simulation", k, "of", n_sim, "...    ", sep = " "))
  
  # Record simulated data
  ## Daily cases, cumulative cases, and growth factors:
  daily_cases_sim <- daily_cases_sim_k %>% as_tibble(rownames = "Run") %>% 
    gather(Date, Daily_cases, dimnames(daily_cases_sim_k)[[2]]) %>% 
    mutate(Date = as.Date(Date)) %>% mutate(Run = as.numeric(Run))  # daily cases
  cumulative_cases_end_sim <- cumulative_cases_end_sim_k %>% as_tibble(rownames = "Run") %>% 
    gather(Date, Cumulative_cases_end, dimnames(daily_cases_sim_k)[[2]]) %>% 
    mutate(Date = as.Date(Date)) %>% mutate(Run = as.numeric(Run))  # cumulative cases
  growth_factor_sim <- growth_factor_sim_k %>% as_tibble(rownames = "Run") %>% 
    gather(Date, Growth_factor, dimnames(daily_cases_sim_k)[[2]]) %>% 
    mutate(Date = as.Date(Date)) %>% mutate(Run = as.numeric(Run))  # growth factor
  data_sim[[k]] <- full_join(daily_cases_sim, cumulative_cases_end_sim) %>% 
    full_join(., growth_factor_sim) %>% arrange(Run) %>% 
    mutate(knots_simulation_k) %>% relocate(names(knots_simulation_k))
  ### Estimated deaths:
  #deaths_sim[[k]] <- deaths_sim_k %>% as_tibble(rownames = "Run") %>%
  #  gather(CFR, Deaths, dimnames(deaths_sim_k)[[2]]) %>% 
  #  mutate(Date = date_T) %>% mutate(Run = as.numeric(Run)) %>%
  #  arrange(Run) %>% mutate(knots_simulation_k) %>% relocate(names(knots_simulation_k))  # deaths
  
  # Record summaries (mean, 2.5 and 97.5 centiles)
  ## Daily cases:
  summary <- apply(X = daily_cases_sim_k, MARGIN = 2, FUN = Summarise_centiles) %>%
    round %>% t %>% as_tibble(rownames = "Date") %>% mutate(Date = as.Date(Date))
  summary_daily_cases_sim[[k]] <- summary %>% mutate(knots_simulation_k) %>%
    relocate(names(knots_simulation_k))
  ## Cumulative cases:
  summary <- apply(X = cumulative_cases_end_sim_k, MARGIN = 2, FUN = Summarise_centiles) %>% 
    round %>% t %>% as_tibble(rownames = "Date") %>% mutate(Date = as.Date(Date))
  summary_cumulative_cases_end_sim[[k]] <- summary %>% mutate(knots_simulation_k) %>%
    relocate(names(knots_simulation_k))
  ## Growth factors:
  summary <- apply(X = growth_factor_sim_k, MARGIN = 2, FUN = Summarise_centiles) %>%
    t %>% as_tibble(rownames = "Date") %>% mutate(Date = as.Date(Date))
  summary_growth_factor_sim[[k]] <- summary %>% mutate(knots_simulation_k) %>%
    relocate(names(knots_simulation_k))
  ## Deaths:
  summary <- apply(X = deaths_sim_k, MARGIN = 2, FUN = Summarise_centiles) %>% 
    round %>% t %>% as_tibble(rownames = "CFR") %>% 
    mutate(CFR_value = sapply(CFR, function(x) eval(parse(text = x)))) %>%
    relocate(CFR_value, .after = CFR)
  summary_deaths_sim[[k]] <- summary %>% mutate(knots_simulation_k) %>%
    mutate(Date = date_T) %>%
    relocate(names(knots_simulation_k), Date)
  
}  # close loop (1)
end <- Sys.time()
end - start

# Combine simulated data from all natural and counterfactual histories
data_sim <- bind_rows(data_sim)  # daily cases, cumulative cases, growth factors
#deaths_sim <- bind_rows(deaths_sim)  # deaths

# Combine summary results from all natural and counterfactual histories 
summary_daily_cases_sim <- bind_rows(summary_daily_cases_sim) 
summary_cumulative_cases_end_sim <- bind_rows(summary_cumulative_cases_end_sim) 
summary_growth_factor_sim <- bind_rows(summary_growth_factor_sim) 
summary_deaths_sim <- bind_rows(summary_deaths_sim) 

# Combine summary data with true (observed) data
summary_daily_cases_sim <- bind_rows(tibble(knots_true, 
                                            Date = cases_eng$Date,
                                            Mean = cases_eng$Daily_cases), 
                                     summary_daily_cases_sim)
summary_cumulative_cases_end_sim <- bind_rows(tibble(knots_true,
                                                     Date = cases_eng$Date,
                                                     Mean = cases_eng$Cumulative_cases_end),
                                              summary_cumulative_cases_end_sim)
summary_growth_factor_sim <- bind_rows(tibble(knots_true,
                                              Date = cases_eng$Date,
                                              Mean = cases_eng$Growth_factor),
                                       summary_growth_factor_sim)
summary_deaths_sim <- bind_rows(tibble(knots_true,
                                       Date = date_T,
                                       CFR = colnames(deaths_sim_k)) %>%
                                  mutate(CFR_value = sapply(CFR, function(x) eval(parse(text = x)))) %>%
                                  mutate(Mean = round(c(filter(deaths_hosp_dod_eng, Date == date_T)$Cumulative_deaths, 
                                                        filter(deaths_hosp_dor_eng, Date == date_T)$Cumulative_deaths))),
                                summary_deaths_sim)

# Export all summary data
write_csv(summary_daily_cases_sim, path = paste0(out, "Summary - daily cases.csv"))
write_csv(summary_cumulative_cases_end_sim, path = paste0(out, "Summary - cumulative cases.csv"))
write_csv(summary_growth_factor_sim, path = paste0(out, "Summary - growth factors.csv"))
write_csv(summary_deaths_sim, path = paste0(out, "Summary - deaths.csv"))

# ------------------------------------------------------------------------------
# Figures
# ------------------------------------------------------------------------------

# Select random sample of 500 simulation runs to plot
set.seed(51)
select <- as.factor(sample(x = seq(1, n_runs), size = 500, replace = FALSE))
data_sim_sample <- data_sim[data_sim$Run %in% select, ]

# Convert columns to factors
# sapply(data_sim_sample, class); sapply(summary_daily_cases_sim, class)
data_sim_sample <- data_sim_sample %>% mutate_at(vars(Simulation, Run), as.factor)
summary_daily_cases_sim <- summary_daily_cases_sim %>% mutate_at(vars(Simulation), as.factor)
summary_cumulative_cases_end_sim <- summary_cumulative_cases_end_sim %>% mutate_at(vars(Simulation), as.factor)

## True incident and cumulative cases against modelled means -------------------

# Incident cases
plot_cases_inc <- ggplot(data = cases_eng, 
                         aes(x = Date, y = Daily_cases)) +
  theme_minimal() +
  theme(plot.margin = unit(c(1, 1, 1, 1), "cm")) +
  labs(title = "Incident lab-confirmed cases of Covid-19 in England by date of testing",
       subtitle = " ",
       caption = "Data from https://coronavirus.data.gov.uk.") +
  geom_col(alpha = 0.4) +
  geom_line(data = filter(summary_daily_cases_sim, Simulation == "Natural history"), 
            aes(x = Date, y = Mean), color = "blue", inherit.aes = FALSE) +
  geom_vline(xintercept = date_sd, col = "red") +
  geom_text(aes(x = date_sd - 1, y = 3000, 
                label = paste0("Date of social distancing:\n", 
                               as.character(date_sd, format = "%d %b %C")), 
                hjust = 1, color = "red"),
            check_overlap = TRUE, show.legend = FALSE) +
  geom_vline(xintercept = date_lockdown, col = "red") +
  geom_text(aes(x = date_lockdown + 1, y = 5000, 
                label = paste0("Date of lockdown:\n", 
                               as.character(date_lockdown, format = "%d %b %C")), 
                hjust = 0, color = "red"),
            check_overlap = TRUE, show.legend = FALSE) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  scale_x_date(name = "Date", 
               limits = c(date_0, date_T), 
               date_breaks = "1 week", 
               date_labels = "%d %b %C",
               expand = expansion(mult = c(0, 0))) +
  scale_y_continuous(name = "Number of lab-confirmed cases",
                     limits = c(0, 6000),
                     breaks = seq(0, 6000, 1000),
                     expand = expansion(mult = c(0.01, 0.01)))
#plot_cases_inc

# Cumulative cases
plot_cases_cum <- ggplot(data = cases_eng, 
                         aes(x = Date, y = Cumulative_cases_end)) +
  theme_minimal() +
  theme(plot.margin = unit(c(1, 1, 1, 1), "cm")) +
  labs(title = "Cumulative lab-confirmed cases of Covid-19 in England by date of testing",
       subtitle = " ",
       caption = "Data from https://coronavirus.data.gov.uk.") +
  geom_col(alpha = 0.4) +
  geom_line(data = filter(summary_cumulative_cases_end_sim, Simulation == "Natural history"), 
            aes(x = Date, y = Mean), 
            color = "blue", inherit.aes = FALSE) +
  geom_vline(xintercept = date_sd, col = "red") +
  geom_text(aes(x = date_sd - 1, y = 80000, 
                label = paste0("Date of social distancing:\n", 
                               as.character(date_sd, format = "%d %b %C")), 
                hjust = 1, color = "red"),
            check_overlap = TRUE, show.legend = FALSE) +
  geom_vline(xintercept = date_lockdown, col = "red") +
  geom_text(aes(x = date_lockdown + 1, y = 130000, 
                label = paste0("Date of lockdown:\n", 
                               as.character(date_lockdown, format = "%d %b %C")), 
                hjust = 0, color = "red"),
            check_overlap = TRUE, show.legend = FALSE) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  scale_x_date(name = "Date", 
               limits = c(date_0, date_T), 
               date_breaks = "1 week", 
               date_labels = "%d %b %C",
               expand = expansion(mult = c(0, 0))) +
  scale_y_continuous(name = "Number of lab-confirmed cases",
                     limits = c(0, 160000),
                     breaks = seq(0, 160000, 20000),
                     labels = comma_format(accuracy = 1),
                     expand = expansion(mult = c(0, 0)))
#plot_cases_cum

# Save plots
g <- ggarrange(plot_cases_inc, plot_cases_cum, nrow = 2)
ggsave(paste0(out, "Plot - True incident and cumulative cases vs modelled.png"),
       plot = g, width = 8, height = 10)

## Natural vs counterfactual histories -----------------------------------------

### Incident cases -------------------------------------------------------------

plot_cases_inc_sim <- list()

for (i in 1:nrow(knots_simulation)) {
  
  # Define as natural or counterfactual history
  simulation_i <- knots_simulation[[i, "Simulation"]]
  description_i <- knots_simulation[[i, "Description"]]
  
  # Calculate knot dates
  knot_date_1_i <- knots_simulation[[i, "Knot_date_1"]]
  knot_date_2_i <- knots_simulation[[i, "Knot_date_2"]]
  
  # Calculate dates of social distancing and lockdown
  date_sd_i <- knots_simulation[[i, "Date_sd"]]
  date_lockdown_i <- knots_simulation[[i, "Date_lockdown"]]
  
  # Filter data
  data_i <- filter(data_sim_sample, 
                   Simulation == simulation_i & Description == description_i)
  summary_data_i <- filter(summary_daily_cases_sim, 
                           Simulation == simulation_i & Description == description_i)
  
  # Create plot
  p <- ggplot(data = data_i, aes(x = Date, y = Daily_cases)) +
    theme_minimal() +
    theme(plot.margin = unit(c(0.5, 1, 0.5, 1), "cm")) +
    labs(title = simulation_i,
         subtitle = description_i) +
    geom_col(data = filter(cases_eng, Date <= date_T), alpha = 0.4) +
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
    geom_line(aes(group = Run, color = Run), size = 0.5, alpha = 0.08, show.legend = FALSE) +
    geom_line(data = summary_data_i, 
              aes(x = Date, y = Mean),
              color = "blue", size = 1) +
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
g <- annotate_figure(p, top = text_grob("Incident lab-confirmed cases of Covid-19 in England", size = 16),
                     bottom = text_grob("Data from https://coronavirus.data.gov.uk.", size = 10))
ggsave(paste0(out, "Plot - True vs counterfactual - incident cases.png"),
       plot = g, width = 6, height = 4*length(plot_cases_inc_sim))


### Cumulative cases -----------------------------------------------------------

plot_cases_cum_sim <- list()

for (i in 1:nrow(knots_simulation)) {
  
  # Define as natural or counterfactual history
  simulation_i <- knots_simulation[[i, "Simulation"]]
  description_i <- knots_simulation[[i, "Description"]]
  
  # Calculate knot dates
  knot_date_1_i <- knots_simulation[[i, "Knot_date_1"]]
  knot_date_2_i <- knots_simulation[[i, "Knot_date_2"]]
  
  # Calculate dates of social distancing and lockdown
  date_sd_i <- knots_simulation[[i, "Date_sd"]]
  date_lockdown_i <- knots_simulation[[i, "Date_lockdown"]]
  
  # Filter data
  data_i <- filter(data_sim_sample, 
                   Simulation == simulation_i & Description == description_i)
  summary_data_i <- filter(summary_cumulative_cases_end_sim, 
                           Simulation == simulation_i & Description == description_i)
  
  # Create plot
  p <- ggplot(data = data_i, aes(x = Date, y = Cumulative_cases_end)) +
    theme_minimal() +
    theme(plot.margin = unit(c(0.5, 1, 0.5, 1), "cm")) +
    labs(title = simulation_i,
         subtitle = description_i) +
    geom_col(data = filter(cases_eng, Date <= date_T), alpha = 0.4) +
    geom_vline(xintercept = date_sd_i, col = "red4") +
    geom_text(aes_(x = date_sd_i - 1, y = 100000, 
                   label = paste0("Date of\nsocial distancing:\n", 
                                  as.character(date_sd_i, format = "%d %b %C")), 
                   hjust = 1),
              color = "red4", size = 3, check_overlap = TRUE, show.legend = FALSE) +
    geom_vline(xintercept = date_lockdown_i, col = "red4") +
    geom_text(aes_(x = date_lockdown_i + 1, y = 160000, 
                   label = paste0("Date of\nlockdown:\n", 
                                  as.character(date_lockdown_i, format = "%d %b %C")), 
                   hjust = 0),
              color = "red4", size = 3, check_overlap = TRUE, show.legend = FALSE) +
    geom_line(aes(group = Run, color = Run), size = 0.5, alpha = 0.1, show.legend = FALSE) +
    geom_line(data = summary_data_i, 
              aes(x = Date, y = Mean),
              color = "blue", size = 1) +
    geom_text(data = summary_data_i, 
              aes(x = Date, y = Mean,
                  label = ifelse(Date == date_T, formatC(Mean, 
                                                         format = "f", big.mark = ",", digits = 0), "")),
              vjust = -1, size = 3, color = "blue", inherit.aes = FALSE) +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
    scale_x_date(name = "Date", 
                 limits = c(date_0, date_T + 7), 
                 date_breaks = "1 week", 
                 date_labels = "%d %b %C",
                 expand = expansion(mult = c(0, 0))) +
    scale_y_continuous(name = "Number of lab-confirmed cases",
                       limits = c(0, 200000), 
                       breaks = seq(0, 200000, 20000),
                       labels = comma_format(accuracy = 1),
                       expand = expansion(mult = c(0, 0)))
  #p
  
  # Add plot to list
  plot_cases_cum_sim[[i]] <- p
  
}

# Save plots
p <- ggarrange(plotlist = plot_cases_cum_sim, nrow = length(plot_cases_cum_sim))
g <- annotate_figure(p, top = text_grob("Cumulative lab-confirmed cases of Covid-19 in England", size = 16),
                     bottom = text_grob("Data from https://coronavirus.data.gov.uk.", size = 10))
ggsave(paste0(out, "Plot - True vs counterfactual - cumulative cases.png"),
       plot = g, width = 6, height = 4*length(plot_cases_cum_sim))

## Plot exponential growth -----------------------------------------------------

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

# Save plot
ggsave(paste0(out, "Plot - Cumulative vs incident cases - normal scale.png"),
       plot = plot_exp_growth_cases, width = 6, height = 6)


