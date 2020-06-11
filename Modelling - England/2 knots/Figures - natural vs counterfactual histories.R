# ------------------------------------------------------------------------------
# Notes
# ------------------------------------------------------------------------------

# This script creates figures displaying differences between natural and counterfactual histories

# ------------------------------------------------------------------------------
# Set up
# ------------------------------------------------------------------------------

# Load required packages
packrat::restore()
library(tidyverse); library(ggpubr); library(scales)

# Run source code to import and format data
source("./Modelling - England/Import and format data.R")

# Import file containing best knot points, define knot points
knots_best <- read_csv("./Modelling - England/2 knots/Best knot points.csv")
knot_date_1 <- filter(knots_best, RMSE_cum == min(RMSE_cum))$Knot_date_1  
knot_date_2 <- filter(knots_best, RMSE_cum == min(RMSE_cum))$Knot_date_2  

# Set simulation-specific directory
path <- paste0("./Modelling - England/2 knots/", paste(knot_date_1, "and", knot_date_2, sep = " "), "/")

# Set storage directory for outputs
# (default is simulation directory)
out <- path

# Import files containing simulated data and descriptions of hypothetical interventions
cases_eng_sim_sample <- read_csv(paste0(path, "Simulated data sample.csv")) %>% mutate_if(is.character, factor)
summary_cases_eng_sim <- read_csv(paste0(path, "Summary - cases.csv")) %>% mutate_if(is.character, factor)
knots_simulation <- read_csv(paste0(path, "Hypothetical interventions.csv"))

# Convert columns to factors
# sapply(cases_eng_sim_sample, class); sapply(deaths_eng_sim, class)
cases_eng_sim_sample <- cases_eng_sim_sample %>% mutate_at(vars(Run), as.factor)

# ------------------------------------------------------------------------------
# Figures
# ------------------------------------------------------------------------------

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
  geom_line(data = filter(summary_cases_eng_sim, Simulation == "Natural history"), 
            aes(x = Date, y = Mean_daily_cases), color = "blue", inherit.aes = FALSE) +
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
  geom_line(data = filter(summary_cases_eng_sim, Simulation == "Natural history"), 
            aes(x = Date, y = Mean_cumulative_cases_end), 
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
  data_i <- filter(cases_eng_sim_sample, 
                   Knot_date_1 == knot_date_1_i & Knot_date_2 == knot_date_2_i)
  summary_data_i <- filter(summary_cases_eng_sim, 
                           Knot_date_1 == knot_date_1_i & Knot_date_2 == knot_date_2_i)
  
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
              aes(x = Date, y = Mean_daily_cases),
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
  data_i <- filter(cases_eng_sim_sample, 
                   Knot_date_1 == knot_date_1_i & Knot_date_2 == knot_date_2_i)
  summary_data_i <- filter(summary_cases_eng_sim, 
                           Knot_date_1 == knot_date_1_i & Knot_date_2 == knot_date_2_i)
  
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
              aes(x = Date, y = Mean_cumulative_cases_end),
              color = "blue", size = 1) +
    geom_text(data = summary_data_i, 
              aes(x = Date, y = Mean_cumulative_cases_end,
                  label = ifelse(Date == date_T, formatC(Mean_cumulative_cases_end, 
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

