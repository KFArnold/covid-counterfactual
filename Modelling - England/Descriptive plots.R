# ------------------------------------------------------------------------------
# Notes
# ------------------------------------------------------------------------------

# This script creates descriptive plots using data on Covid-19 from gov.uk, NHS England, and ONS

# ------------------------------------------------------------------------------
# Set up
# ------------------------------------------------------------------------------

# Load required packages
packrat::restore()
library(tidyverse); library(ggpubr); library(scales)

# Run source code to import and format data
source("./Modelling - England/Import and format data.R")

# Set simulation-specific directory
path <- paste0("./Modelling - England/")

# Set storage directory for outputs
# (default is simulation directory)
out <- path

# ------------------------------------------------------------------------------
# Descriptive plots
# ------------------------------------------------------------------------------

## Case fatality rates ---------------------------------------------------------

# By date of death
cfr_hosp_dod_plot <- ggplot(data = deaths_hosp_dod_eng,
                            aes(x = Date, y = Case_fatality_rate)) +
  theme_minimal() +
  theme(plot.margin = unit(c(1, 1, 1, 1), "cm")) +
  labs(title = "Case fatality ratio in England",
       subtitle = "According to hospital deaths by date of death",
       caption = "Data from https://www.england.nhs.uk/statistics/statistical-work-areas/covid-19-daily-deaths/. \nMost recent 5 days omitted due to reporting delays.") +
  geom_line() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  scale_x_date(name = "Date", 
               limits = c(date_0, date_max + 5), 
               date_breaks = "1 week", 
               date_labels = "%d %b %C",
               expand = expansion(mult = c(0, 0))) +
  scale_y_continuous(name = "Case fatality ratio",
                     limits = c(0, 0.25), 
                     breaks = seq(0, 0.25, 0.05),
                     expand = expansion(mult = c(0, 0)))
cfr_hosp_dod_plot

# By date of reporting
cfr_hosp_dor_plot <- ggplot(data = deaths_hosp_dor_eng,
                            aes(x = Date, y = Case_fatality_rate)) +
  theme_minimal() +
  theme(plot.margin = unit(c(1, 1, 1, 1), "cm")) +
  labs(title = "Case fatality ratio in England",
       subtitle = "According to hospital deaths by date of reporting",
       caption = "Data from https://coronavirus.data.gov.uk. \nMost recent 5 days omitted due to reporting delays.") +
  geom_line() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  scale_x_date(name = "Date", 
               limits = c(date_0, date_max + 5), 
               date_breaks = "1 week", 
               date_labels = "%d %b %C",
               expand = expansion(mult = c(0, 0))) +
  scale_y_continuous(name = "Case fatality ratio",
                     limits = c(0, 0.25), 
                     breaks = seq(0, 0.25, 0.05),
                     expand = expansion(mult = c(0, 0)))
cfr_hosp_dor_plot

# Save plot
#ggarrange(cfr_hosp_dor_plot, cfr_hosp_dod_plot, nrow = 2)
g <- ggarrange(cfr_hosp_dor_plot, cfr_hosp_dod_plot, nrow = 2)
filename <- paste0("Plot - descriptive - case fatality ratio.png")
ggsave(filename = paste0(out, filename), plot = g, width = 7, height = 10)

## Incidence -------------------------------------------------------------------

# Cases
plot_cases_inc <- ggplot(data = cases_eng, 
                         aes(x = Date, y = Daily_cases)) +
  theme_minimal() +
  theme(plot.margin = unit(c(1, 1, 1, 1), "cm")) +
  labs(title = "Incident lab-confirmed cases of Covid-19 in England by date of testing",
       subtitle = " ",
       caption = "Data from https://coronavirus.data.gov.uk. \nMost recent 5 days omitted due to reporting delays.") +
  geom_col() +
  geom_vline(xintercept = date_sd, col = "red") +
  geom_text(aes(x = date_sd - 1, y = 3000, 
                label = paste0("Date of social distancing:\n", as.character(date_sd, format = "%d %b %C")), 
                hjust = 1, color = "red"),
            check_overlap = TRUE, show.legend = FALSE) +
  geom_vline(xintercept = date_lockdown, col = "red") +
  geom_text(aes (x = date_lockdown + 1, y = 5000, 
                 label = paste0("Date of lockdown:\n", as.character(date_lockdown, format = "%d %b %C")),
                 hjust = 0, color = "red"),
            check_overlap = TRUE, show.legend = FALSE) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  scale_x_date(name = "Date", 
               limits = c(date_0, date_max + 5), 
               date_breaks = "1 week", 
               date_labels = "%d %b %C",
               expand = expansion(mult = c(0, 0))) +
  scale_y_continuous(name = "Number of lab-confirmed cases",
                     limits = c(0, 6000), 
                     breaks = seq(0, 6000, 1000),
                     expand = expansion(mult = c(0, 0)))
plot_cases_inc

# Hospital deaths (by day of reporting)
plot_deaths_hosp_inc <- ggplot(data = deaths_hosp_dor_eng, 
                               aes(x = Date, y = Daily_deaths)) +
  theme_minimal() +
  theme(plot.margin = unit(c(1, 1, 1, 1), "cm")) +
  labs(title = "Incident hospital deaths from Covid-19 in England by date of reporting",
       subtitle = " ",
       caption = "Data from https://www.england.nhs.uk/. \nMost recent 5 days omitted due to reporting delays.") +
  geom_col() +
  geom_vline(xintercept = date_sd, col = "red") +
  geom_text(aes(x = date_sd - 1, y = 3000, 
                label = paste0("Date of social distancing:\n", as.character(date_sd, format = "%d %b %C")), 
                hjust = 1, color = "red"),
            check_overlap = TRUE, show.legend = FALSE) +
  geom_vline(xintercept = date_lockdown, col = "red") +
  geom_text(aes (x = date_lockdown + 1, y = 5000, 
                 label = paste0("Date of lockdown:\n", as.character(date_lockdown, format = "%d %b %C")),
                 hjust = 0, color = "red"),
            check_overlap = TRUE, show.legend = FALSE) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  scale_x_date(name = "Date", 
               limits = c(date_0, date_max + 5), 
               date_breaks = "1 week", date_labels = "%d %b %C",
               expand = expansion(mult = c(0, 0))) +
  scale_y_continuous(name = "Number of hospital deaths",
                     limits = c(0, 6000), 
                     breaks = seq(0, 6000, 1000),
                     expand = expansion(mult = c(0, 0)))
plot_deaths_hosp_inc

# All deaths (by day of death)
plot_deaths_all_inc <- ggplot(data = deaths_all_dod_eng, 
                              aes(x = Date, y = Daily_deaths)) +
  theme_minimal() +
  theme(plot.margin = unit(c(1, 1, 1, 1), "cm")) +
  labs(title = "Incident deaths from Covid-19 in England by date of death",
       subtitle = " ",
       caption = "Data from https://www.ons.gov.uk/.") +
  geom_col() +
  geom_vline(xintercept = date_sd, col = "red") +
  geom_text(aes(x = date_sd - 1, y = 3000, 
                label = paste0("Date of social distancing:\n", as.character(date_sd, format = "%d %b %C")), 
                hjust = 1, color = "red"),
            check_overlap = TRUE, show.legend = FALSE) +
  geom_vline(xintercept = date_lockdown, col = "red") +
  geom_text(aes (x = date_lockdown + 1, y = 5000, 
                 label = paste0("Date of lockdown:\n", as.character(date_lockdown, format = "%d %b %C")),
                 hjust = 0, color = "red"),
            check_overlap = TRUE, show.legend = FALSE) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  scale_x_date(name = "Date", 
               limits = c(date_0, date_max + 5), 
               date_breaks = "1 week", date_labels = "%d %b %C",
               expand = expansion(mult = c(0, 0))) +
  scale_y_continuous(name = "Number of hospital deaths",
                     limits = c(0, 6000), 
                     breaks = seq(0, 6000, 1000),
                     expand = expansion(mult = c(0, 0)))
plot_deaths_all_inc

# Save plot
#ggarrange(plot_cases_inc, plot_deaths_hosp_inc, plot_deaths_all_inc, nrow = 3)
g <- ggarrange(plot_cases_inc, plot_deaths_hosp_inc, plot_deaths_all_inc, nrow = 3)
filename <- paste0("Plot - descriptive - incident cases and deaths.png")
ggsave(filename = paste0(out, filename), plot = g, width = 8, height = 15)

## Cumulative ------------------------------------------------------------------

# Cases
plot_cases_cum <- ggplot(data = cases_eng, 
                         aes(x = Date, y = Cumulative_cases_end)) +
  theme_minimal() +
  theme(plot.margin = unit(c(1, 1, 1, 1), "cm")) +
  labs(title = "Cumulative lab-confirmed cases of Covid-19 in England by date of testing",
       subtitle = " ",
       caption = "Data from https://coronavirus.data.gov.uk. \nMost recent 5 days omitted due to reporting delays.") +
  geom_col() +
  geom_vline(xintercept = date_sd, col = "red") +
  geom_text(aes(x = date_sd - 1, y = 100000, 
                label = paste0("Date of social distancing:\n", as.character(date_sd, format = "%d %b %C")), 
                hjust = 1, color = "red"),
            check_overlap = TRUE, show.legend = FALSE) +
  geom_vline(xintercept = date_lockdown, col = "red") +
  geom_text(aes(x = date_lockdown + 1, y = 160000, 
                label = paste0("Date of lockdown:\n", as.character(date_lockdown, format = "%d %b %C")),
                hjust = 0, color = "red"),
            check_overlap = TRUE, show.legend = FALSE) +
  geom_text(data = cases_eng, 
            aes(label = ifelse(Date == date_max, formatC(Cumulative_cases_end, 
                                                       format = "f", big.mark = ",", digits = 0), "")),
            vjust = -1, size = 4) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  scale_x_date(name = "Date", 
               limits = c(date_0, date_max + 5), 
               date_breaks = "1 week", 
               date_labels = "%d %b %C",
               expand = expansion(mult = c(0, 0))) +
  scale_y_continuous(name = "Number of lab-confirmed cases",
                     limits = c(0, 200000), 
                     breaks = seq(0, 200000, 20000),
                     labels = comma_format(accuracy = 1),
                     expand = expansion(mult = c(0, 0)))
plot_cases_cum

# Hospital deaths (by day of reporting)
plot_deaths_hosp_cum <- ggplot(data = deaths_hosp_dor_eng, 
                               aes(x = Date, y = Cumulative_deaths)) +
  theme_minimal() +
  theme(plot.margin = unit(c(1, 1, 1, 1), "cm")) +
  labs(title = "Cumulative hospital dealths from Covid-19 in England by date of reporting",
       subtitle = " ", 
       caption = "Data from https://www.england.nhs.uk/. \nMost recent 5 days omitted due to reporting delays.") +
  geom_col() +
  geom_vline(xintercept = date_sd, col = "red") +
  geom_text(aes(x = date_sd - 1, y = 100000, 
                label = paste0("Date of social distancing:\n", as.character(date_sd, format = "%d %b %C")), 
                hjust = 1, color = "red"),
            check_overlap = TRUE, show.legend = FALSE) +
  geom_vline(xintercept = date_lockdown, col = "red") +
  geom_text(aes(x = date_lockdown + 1, y = 160000, 
                label = paste0("Date of lockdown:\n", as.character(date_lockdown, format = "%d %b %C")),
                hjust = 0, color = "red"),
            check_overlap = TRUE, show.legend = FALSE) +
  geom_text(data = deaths_hosp_dor_eng, 
            aes(label = ifelse(Date == date_max, formatC(Cumulative_deaths, 
                                                       format = "f", big.mark = ",", digits = 0), "")),
            vjust = -1, size = 4) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  scale_x_date(name = "Date", 
               limits = c(date_0, date_max + 5), 
               date_breaks = "1 week", date_labels = "%d %b %C",
               expand = expansion(mult = c(0, 0))) +
  scale_y_continuous(name = "Number of hospital deaths",
                     limits = c(0, 200000), 
                     breaks = seq(0, 200000, 20000),
                     labels = comma_format(accuracy = 1),
                     expand = expansion(mult = c(0, 0)))
plot_deaths_hosp_cum

# All deaths (by day of death)
plot_deaths_all_cum <- ggplot(data = deaths_all_dod_eng, 
                              aes(x = Date, y = Cumulative_deaths)) +
  theme_minimal() +
  theme(plot.margin = unit(c(1, 1, 1, 1), "cm")) +
  labs(title = "Cumulative dealths from Covid-19 in England by date of death",
       subtitle = " ", 
       caption = "Data from https://www.ons.gov.uk/.") +
  geom_col() +
  geom_vline(xintercept = date_sd, col = "red") +
  geom_text(aes(x = date_sd - 1, y = 100000, 
                label = paste0("Date of social distancing:\n", as.character(date_sd, format = "%d %b %C")), 
                hjust = 1, color = "red"),
            check_overlap = TRUE, show.legend = FALSE) +
  geom_vline(xintercept = date_lockdown, col = "red") +
  geom_text(aes(x = date_lockdown + 1, y = 160000, 
                label = paste0("Date of lockdown:\n", as.character(date_lockdown, format = "%d %b %C")),
                hjust = 0, color = "red"),
            check_overlap = TRUE, show.legend = FALSE) +
  geom_text(data = deaths_all_dod_eng, 
            aes(label = ifelse(Date == max(Date), 
                               formatC(Cumulative_deaths, 
                                       format = "f", big.mark = ",", digits = 0), "")),
            vjust = -1, size = 4) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  scale_x_date(name = "Date", 
               limits = c(date_0, date_max + 5), 
               date_breaks = "1 week", date_labels = "%d %b %C",
               expand = expansion(mult = c(0, 0))) +
  scale_y_continuous(name = "Number of hospital deaths",
                     limits = c(0, 200000), 
                     breaks = seq(0, 200000, 20000),
                     labels = comma_format(accuracy = 1),
                     expand = expansion(mult = c(0, 0)))
plot_deaths_all_cum

# Save plot
#ggarrange(plot_cases_cum, plot_deaths_hosp_cum, plot_deaths_all_cum, nrow = 3)
g <- ggarrange(plot_cases_cum, plot_deaths_hosp_cum, plot_deaths_all_cum, nrow = 3)
filename <- paste0("Plot - descriptive - cumulative cases and deaths.png")
ggsave(filename = paste0(out, filename), plot = g, width = 8, height = 15)

