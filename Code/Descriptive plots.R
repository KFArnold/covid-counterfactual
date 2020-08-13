# ------------------------------------------------------------------------------
# Notes
# ------------------------------------------------------------------------------

# This script creates descriptive plots using data on COVID-19 from gov.uk, NHS England, and ONS

# ------------------------------------------------------------------------------
# Set up
# ------------------------------------------------------------------------------

# Load required packages
packrat::restore()
library(tidyverse); library(ggpubr); library(scales)

# Run source code to import and format data
source("./Code/Import and format data.R")

# Set storage directory for outputs
out <- paste0("./Results/")

# ------------------------------------------------------------------------------
# Descriptive plots
# ------------------------------------------------------------------------------

## Case fatality ratios --------------------------------------------------------

# Set colour key
colors <- c("Hospital deaths" = "darkorange", "All deaths" = "darkgreen")

# Figure (with CFRs by hospital and all deaths)
cfr_plot <- ggplot(data = deaths_hosp_dod_eng,
                            aes(x = Date, y = Case_fatality_rate)) +
  theme_minimal() +
  theme(plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm")) +
  labs(title = "Case fatality ratio (CFR) of COVID-19 in England",
       subtitle = " ") +
  geom_line(aes(color = "Hospital deaths")) +
  geom_line(data = deaths_all_dod_eng,
            aes(x = Date, y = Case_fatality_rate, color = "All deaths")) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  scale_x_date(name = "Date", 
               limits = c(date_0, date_max + 5), 
               date_breaks = "1 week", 
               date_labels = "%d %b %C",
               expand = expansion(mult = c(0, 0))) +
  scale_y_continuous(name = "CFR",
                     limits = c(0, 0.35), 
                     breaks = seq(0, 0.35, 0.05),
                     expand = expansion(mult = c(0, 0))) +
  scale_color_manual(name = " ", values = colors)
cfr_plot

# Save plot
filename <- paste0("Plot - descriptive - case fatality ratio.png")
ggsave(filename = paste0(out, filename), plot = cfr_plot, width = 8, height = 5)

## Incidence -------------------------------------------------------------------

# Cases
plot_cases_inc <- ggplot(data = cases_eng, 
                         aes(x = Date, y = Daily_cases)) +
  theme_minimal() +
  theme(plot.margin = unit(c(1, 1, 1, 1), "cm")) +
  labs(title = "Incident lab-confirmed cases of COVID-19 in England",
       subtitle = "Pillar 1 data only",
       caption = "Data from https://www.gov.uk/guidance/coronavirus-covid-19-information-for-the-public.") +
  geom_col(alpha = 0.5) +
  geom_line(aes(x = Date, y = Daily_cases_MA7), col = "navyblue") +
  geom_vline(xintercept = date_sd, col = "red4") +
  geom_text(aes(x = date_sd - 1, y = 3000, 
                label = paste0("Date of\nsocial distancing:\n", as.character(date_sd, format = "%d %b %C")), 
                hjust = 1),
            color = "red4", check_overlap = TRUE, show.legend = FALSE) +
  geom_vline(xintercept = date_lockdown, col = "red4") +
  geom_text(aes (x = date_lockdown + 1, y = 5000, 
                 label = paste0("Date of\nlockdown:\n", as.character(date_lockdown, format = "%d %b %C")),
                 hjust = 0),
            color = "red4", check_overlap = TRUE, show.legend = FALSE) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  scale_x_date(name = "Date", 
               limits = c(date_0, date_max + 7), 
               date_breaks = "1 week", 
               date_labels = "%d %b %C",
               expand = expansion(mult = c(0, 0))) +
  scale_y_continuous(name = "Number of lab-confirmed cases",
                     limits = c(0, 6000), 
                     breaks = seq(0, 6000, 1000),
                     expand = expansion(mult = c(0, 0)))
plot_cases_inc

# Hospital deaths 
plot_deaths_hosp_inc <- ggplot(data = deaths_hosp_dod_eng, 
                               aes(x = Date, y = Daily_deaths)) +
  theme_minimal() +
  theme(plot.margin = unit(c(1, 1, 1, 1), "cm")) +
  labs(title = "Incident hospital deaths from COVID-19 in England",
       subtitle = " ",
       caption = "Data from https://www.england.nhs.uk/.") +
  geom_col(alpha = 0.5) +
  geom_line(aes(x = Date, y = Daily_deaths_MA7), col = "navyblue") +
  geom_vline(xintercept = date_sd, col = "red4") +
  geom_text(aes(x = date_sd - 1, y = 3000, 
                label = paste0("Date of\nsocial distancing:\n", as.character(date_sd, format = "%d %b %C")), 
                hjust = 1),
            color = "red4", check_overlap = TRUE, show.legend = FALSE) +
  geom_vline(xintercept = date_lockdown, col = "red4") +
  geom_text(aes (x = date_lockdown + 1, y = 5000, 
                 label = paste0("Date of\nlockdown:\n", as.character(date_lockdown, format = "%d %b %C")),
                 hjust = 0),
            color = "red4", check_overlap = TRUE, show.legend = FALSE) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  scale_x_date(name = "Date", 
               limits = c(date_0, date_max + 7), 
               date_breaks = "1 week", date_labels = "%d %b %C",
               expand = expansion(mult = c(0, 0))) +
  scale_y_continuous(name = "Number of hospital deaths",
                     limits = c(0, 6000), 
                     breaks = seq(0, 6000, 1000),
                     expand = expansion(mult = c(0, 0)))
plot_deaths_hosp_inc

# All deaths 
plot_deaths_all_inc <- ggplot(data = deaths_all_dod_eng, 
                              aes(x = Date, y = Daily_deaths)) +
  theme_minimal() +
  theme(plot.margin = unit(c(1, 1, 1, 1), "cm")) +
  labs(title = "Incident deaths from COVID-19 across all settings in England",
       subtitle = " ",
       caption = "Data from https://www.ons.gov.uk/.") +
  geom_col(alpha = 0.5) +
  geom_line(aes(x = Date, y = Daily_deaths_MA7), col = "navyblue") +
  geom_vline(xintercept = date_sd, col = "red4") +
  geom_text(aes(x = date_sd - 1, y = 3000, 
                label = paste0("Date of\nsocial distancing:\n", as.character(date_sd, format = "%d %b %C")), 
                hjust = 1),
            color = "red4", check_overlap = TRUE, show.legend = FALSE) +
  geom_vline(xintercept = date_lockdown, col = "red4") +
  geom_text(aes (x = date_lockdown + 1, y = 5000, 
                 label = paste0("Date of\nlockdown:\n", as.character(date_lockdown, format = "%d %b %C")),
                 hjust = 0),
            color = "red4", check_overlap = TRUE, show.legend = FALSE) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  scale_x_date(name = "Date", 
               limits = c(date_0, date_max + 7), 
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
ggsave(filename = paste0(out, filename), plot = g, width = 7, height = 15)

## Cumulative ------------------------------------------------------------------

# Cases
plot_cases_cum <- ggplot(data = cases_eng, 
                         aes(x = Date, y = Cumulative_cases_end)) +
  theme_minimal() +
  theme(plot.margin = unit(c(1, 1, 1, 1), "cm")) +
  labs(title = "Cumulative lab-confirmed cases of COVID-19 in England",
       subtitle = "Pillar 1 data only",
       caption = "Data from https://www.gov.uk/guidance/coronavirus-covid-19-information-for-the-public.") +
  geom_col(alpha = 0.5) +
  geom_line(aes(x = Date, y = Cumulative_cases_end_MA7), col = "navyblue") +
  geom_vline(xintercept = date_sd, col = "red4") +
  geom_text(aes(x = date_sd - 1, y = 100000, 
                label = paste0("Date of\nsocial distancing:\n", as.character(date_sd, format = "%d %b %C")), 
                hjust = 1),
            color = "red4", check_overlap = TRUE, show.legend = FALSE) +
  geom_vline(xintercept = date_lockdown, col = "red4") +
  geom_text(aes(x = date_lockdown + 1, y = 160000, 
                label = paste0("Date of\nlockdown:\n", as.character(date_lockdown, format = "%d %b %C")),
                hjust = 0),
            color = "red4", check_overlap = TRUE, show.legend = FALSE) +
  geom_text(data = cases_eng, 
            aes(label = ifelse(Date == date_max, formatC(Cumulative_cases_end, 
                                                       format = "f", big.mark = ",", digits = 0), "")),
            vjust = -1, size = 3) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  scale_x_date(name = "Date", 
               limits = c(date_0, date_max + 7), 
               date_breaks = "1 week", 
               date_labels = "%d %b %C",
               expand = expansion(mult = c(0, 0))) +
  scale_y_continuous(name = "Number of lab-confirmed cases",
                     limits = c(0, 200000), 
                     breaks = seq(0, 200000, 20000),
                     labels = comma_format(accuracy = 1),
                     expand = expansion(mult = c(0, 0)))
plot_cases_cum

# Hospital deaths
plot_deaths_hosp_cum <- ggplot(data = deaths_hosp_dod_eng, 
                               aes(x = Date, y = Cumulative_deaths)) +
  theme_minimal() +
  theme(plot.margin = unit(c(1, 1, 1, 1), "cm")) +
  labs(title = "Cumulative hospital dealths from COVID-19 in England",
       subtitle = " ", 
       caption = "Data from https://www.england.nhs.uk/.") +
  geom_col(alpha = 0.5) +
  geom_line(aes(x = Date, y = Cumulative_deaths_MA7), col = "navyblue") +
  geom_vline(xintercept = date_sd, col = "red4") +
  geom_text(aes(x = date_sd - 1, y = 100000, 
                label = paste0("Date of\nsocial distancing:\n", as.character(date_sd, format = "%d %b %C")), 
                hjust = 1),
            color = "red4", check_overlap = TRUE, show.legend = FALSE) +
  geom_vline(xintercept = date_lockdown, col = "red4") +
  geom_text(aes(x = date_lockdown + 1, y = 160000, 
                label = paste0("Date of\nlockdown:\n", as.character(date_lockdown, format = "%d %b %C")),
                hjust = 0),
            color = "red4", check_overlap = TRUE, show.legend = FALSE) +
  geom_text(data = deaths_hosp_dod_eng, 
            aes(label = ifelse(Date == date_max, formatC(Cumulative_deaths, 
                                                       format = "f", big.mark = ",", digits = 0), "")),
            vjust = -1, size = 3) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  scale_x_date(name = "Date", 
               limits = c(date_0, date_max + 7), 
               date_breaks = "1 week", date_labels = "%d %b %C",
               expand = expansion(mult = c(0, 0))) +
  scale_y_continuous(name = "Number of hospital deaths",
                     limits = c(0, 200000), 
                     breaks = seq(0, 200000, 20000),
                     labels = comma_format(accuracy = 1),
                     expand = expansion(mult = c(0, 0)))
plot_deaths_hosp_cum

# All deaths
plot_deaths_all_cum <- ggplot(data = deaths_all_dod_eng, 
                              aes(x = Date, y = Cumulative_deaths)) +
  theme_minimal() +
  theme(plot.margin = unit(c(1, 1, 1, 1), "cm")) +
  labs(title = "Cumulative deaths from COVID-19 across all settings in England",
       subtitle = " ", 
       caption = "Data from https://www.ons.gov.uk/.") +
  geom_col(alpha = 0.5) +
  geom_line(aes(x = Date, y = Cumulative_deaths_MA7), col = "navyblue") +
  geom_vline(xintercept = date_sd, col = "red4") +
  geom_text(aes(x = date_sd - 1, y = 100000, 
                label = paste0("Date of\nsocial distancing:\n", as.character(date_sd, format = "%d %b %C")), 
                hjust = 1),
            color = "red4", check_overlap = TRUE, show.legend = FALSE) +
  geom_vline(xintercept = date_lockdown, col = "red4") +
  geom_text(aes(x = date_lockdown + 1, y = 160000, 
                label = paste0("Date of\nlockdown:\n", as.character(date_lockdown, format = "%d %b %C")),
                hjust = 0),
            color = "red4", check_overlap = TRUE, show.legend = FALSE) +
  geom_text(data = deaths_all_dod_eng, 
            aes(label = ifelse(Date == max(Date), 
                               formatC(Cumulative_deaths, 
                                       format = "f", big.mark = ",", digits = 0), "")),
            vjust = -1, size = 3) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  scale_x_date(name = "Date", 
               limits = c(date_0, date_max + 7), 
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
ggsave(filename = paste0(out, filename), plot = g, width = 7, height = 15)

