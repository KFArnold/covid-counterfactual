# ------------------------------------------------------------------------------
# Notes
# ------------------------------------------------------------------------------

# This script imports and formats Covid-19 data from gov.uk, NHS England, and ONS
# It also estimates crude case fatality and growth factors

# ------------------------------------------------------------------------------
# Setup
# ------------------------------------------------------------------------------

# Load required packages
packrat::restore()
library(tidyverse)

# ------------------------------------------------------------------------------
# Import and format data
# ------------------------------------------------------------------------------

# Load data
# Datasets downloaded from https://coronavirus.data.gov.uk:
govuk_data_cases <- read_csv("./Data/Gov.uk data/coronavirus-cases_latest.csv")
govuk_data_deaths_hosp_dor <- read_csv("./Data/Gov.uk data/coronavirus-deaths_latest.csv") # by day of reporting
# Dataset downloaded from https://www.england.nhs.uk/statistics/statistical-work-areas/covid-19-daily-deaths/: 
nhseng_data_deaths_hosp_dod <- read_csv("./Data/NHS England data/coronavirus-deaths_latest.csv") # by day of death
# Dataset downloaded from ONS:
ons_data_deaths_all_dod <- read_csv("./Data/ONS data/ons-coronavirus-deaths_latest.csv")

# Remove spaces from variable names
names(govuk_data_cases) <- str_replace_all(names(govuk_data_cases), 
                                           c(" " = "_", "-" = "_"))
names(govuk_data_deaths_hosp_dor) <- str_replace_all(names(govuk_data_deaths_hosp_dor), 
                                                     c(" " = "_", "-" = "_"))
names(nhseng_data_deaths_hosp_dod) <- str_replace_all(names(nhseng_data_deaths_hosp_dod), 
                                                      c(" " = "_", "-" = "_"))
names(ons_data_deaths_all_dod) <- str_replace_all(names(ons_data_deaths_all_dod), 
                                                  c(" " = "_", "-" = "_"))

# Copy datasets and remove originals
cases <- govuk_data_cases %>% 
  select(c(Area_name, Area_code, Area_type, Date = Specimen_date, 
           Daily_cases = Daily_lab_confirmed_cases, Cumulative_cases = Cumulative_lab_confirmed_cases))
deaths_hosp_dor <- govuk_data_deaths_hosp_dor %>% 
  select(c(Area_name, Area_code, Area_type, Date = Reporting_date, 
           Daily_deaths = Daily_change_in_deaths, Cumulative_deaths))
deaths_hosp_dod <- nhseng_data_deaths_hosp_dod %>% 
  select(Area_name, Date = Death_date, Daily_deaths = Daily_hospital_deaths,
         Cumulative_deaths = Cumulative_hospital_deaths)
deaths_all_dod <- ons_data_deaths_all_dod %>% 
  select(Area_name, Date = Death_date, Cumulative_deaths, Daily_deaths)
rm(govuk_data_cases, govuk_data_deaths_hosp_dor, nhseng_data_deaths_hosp_dod, ons_data_deaths_all_dod)

# View data
# cases; deaths_hosp_dor; deaths_hosp_dod; deaths_all_dod

# Factorise data
cases <- cases %>% mutate_at(vars(Area_name, Area_code, Area_type), funs(as.factor))
deaths_hosp_dor <- deaths_hosp_dor %>% mutate_at(vars(Area_name, Area_code, Area_type), funs(as.factor))
deaths_hosp_dod <- deaths_hosp_dod %>% mutate_at(vars(Area_name), funs(as.factor))
deaths_all_dod <- deaths_all_dod %>% mutate_at(vars(Area_name), funs(as.factor))

# Reorder data by date
cases <- cases %>% arrange(Date)
deaths_hosp_dor <- deaths_hosp_dor %>% arrange(Date)
deaths_hosp_dod <- deaths_hosp_dod %>% arrange(Date)
deaths_all_dod <- deaths_all_dod %>% arrange(Date)

# Define important dates
date_0 <- min(cases$Date)  # date of first confimed case
date_max <- max(cases$Date) - 5  # date for which data can be reasonably assumed complete
date_T <- as.Date("2020-06-01")  # end date for simulation
date_sd <- as.Date("2020-03-17")  # first full day of social distancing
date_lockdown <- as.Date("2020-03-24")  # first full day of lockdown

# Omit data from previous 5 days (due to reporting delays)
cases <- cases %>% filter(Date <= date_max)
deaths_hosp_dor <- deaths_hosp_dor %>% filter(Date <= date_max)
deaths_hosp_dod <- deaths_hosp_dod %>% filter(Date <= date_max)
deaths_all_dod <- deaths_all_dod %>% filter(Date <= date_max)

# Retain only country-level data for England and remove other data files
cases_eng <- filter(cases, Area_name == "England")
deaths_hosp_dor_eng <- filter(deaths_hosp_dor, Area_name == "England")
deaths_hosp_dod_eng <- filter(deaths_hosp_dod, Area_name == "England")
deaths_all_dod_eng <- filter(deaths_all_dod, Area_name == "England")
rm(cases, deaths_hosp_dor, deaths_hosp_dod, deaths_all_dod)

# Calculate cumulative cases at beginning of each day
# (current counts represent cases at end of each day)
names(cases_eng)[names(cases_eng) == "Cumulative_cases"] <- 
  "Cumulative_cases_end"
cases_eng$Cumulative_cases_beg <- 
  c(0, cases_eng$Cumulative_cases_end[1:(length(cases_eng$Cumulative_cases_end) - 1)])
cases_eng <- cases_eng %>% 
  select(Area_name, Area_code, Area_type, Date, 
         Cumulative_cases_beg, Daily_cases, 
         Cumulative_cases_end)

# Calculate growth factor for each time point
for (t in 2:nrow(cases_eng)) {
  inc_tminus1 <- cases_eng[(t-1), "Daily_cases"]
  inc_t <- cases_eng[t, "Daily_cases"]
  growth_factor <- (inc_t / inc_tminus1)
  cases_eng[t, "Growth_factor"] <- growth_factor
}
# Remove loop variables
rm(t, inc_tminus1, inc_t, growth_factor)

# Calculate first full date at which cumulative cases in England exceeded 100
date_100 <- filter(cases_eng, Cumulative_cases_beg >= 100)$Date[1]  # 3 Mar

# Create copy of dataset where cumulative cases > 100
cases_eng_100 <- filter(cases_eng, Date >= date_100 & Date <= date_T)

# ------------------------------------------------------------------------------
# Estimate case fatality rates
# ------------------------------------------------------------------------------

# Calculate case fatality rates = cumulative deaths / cumulative confirmed cases

# (1) Using hospital deaths in England by day of death
for (i in 1:nrow(deaths_hosp_dod_eng)) {
  
  date <- deaths_hosp_dod_eng$Date[i]
  deaths_i <- filter(deaths_hosp_dod_eng, Date == date)$Cumulative_deaths
  cases_i <- filter(cases_eng, Date == date)$Cumulative_cases_end
  
  cfr_i <- deaths_i / cases_i
  
  deaths_hosp_dod_eng[i, "Case_fatality_rate"] <- cfr_i
} 
cfr_hosp_dod <- tail(deaths_hosp_dod_eng$Case_fatality_rate, n = 1); cfr_hosp_dod
#plot(deaths_hosp_dod_eng$Date, deaths_hosp_dod_eng$Case_fatality_rate)

# (2) Using hospital deaths in England by day of reporting
for (i in 1:nrow(deaths_hosp_dor_eng)) {
  
  date <- deaths_hosp_dor_eng$Date[i]
  deaths_i <- filter(deaths_hosp_dor_eng, Date == date)$Cumulative_deaths
  cases_i <- filter(cases_eng, Date == date)$Cumulative_cases_end
  
  cfr_i <- deaths_i / cases_i
  
  deaths_hosp_dor_eng[i, "Case_fatality_rate"] <- cfr_i
} 
cfr_hosp_dor <- tail(deaths_hosp_dor_eng$Case_fatality_rate, n = 1); cfr_hosp_dor
#plot(deaths_hosp_dor_eng$Date, deaths_hosp_dor_eng$Case_fatality_rate)

# Remove loop variables
rm(i, date, deaths_i, cases_i, cfr_i)
