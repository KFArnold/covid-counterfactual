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
library(tidyverse); library(caTools)

# ------------------------------------------------------------------------------
# Import and format data
# ------------------------------------------------------------------------------

# Load data
## Dataset downloaded from https://www.gov.uk/guidance/coronavirus-covid-19-information-for-the-public:
govuk_data_cases <- read_csv("./Data/Gov.uk data/coronavirus-cases-pillars_latest.csv")
## Dataset downloaded from https://coronavirus.data.gov.uk:
#govuk_data_cases <- read_csv("./Data/Gov.uk data/coronavirus-cases_latest.csv")
govuk_data_deaths_hosp_dor <- read_csv("./Data/Gov.uk data/coronavirus-deaths_latest.csv") # by day of reporting
## Dataset downloaded from https://www.england.nhs.uk/statistics/statistical-work-areas/covid-19-daily-deaths/: 
nhseng_data_deaths_hosp_dod <- read_csv("./Data/NHS England data/coronavirus-deaths_latest.csv") # by day of death
## Dataset downloaded from ONS:
ons_data_deaths_all_dod <- read_csv("./Data/ONS data/ons-coronavirus-deaths_latest.csv")

# Remove spaces, dashes, and parentheses from variable names
names(govuk_data_cases) <- str_replace_all(names(govuk_data_cases), 
                                           c(" " = "_", "-" = "_", "\\)" = "", "\\(" = ""))
names(govuk_data_deaths_hosp_dor) <- str_replace_all(names(govuk_data_deaths_hosp_dor), 
                                                     c(" " = "_", "-" = "_", "\\)" = "", "\\(" = ""))
names(nhseng_data_deaths_hosp_dod) <- str_replace_all(names(nhseng_data_deaths_hosp_dod), 
                                                      c(" " = "_", "-" = "_", "\\)" = "", "\\(" = ""))
names(ons_data_deaths_all_dod) <- str_replace_all(names(ons_data_deaths_all_dod), 
                                                  c(" " = "_", "-" = "_", "\\)" = "", "\\(" = ""))

# Copy datasets, standardise variable names, and remove originals
cases <- govuk_data_cases %>% 
  select(c(Area_name = Nation, Pillar, Date = Earliest_Specimen_Date, 
           Daily_cases = Daily_number_of_positive_cases_new_methodology, 
           Cumulative_cases = Cumulative_number_of_positive_cases_new_methodology))
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

# Format variable types, group by Area_name (and Pillar, if applicable)
cases <- cases %>% mutate_at(vars(Area_name, Pillar), as.factor) %>%
  mutate_at(vars(Daily_cases, Cumulative_cases), as.numeric) %>% 
  mutate_at(vars(Date), as.Date, format = "%d/%m/%Y") %>%
  group_by(Area_name, Pillar)
deaths_hosp_dor <- deaths_hosp_dor %>% mutate_at(vars(Area_name, Area_code, Area_type), as.factor) %>% 
  group_by(Area_name)
deaths_hosp_dod <- deaths_hosp_dod %>% mutate_at(vars(Area_name), as.factor) %>% 
  group_by(Area_name)
deaths_all_dod <- deaths_all_dod %>% mutate_at(vars(Area_name), as.factor) %>% 
  group_by(Area_name)

# Reorder data by date
cases <- cases %>% arrange(Date)
deaths_hosp_dor <- deaths_hosp_dor %>% arrange(Date)
deaths_hosp_dod <- deaths_hosp_dod %>% arrange(Date)
deaths_all_dod <- deaths_all_dod %>% arrange(Date)

# Define date for which data can be reasonably assumed complete (due to reporting delays)
date_max <- max(cases$Date) - 5

# Omit data that is incomplete due to reporting delays
cases <- cases %>% filter(Date <= date_max)
deaths_hosp_dor <- deaths_hosp_dor %>% filter(Date <= date_max)
deaths_hosp_dod <- deaths_hosp_dod %>% filter(Date <= date_max)
deaths_all_dod <- deaths_all_dod %>% filter(Date <= date_max)

# Retain only country-level (and Pillar 1) data for England and remove other data files
cases_eng <- cases %>% filter(Area_name == "England", Pillar == "Pillar 1")
deaths_hosp_dor_eng <- deaths_hosp_dor %>% filter(Area_name == "England")
deaths_hosp_dod_eng <- deaths_hosp_dod %>% filter(Area_name == "England")
deaths_all_dod_eng <- deaths_all_dod %>% filter(Area_name == "England")
rm(cases, deaths_hosp_dor, deaths_hosp_dod, deaths_all_dod)

# Calculate cumulative cases at beginning of each day
# (current counts represent cases at end of each day)
names(cases_eng)[names(cases_eng) == "Cumulative_cases"] <- 
  "Cumulative_cases_end"
cases_eng <- cases_eng %>% mutate(Cumulative_cases_beg = lag(Cumulative_cases_end, n = 1, default = 0)) %>%
  relocate(c(Cumulative_cases_beg, Daily_cases), .before = Cumulative_cases_end)

# Calculate growth factor for each time point
for (t in 2:nrow(cases_eng)) {
  inc_tminus1 <- cases_eng[(t-1), "Daily_cases"]
  inc_t <- cases_eng[t, "Daily_cases"]
  growth_factor <- (inc_t / inc_tminus1)
  cases_eng[t, "Growth_factor"] <- growth_factor
}
# Remove loop variables
rm(t, inc_tminus1, inc_t, growth_factor)

# Calculate 7-day moving averages of daily and cumulative cases and deaths
cases_eng <- cases_eng %>% mutate(Cumulative_cases_beg_MA7 = round(runmean(Cumulative_cases_beg, k = 7, alg = "C", endrule = "mean"), 3),
                                  Daily_cases_MA7 = round(runmean(Daily_cases, k = 7, alg = "C", endrule = "mean"), 3),
                                  Cumulative_cases_end_MA7 = round(runmean(Cumulative_cases_end, k = 7, alg = "C", endrule = "mean"), 3))
deaths_hosp_dor_eng <- deaths_hosp_dor_eng %>% mutate(Daily_deaths_MA7 = round(runmean(Daily_deaths, k = 7, alg = "C", endrule = "mean"), 3),
                                                      Cumulative_deaths_MA7 = round(runmean(Cumulative_deaths, k = 7, alg = "C", endrule = "mean"), 3))
deaths_hosp_dod_eng <- deaths_hosp_dod_eng %>% mutate(Daily_deaths_MA7 = round(runmean(Daily_deaths, k = 7, alg = "C", endrule = "mean"), 3),
                                                      Cumulative_deaths_MA7 = round(runmean(Cumulative_deaths, k = 7, alg = "C", endrule = "mean"), 3))
deaths_all_dod_eng <- deaths_all_dod_eng %>% mutate(Daily_deaths_MA7 = round(runmean(Daily_deaths, k = 7, alg = "C", endrule = "mean"), 3),
                                                    Cumulative_deaths_MA7 = round(runmean(Cumulative_deaths, k = 7, alg = "C", endrule = "mean"), 3))

# Define important dates
date_0 <- min(cases_eng$Date)  # date of first confimed case in England
date_T <- as.Date("2020-06-01")  # end date for simulation
date_sd <- as.Date("2020-03-17")  # first full day of social distancing
date_lockdown <- as.Date("2020-03-24")  # first full day of lockdown
date_100 <- min(filter(cases_eng, Cumulative_cases_beg >= 100)$Date)  # first full date at which cumulative cases in England exceeded 100

# Create variable for number of days since date_100
cases_eng <- cases_eng %>% mutate(Days_since_date_100 = as.numeric(Date - date_100)) %>%
  relocate(Days_since_date_100, .after = Date)

# Create copy of dataset where cumulative cases >= 100 and date <= end date for simulation
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
