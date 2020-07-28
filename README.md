# covid-counterfactual

This repository contains code for simulating the growth of COVID-19 cases in England between 3 March and 1 June.

Growth is simulated under three conditions:

1. Natural growth: Social distancing and lockdown as implemented.
2. Counterfactual growth: Social distancing and lockdown 1 week earlier.
3. Counterfactual growth: Social distancing and lockdown 2 weeks earlier.

## Table of contents

*Data* folder contains cases and/or deaths data from:

1. UK government (https://coronavirus.data.gov.uk and https://www.gov.uk/guidance/coronavirus-covid-19-information-for-the-public)
2. NHS England (https://www.england.nhs.uk/statistics/statistical-work-areas/covid-19-daily-deaths/)
3. ONS (https://www.ons.gov.uk/peoplepopulationandcommunity/birthsdeathsandmarriages/deaths/datasets/weeklyprovisionalfiguresondeathsregisteredinenglandandwales)

*Code* folder contains the following R scripts:

1. **Import and format data.R**: Imports and formats cases/deaths data for use in other scripts 
2. **Descriptive plots.R**: Produces descriptive plots of data (incident and cumulative cases/deaths, case fatality ratios)
3. **Identify 2 knot points.R**: Finds the points at which growth of COVID-19 cases changed by fitting a linear spline with autocorrelated errors and finding the best knot point pairs
4. **Simulation with 2 knots.R**: Simulates the growth of COVID-19 cases under both natural and counterfactual growth, estimates the number of deaths expected under each condition, and produces figures of incident and cumulative cases for both natural and counterfactual growth

*Results* folder contains outputs from R scripts.

*packrat* folder contains package management files.

## Usage

To simulate the growth of COVID-19 cases in England between 3 March and 1 June under the three conditions, run the code **Simulation with 2 knots.R**. This script:

* Imports a .csv file (*Best knot points.csv* in *Results* folder) which contains a summary of the best knot point pairs and corresponding growth parameters;
* Simulates the natural and counterfactual growth of COVID-19 using the best knot point pairs and corresponding growth parameters; 
* Produces figures of incident and cumulative cases for both natural and counterfactual growth; and
* Saves simulation summary outputs and figures to the *Results* folder.
