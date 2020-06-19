# covid-counterfactual

This repository contains code for simulating the growth of COVID-19 cases in England between 3 March and 1 June.

Growth is simulated under three conditions:

1. Natural growth: Social distancing and lockdown as implemented.
2. Counterfactual growth: Social distancing and lockdown 1 week earlier.
3. Counterfactual growth: Social distancing and lockdown 2 weeks earlier.

## Table of contents

*Data* folder contains data from:

1. UK goverment (https://coronavirus.data.gov.uk)
2. NHS England (https://www.england.nhs.uk/statistics/statistical-work-areas/covid-19-daily-deaths/)
3. ONS (https://www.ons.gov.uk/peoplepopulationandcommunity/birthsdeathsandmarriages/deaths/datasets/weeklyprovisionalfiguresondeathsregisteredinenglandandwales)


*Modelling - England* folder contains:

1. R scripts for importing/formatting data and producing descriptive plots
    1. **Import and format data.R**: Imports and formats cases/deaths data for use in other scripts
    2. **Descriptive plots.R**: Produces descriptive plots of data (incident and cumulative cases/deaths, case fatality ratios)

2. Sub-folder *2 knots* for simulation code and output
    1. **Identify 2 knot points.R**: Finds the points at which growth of COVID-19 cases changes by fitting a linear spline with autocorrelated errors and finding the best knot point pairs
        * The best knot point pairs are saved in the .csv file *Best knot points.csv*
    2. **Simulation with 2 knots.R**: Simulates the growth of COVID-19 cases under both natural and counterfactual growth, estimates the number of deaths expected under each condition, and produces figures of incident and cumulative cases for both natural and counterfactual growth
        * Simulation outputs and figures are stored in sub-folders named with the pair of knot dates used


*packrat* folder contains package management files.

## Usage

To simulate the growth of COVID-19 cases in England between 3 March and 1 June under the three conditions, run the code **Simulation with 2 knots.R** in the *2 knots* subfolder. This script:

* Imports a .csv file containing the best knot point pairs identified (*Best knot points.csv*);
* Selects a specified pair of knot points and estimates the growth parameters associated with those knot points;
* Simulates the natural and counterfactual growth of COVID-19 according to the selected knot points and growth parameters; 
* Produces figures of incident and cumulative cases for both natural and counterfactual growth; and
* Saves simulation summary outputs and figures to a sub-folder (named with the pair of knot dates used).
