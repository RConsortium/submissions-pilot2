# Introduction

This application is intended for a pilot submission to the FDA composing of a Shiny application, as part of the [R Submissions Working Group](https://rconsortium.github.io/submissions-wg/) Pilot 2. The data sets and results displayed in the application originate from the [Pilot 1 project](https://rconsortium.github.io/submissions-wg/pilot-overall.html#pilot-1---common-analyses). Below is a brief description of the rest of the application:

## Note on Dynamic Filters

This application enables dynamic filtering of the input datasets used within each of the modules described below. When one or more filters are applied, the statistical estimates displayed in the table or visualization will dynamically update. However, since the statistical models and comparison methods were not powered for dynamic subgroups produced by these filters, p-values will be removed from the view until all filters are removed.
### Demographic Table

In this interface, summary statistics associated with baseline clinical characteristics and other demographic factors is shown.

### KM-Plot for TTDE

A Kaplan-Meier (KM) plot of the Time to First Dermatologic Event (TTDE) with strata defined by treatment group is displayed along with an informative risk set table across time.

### Primary Table

A summary table of the primary efficacy analysis is shown for each of the time points of assessment (baseline and week 24) comparing each treatment group. The primary efficacy variable (change from baseline in ADAS Cog (11)) was analyzed using an Analysis of Covariance (ANCOVA) model with treatment and baseline value as covariates, comparing Placebo to Xanomeline High Dose.

### Efficacy Table

A summary table of an additional efficacy analysis is shown for baseline and week 20. The efficacy variable (Glucose) was analzying using ANCOVA model with treatment and baseline value as covariates, comparing Placebo to Xanomeline High Dose.
