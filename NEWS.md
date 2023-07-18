# pilot2wrappers 0.10.0

- Add usage guide for application (#61)
- Remove treatment variables from the `adtte` set prior to generating filters in KM plot module (#60).
- Ensure data filters within the KM-Plot module are only applied to data sets inside that module, and not on the remaining modules (#59).
- Ensure inference p-values are still displayed within primary and efficacy tables after applying filters in KM-Plot module.
- Add specific notes regarding filter usage and overlapping variables inside the KM module per user feedback.
- Display a message to the user when filtered data does not contain enough observations for survival probabilities and confidence intervals in the KM-Plot module. A note in the ADRG has also been added for this situation.
- Add note in ADRG regarding warning messages when restoring the `teal` and `teal.data` packages after the `renv` package library completes installation.
- Correct typos in ADRG Quarto document.

# pilot2wrappers 0.9.0

- Fix RMSE calculation in efficacy table to match result in Pilot 1

# pilot2wrappers 0.8.0

- Switch to `adlbc` as source for visit completion to remain consistent with efficacy analysis
- Fix incorrect sample sizes for baseline visit in primary analysis table
- Fix misc typos for ADRG

# pilot2wrappers 0.7.0

- Include analysis model specification as a footnote in the efficacy table (#55)
- Add new frequency table of visit completion by treatment group (#53)
- Revise title for efficacy table (#52)
- Fix incorrect header order for primary table (#51)
- Ensure decimal places for rounding numeric results in demographics table is consistent with Pilot 1 programming (#54)

# pilot2wrappers 0.6.0

- Remove Teal filters for every module except KM-plot to address FDA reviewer feedback
- Increase risk table and plot label font sizes
- Add bootstrap alert box with subgroup disclaimer in KM-plot module

# pilot2wrappers 0.5.0

- Hide p-values when a data filter is applied, and display only when no filters are applied. This addresses comments from FDA reviewers after they saw a preview of the application.
- Add a new description in the App Information module regarding display of p-values.
- Add another footnote to the primary and efficacy table displays regarding the p-value display.
- Increase plot window height for KM module, improving readability

# pilot2wrappers 0.4.0

- Ensure column width of efficacy table's 95% CI header is wide enough to fit in a single row
- Add missing footnotes to primary table
- Add new vignettes for ADRG as a [Quarto](https://quarto.org/) document (with HTML and PDF output) and cover letter as a R-Markdown document using the [`{pagedown}`](https://pagedown.rbind.io/#letter)  package.

# pilot2wrappers 0.3.0

- Enable support for using `{pkglite}` to create package bundle compliant with ECTD submission transfer standards
- Fix display of tooltips in select modules by using the `{tippy}` R package
- Add helper function `set_data_path` for the user to set the directory path to `.xpt` data files loaded into the application

# pilot2wrappers 0.2.0

- Convert Shiny application to a package structure using [`{golem}`](https://thinkr-open.github.io/golem).
- Incorporate the open-source [`{teal}`](https://insightsengineering.github.io/teal/main) package inside application to provide dynamic filters in all modules

# pilot2wrappers 0.1.0

- Initial version.
- Added a `NEWS.md` file to track changes to the package.
