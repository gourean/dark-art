# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## \[1.2.0] - 2026-02-13

### Added

* **Data Editing**: A fully functional Data Editor allows you to directly modify your dataset within the app (edit cells, adding/deleting rows/cols).
* **Filter Data**: New sidebar controls allow you to filter the dataset by a specific variable and value(s) before running the analysis.
* **Enhanced Plot Controls**: Added stroke width, point size control, and an option to remove the gap from the Y-axis.
* **Reset Appearance**: A new button to reset all graph settings to defaults.
* **Smart Reset**: Automatically clears custom axis labels, plot titles, and axis limits when the underlying variables are changed.
* **Result Tables**: Sample Size (N) is now displayed in group headers.

### Changed

* **Group Ordering**: Reworked the Group Order setting for robust handling.
* **Graph Defaults**: 'Remove Gap from X-Axis' is now enabled by default.
* **Mobile Experience**: Optimized layout for mobile devices.

### Fixed

* **Split Analysis Stability**: Fixed crash in Split Analysis logic for categorical outcomes ("max not meaningful for factors").
* **Robustness**: Improved error handling for empty groups ("argument is of length zero").
* **CSV Handling**: Fixed issue where special characters (e.g., spaces, brackets) in column names were replaced by dots on import.

## \[1.1.0] - 2026-02-02

### Added

* **Datasheet Viewer**: New dedicated tab to inspect raw datasets directly within the app.
* **Export Options**:

  * Added "Copy to Clipboard", "CSV", and "Excel" export buttons to the Datasheet Viewer.
  * Added HTML/Word export capability for the Result Table.

* **Brand Identity**: Added official application logo to the header and browser tab icon.

### Changed

* **UI Layout**: Refined Visualisation Layout for consistency and spacing across tabs to prevent layout shifts.
* **Responsiveness**: Optimized layout for better mobile device support ("Mobile Friendly").

### Fixed

* **Stability**: Resolved "max not meaningful for factors" error that occurred during specific categorical analyses.

## \[1.0.0] - 2026-01-31

### Added

* Initial release of **DARK-ART: Data Analysis Rapid Kit-Automated R Tool**.
* **Automated Workflow**:

  * Automatic selection of statistical tests based on data properties (normality, variance, sphericity).
  * Smart fallbacks to robust alternatives (e.g., Welch’s, Greenhouse-Geisser).
  * Automated post-hoc corrections (Bonferroni, Dunn-Bonferroni).

* **Supported Analyses**:

  * Independent Groups (T-Tests, ANOVA, Mann-Whitney U, Kruskal-Wallis).
  * Paired/Repeated Measures (Paired T-Tests, Wilcoxon, RM ANOVA, Friedman).
  * Categorical Analysis (Fisher's Exact, Pearson Chi-Square).
  * Correlations (Pearson/Spearman).
  * Subgroup analysis support.

* **Visualizations**:

  * Publication-ready ggplot2 graphics.
  * Customization options (titles, labels, fonts, colors).
  * Auto-significance brackets.

* **Reporting**: Natural language interpretation of statistical results.
