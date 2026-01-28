# DARK-ART: Data Analysis Rapid Kit - Automated R Tool

**DARK-ART** is a comprehensive, user-friendly R Shiny application designed to automate biostatistical analysis. It bridges the gap between complex statistical coding and intuitive graphical user interfaces, making rigorous data analysis accessible to researchers and clinicians.

**Live Demo:** [View the App (v1.0.0-beta) on ShinyApps.io](https://gourean.shinyapps.io/dark-art)

## Key Features

*   **Smart Data Import**:
    *   Accepts .csv files.
    *   Auto-detects numeric and categorical variables.
    *   Allows manual overrides for variable types.

*   **Versatile Analysis Modes**:
    *   **Independent Groups**: 
        *   Automatically selects between Parametric (T-Test, ANOVA) and Non-Parametric (Mann-Whitney, Kruskal-Wallis) tests based on normality assumptions.
        *   Handles categorical outcomes with Fisher's Exact or Chi-Square tests.
    *   **Paired/Repeated Measures**: 
        *   Paired T-Test / Wilcoxon Signed-Rank for 2 timepoints.
        *   Repeated Measures ANOVA / Friedman Test for >2 timepoints.
        *   McNemar's test for paired categorical data.
    *   **Correlation**:
        *   Pearson (parametric) and Spearman (non-parametric) correlation.
    *   **Split Group Analysis**:
        *   Perform separate analyses (Independent, Paired, or Correlation) for subgroups within your data automatically.

*   **Advanced Visualizations**:
    *   Publication-ready plots based on ggplot2.
    *   **Customization**: Fully adjustable titles, axis labels, fonts, sizes, and colors.
    *   **Significance**: Auto-generated significance brackets and p-values/stars.
    *   **Export**: High-resolution downloads in PDF, SVG, and JPEG formats.

*   **Statistical Rigor**:
    *   **Assumption Checking**: Automatic Shapiro-Wilk (normality), Levene's test (homogeneity of variance), and Mauchly's test (sphericity).
    *   **Post-hoc Analysis**: Bonferroni corrections for ANOVA; Custom Dunn-Bonferroni implementations for non-parametric multi-group comparisons to match commercial software standards.
    *   **Interpretation**: "Plain English" text generation explaining the statistical results alongside detailed methodology.

*   **Result Export**:
    *   Export result tables to CSV or Word-compatible HTML.
    *   Includes pairwise comparison tables and descriptive statistics.

## Installation & Requirements

This app requires **R** and several R packages.

### 1. Prerequisites
Ensure you have R installed. It is recommended to use RStudio for running the app locally.

### 2. Install Dependencies
Run the following code in your R console to install the required packages:

```r
install.packages(c(
  "shiny", 
  "bslib", 
  "ggplot2", 
  "dplyr", 
  "glue", 
  "dunn.test", 
  "scales", 
  "svglite", 
  "ggsignif", 
  "colourpicker", 
  "tidyr"
))
```

## Usage

1.  **Run the App**:
    Open `app.R` in RStudio and click the **"Run App"** button, or run:
    ```r
    shiny::runApp("path/to/app.R")
    ```

2.  **Step-by-Step Workflow**:
    *   **Upload**: import your .csv dataset.
    *   **Configure**:
        *   Select **Basic** or **Advanced** complexity.
        *   Choose your Analysis Mode (Independent, Paired, etc.).
        *   Select your Outcome (Dependent) and Grouping (Independent) variables.
    *   **Analyze**: Click **Run Analysis**.
    *   **Visualize & Customize**: Use the right-hand sidebar to tweak the plot appearance.
    *   **Export**: Download your plot and results table.

## Methodology

The application employs a robust decision logic to select the most appropriate statistical test:

| Comparison | Normality Met? | Assumptions | Test Used | Post-hoc Analysis |
| :--- | :--- | :--- | :--- | :--- |
| **2 Independent Groups** | Yes | Equal Variance | Independent T-Test | - |
| | Yes | Unequal Variance | Welch's T-Test | - |
| | No | - | Mann-Whitney U Test | - |
| **>2 Independent Groups** | Yes | Equal Variance | One-Way ANOVA | Pairwise T-tests (Bonferroni) |
| | Yes | Unequal Variance | Welch's ANOVA | Pairwise T-tests (Bonferroni) |
| | No | - | Kruskal-Wallis Test | Dunn's Test (Bonferroni)* |
| **2 Paired Groups** | Yes | - | Paired T-Test | - |
| | No | - | Wilcoxon Signed-Rank Test | - |
| **>2 Paired Groups** | Yes | Sphericity Met | Repeated Measures ANOVA | Pairwise Paired T-tests (Bonferroni) |
| | Yes | Sphericity Violated | RM ANOVA (Greenhouse-Geisser) | Pairwise Paired T-tests (Bonferroni) |
| | No | - | Friedman Test | Dunn-Bonferroni (Manual Implementation) |
| **Correlation** | Yes (Both) | - | Pearson Correlation | - |
| | No (Either) | - | Spearman Correlation | - |
| **Categorical vs Categorical** | - | Exp. counts < 5 | Fisher's Exact Test | - |
| | - | Exp. counts >= 5 | Pearson Chi-Square | - |

*(Note: Normality is assessed using the Shapiro-Wilk test. Post-hoc Dunn's tests use 2-sided p-values calculated from Z-scores to ensure consistency with standard statistical packages)*

## Support

If you find this tool useful, consider supporting its development:

[![Donate](https://raw.githubusercontent.com/stefan-niedermann/paypal-donate-button/master/paypal-donate-button.png)](https://paypal.me/gourean)

## Credits

Developed for efficient and automated biostatistical analysis via **Vibe Coding** with **Gemini 3 Pro**.
*   **UI/UX**: `bslib` "Minty" theme.
*   **Plotting**: `ggplot2` ecosystem.


