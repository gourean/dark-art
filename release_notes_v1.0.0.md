# Release Notes v1.0.0

**DARK-ART v1.0.0** is now available. This release introduces an automated **R programming** workflow for statistical decision-making and result generation.

## Key Features

### **Supported Analysis**
The app automatically selects the correct statistical test based on data properties (normality, variance, sphericity):
- **Independent Groups**: T-Tests, ANOVA, Mann-Whitney U, Kruskal-Wallis.
- **Paired/Repeated Measures**: Paired T-Tests, Wilcoxon, RM ANOVA, Friedman Test.
- **Categorical Analysis**: Fisher's Exact Test, Pearson Chi-Square.
- **Correlations**: Pearson/Spearman.
- **Split Group**: support subgroup analysis.

### **Automated Workflow**
- **Normality & Assumptions**: Automatically runs Shapiro-Wilk, Levene’s, and Mauchly’s tests to determine the best method.
- **Smart Fallbacks**: Switches to robust alternatives (e.g., Welch’s, Greenhouse-Geisser) when assumptions are violated.
- **Post-hoc Corrections**: Applies Bonferroni and Dunn-Bonferroni (custom) corrections for multi-group comparisons.

### **Visualizations**
- **ggplot2 Graphics**: Generates publication-ready plots.
- **Customization**: Edit titles, labels, fonts, and colors instantly.
- **Auto-Significance**: Adds significance brackets and stars ($p < .05$) to plots automatically.

### **Natural Language Reporting**
- **Interpretation**: Methods and results are explained in natural language.
- **No AI Hallucinations**: Generated via deterministic, hardcoded logic. This is **not** an LLM.

## Installation & Usage

**Web (No Installation)**:
- [**Serverless (ShinyApps.io)**](https://gourean.shinyapps.io/dark-art): Faster initial load.
- [**Shinylive (Browser)**](https://gourean.github.io/dark-art): Client-side processing (slower initial load, runs locally).

**Local**:
- Run via RStudio with `shiny`, `bslib`, `ggplot2` and other dependencies.

## Credits
Developed via **Vibe Coding** with **Gemini 3 Pro**.

---
*Built by Wong GR. available on [GitHub](https://github.com/gourean/dark-art).*
