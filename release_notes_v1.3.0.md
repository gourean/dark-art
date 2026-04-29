# Release Notes v1.3.0

**DARK-ART v1.3.0** is here with a focus on statistical robustness, manual significance visualization, and enhanced plot controls. This update addresses several edge-case crashes and improves the reliability of complex subgroup analyses.

## New Features

### **Plot Customization**
- **Whisker Caps**: You can now add caps to the whiskers of boxplots and error bars for improved visual clarity.
- **Force Plot Type**: Added the ability to manually override the automated plot selection, allowing users to force a Barplot or Boxplot regardless of the data distribution.

## Bug Fixes & Stability

### **Statistical Engine**
- **Zero Variance Handling**: Implemented robust error handling across all statistical tests to prevent application crashes when encountering groups with constant values (zero variance).
- **Chi-Square Tests**: Fixed a specific bug in the categorical analysis module when data is constant.
- **Warning Suppression**: Suppressed "cannot compute exact p-value with ties" warnings in non-parametric tests (Mann-Whitney U, Wilcoxon) to keep the results console clean.

### **Visualization Improvements**
- **Manual Significance Brackets**: Replaced the `ggsignif` dependency with a custom manual drawing logic using `geom_segment` and `geom_text`. This fixes the "Can only handle data with groups plotted on x-axis" crash and ensures significance brackets work reliably across all analysis modes (Independent, Paired, and Split-Group).
- **Whisker Alignment**: Fixed a minor rendering issue with whisker bar alignment in certain error bar configurations.

### **Analysis & Filtering**
- **Paired Split Analysis**: 
    - Fixed a blank plot issue caused by `pivot_longer` failing on column names containing spaces; now uses a more robust base R `reshape` method.
    - Resolved a naming collision crash that occurred when the split variable was named "Group".
- **Global Filtering**: Improved the data filtering logic to properly handle missing values (NA) and ensure that filtered datasets are correctly applied to all tables and plots simultaneously.
- **UI Persistence**: Fixed a bug where the "Reset Appearance" button failed to restore certain axis gap settings to their default states.

---
*Built by Wong GR. Available on [GitHub](https://github.com/gourean/dark-art).*
