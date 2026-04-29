# Release Notes v1.2.0

**DARK-ART v1.2.0** is now available. This update introduces a powerful data editing suite, enhanced customization for visualizations, and critical stability improvements for complex datasets.

## New Features

### **Data Management**
- **Data Editing**: A fully functional Data Editor allows you to directly modify your dataset within the app. Features include:
  - Double-click to edit cells.
  - Add or delete rows and columns.
  - Rename and reorder columns.
  - Download the modified dataset as CSV.
- **Filter Data**: A new sidebar tool allows you to filter your dataset by specific variables and values before analysis, making it easy to focus on specific subgroups without editing the raw data.

### **Visualization & Customization**
- **Enhanced Plot Controls**: Added fine-grained control over appearance:
  - **Stroke Width**: Adjust the thickness of lines and borders.
  - **Point Size**: Control the size of data points in jitter and scatter plots.
- **Smart Reset**: Automatically clears custom axis labels, plot titles, and axis limits when the underlying variables are changed, preventing mismatched labels and incorrect scales.
- **Reset Appearance**: A new "Reset Appearance" button instantly restores all graph settings to their defaults.

### **Result Tables**
- **Group Counts**: The Sample Size (N) is now automatically displayed for each group in the result table headers, providing immediate context.

## Enhancements
- **Group Ordering**: Reworked the Group Order setting for robust handling of factor levels across different analysis modes.
- **Mobile Experience**: Optimized the layout for mobile devices, ensuring better visibility and usability on small screens.

## Bug Fixes
- **Split Analysis Stability**: Fixed a crash in Split Analysis logic where significance lines attempted to calculate `max()` on categorical outcomes.
- **Robustness**: Improved error handling for specific datasets, preventing crashes when determining ranges for empty groups (e.g., "argument is of length zero").
- **Test Selection Logic**: Corrected an issue where empty groups caused multi-group tests (e.g., Kruskal-Wallis, ANOVA) to be selected for 2-group comparisons instead of the appropriate 2-group tests (e.g., Mann-Whitney U, T-Test).
- **CSV Handling**: Special characters in column names (e.g., "Group (A vs B)", " % Conc.") are now strictly preserved upon import, allowing for clearer table headers and plot labels without dots replacing spaces.

---
*Built by Wong GR. available on [GitHub](https://github.com/gourean/dark-art).*
