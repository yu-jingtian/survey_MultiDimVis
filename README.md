# Dissecting Survey Data with Multidimensional Visualization

## 1. Overview

This study introduces a multidimensional visualization framework for analyzing complex structures in survey-based policy preferences. Conventional analyses typically focus on individual policy issues, providing valuable insights into specific dimensions of public opinion but offering limited perspectives on how multiple attitudes interact within respondents. To address this gap, we propose a two-dimensional heatmap approach that integrates random forest (RF) predictions to account for demographic and interaction effects. This model-based visualization highlights joint patterns of policy preferences and enables interpretable exploration of multidimensional relationships in survey data. We apply the framework to national survey responses from 2014 to 2021 to illustrate how it can reveal evolving associations among key policy domains and identify systematic subgroup differences. Beyond this application, the proposed method provides a general and scalable tool for dissecting high-dimensional survey data, offering new possibilities for studying interconnected attitudes and behavioral patterns across diverse research contexts.

This repository contains the R code, processed data, and visualization scripts for the project.

---

## 2. How to Run the Project

### Install and load the package

Install the package directly from GitHub:

```r
devtools::install_github("yu-jingtian/surveyMDV")
library(surveyMDV)
```

This installs the package along with its required dependencies (e.g., ggplot2, dplyr).

### Load packaged data (2014–2021)

The package ships with three respondent-level datasets covering survey years 2014–2021:

- `policy_raw`: raw policy preference scores
- `policy_rf`: random-forest predicted policy scores
- `demographics`: respondent demographics and survey weights

Each dataset includes the keys case_id and year, which can be used for merging.

You can load the full datasets directly:

```r
data("policy_raw", package = "surveyMDV")
data("policy_rf", package = "surveyMDV")
data("demographics", package = "surveyMDV")
```

Or use the provided helper functions to subset by year and/or select columns:

```r
raw_2016 <- get_policy_raw(year = 2016, cols = c("immig", "guns"))
rf_2016  <- get_policy_rf(year = 2016, cols = c("immig_rf", "guns_rf"))
demo_2016 <- get_demographics(
  year = 2016,
  cols = c("partisan", "race", "gender", "weight_cumulative")
)
```

Datasets are designed to be joined using case_id and year:

```r
library(dplyr)

df <- policy_raw |>
  inner_join(policy_rf, by = c("case_id", "year")) |>
  inner_join(demographics, by = c("case_id", "year"))
```

This merged table can be used directly for visualization, subgroup analysis, or model-based summaries as described in the paper.

### RF heatmap for gun control vs immigration (2014–2021)

This example visualizes the joint distribution of RF–predicted gun control and immigration attitudes across survey years and partisan subgroups, using year-specific population scaling and automatically dropping years with missing policy data.

```r
# Full range of years covered by packaged data
years <- 2014:2021

# Basic partisan panels (Population / Republican / Independent / Democrat),
# using RF-predicted policy scores
p <- plot_policy_heatgrid(
  years     = years,
  policy_x = "guns",     # resolved internally to "guns_rf"
  policy_y = "immig",    # resolved internally to "immig_rf"
  type      = "rf",
  panels    = panels_partisan(),
  scale     = "within_year",   # year-specific scaling based on population
  scale_ref = "population"
)

p
```

```
Warning message:
Dropping year(s) with missing policy data:
- 2018: all NA for immig_rf
```

---

## 3. Project Highlights

Below are examples of visualization outputs:

<table>
  <tr>
    <th width="70%">Figure</th>
    <th width="30%">Description</th>
  </tr>
  <tr>
    <td>
      <img src="figs/heatmap_partisan_guns_pred_immig_pred.jpeg" width="100%">
    </td>
    <td>
      <b>Partisan Breakdown.</b> Heatmaps by political affiliation (Democrat, Republican, Independent) for Gun Control vs Immigration combination. Republicans are concentrated in the conservative pole, Democrats in the liberal pole, and Independents occupy intermediate or mixed regions. This pattern indicates that partisanship largely explains the distinct poles in the population distribution.
    </td>
  </tr>
  <tr>
    <td>
      <img src="figs/heatmap_republican_decompose_enviro_pred_guns_pred.jpeg" width="100%">
    </td>
    <td>
      <b>Subgroup Analysis.</b> Subgroup breakdown for Republicans. The overall two-pole structure, pro-environment/pro-gun-control vs anti-environment/permissive-gun, persists across geography and education but disappears when stratified by gender, indicating that gender explains most of the within-party divide.
    </td>
  </tr>
</table>

For more figures and explanations, see the `docs/` folder.

---

## 4. Data Sources
- **Public Political Preference Survey Dataset** Cumulative CES Policy Preferences (2014-2021):  
  [https://doi.org/10.7910/DVN/OSXDQO](https://doi.org/10.7910/DVN/OSXDQO)
- **Corresponding Demographic Survey** Cumulative CES Common Content:  
  [https://doi.org/10.7910/DVN/II2DB6](https://doi.org/10.7910/DVN/II2DB6)


---

## 5. Contact
For questions or collaboration:
- **Author:** [Your Name]  
- **Email:** [your_email@domain.edu]  
- **Institution:** Oregon State University