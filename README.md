# Skipped-Quantile

This repository contains codes necessary to replicate **Joo and Lee**: "Accurate quantile estimation via clipped check loss minimization". The `fit.rob` function in the `code` folder is designed for estimation and regression based on the skipped check loss. For estimation, the function requires input in the form of a numeric vector. For regression or spline applications, the input must be provided as a data frame or a matrix, where the target variable is assumed to be located in the last column of the data. The key purpose of this function is to enhance robustness by selectively trimming the data based on a specified efficiency level. By default, the efficiency is set to 0.95, but this can be adjusted within the function as needed to suit specific analysis goals.


## Folder structure

| Folder   | Detail                                                                 |
|----------|------------------------------------------------------------------------|
| analysis | R scripts for real data analysis                                       |
| code     | R scripts for the proposed approach / `function.R` integrates three codes |
| data     | Data used for real-data analysis                                       |
| sim      | R scripts for simulation studies                                       |


### analysis

| Data File     | Detail                                                                 |
|---------------|------------------------------------------------------------------------|
| Regression.R  | Quantile regression R code using the `engel` dataset                   |
| Spline.R      | Quantile regression with spline using the `balloon` dataset            |


### code

| Data File                   | Detail                                                                 |
|-----------------------------|------------------------------------------------------------------------|
| clipping_point.R            | R code for selecting clipping points (`c1`, `c2`) based on α, controlling the influence of outliers in quantile estimation.<br>Note: `c1`, `c2` in the paper correspond to `a1`, `a2` in the code implementation. |
| function.R                  | Combined R script integrating `clipping_point.R`, `rho_psi.R`, and `skipped_quantile_function.R` into a single workflow. |
| rho_psi.R                   | R script containing the rho and psi functions for the skipped check loss. |
| skipped_quantile_function.R | R script implementing both estimation and regression functions based on the skipped check loss. |

