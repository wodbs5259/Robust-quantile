# Robust Quantile

This repository contains codes necessary to replicate **Joo and Lee**: "Robust quantile estimation via clipped check loss minimization". The `fit.rob` function in the `code` folder is designed for estimation and regression based on the clipped check loss. For estimation, the function requires input in the form of a numeric vector. For regression or spline applications, the input must be provided as a data frame or a matrix, where the target variable is assumed to be located in the last column of the data. The key purpose of this function is to enhance robustness by selectively trimming the data based on a specified efficiency level. By default, the efficiency is set to 0.95, but this can be adjusted within the function as needed to suit specific analysis goals.


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
| clipped_point.R             | R code for selecting clipping points (`c1`, `c2`) based on α, controlling the influence of outliers in quantile estimation.<br>Note: `c1`, `c2` in the paper correspond to `a1`, `a2` in the code implementation. |
| function.R                  | Combined R script integrating `clipped_point.R`, `rho_psi.R`, and `robust_quantile_function.R` into a single workflow. |
| rho_psi.R                   | R script containing the rho and psi functions for the clipped check loss. |
| robust_quantile_function.R  | R script implementing both estimation and regression functions based on the clipped check loss. |


### data

| Data File                   | Detail                                                                 |
|-----------------------------|------------------------------------------------------------------------|
| balloon.txt            | The data consist of 4984 observations taken from a balloon about 30 kilometres above the surface of the earth. The outliers are caused by the fact that the balloon slowly rotates, causing the ropes from which the measuring instrument is suspended to cut off the direct radiation from the sun.<br>`Davies, L., Kovac, A., Kovac, M. A., Davies, P. L., Kovac, A., & Local Extremes, R. Package `**`ftnonpar`**. |
| engel.csv                  | Engel food expenditure data used in Koenker and Bassett(1982). This is a regression data set consisting of 235 observations on income and expenditure on food for Belgian working class households.<br>Koenker, R., Portnoy, S., Ng, P. T., Zeileis, A., Grosjean, P., & Ripley, B. D. (2018). Package `quantreg`. |


### code

| Data File               | Detail                                                                |
|-------------------------|-----------------------------------------------------------------------|
| simulation_estimation.R | R script for running simulation studies for estimation.               |
| simulation_regression.R | R script for running simulation studies for regression.               |


### Report errors


### References

