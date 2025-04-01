# Skipped-Quantile

This repository contains codes necessary to replicate **Joo and Lee**: "Accurate quantile estimation via clipped check loss minimization". The `fit.rob` function in the `code` folder is designed for estimation and regression based on the skipped check loss. For estimation, the function requires input in the form of a numeric vector. For regression or spline applications, the input must be provided as a data frame or a matrix, where the target variable is assumed to be located in the last column of the data. The key purpose of this function is to enhance robustness by selectively trimming the data based on a specified efficiency level. By default, the efficiency is set to 0.95, but this can be adjusted within the function as needed to suit specific analysis goals.

1. Thesis title
   - Accurate quantile estimation via clipped check loss minimization
  
2. Our goal
   - We propose accurate quantile estimation method
   - Our method include bounded loss function that is more robust method.
  
3. File construct
   - src/ : Function of proposed method code and simulation code
   - data/ : Real data


