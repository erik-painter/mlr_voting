# Multiple Linear Regression Analysis on Pennsylvania Voting Data
This project explores the application of multiple linear regression (MLR) to analyze the factors influencing voting patterns in Pennsylvania during the 1992 U.S. Presidential Election. The goal was to develop a predictive model using demographic and socioeconomic variables to estimate the percentage of votes received by Bill Clinton across the state's 67 counties.

## Project Overview
The analysis focuses on understanding how county-level factors such as median age, per capita income, poverty rate, and population density relate to voting outcomes. The project emphasizes statistical rigor through data cleaning, transformation, model selection, and validation.

## Key Objectives
- Develop a robust multiple linear regression model for vote share prediction
- Explore relationships between demographic variables and voting patterns
- Validate assumptions and test for multicollinearity

## Methodology and Statistical Techniques
- Exploratory Data Analysis (EDA)
- Visual inspection using histograms and scatterplots to identify outliers and data distributions.
- Correlation matrices to detect potential multicollinearity among predictors.

## Data Cleaning and Transformations
- Outlier removal and Box-Cox transformations applied to reduce skewness.
- Key transformations included log transformations for variables like population density.

## Model Selection Process:
- Initial full model fitted with all predictors.
- Multiple iterations of model selection using various techniques and metrics
- Residual diagnostics confirmed assumptions of normality and homoscedasticity were satisfied.

## Key Findings
- Significant Factors: Poverty rate, veteran population, and population density were the most influential predictors of Clinton's vote share
- Multicollinearity: Diagnosed using Variance Inflation Factor (VIF); no severe multicollinearity detected
- Model Validation: The final model explained 68.7% of the variance in vote share with minimal residual issues
