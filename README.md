# CCHS-analysis
CCHS analysis in R - Project is ongoing

The Canadian Community Health Survey (CCHS) is a cross-sectional survey that collects information related to health status, health care utilization and health determinants for a population of Canadians 12 years of age and over living in the ten provinces and three territories. The survey was first administered in 2001 and was repeated every two years until 2007, interviewing approximately 130,000 respondents each period. Starting in 2007, data was collected annually instead of every two years, and the sample size was reduced to 65,000 respondents each year.

Parallel analysis followed by exploratory factor analysis (EFA) were chosen as methods of grouping variables with high levels of correlation prior to future pooling of data. For both parallel analysis and EFA a common factor (CF) model was chosen to produce more conservative loading factors and to limit the effect of data collection errors on variance calculations. Once loadings were determined, a standard cut off point of 0.3 was selected to ensure that no factor held multiple loadings.To ensure the statistical validity of the simple structure factor model, the root mean square of residuals (RMSR) was required to be close to 0, the root mean square error of approximation (RMSEA) index needed to be below 0.05, showing good model fit, and the Tucker-Lewis Index (TLI) had to remain above 0.9.

Due to the size of the CCHS surveys, data from 2014 alone was used to create less complex test data frames with faster processing speed to ensure the functionality of the tests and models chosen prior to application across all data sets.
