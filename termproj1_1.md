Regression Analysis Project
================
Caitlyn Koyabu
2023-12-05

``` r
library(MASS)
library(leaps)
library(readr)
library(faraway)
```

``` r
setwd("~/Downloads")

countries <- read.csv("countries.csv") 

# The total sample size
n <- nrow(countries) 

# Set a random seed for reproducing. Change it to a different number.
set.seed(109) 

# Get a random sample of size 80% * n from 1:n (the row indices)
subset_id <- sample(n, 0.8*n) 

# Access the subset data by row indices and save the data in a new data set
## Make sure to save it in a new data set or you will be still using the whole data set
countries_subset <- countries[subset_id, ] 
```

``` r
#we are taking the simple linear regression for each of the predictor variables and doing residual analysis from it

#scatter plot with all the predictors vs response
# List of predictor variables
predictor_vars <- c("LandArea", "Population", "Rural", "Health", "Internet", "BirthRate", "ElderlyPop", "CO2","GDP",  "Cell")

par(mfrow = c(3,3))

# Loop through each predictor and create scatter plot
for (predictor in predictor_vars) {
  plot(countries_subset[[predictor]], countries_subset$LifeExpectancy,
       xlab = predictor, ylab = "Life Expectancy", main = paste("Scatter Plot of", predictor, "vs. Life Expectancy"))
   abline(lm(countries_subset$LifeExpectancy~countries_subset[[predictor]]), col = "red")
}
```

![](termproj1_1_files/figure-gfm/unnamed-chunk-3-1.png)<!-- -->

``` r
#residual vs fitted plots
for (i in seq_along(predictor_vars)) {
  predictor <- predictor_vars[i]
  temp_data <- data.frame(predictor = countries_subset[[predictor]], LifeExpectancy = countries_subset$LifeExpectancy)
  
  # Fit a simple linear regression model
  temp_model <- lm(LifeExpectancy ~ predictor, data = temp_data)
  
  # Residuals vs. Fitted plot
  plot(temp_model, which = 1, main = paste("Residuals vs. Fitted for", predictor),
       pch = 16, cex = 0.7, cex.main = 1)
}
```

![](termproj1_1_files/figure-gfm/unnamed-chunk-3-2.png)<!-- -->

``` r
# Loop through each predictor and create Q-Q plot
for (predictor in predictor_vars) {
  qqnorm(resid(lm(LifeExpectancy ~ countries_subset[[predictor]], data = countries_subset)), 
         main = paste("Q-Q Plot for", predictor))
  qqline(resid(lm(LifeExpectancy ~ countries_subset[[predictor]], data = countries_subset)), col = 2)
}
```

![](termproj1_1_files/figure-gfm/unnamed-chunk-3-3.png)<!-- -->

``` r
# Reset plotting parameters to default
par(mfrow = c(1, 1), mar = c(5, 4, 4, 2) + 0.1)
```

![](termproj1_1_files/figure-gfm/unnamed-chunk-3-4.png)<!-- -->

``` r
#Analysis:
#area - nonlinearity, nonconstant variance,nonnormality -> transform y
#pop - nonlinearity, nonconstant variance, nonnormality -> transform y
#rural - linearity, nonconstant variance, nonnomarlity -> ?
#health - linearity, nonconstant variance,nonnormality -> transform y
#internet - nonlinearity, nonconstant variance, nonnormality -> transform y
#birthrate - nonlinearity, nonconstant variance, normality -> transform y
#elderly - nonlinearity, nonconstant variance, normality -> transform y
#co2 - nonlinearity, nonconstant variance, nonnormality -> transform y
#gdp - nonlinearity, nonconstant variance, nonnormality -> transform y
#cell - nonlinearity, nonconstant variance, normality -> transform y
```

``` r
par(mfrow = c(2,2))

#remove outliers
#interquartile range method
remove_outliers <- function(x) {
  non_missing_values <- x[!is.na(x)]
  q <- quantile(non_missing_values, c(0.25, 0.75), na.rm = TRUE)
  iqr <- q[2] - q[1]
  lower_bound <- q[1] - 1.5 * iqr
  upper_bound <- q[2] + 1.5 * iqr
  outliers <- x < lower_bound | x > upper_bound
  x[outliers] <- NA
  return(x)
}

#loop through each predictor and remove outliers
for (predictor in predictor_vars) {
  countries_subset[[predictor]] <- remove_outliers(countries_subset[[predictor]])
}

#rebuild model after removing outliers
model_without_outliers <- lm(LifeExpectancy~LandArea+Population+Rural+Health+Internet+BirthRate+ElderlyPop+CO2+GDP+Cell, data = countries_subset)
summary(model_without_outliers)
```

    ## 
    ## Call:
    ## lm(formula = LifeExpectancy ~ LandArea + Population + Rural + 
    ##     Health + Internet + BirthRate + ElderlyPop + CO2 + GDP + 
    ##     Cell, data = countries_subset)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -16.2838  -2.7853   0.4557   3.1538  11.6390 
    ## 
    ## Coefficients:
    ##               Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)  8.501e+01  4.744e+00  17.919   <2e-16 ***
    ## LandArea    -4.418e-06  1.942e-06  -2.275   0.0252 *  
    ## Population   3.820e-02  5.194e-02   0.736   0.4639    
    ## Rural       -4.414e-02  3.555e-02  -1.242   0.2175    
    ## Health       3.247e-01  1.342e-01   2.419   0.0175 *  
    ## Internet     3.150e-02  4.822e-02   0.653   0.5152    
    ## BirthRate   -7.471e-01  1.025e-01  -7.290    1e-10 ***
    ## ElderlyPop  -3.902e-01  2.281e-01  -1.710   0.0906 .  
    ## CO2         -4.590e-01  2.910e-01  -1.577   0.1181    
    ## GDP          1.390e-04  1.222e-04   1.138   0.2580    
    ## Cell         1.740e-02  1.903e-02   0.914   0.3629    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 5.276 on 93 degrees of freedom
    ##   (44 observations deleted due to missingness)
    ## Multiple R-squared:  0.7708, Adjusted R-squared:  0.7461 
    ## F-statistic: 31.27 on 10 and 93 DF,  p-value: < 2.2e-16

``` r
plot(model_without_outliers, which = 1:2)

#from analysis - removing the outliers helped the plots look better.

#Analysis:
#area - nonlinearity, constant variance, nonnormality ->
#pop - nonlinearity, constant variance, nonnormality ->
#rural - linearity, nonconstant variance, nonnomarlity ->
#health - linearity, constant variance,nonnormality ->
#internet - nonlinearity, nonconstant variance, nonnormality -> transform y
#birthrate - nonlinearity, constant variance, nonnormality -> transform x
#elderly - nonlinearity, nonconstant variance, nonnormality ->
#co2 - nonlinearity, nonconstant variance, nonnormality -> transform y
#gdp - nonlinearity, nonconstant variance, nonnormality -> transform y
#cell - nonlinearity, nonconstant variance, normality -> transform y
```

![](termproj1_1_files/figure-gfm/unnamed-chunk-4-1.png)<!-- -->

``` r
# running this again to reset the graphs to be what is expected
par(mfrow = c(3,3))
# Loop through each predictor and create scatter plot
for (predictor in predictor_vars) {
  plot(countries_subset[[predictor]], countries_subset$LifeExpectancy,
       xlab = predictor, ylab = "Life Expectancy", main = paste("Scatter Plot of", predictor, "vs. Life Expectancy"))
   abline(lm(countries_subset$LifeExpectancy~countries_subset[[predictor]]), col = "red")
}
```

![](termproj1_1_files/figure-gfm/unnamed-chunk-5-1.png)<!-- -->

``` r
#residual vs fitted plots
for (i in seq_along(predictor_vars)) {
  predictor <- predictor_vars[i]
  temp_data <- data.frame(predictor = countries_subset[[predictor]], LifeExpectancy = countries_subset$LifeExpectancy)
  
  # Fit a simple linear regression model
  temp_model <- lm(LifeExpectancy ~ predictor, data = temp_data)
  
  # Residuals vs. Fitted plot
  plot(temp_model, which = 1, main = paste("Residuals vs. Fitted for", predictor),
       pch = 16, cex = 0.7, cex.main = 1)
}
```

![](termproj1_1_files/figure-gfm/unnamed-chunk-5-2.png)<!-- -->

``` r
# Loop through each predictor and create Q-Q plot
for (predictor in predictor_vars) {
  qqnorm(resid(lm(LifeExpectancy ~ countries_subset[[predictor]], data = countries_subset)), 
         main = paste("Q-Q Plot for", predictor))
  qqline(resid(lm(LifeExpectancy ~ countries_subset[[predictor]], data = countries_subset)), col = 2)
}
```

![](termproj1_1_files/figure-gfm/unnamed-chunk-5-3.png)<!-- -->

``` r
# Reset plotting parameters to default
par(mfrow = c(1, 1), mar = c(5, 4, 4, 2) + 0.1)
```

![](termproj1_1_files/figure-gfm/unnamed-chunk-5-4.png)<!-- -->

``` r
#look at scatter plots and take summary to see the p-value
modelarea <- lm(LifeExpectancy~LandArea, data = countries_subset)
summary(modelarea)
```

    ## 
    ## Call:
    ## lm(formula = LifeExpectancy ~ LandArea, data = countries_subset)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -25.845  -5.023   1.940   6.776  19.161 
    ## 
    ## Coefficients:
    ##               Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)  7.188e+01  1.051e+00  68.387  < 2e-16 ***
    ## LandArea    -1.386e-05  2.422e-06  -5.723 6.69e-08 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 9.626 on 132 degrees of freedom
    ##   (14 observations deleted due to missingness)
    ## Multiple R-squared:  0.1988, Adjusted R-squared:  0.1927 
    ## F-statistic: 32.76 on 1 and 132 DF,  p-value: 6.695e-08

``` r
modelpopulation <- lm(LifeExpectancy~Population, data = countries_subset)
summary(modelpopulation)
```

    ## 
    ## Call:
    ## lm(formula = LifeExpectancy ~ Population, data = countries_subset)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -23.678  -7.164   3.418   7.374  16.805 
    ## 
    ## Coefficients:
    ##             Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept) 69.56200    1.23803  56.187   <2e-16 ***
    ## Population  -0.13510    0.07833  -1.725    0.087 .  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 10.46 on 126 degrees of freedom
    ##   (20 observations deleted due to missingness)
    ## Multiple R-squared:  0.02307,    Adjusted R-squared:  0.01531 
    ## F-statistic: 2.975 on 1 and 126 DF,  p-value: 0.08702

``` r
modelrural <- lm(LifeExpectancy~Rural, data = countries_subset)
summary(modelrural)
```

    ## 
    ## Call:
    ## lm(formula = LifeExpectancy ~ Rural, data = countries_subset)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -22.071  -4.058   1.740   5.642  16.884 
    ## 
    ## Coefficients:
    ##             Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept) 81.40947    1.49467  54.466   <2e-16 ***
    ## Rural       -0.28496    0.02925  -9.742   <2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 8.197 on 146 degrees of freedom
    ## Multiple R-squared:  0.394,  Adjusted R-squared:  0.3898 
    ## F-statistic: 94.91 on 1 and 146 DF,  p-value: < 2.2e-16

``` r
modelhealth <- lm(LifeExpectancy~Health, data = countries_subset)
summary(modelhealth)
```

    ## 
    ## Call:
    ## lm(formula = LifeExpectancy ~ Health, data = countries_subset)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -25.852  -4.513   3.874   7.265  15.303 
    ## 
    ## Coefficients:
    ##             Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)  60.2121     2.3988  25.100  < 2e-16 ***
    ## Health        0.7216     0.1997   3.612 0.000417 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 10.09 on 145 degrees of freedom
    ##   (1 observation deleted due to missingness)
    ## Multiple R-squared:  0.08257,    Adjusted R-squared:  0.07624 
    ## F-statistic: 13.05 on 1 and 145 DF,  p-value: 0.0004173

``` r
modelinternet <- lm(LifeExpectancy~Internet, data = countries_subset)
summary(modelinternet)
```

    ## 
    ## Call:
    ## lm(formula = LifeExpectancy ~ Internet, data = countries_subset)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -19.726  -5.277   1.193   5.537  14.340 
    ## 
    ## Coefficients:
    ##             Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept) 60.62429    0.87962   68.92   <2e-16 ***
    ## Internet     0.28960    0.02365   12.25   <2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 7.395 on 146 degrees of freedom
    ## Multiple R-squared:  0.5068, Adjusted R-squared:  0.5034 
    ## F-statistic:   150 on 1 and 146 DF,  p-value: < 2.2e-16

``` r
modelbirthrate <- lm(LifeExpectancy~BirthRate, data = countries_subset)
summary(modelbirthrate)
```

    ## 
    ## Call:
    ## lm(formula = LifeExpectancy ~ BirthRate, data = countries_subset)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -18.1173  -3.3621   0.5574   3.3177  10.5519 
    ## 
    ## Coefficients:
    ##             Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept) 87.09732    0.98727   88.22   <2e-16 ***
    ## BirthRate   -0.82876    0.03939  -21.04   <2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 5.244 on 146 degrees of freedom
    ## Multiple R-squared:  0.752,  Adjusted R-squared:  0.7503 
    ## F-statistic: 442.6 on 1 and 146 DF,  p-value: < 2.2e-16

``` r
modelelderlypop <- lm(LifeExpectancy~ElderlyPop, data = countries_subset)
summary(modelelderlypop)
```

    ## 
    ## Call:
    ## lm(formula = LifeExpectancy ~ ElderlyPop, data = countries_subset)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -19.573  -5.292   1.209   5.774  17.964 
    ## 
    ## Coefficients:
    ##             Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)  58.3899     1.1441   51.03   <2e-16 ***
    ## ElderlyPop    1.3457     0.1265   10.64   <2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 7.902 on 146 degrees of freedom
    ## Multiple R-squared:  0.4368, Adjusted R-squared:  0.4329 
    ## F-statistic: 113.2 on 1 and 146 DF,  p-value: < 2.2e-16

``` r
modelco2 <- lm(LifeExpectancy~CO2, data = countries_subset)
summary(modelco2)
```

    ## 
    ## Call:
    ## lm(formula = LifeExpectancy ~ CO2, data = countries_subset)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -24.834  -4.661   2.153   6.739  14.523 
    ## 
    ## Coefficients:
    ##             Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)  62.0640     1.0454  59.368  < 2e-16 ***
    ## CO2           1.5974     0.2012   7.938 6.61e-13 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 8.789 on 137 degrees of freedom
    ##   (9 observations deleted due to missingness)
    ## Multiple R-squared:  0.315,  Adjusted R-squared:   0.31 
    ## F-statistic: 63.01 on 1 and 137 DF,  p-value: 6.612e-13

``` r
modelgdp<- lm(LifeExpectancy~GDP, data = countries_subset)
summary(modelgdp)
```

    ## 
    ## Call:
    ## lm(formula = LifeExpectancy ~ GDP, data = countries_subset)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -26.922  -5.324   2.610   6.347  12.172 
    ## 
    ## Coefficients:
    ##              Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept) 6.174e+01  9.785e-01  63.095  < 2e-16 ***
    ## GDP         7.692e-04  9.994e-05   7.697 3.53e-12 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 8.417 on 126 degrees of freedom
    ##   (20 observations deleted due to missingness)
    ## Multiple R-squared:  0.3198, Adjusted R-squared:  0.3144 
    ## F-statistic: 59.24 on 1 and 126 DF,  p-value: 3.527e-12

``` r
modelcell <- lm(LifeExpectancy~Cell, data = countries_subset)
summary(modelcell)
```

    ## 
    ## Call:
    ## lm(formula = LifeExpectancy ~ Cell, data = countries_subset)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -19.425  -4.755   1.147   4.880  22.999 
    ## 
    ## Coefficients:
    ##             Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept) 54.31013    1.49540   36.32   <2e-16 ***
    ## Cell         0.15614    0.01489   10.48   <2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 7.953 on 146 degrees of freedom
    ## Multiple R-squared:  0.4295, Adjusted R-squared:  0.4256 
    ## F-statistic: 109.9 on 1 and 146 DF,  p-value: < 2.2e-16

``` r
# inlcude landarea, rural, health, internet, birthrate, elderlypop, co2, gdp, cell
```

``` r
par(mfrow = c(2,2))

#create multiple regression model
model <- lm(LifeExpectancy~LandArea+Population+Rural+Health+Internet+BirthRate+ElderlyPop+CO2+GDP+Cell, data = countries_subset)
model
```

    ## 
    ## Call:
    ## lm(formula = LifeExpectancy ~ LandArea + Population + Rural + 
    ##     Health + Internet + BirthRate + ElderlyPop + CO2 + GDP + 
    ##     Cell, data = countries_subset)
    ## 
    ## Coefficients:
    ## (Intercept)     LandArea   Population        Rural       Health     Internet  
    ##   8.501e+01   -4.418e-06    3.820e-02   -4.414e-02    3.247e-01    3.150e-02  
    ##   BirthRate   ElderlyPop          CO2          GDP         Cell  
    ##  -7.471e-01   -3.902e-01   -4.590e-01    1.390e-04    1.740e-02

``` r
plot(model, which = 1:2)
summary(model)
```

    ## 
    ## Call:
    ## lm(formula = LifeExpectancy ~ LandArea + Population + Rural + 
    ##     Health + Internet + BirthRate + ElderlyPop + CO2 + GDP + 
    ##     Cell, data = countries_subset)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -16.2838  -2.7853   0.4557   3.1538  11.6390 
    ## 
    ## Coefficients:
    ##               Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)  8.501e+01  4.744e+00  17.919   <2e-16 ***
    ## LandArea    -4.418e-06  1.942e-06  -2.275   0.0252 *  
    ## Population   3.820e-02  5.194e-02   0.736   0.4639    
    ## Rural       -4.414e-02  3.555e-02  -1.242   0.2175    
    ## Health       3.247e-01  1.342e-01   2.419   0.0175 *  
    ## Internet     3.150e-02  4.822e-02   0.653   0.5152    
    ## BirthRate   -7.471e-01  1.025e-01  -7.290    1e-10 ***
    ## ElderlyPop  -3.902e-01  2.281e-01  -1.710   0.0906 .  
    ## CO2         -4.590e-01  2.910e-01  -1.577   0.1181    
    ## GDP          1.390e-04  1.222e-04   1.138   0.2580    
    ## Cell         1.740e-02  1.903e-02   0.914   0.3629    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 5.276 on 93 degrees of freedom
    ##   (44 observations deleted due to missingness)
    ## Multiple R-squared:  0.7708, Adjusted R-squared:  0.7461 
    ## F-statistic: 31.27 on 10 and 93 DF,  p-value: < 2.2e-16

![](termproj1_1_files/figure-gfm/unnamed-chunk-7-1.png)<!-- -->

``` r
#multicollinearity
modelvif2 <- lm(LifeExpectancy~LandArea+Population+Rural+Health+Internet+BirthRate+ElderlyPop+CO2+GDP+Cell, data = countries_subset)
vif(modelvif2)
```

    ##   LandArea Population      Rural     Health   Internet  BirthRate ElderlyPop 
    ##   1.839734   1.507146   2.121045   1.073318   3.663301   4.951540   4.038328 
    ##        CO2        GDP       Cell 
    ##   3.135684   2.941638   2.344324

``` r
#conclusion
#landarea, population, rural, health, internet, co2, gdp, cell is good
#birthrate, elderlypop, are moderately problematic
```

``` r
#transforming the y variable of the modelwithoutoutliers and seeing if it helps the models
par(mfrow = c(3,3))
#possible transformations of outcome variable
#fit a linear regression model for life expectancy on predictor variables

box_cox_res <- boxcox(model, plotit = TRUE, lambda = seq(-7, 7, 1/10))

#extract lambda value
lambda <- box_cox_res$x[which.max(box_cox_res$y)]
print(paste("The optimal lambda is ", lambda))
```

    ## [1] "The optimal lambda is  3.7"

``` r
model_trans_3 <- lm(LifeExpectancy^3~LandArea+Population+Rural+Health+Internet+BirthRate+ElderlyPop+CO2+GDP+Cell, data = countries_subset)
plot(model_trans_3, which = 1:2)
summary(model_trans_3)
```

    ## 
    ## Call:
    ## lm(formula = LifeExpectancy^3 ~ LandArea + Population + Rural + 
    ##     Health + Internet + BirthRate + ElderlyPop + CO2 + GDP + 
    ##     Cell, data = countries_subset)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -177543  -41161    7195   37898  148960 
    ## 
    ## Coefficients:
    ##               Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)  5.058e+05  5.477e+04   9.235 8.49e-15 ***
    ## LandArea    -4.716e-02  2.242e-02  -2.103   0.0381 *  
    ## Population   4.063e+02  5.996e+02   0.678   0.4997    
    ## Rural       -5.470e+02  4.103e+02  -1.333   0.1858    
    ## Health       3.669e+03  1.549e+03   2.368   0.0199 *  
    ## Internet     4.157e+02  5.567e+02   0.747   0.4572    
    ## BirthRate   -8.271e+03  1.183e+03  -6.990 4.09e-10 ***
    ## ElderlyPop  -3.181e+03  2.634e+03  -1.208   0.2302    
    ## CO2         -6.301e+03  3.360e+03  -1.875   0.0639 .  
    ## GDP          3.333e+00  1.410e+00   2.364   0.0202 *  
    ## Cell         1.771e+02  2.197e+02   0.806   0.4221    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 60910 on 93 degrees of freedom
    ##   (44 observations deleted due to missingness)
    ## Multiple R-squared:  0.7934, Adjusted R-squared:  0.7712 
    ## F-statistic: 35.71 on 10 and 93 DF,  p-value: < 2.2e-16

``` r
#Linearity - From the Residuals vs. Fitted plot, the model assumptions are violated because the red line seems to curve and seems to deviate from the 0 line, which means we can't assume linearity.
#Constant Variance - From the Residuals vs. Fitted plot, the model assumpton seems to hold. It looks better than the first model. Transformed model constant variance looks better and seems to not have patterns.
#Normality - From the Normal QQ plot, the model assumption seems to hold true since most of the values lay on the regression line so we can assume normality.


model_trans_4.1 <- lm(LifeExpectancy^4.1~LandArea+Population+Rural+Health+Internet+BirthRate+ElderlyPop+CO2+GDP+Cell, data = countries_subset)
plot(model_trans_4.1, which = 1:2)
summary(model_trans_4.1)
```

    ## 
    ## Call:
    ## lm(formula = LifeExpectancy^4.1 ~ LandArea + Population + Rural + 
    ##     Health + Internet + BirthRate + ElderlyPop + CO2 + GDP + 
    ##     Cell, data = countries_subset)
    ## 
    ## Residuals:
    ##       Min        1Q    Median        3Q       Max 
    ## -23993801  -5623084    591579   4955607  20634246 
    ## 
    ## Coefficients:
    ##               Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)  5.735e+07  7.374e+06   7.776 9.91e-12 ***
    ## LandArea    -5.985e+00  3.019e+00  -1.982   0.0504 .  
    ## Population   5.300e+04  8.073e+04   0.656   0.5131    
    ## Rural       -7.535e+04  5.525e+04  -1.364   0.1759    
    ## Health       4.839e+05  2.086e+05   2.320   0.0225 *  
    ## Internet     5.509e+04  7.496e+04   0.735   0.4643    
    ## BirthRate   -1.058e+06  1.593e+05  -6.640 2.08e-09 ***
    ## ElderlyPop  -3.160e+05  3.546e+05  -0.891   0.3752    
    ## CO2         -9.172e+05  4.524e+05  -2.028   0.0455 *  
    ## GDP          5.902e+02  1.899e+02   3.108   0.0025 ** 
    ## Cell         2.087e+04  2.958e+04   0.706   0.4822    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 8201000 on 93 degrees of freedom
    ##   (44 observations deleted due to missingness)
    ## Multiple R-squared:  0.7988, Adjusted R-squared:  0.7772 
    ## F-statistic: 36.93 on 10 and 93 DF,  p-value: < 2.2e-16

``` r
#Linearity - From the Residuals vs. Fitted plot, the model assumptions could be assumed and can hold. It looks better transformed than when it did before it was transformed. most of the plots are on line and doesn't curve as much than it did for the first plot.
#Constant Variance - From the Residuals vs. Fitted plot, the model assumpton seems to hold.Transformed model constant variance looks better and seems to not have patterns.
#Normality - From the Normal QQ plot, the model assumption seems to hold true since most of the values lay on the regression line so we can assume normality.

model_trans_2.7 <- lm(LifeExpectancy^2.7~LandArea+Population+Rural+Health+Internet+BirthRate+ElderlyPop+CO2+GDP+Cell, data = countries_subset)
plot(model_trans_2.7, which = 1:2)
summary(model_trans_2.7)
```

    ## 
    ## Call:
    ## lm(formula = LifeExpectancy^2.7 ~ LandArea + Population + Rural + 
    ##     Health + Internet + BirthRate + ElderlyPop + CO2 + GDP + 
    ##     Cell, data = countries_subset)
    ## 
    ## Residuals:
    ##    Min     1Q Median     3Q    Max 
    ## -45740 -10235   1450  10273  38135 
    ## 
    ## Coefficients:
    ##               Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)  1.386e+05  1.418e+04   9.776 6.08e-16 ***
    ## LandArea    -1.239e-02  5.805e-03  -2.134   0.0355 *  
    ## Population   1.063e+02  1.552e+02   0.685   0.4951    
    ## Rural       -1.405e+02  1.062e+02  -1.322   0.1893    
    ## Health       9.541e+02  4.011e+02   2.379   0.0194 *  
    ## Internet     1.070e+02  1.441e+02   0.742   0.4598    
    ## BirthRate   -2.164e+03  3.063e+02  -7.066 2.87e-10 ***
    ## ElderlyPop  -8.802e+02  6.818e+02  -1.291   0.1999    
    ## CO2         -1.594e+03  8.697e+02  -1.832   0.0701 .  
    ## GDP          7.908e-01  3.651e-01   2.166   0.0329 *  
    ## Cell         4.715e+01  5.687e+01   0.829   0.4092    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 15770 on 93 degrees of freedom
    ##   (44 observations deleted due to missingness)
    ## Multiple R-squared:  0.7912, Adjusted R-squared:  0.7687 
    ## F-statistic: 35.23 on 10 and 93 DF,  p-value: < 2.2e-16

``` r
#Linearity - From the Residuals vs. Fitted plot, the model assumptions are violated because the red line seems to curve and seems to devite slightly so it can be argueed it is violated.
#Constant Variance - From the Residuals vs. Fitted plot, the model assumpton seems to hold. It looks better than the first model. Transformed model constant variance looks better and seems to not have patterns.
#Normality - From the Normal QQ plot, the model assumption seems to hold true since most of the values lay on the regression line so we can assume normality.


#In conclusion, the transformed y value at lambda = 4.1 is the best transformed model out of the model with lambdas = 2.7 and 3.

countries_subset$transformed_LifeExpectancy <- (countries_subset$LifeExpectancy)^4.1
model_trans_y <- lm(transformed_LifeExpectancy~LandArea+Population+Rural+Health+Internet+BirthRate+ElderlyPop+CO2+GDP+Cell, data = countries_subset)
summary(model_trans_y)
```

    ## 
    ## Call:
    ## lm(formula = transformed_LifeExpectancy ~ LandArea + Population + 
    ##     Rural + Health + Internet + BirthRate + ElderlyPop + CO2 + 
    ##     GDP + Cell, data = countries_subset)
    ## 
    ## Residuals:
    ##       Min        1Q    Median        3Q       Max 
    ## -23993801  -5623084    591579   4955607  20634246 
    ## 
    ## Coefficients:
    ##               Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)  5.735e+07  7.374e+06   7.776 9.91e-12 ***
    ## LandArea    -5.985e+00  3.019e+00  -1.982   0.0504 .  
    ## Population   5.300e+04  8.073e+04   0.656   0.5131    
    ## Rural       -7.535e+04  5.525e+04  -1.364   0.1759    
    ## Health       4.839e+05  2.086e+05   2.320   0.0225 *  
    ## Internet     5.509e+04  7.496e+04   0.735   0.4643    
    ## BirthRate   -1.058e+06  1.593e+05  -6.640 2.08e-09 ***
    ## ElderlyPop  -3.160e+05  3.546e+05  -0.891   0.3752    
    ## CO2         -9.172e+05  4.524e+05  -2.028   0.0455 *  
    ## GDP          5.902e+02  1.899e+02   3.108   0.0025 ** 
    ## Cell         2.087e+04  2.958e+04   0.706   0.4822    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 8201000 on 93 degrees of freedom
    ##   (44 observations deleted due to missingness)
    ## Multiple R-squared:  0.7988, Adjusted R-squared:  0.7772 
    ## F-statistic: 36.93 on 10 and 93 DF,  p-value: < 2.2e-16

``` r
plot(model_trans_y, which = 1:2)
```

![](termproj1_1_files/figure-gfm/unnamed-chunk-9-1.png)<!-- -->

``` r
#scatterplot transformed y with x

# Scatter plot with transformed y and x predictors
par(mfrow = c(3, 3))

# Loop through each predictor and create scatter plot
for (predictor in predictor_vars) {
  if (predictor %in% colnames(countries_subset)) {
    plot(countries_subset[[predictor]], countries_subset$transformed_LifeExpectancy,
         xlab = predictor, ylab = "Transformed Life Expectancy",
         main = paste("Scatter Plot of", predictor, "vs. Transformed Life Expectancy"))
    abline(lm(transformed_LifeExpectancy ~ countries_subset[[predictor]], data = countries_subset), col = "red")
  } else {
    cat("Variable not found:", predictor, "\n")
  }
}
```

![](termproj1_1_files/figure-gfm/unnamed-chunk-10-1.png)<!-- -->

``` r
# Residuals vs. Fitted plot for transformed model
par(mfrow = c(3, 3))
```

![](termproj1_1_files/figure-gfm/unnamed-chunk-10-2.png)<!-- -->

``` r
# Loop through each predictor and create Residuals vs. Fitted plot
for (predictor in predictor_vars) {
  if (predictor %in% colnames(countries_subset)) {
    temp_data <- data.frame(
      predictor = countries_subset[[predictor]],
      transformed_LifeExpectancy= countries_subset$transformed_LifeExpectancy
    )
    
    # Fit a simple linear regression model
    temp_model <- lm(transformed_LifeExpectancy ~ predictor, data = temp_data)
    
    # Residuals vs. Fitted plot
    plot(temp_model, which = 1, main = paste("Residuals vs. Fitted for", predictor),
         pch = 16, cex = 0.7, cex.main = 1)
  } else {
    cat("Variable not found:", predictor, "\n")
  }
}
```

![](termproj1_1_files/figure-gfm/unnamed-chunk-10-3.png)<!-- -->

``` r
# Reset plotting parameters to default
par(mfrow = c(1, 1), mar = c(5, 4, 4, 2) + 0.1)
```

![](termproj1_1_files/figure-gfm/unnamed-chunk-10-4.png)<!-- -->

``` r
# QQ plots for each predictor
par(mfrow = c(3, 3))

# Loop through each predictor and create Q-Q plot
for (predictor in predictor_vars) {
  if (predictor %in% colnames(countries_subset)) {
    qqnorm(resid(lm(transformed_LifeExpectancy ~ countries_subset[[predictor]], data = countries_subset)), 
           main = paste("Q-Q Plot for", predictor))
    qqline(resid(lm(transformed_LifeExpectancy ~ countries_subset[[predictor]], data = countries_subset)), col = 2)
  } else {
    cat("Variable not found:", predictor, "\n")
  }
}
```

![](termproj1_1_files/figure-gfm/unnamed-chunk-10-5.png)<!-- -->

``` r
# Reset plotting parameters to default
par(mfrow = c(1, 1), mar = c(5, 4, 4, 2) + 0.1)
```

![](termproj1_1_files/figure-gfm/unnamed-chunk-10-6.png)<!-- -->

``` r
#Analysis:
#area: linearity, constant variance, normality 
#pop: linearity, constant varinace, nonnormality
#rural: linearity, constant variance, normality
#health: linearity, constant variance, nonnormality
#internet: nonlinearity, nonconstant variance, nonnormality -> transform x
#birthrate: linearity, nonconstant variance, normality
#elderly: non linearity, nonconstant variance, nonnormality -> transform x
#co2: nonlinearity, nonconstant variance, nonnormality -> transform x
#gdp:nonlinearity, nonconstant variance, nonnormality -> transform x
#cell: nonlinearity, nonconstant variance, normality -> transform x
```

``` r
#transformation of x


#internet
par(mfrow = c(3,3))

model_internet_0.5 <- lm(LifeExpectancy~I(Internet^0.5), data = countries_subset)
plot(model_internet_0.5, which =1:2)
#linearity - linearity not met. Model assumptions are violated. 
#constant variance - Model assumptions are violated. There is a pattern. 
#normality - violates

model_internet_0.3 <- lm(LifeExpectancy~I(Internet^0.3), data = countries_subset)
plot(model_internet_0.3, which =1:2)
#linearity - can still assume linearity as it is mostly on 0 line. 
#constant variance - non constant variance. Model assumptions are violated. There is a pattern.
#normality - violates

model_internet_0.8<- lm(LifeExpectancy~I(Internet^0.8), data = countries_subset)
plot(model_internet_0.8, which =1:2)
#linearity - Model assumptions are violated. The line curves. Nonlinearity.
#constant variance - Pattern so nonconstant variance. Model assumptions are violted
#normality - violates
#internet best is 0.3. for transformation of x. 


#elderly 
model_elderly_neg0.2 <- lm(LifeExpectancy~I(ElderlyPop^-0.2), data = countries_subset)
plot(model_elderly_neg0.2, which =1:2)
#linearity - Model assumptions are violated. Nonlinearity.
#constant variance - violated. There is a pattern. we can't assume constant variance.
#normality - violates

model_elderly_neg0.3 <- lm(LifeExpectancy~I(ElderlyPop^-0.3), data = countries_subset)
plot(model_elderly_neg0.3, which =1:2)
```

![](termproj1_1_files/figure-gfm/unnamed-chunk-11-1.png)<!-- -->

``` r
#linearity - violates, but better than the other plots and better than before
#constant variance - violated
#normality - violated

model_elderly_neg0.35 <-  lm(LifeExpectancy~I(ElderlyPop^-0.35), data = countries_subset)
plot(model_elderly_neg0.35, which =1:2)
#linearity - violated.
#constant variance - Pattern so nonconstant variance. Model assumptions are violted
#normality - violates
#elderly best lambda is -0.3 for transformation of x. 


#co2
model_co2_0.4 <- lm(LifeExpectancy~I(CO2^0.4), data = countries_subset)
plot(model_co2_0.4, which =1:2)
#linearity - violates
#constant variance - constant variance assumed
#normality - violates

model_co2_0.1 <- lm(LifeExpectancy~I(CO2^0.1), data = countries_subset)
plot(model_co2_0.1, which =1:2)
#linearity - can still assume linearity as it is mostly on 0 line. -> slightly curved tho
#constant variance - can assume
#normality - violates

model_co2_log <- lm(LifeExpectancy~log(CO2), data = countries_subset)
plot(model_co2_log, which =1:2)
```

![](termproj1_1_files/figure-gfm/unnamed-chunk-11-2.png)<!-- -->

``` r
#linearity - can still assume linearity as it is mostly on 0 line. 
#constant variance - can assume
#normality - violates
#co2 best is log(co2) for transformation of x. 


#gdp
model_gdp_0.6 <- lm(LifeExpectancy~I(GDP^0.6), data = countries_subset)
plot(model_gdp_0.6, which =1:2)
#linearity - violates
#constant variance - violates
#normality - violates

model_gdp_0.5 <- lm(LifeExpectancy~I(GDP^0.5), data = countries_subset)
plot(model_gdp_0.5, which =1:2)
#linearity - best one but still violates 
#constant variance - violates
#normality - violates

model_gdp_log <- lm(LifeExpectancy~log(GDP), data = countries_subset)
plot(model_gdp_log, which =1:2)
#linearity - best one but still violates, can argue it could.
#constant variance - can argue both ways.
#normality - violates
#gdp best is log for x transformation. 


#cell
model_cell_1.2 <- lm(LifeExpectancy~I(Cell^1.2), data = countries_subset)
plot(model_cell_1.2, which =1:2)
#linearity - violates
#constant variance - violates
#normality - violates

model_cell_0.6 <- lm(LifeExpectancy~I(Cell^0.6), data = countries_subset)
plot(model_cell_0.6, which =1:2)
```

![](termproj1_1_files/figure-gfm/unnamed-chunk-11-3.png)<!-- -->

``` r
#linearity - violates
#constant variance - violates
#normality - violates

model_cell_0.8 <- lm(LifeExpectancy~I(Cell^0.8), data = countries_subset)
plot(model_cell_0.8, which =1:2)
#linearity - best one but still violates 
#constant variance - violates
#normality - violates
#cell best is 0.6 for transformation of x. 


#conclusion: 
#internet best is 0.3. for transformation of x. 
#elderly best lambda is -0.3 for transformation of x. 
#co2 best is log(co2) for transformation of x. 
#gdp best is log for x transformation. 
#cell best is 0.6 for transformation of x. 

#put in the graphs
countries_subset$transformed_Internet <- (countries_subset$Internet^0.3)
countries_subset$transformed_ElderlyPop <- (countries_subset$ElderlyPop^-0.3)
countries_subset$transformed_CO2 <- log(countries_subset$CO2)
countries_subset$transformed_GDP <- log(countries_subset$GDP)
countries_subset$transformed_Cell <- (countries_subset$Cell^0.6)

#model with transformed y and x's 
model_trans_y_x <- lm(transformed_LifeExpectancy~LandArea+Population+Rural+Health+transformed_Internet+BirthRate+transformed_ElderlyPop+transformed_CO2+transformed_GDP+transformed_Cell, data = countries_subset)
summary(model_trans_y_x)
```

    ## 
    ## Call:
    ## lm(formula = transformed_LifeExpectancy ~ LandArea + Population + 
    ##     Rural + Health + transformed_Internet + BirthRate + transformed_ElderlyPop + 
    ##     transformed_CO2 + transformed_GDP + transformed_Cell, data = countries_subset)
    ## 
    ## Residuals:
    ##       Min        1Q    Median        3Q       Max 
    ## -22462587  -5576753    380534   4864209  17853939 
    ## 
    ## Coefficients:
    ##                          Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)             1.993e+07  1.732e+07   1.151 0.252667    
    ## LandArea               -7.747e+00  3.133e+00  -2.472 0.015235 *  
    ## Population              6.495e+04  8.322e+04   0.780 0.437094    
    ## Rural                  -9.290e+04  5.787e+04  -1.605 0.111822    
    ## Health                  4.154e+05  2.196e+05   1.891 0.061698 .  
    ## transformed_Internet    2.647e+06  2.161e+06   1.225 0.223684    
    ## BirthRate              -8.799e+05  2.549e+05  -3.451 0.000841 ***
    ## transformed_ElderlyPop  6.919e+06  2.230e+07   0.310 0.757090    
    ## transformed_CO2        -1.661e+06  1.350e+06  -1.231 0.221542    
    ## transformed_GDP         3.322e+06  1.534e+06   2.165 0.032937 *  
    ## transformed_Cell        4.224e+04  2.807e+05   0.150 0.880722    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 8380000 on 93 degrees of freedom
    ##   (44 observations deleted due to missingness)
    ## Multiple R-squared:   0.79,  Adjusted R-squared:  0.7674 
    ## F-statistic: 34.98 on 10 and 93 DF,  p-value: < 2.2e-16

``` r
#plot
plot(model_trans_y_x, which = 1:2)
```

![](termproj1_1_files/figure-gfm/unnamed-chunk-11-4.png)<!-- -->

``` r
#forwards selection, exhaustive and backwards elimination
model_best_subset<- regsubsets(transformed_LifeExpectancy~LandArea+Population+Rural+Health+transformed_Internet+BirthRate+transformed_ElderlyPop+transformed_CO2+transformed_GDP+transformed_Cell, data = countries_subset, method = "exhaustive")
model_forward<- regsubsets(transformed_LifeExpectancy~LandArea+Population+Rural+Health+transformed_Internet+BirthRate+transformed_ElderlyPop+transformed_CO2+transformed_GDP+transformed_Cell, data = countries_subset, method = "forward")
model_backward<- regsubsets(transformed_LifeExpectancy~LandArea+Population+Rural+Health+transformed_Internet+BirthRate+transformed_ElderlyPop+transformed_CO2+transformed_GDP+transformed_Cell, data = countries_subset, method = "backward")

summary(model_best_subset)
```

    ## Subset selection object
    ## Call: regsubsets.formula(transformed_LifeExpectancy ~ LandArea + Population + 
    ##     Rural + Health + transformed_Internet + BirthRate + transformed_ElderlyPop + 
    ##     transformed_CO2 + transformed_GDP + transformed_Cell, data = countries_subset, 
    ##     method = "exhaustive")
    ## 10 Variables  (and intercept)
    ##                        Forced in Forced out
    ## LandArea                   FALSE      FALSE
    ## Population                 FALSE      FALSE
    ## Rural                      FALSE      FALSE
    ## Health                     FALSE      FALSE
    ## transformed_Internet       FALSE      FALSE
    ## BirthRate                  FALSE      FALSE
    ## transformed_ElderlyPop     FALSE      FALSE
    ## transformed_CO2            FALSE      FALSE
    ## transformed_GDP            FALSE      FALSE
    ## transformed_Cell           FALSE      FALSE
    ## 1 subsets of each size up to 8
    ## Selection Algorithm: exhaustive
    ##          LandArea Population Rural Health transformed_Internet BirthRate
    ## 1  ( 1 ) " "      " "        " "   " "    " "                  "*"      
    ## 2  ( 1 ) " "      " "        " "   " "    " "                  "*"      
    ## 3  ( 1 ) " "      " "        " "   "*"    " "                  "*"      
    ## 4  ( 1 ) "*"      " "        " "   "*"    " "                  "*"      
    ## 5  ( 1 ) "*"      " "        "*"   "*"    " "                  "*"      
    ## 6  ( 1 ) "*"      " "        "*"   "*"    "*"                  "*"      
    ## 7  ( 1 ) "*"      " "        "*"   "*"    "*"                  "*"      
    ## 8  ( 1 ) "*"      "*"        "*"   "*"    "*"                  "*"      
    ##          transformed_ElderlyPop transformed_CO2 transformed_GDP
    ## 1  ( 1 ) " "                    " "             " "            
    ## 2  ( 1 ) " "                    " "             "*"            
    ## 3  ( 1 ) " "                    " "             "*"            
    ## 4  ( 1 ) " "                    " "             "*"            
    ## 5  ( 1 ) " "                    " "             "*"            
    ## 6  ( 1 ) " "                    " "             "*"            
    ## 7  ( 1 ) " "                    "*"             "*"            
    ## 8  ( 1 ) " "                    "*"             "*"            
    ##          transformed_Cell
    ## 1  ( 1 ) " "             
    ## 2  ( 1 ) " "             
    ## 3  ( 1 ) " "             
    ## 4  ( 1 ) " "             
    ## 5  ( 1 ) " "             
    ## 6  ( 1 ) " "             
    ## 7  ( 1 ) " "             
    ## 8  ( 1 ) " "

``` r
summary(model_forward)
```

    ## Subset selection object
    ## Call: regsubsets.formula(transformed_LifeExpectancy ~ LandArea + Population + 
    ##     Rural + Health + transformed_Internet + BirthRate + transformed_ElderlyPop + 
    ##     transformed_CO2 + transformed_GDP + transformed_Cell, data = countries_subset, 
    ##     method = "forward")
    ## 10 Variables  (and intercept)
    ##                        Forced in Forced out
    ## LandArea                   FALSE      FALSE
    ## Population                 FALSE      FALSE
    ## Rural                      FALSE      FALSE
    ## Health                     FALSE      FALSE
    ## transformed_Internet       FALSE      FALSE
    ## BirthRate                  FALSE      FALSE
    ## transformed_ElderlyPop     FALSE      FALSE
    ## transformed_CO2            FALSE      FALSE
    ## transformed_GDP            FALSE      FALSE
    ## transformed_Cell           FALSE      FALSE
    ## 1 subsets of each size up to 8
    ## Selection Algorithm: forward
    ##          LandArea Population Rural Health transformed_Internet BirthRate
    ## 1  ( 1 ) " "      " "        " "   " "    " "                  "*"      
    ## 2  ( 1 ) " "      " "        " "   " "    " "                  "*"      
    ## 3  ( 1 ) " "      " "        " "   "*"    " "                  "*"      
    ## 4  ( 1 ) "*"      " "        " "   "*"    " "                  "*"      
    ## 5  ( 1 ) "*"      " "        "*"   "*"    " "                  "*"      
    ## 6  ( 1 ) "*"      " "        "*"   "*"    "*"                  "*"      
    ## 7  ( 1 ) "*"      " "        "*"   "*"    "*"                  "*"      
    ## 8  ( 1 ) "*"      "*"        "*"   "*"    "*"                  "*"      
    ##          transformed_ElderlyPop transformed_CO2 transformed_GDP
    ## 1  ( 1 ) " "                    " "             " "            
    ## 2  ( 1 ) " "                    " "             "*"            
    ## 3  ( 1 ) " "                    " "             "*"            
    ## 4  ( 1 ) " "                    " "             "*"            
    ## 5  ( 1 ) " "                    " "             "*"            
    ## 6  ( 1 ) " "                    " "             "*"            
    ## 7  ( 1 ) " "                    "*"             "*"            
    ## 8  ( 1 ) " "                    "*"             "*"            
    ##          transformed_Cell
    ## 1  ( 1 ) " "             
    ## 2  ( 1 ) " "             
    ## 3  ( 1 ) " "             
    ## 4  ( 1 ) " "             
    ## 5  ( 1 ) " "             
    ## 6  ( 1 ) " "             
    ## 7  ( 1 ) " "             
    ## 8  ( 1 ) " "

``` r
summary(model_backward)
```

    ## Subset selection object
    ## Call: regsubsets.formula(transformed_LifeExpectancy ~ LandArea + Population + 
    ##     Rural + Health + transformed_Internet + BirthRate + transformed_ElderlyPop + 
    ##     transformed_CO2 + transformed_GDP + transformed_Cell, data = countries_subset, 
    ##     method = "backward")
    ## 10 Variables  (and intercept)
    ##                        Forced in Forced out
    ## LandArea                   FALSE      FALSE
    ## Population                 FALSE      FALSE
    ## Rural                      FALSE      FALSE
    ## Health                     FALSE      FALSE
    ## transformed_Internet       FALSE      FALSE
    ## BirthRate                  FALSE      FALSE
    ## transformed_ElderlyPop     FALSE      FALSE
    ## transformed_CO2            FALSE      FALSE
    ## transformed_GDP            FALSE      FALSE
    ## transformed_Cell           FALSE      FALSE
    ## 1 subsets of each size up to 8
    ## Selection Algorithm: backward
    ##          LandArea Population Rural Health transformed_Internet BirthRate
    ## 1  ( 1 ) " "      " "        " "   " "    " "                  "*"      
    ## 2  ( 1 ) " "      " "        " "   " "    " "                  "*"      
    ## 3  ( 1 ) " "      " "        " "   "*"    " "                  "*"      
    ## 4  ( 1 ) "*"      " "        " "   "*"    " "                  "*"      
    ## 5  ( 1 ) "*"      " "        "*"   "*"    " "                  "*"      
    ## 6  ( 1 ) "*"      " "        "*"   "*"    "*"                  "*"      
    ## 7  ( 1 ) "*"      " "        "*"   "*"    "*"                  "*"      
    ## 8  ( 1 ) "*"      "*"        "*"   "*"    "*"                  "*"      
    ##          transformed_ElderlyPop transformed_CO2 transformed_GDP
    ## 1  ( 1 ) " "                    " "             " "            
    ## 2  ( 1 ) " "                    " "             "*"            
    ## 3  ( 1 ) " "                    " "             "*"            
    ## 4  ( 1 ) " "                    " "             "*"            
    ## 5  ( 1 ) " "                    " "             "*"            
    ## 6  ( 1 ) " "                    " "             "*"            
    ## 7  ( 1 ) " "                    "*"             "*"            
    ## 8  ( 1 ) " "                    "*"             "*"            
    ##          transformed_Cell
    ## 1  ( 1 ) " "             
    ## 2  ( 1 ) " "             
    ## 3  ( 1 ) " "             
    ## 4  ( 1 ) " "             
    ## 5  ( 1 ) " "             
    ## 6  ( 1 ) " "             
    ## 7  ( 1 ) " "             
    ## 8  ( 1 ) " "

``` r
par(mfrow=c(2,2))
summary(model_best_subset)$cp
```

    ## [1] 19.636031 11.635107  8.476738  5.197641  4.654619  4.975973  5.700459
    ## [8]  7.118798

``` r
summary(model_forward)$cp
```

    ## [1] 19.636031 11.635107  8.476738  5.197641  4.654619  4.975973  5.700459
    ## [8]  7.118798

``` r
summary(model_backward)$cp
```

    ## [1] 19.636031 11.635107  8.476738  5.197641  4.654619  4.975973  5.700459
    ## [8]  7.118798

``` r
summary(model_best_subset)$adjr2
```

    ## [1] 0.7271670 0.7474989 0.7569729 0.7669220 0.7705798 0.7722402 0.7729583
    ## [8] 0.7719927

``` r
summary(model_forward)$adjr2
```

    ## [1] 0.7271670 0.7474989 0.7569729 0.7669220 0.7705798 0.7722402 0.7729583
    ## [8] 0.7719927

``` r
summary(model_backward)$adjr2
```

    ## [1] 0.7271670 0.7474989 0.7569729 0.7669220 0.7705798 0.7722402 0.7729583
    ## [8] 0.7719927

``` r
#best model 5 predictors (cp): 
#best model with the predictors from exhaustive:landarea, rural health, birthrate, transformedgdp
#best model with the predictors from forward:landarea, rural health, birthrate, transformedgdp 
#best model with the predictors from backward:landarea, rural, health, birthrate, transformedgdp 

# Remove rows with missing values
countries_subset_no_missing <- na.omit(countries_subset)

# Fit models without missing values
intercept_only <- lm(LifeExpectancy ~ 1, data = countries_subset_no_missing)
model_all <- lm(LifeExpectancy ~ LandArea + Rural + Health + BirthRate + transformed_GDP, data = countries_subset_no_missing)

# Perform stepwise regression for order
stepwise <- step(intercept_only, direction = 'both', scope = formula(model_all), trace = 0)
stepwise$anova
```

    ##          Step Df   Deviance Resid. Df Resid. Dev      AIC
    ## 1             NA         NA       103  11295.653 489.5294
    ## 2 + BirthRate -1 8099.23582       102   3196.418 360.2411
    ## 3    + Health -1  168.76485       101   3027.653 356.5998
    ## 4  + LandArea -1  129.48312       100   2898.170 354.0542
    ## 5     + Rural -1   96.96167        99   2801.208 352.5152

``` r
stepwise$coefficients
```

    ##   (Intercept)     BirthRate        Health      LandArea         Rural 
    ##  8.270759e+01 -6.511250e-01  3.285287e-01 -4.225891e-06 -5.907714e-02

``` r
#best order is BirthRate, Health, LandArea, Rural

#best model 7 (adjr2):
#best model with the predictors from exhaustive:landarea,  , rural, health, birthrate, ,transformed_gdp, transformed_internet, transformed_CO2
#best model with the predictors from forward:landarea,  rural, health, birthrate, ,transformed_gdp, transformed_internet, transformed_CO2
#best model with the predictors from backward:landarea,  rural, health, birthrate, ,transformed_gdp, transformed_internet, transformed_CO2

# Remove rows with missing values
countries_subset_no_missing <- na.omit(countries_subset)

# Fit models without missing values
intercept_only <- lm(LifeExpectancy ~ 1, data = countries_subset_no_missing)
model_all <- lm(LifeExpectancy ~ LandArea + Rural + Health+transformed_Internet+BirthRate+transformed_CO2 + transformed_GDP, data = countries_subset_no_missing)

# Perform stepwise regression for order
stepwise <- step(intercept_only, direction = 'both', scope = formula(model_all), trace = 0)
stepwise$anova
```

    ##          Step Df   Deviance Resid. Df Resid. Dev      AIC
    ## 1             NA         NA       103  11295.653 489.5294
    ## 2 + BirthRate -1 8099.23582       102   3196.418 360.2411
    ## 3    + Health -1  168.76485       101   3027.653 356.5998
    ## 4  + LandArea -1  129.48312       100   2898.170 354.0542
    ## 5     + Rural -1   96.96167        99   2801.208 352.5152

``` r
stepwise$coefficients
```

    ##   (Intercept)     BirthRate        Health      LandArea         Rural 
    ##  8.270759e+01 -6.511250e-01  3.285287e-01 -4.225891e-06 -5.907714e-02

``` r
#Order from radj2 -> BirthRate, Health, LandArea, Rural
```

``` r
#running diagnostics
par(mfrow = c(2,2))

final_model <- lm(transformed_LifeExpectancy~BirthRate+Health+LandArea+Rural, data = countries_subset)
plot(final_model, which = 1:2)

#analysis
#linearity - values are mostly on the zero line, model assumptions are not violated. We can assume linearity.
#constant variance - there is a good spread on the plot. We can assume constant variance.
#Normality - points are mostly on the regression line, however there are deviations in the tails. Can argue it can violate and doesn't violate. 
```

![](termproj1_1_files/figure-gfm/unnamed-chunk-13-1.png)<!-- -->

``` r
#transform y again
par(mfrow = c(3,3))

#possible transformations of outcome variable
#fit a linear regression model for life expectancy on predictor variables
box_cox_res <- boxcox(final_model, plotit = TRUE, lambda = seq(-7, 7, 1/10))

#extract lambda value
lambda <- box_cox_res$x[which.max(box_cox_res$y)]
print(paste("The optimal lambda is ", lambda))
```

    ## [1] "The optimal lambda is  0.800000000000001"

``` r
#perform transformations

model_trans2_log <- lm(log(transformed_LifeExpectancy)~BirthRate+Health+LandArea+Rural, data = countries_subset)
plot(model_trans2_log, which = 1:2)
summary(model_trans2_log)
```

    ## 
    ## Call:
    ## lm(formula = log(transformed_LifeExpectancy) ~ BirthRate + Health + 
    ##     LandArea + Rural, data = countries_subset)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -1.17050 -0.18031  0.02463  0.21398  0.72804 
    ## 
    ## Coefficients:
    ##               Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)  1.829e+01  1.242e-01 147.225  < 2e-16 ***
    ## BirthRate   -4.365e-02  3.813e-03 -11.449  < 2e-16 ***
    ## Health       1.819e-02  7.437e-03   2.446  0.01580 *  
    ## LandArea    -2.582e-07  9.769e-08  -2.643  0.00926 ** 
    ## Rural       -3.821e-03  1.665e-03  -2.294  0.02341 *  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.3427 on 128 degrees of freedom
    ##   (15 observations deleted due to missingness)
    ## Multiple R-squared:  0.7675, Adjusted R-squared:  0.7602 
    ## F-statistic: 105.6 on 4 and 128 DF,  p-value: < 2.2e-16

``` r
#Linearity - violates
#Constant Variance - questionable
#Normality - violates

model_trans2_sqrt <- lm(sqrt(transformed_LifeExpectancy)~BirthRate+Health+LandArea+Rural, data = countries_subset)
plot(model_trans2_sqrt, which = 1:2)
summary(model_trans2_sqrt)
```

    ## 
    ## Call:
    ## lm(formula = sqrt(transformed_LifeExpectancy) ~ BirthRate + Health + 
    ##     LandArea + Rural, data = countries_subset)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -2314.0  -599.8   119.3   512.1  1784.1 
    ## 
    ## Coefficients:
    ##               Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)  8.497e+03  2.832e+02  29.999  < 2e-16 ***
    ## BirthRate   -1.059e+02  8.692e+00 -12.187  < 2e-16 ***
    ## Health       5.274e+01  1.696e+01   3.110 0.002306 ** 
    ## LandArea    -5.763e-04  2.227e-04  -2.588 0.010779 *  
    ## Rural       -1.361e+01  3.797e+00  -3.585 0.000478 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 781.3 on 128 degrees of freedom
    ##   (15 observations deleted due to missingness)
    ## Multiple R-squared:  0.8048, Adjusted R-squared:  0.7987 
    ## F-statistic: 131.9 on 4 and 128 DF,  p-value: < 2.2e-16

``` r
#Linearity - violates 
#Constant Variance - From the Residuals vs. Fitted plot, the model assumpton seems to hold.Transformed model constant variance looks better and seems to not have patterns.
#Normality - violates

model_trans2_0.9 <- lm(transformed_LifeExpectancy^0.9~BirthRate+Health+LandArea+Rural, data = countries_subset)
plot(model_trans2_0.9, which = 1:2)
summary(model_trans2_0.9)
```

    ## 
    ## Call:
    ## lm(formula = transformed_LifeExpectancy^0.9 ~ BirthRate + Health + 
    ##     LandArea + Rural, data = countries_subset)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -4160285  -894432   101711   919301  3003845 
    ## 
    ## Coefficients:
    ##               Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)  1.097e+07  4.907e+05  22.353  < 2e-16 ***
    ## BirthRate   -1.775e+05  1.506e+04 -11.785  < 2e-16 ***
    ## Health       1.049e+05  2.937e+04   3.572   0.0005 ***
    ## LandArea    -9.073e-01  3.858e-01  -2.352   0.0202 *  
    ## Rural       -2.932e+04  6.577e+03  -4.458 1.79e-05 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 1354000 on 128 degrees of freedom
    ##   (15 observations deleted due to missingness)
    ## Multiple R-squared:  0.809,  Adjusted R-squared:  0.803 
    ## F-statistic: 135.5 on 4 and 128 DF,  p-value: < 2.2e-16

``` r
#Linearity - good, can assume linearity
#Constant Variance - can assume constant variance
#Normality - From the Normal QQ plot, still seems violating

#best lambda is 0.9

countries_subset$transformed_LifeExpectancy2 <- (countries_subset$transformed_LifeExpectancy)^0.9

#create model with new transformed variable
model_trans2_y <- lm(transformed_LifeExpectancy2~BirthRate+Health+LandArea+Rural, data = countries_subset)
summary(model_trans2_y)
```

    ## 
    ## Call:
    ## lm(formula = transformed_LifeExpectancy2 ~ BirthRate + Health + 
    ##     LandArea + Rural, data = countries_subset)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -4160285  -894432   101711   919301  3003845 
    ## 
    ## Coefficients:
    ##               Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)  1.097e+07  4.907e+05  22.353  < 2e-16 ***
    ## BirthRate   -1.775e+05  1.506e+04 -11.785  < 2e-16 ***
    ## Health       1.049e+05  2.937e+04   3.572   0.0005 ***
    ## LandArea    -9.073e-01  3.858e-01  -2.352   0.0202 *  
    ## Rural       -2.932e+04  6.577e+03  -4.458 1.79e-05 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 1354000 on 128 degrees of freedom
    ##   (15 observations deleted due to missingness)
    ## Multiple R-squared:  0.809,  Adjusted R-squared:  0.803 
    ## F-statistic: 135.5 on 4 and 128 DF,  p-value: < 2.2e-16

``` r
#run diagnostis again
plot(model_trans2_y, which = 1:2)
```

![](termproj1_1_files/figure-gfm/unnamed-chunk-14-1.png)<!-- -->
