FMI330
================

> Example R scripts to create a dose-response curve and extract
> EC<sub>X</sub>-values.

## Installing and loading packages

Before the fun-part of dose-response modelling, a handful of add-on
packages have to be installed. `tidyverse` and `readxl` provide
functionalities for easy data handling and reading straight from .xlsx
files, respectively. `drc` is used to perform dose-response modelling,
and `lmtest` and `sandwich` provide additional functionalities for
standard error and confidence interval estimations.

``` r
install.packages(c("tidyverse", "multcomp", "drc", "lmtest", "sandwich"))
```

After succesful installation, the packages need to be loaded by using
the `library()` functionality.

``` r
library(multcomp)
library(tidyverse)
library(readxl)
library(drc)
library(lmtest)
library(sandwich)
```

We are now ready to load the data\!

## Loading and cleaning up the data

First, we load the raw data and inspect it.

``` r
data <- read_xlsx(path = "Data/FMI330 Data.xlsx", sheet = "Summary")

data
```

    ## # A tibble: 21 x 11
    ##    Sample_name Concentration Note  Replicate Fronds_number Frond_size
    ##    <chr>               <dbl> <chr>     <dbl>         <dbl>      <dbl>
    ##  1 S1                  0     CT            1         0.780      -5.74
    ##  2 S2                  0.125 <NA>          1         5.24       16.4 
    ##  3 S3                  0.25  <NA>          1        29.2         9.02
    ##  4 S4                  0.5   <NA>          1        48.1        45.9 
    ##  5 S5                  1     <NA>          1        80.6        97.5 
    ##  6 S6                  2     <NA>          1        91.0        95.1 
    ##  7 S7                  4     <NA>          1        91.0       100   
    ##  8 S8                  0     CT            2         2.51       -3.28
    ##  9 S9                  0.125 <NA>          2         5.24       16.4 
    ## 10 S10                 0.25  <NA>          2        29.2         9.02
    ## # ... with 11 more rows, and 5 more variables: Photosystem_II <dbl>,
    ## #   ROS_formation <dbl>, Chlorophyll_A <lgl>, Chlorophyll_B <lgl>,
    ## #   Carotenoids <lgl>

# Fronds\_number

## Visualizing the raw data

Let’s take a look at the reproduction along the stressor gradient. Note
that this plot is meant to help you understand the data, and is not part
of your reports\!

``` r
data %>% 
  ggplot() +
  geom_point(mapping = aes(x = Concentration, y = Fronds_number), size = 2, alpha = 0.5) +
  labs(x = "Stressor (mg/L)", 
       y = "Fronds number (%)") +
  theme_light()
```

![](README_files/figure-gfm/unnamed-chunk-4-1.png)<!-- -->

## LOEC and NOEC derivation

Based on the figure of the raw data, we can see that there is a strong
decreasing trend in reproduction with increasing Concentration. To find
out statistically significant differences, we need to run a ANOVA with
Dunnett’s *post hoc* test.

In the first step, we have to transfrom the `Concentration` from a
numeric column to a (categorical) factor column; otherwise it is not
possible to run a *post hoc* analysis. This happens inside the
`mutate()` function. In the next step, we run a ANOVA. But instead of
using a convenience function, we do it more manual, by fitting a linear
model using `lm()`. Note that a ANOVA is nothing else than a linear
model. Within the linear model, the contrast treatment specifies that
Concentration `"0"` contains the control values. The linear model is
then used in a generalized linear hypothesis test with the `glht()`
function. Inside this function, the multiple comparison is set to
Dunnett’s contrasts (everything vs. the control), and
variance-covariance matrix is updated using the sandwich estimator. In
the final step, the *p*-values are adjusted for multiple comparisons
using Holm’s method.

``` r
data %>% 
  mutate(Concentration = fct_relevel(as.character(Concentration), "0")) %>% 
  lm(formula = Fronds_number ~ Concentration, data = ., contrasts = list(Concentration = "contr.treatment")) %>% 
  glht(linfct = mcp(Concentration = "Dunnett"), vcov = sandwich) %>% 
  summary(test = adjusted(type = "holm"))
```

    ## 
    ##   Simultaneous Tests for General Linear Hypotheses
    ## 
    ## Multiple Comparisons of Means: Dunnett Contrasts
    ## 
    ## 
    ## Fit: lm(formula = Fronds_number ~ Concentration, data = ., contrasts = list(Concentration = "contr.treatment"))
    ## 
    ## Linear Hypotheses:
    ##                Estimate Std. Error t value Pr(>|t|)    
    ## 0.125 - 0 == 0    5.241      1.405    3.73  0.00224 ** 
    ## 0.25 - 0 == 0    29.152      1.405   20.75 1.31e-11 ***
    ## 0.5 - 0 == 0     48.132      1.405   34.25 2.66e-14 ***
    ## 1 - 0 == 0       80.577      1.405   57.34  < 2e-16 ***
    ## 2 - 0 == 0       91.023      1.405   64.78  < 2e-16 ***
    ## 4 - 0 == 0      100.000      3.925   25.48 1.19e-12 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## (Adjusted p values reported -- holm method)

From the results we can deduce that the NOEC has a Concentration of 1
mg/L, and the LOEC has a Concentration of 1.5 mg/L.

## Dose-response model fitting

From the visualization of the raw data, we see a very clear monotonic
dose-response pattern\! Let’s fit a four-parametric log-logistic model
using the `drc` package. We then take a look at the results (i.e., the
four parameters, or coefficients) using `coeftest` with adjusted
variance-covariance matrix.

``` r
reproduction.drm <- data %>% 
  drm(formula = Fronds_number ~ Concentration, data = .,
      fct = LL.4(names = c("Slope", "Lower Limit", "Upper Limit", "EC50")))

reproduction.drm %>% 
  coeftest(vcov. = sandwich)
```

    ## 
    ## t test of coefficients:
    ## 
    ##                           Estimate Std. Error  t value  Pr(>|t|)    
    ## Slope:(Intercept)        -1.644655   0.135219 -12.1629 8.185e-10 ***
    ## Lower Limit:(Intercept)  -1.100778   1.491234  -0.7382    0.4705    
    ## Upper Limit:(Intercept) 102.321014   3.742989  27.3367 1.721e-15 ***
    ## EC50:(Intercept)          0.488845   0.033115  14.7620 3.989e-11 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

The fitted values for the parameters seem to make sense. The lower and
upper limit are in accordance with what we observe in the raw data, and
the EC<sub>50</sub> value is also realistic.

## EC<sub>X</sub> derivation

Let’s take a look at the corrected parameter estimates and the
EC<sub>5</sub> and EC<sub>50</sub> values with 95% confidence limits.

``` r
reproduction.drm %>% 
  ED(respLev = c(5, 50), interval = "delta", vcov. = sandwich)
```

    ## 
    ## Estimated effective doses
    ## 
    ##        Estimate Std. Error    Lower    Upper
    ## e:1:5  0.081592   0.010578 0.059274 0.103910
    ## e:1:50 0.488845   0.033115 0.418978 0.558711

## Plotting of the dose-response curve

Now that we got all necessary information, let’s plot the dose-response
curve. This is *not* straight forward, as it requires advanced use of
the `predict()` function, so feel free to copy-paste.

``` r
reproduction.pred <- data.frame(Concentration = seq(from = min(data$Concentration),
                                                    to = max(data$Concentration),
                                                    length.out = 1000)) %>% 
  mutate(fit = predict(reproduction.drm, newdata = .), 
         lwr = predict(reproduction.drm, newdata = ., interval = "confidence", vcov. = sandwich)[, 2], 
         upr = predict(reproduction.drm, newdata = ., interval = "confidence", vcov. = sandwich)[, 3])

data %>% 
  ggplot() +
  geom_ribbon(mapping = aes(x = Concentration, ymin = lwr, ymax = upr), data = reproduction.pred, alpha = 0.2) +
  geom_line(mapping = aes(x = Concentration, y = fit), data = reproduction.pred, size = 1) +
  geom_point(mapping = aes(x = Concentration, y = Fronds_number), size = 2, alpha = 0.5) +
  labs(x = "Stressor (mg/L)", 
       y = "Fronds number (%)") +
  theme_light()
```

![](README_files/figure-gfm/unnamed-chunk-8-1.png)<!-- -->

Looks good, so let’s save the plot.

``` r
ggsave("Figures/Fronds number.png", height = 5.25, width = 7, units = "in", dpi = 600, type = "cairo-png")
```
