## This is the R script to perform a dose-response analysis of growth inhibition

## (Installing and) Loading the necessary packages
# install.packages(c("tidyverse", "drc", "lmtest", "sandwich"))
library(tidyverse)
library(readxl)
library(drc)
library(lmtest)
library(sandwich)


## Reading in the data and inspecting it
Data_raw <- read_excel(path = "Data/Data-FMI310.xlsx", 
                       sheet = "One stressor -SM")

Data_raw

## Cleaning up the data for ideal use
Data <- Data_raw %>% 
  rename(Stressor_A = `Stressor A`, 
         Fronds_number = `Fronds Number`, 
         Growth_rate = `Growth rate (pr day)`,
         Growth_inhibition = `Growth inhibition (%)`, 
         Fv_Fm = `Fv/Fm`, 
         PS_II_inhibition = `PS II inhibition (%)`, 
         ROS_formation = `ROS formation`, 
         ROS_formation_fold_increase = `ROS formation (fold increase)`) %>% 
  mutate(Replicate = as.factor(Replicate), 
         Fronds_number = as.integer(Fronds_number)) %>% 
  arrange(Stressor_A, Replicate)

Data

rm(Data_raw)

## First visualisation of the raw data
Data %>% 
  ggplot() +
  geom_point(mapping = aes(x = Stressor_A, y = Growth_inhibition), size = 2, alpha = 0.5) +
  labs(title = expression(italic("Lemna minor")), 
       subtitle = "Raw data",
       x = "3,5-Dichlorophenol (mg/L)", 
       y = "Growth inhibition (%)", 
       caption = "FMI330")

## Fitting a four-parametric log-logistic dose response curve
Growth_inhibition.drm <- drm(formula = Growth_inhibition ~ Stressor_A, 
                             data = Data, 
                             fct = LL.4(fixed = c(NA, NA, NA, NA), 
                                        names = c("Slope", "Lower Limit", "Upper Limit", "EC50")),
                             type = "continuous")

Growth_inhibition.drm %>% summary()

coeftest(Growth_inhibition.drm, vcov. = sandwich)

## Getting the EC5, EC10, EC50 and EC90 values
Growth_inhibition.drm %>% ED(respLev = c(5, 10, 50, 90), interval = "delta", vcov. = sandwich)

## Creating the curve data for visualisation
Growth_inhibition_pred <- data.frame(Stressor_A = seq(from = min(Data$Stressor_A), 
                                                      to = max(Data$Stressor_A), 
                                                      length.out = 1000)) %>% 
  mutate(fit = predict(Growth_inhibition.drm, newdata = .), 
         lwr = predict(Growth_inhibition.drm, newdata = ., interval = "confidence", vcov. = sandwich)[, 2], 
         upr = predict(Growth_inhibition.drm, newdata = ., interval = "confidence", vcov. = sandwich)[, 3]) %>% 
  as_tibble()

Growth_inhibition_pred

## Visualizing a summary of the data and the dose-response curve
Data %>% 
  ggplot() +
  geom_ribbon(mapping = aes(x = Stressor_A, ymin = lwr, ymax = upr), 
              data = Growth_inhibition_pred, alpha = 0.2) +
  geom_line(mapping = aes(x = Stressor_A, y = fit), 
            data = Growth_inhibition_pred, size = 1) +
  geom_point(mapping = aes(x = Stressor_A, y = Growth_inhibition), size = 2, alpha = 0.5) +
  geom_vline(xintercept = coef(Growth_inhibition.drm)[4], linetype = 3) +
  geom_hline(yintercept = coef(Growth_inhibition.drm)[2] +
               ((coef(Growth_inhibition.drm)[3] - coef(Growth_inhibition.drm)[2]) / 2), linetype = 3) +
  labs(title = expression(italic("Lemna minor")),
       subtitle = "Four-parametric log-logistic dose-response curve",
       x = "3,5-Dichlorophenol (mg/L)", 
       y = "Growth inhibition (%)", 
       caption = "FMI330")

## Saving the plot
ggsave("Plots/Growth_inhibition_DRC.png", height = 5.25, width = 7, units = "in", dpi = 600, type = "cairo-png")

