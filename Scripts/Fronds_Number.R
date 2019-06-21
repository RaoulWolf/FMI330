## This is the R script to perform a dose-response analysis of fronds number

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
  geom_point(mapping = aes(x = Stressor_A, y = Fronds_number), size = 2, alpha = 0.5) +
  labs(x = "3,5-Dichlorophenol (mg/L)", 
       y = "Fronds number (count)")

## Fitting a four-parametric log-logistic dose response curve
Fronds_number.drm <- drm(formula = Fronds_number ~ Stressor_A, 
                         data = Data, 
                         fct = LL.4(fixed = c(NA, NA, NA, NA), 
                                    names = c("Slope", "Lower Limit", "Upper Limit", "EC50")),
                         type = "Poisson")

Fronds_number.drm %>% summary()

coeftest(Fronds_number.drm, vcov. = sandwich)

## Getting the EC5, EC10, EC50 and EC90 values
Fronds_number.drm %>% ED(respLev = c(5, 10, 50, 90), interval = "delta", vcov. = sandwich)

## Creating the curve data for visualisation
Fronds_number.pred <- data.frame(Stressor_A = seq(from = min(Data$Stressor_A), 
                                                  to = max(Data$Stressor_A), 
                                                  length.out = 1000)) %>% 
  mutate(fit = predict(Fronds_number.drm, newdata = .), 
         lwr = predict(Fronds_number.drm, newdata = ., interval = "confidence", vcov. = sandwich)[, 2], 
         upr = predict(Fronds_number.drm, newdata = ., interval = "confidence", vcov. = sandwich)[, 3]) %>% 
  as_tibble()

Fronds_number.pred

## Visualizing a summary of the data and the dose-response curve
Data %>% 
  ggplot() +
  geom_ribbon(mapping = aes(x = Stressor_A, ymin = lwr, ymax = upr), 
              data = Fronds_number.pred, alpha = 0.2) +
  geom_line(mapping = aes(x = Stressor_A, y = fit), 
            data = Fronds_number.pred, size = 1) +
  geom_point(mapping = aes(x = Stressor_A, y = Fronds_number), size = 2, alpha = 0.5) +
  geom_vline(xintercept = coef(Fronds_number.drm)[4], linetype = 3) +
  geom_hline(yintercept = coef(Fronds_number.drm)[2] +
               ((coef(Fronds_number.drm)[3] - coef(Fronds_number.drm)[2]) / 2), linetype = 3) +
  labs(x = "3,5-Dichlorophenol (mg/L)", 
       y = "Fronds number (count)")

## Saving the plot
ggsave("Plots/Fronds_number_DRC.png", height = 5.25, width = 7, units = "in", dpi = 600, type = "cairo-png")
