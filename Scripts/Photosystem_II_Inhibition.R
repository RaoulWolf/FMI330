## This is the R script to perform a dose-response analysis of Photosystem II inhibition

## (Installing and) Loading the necessary packages
# install.packages(c("tidyverse", "drc"))
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
  geom_point(mapping = aes(x = Stressor_A, y = PS_II_inhibition), size = 2, alpha = 0.5) +
  labs(title = expression(italic("Lemna minor")), 
       subtitle = "Raw data",
       x = "3,5-Dichlorophenol (mg/L)", 
       y = "Photosystem II inhibition (%)", 
       caption = "FMI330")

## Fitting a four-parametric log-logistic dose response curve
PSII_inhibition.drm <- drm(formula = PS_II_inhibition ~ Stressor_A, 
                           data = Data, 
                           fct = LL.4(fixed = c(NA, NA, NA, NA), 
                                      names = c("Slope", "Lower Limit", "Upper Limit", "EC50")),
                           type = "continuous")

PSII_inhibition.drm %>% summary()

coeftest(PSII_inhibition.drm, vcov. = sandwich)

## Getting the EC5, EC10, EC50 and EC90 values
PSII_inhibition.drm %>% ED(respLev = c(5, 10, 50, 90), interval = "delta", vcov. = sandwich)

## Creating the curve data for visualisation
PSII_inhibition_pred <- data.frame(Stressor_A = seq(from = min(Data$Stressor_A), 
                                                 to = max(Data$Stressor_A), 
                                                 length.out = 1000)) %>% 
  mutate(fit = predict(PSII_inhibition.drm, newdata = .), 
         lwr = predict(PSII_inhibition.drm, newdata = ., interval = "confidence", vcov. = sandwich)[, 2], 
         upr = predict(PSII_inhibition.drm, newdata = ., interval = "confidence", vcov. = sandwich)[, 3]) %>%  
  as_tibble()

PSII_inhibition_pred

## Visualizing a summary of the data and the dose-response curve
Data %>% 
  ggplot() +
  geom_ribbon(mapping = aes(x = Stressor_A, ymin = lwr, ymax = upr),
              data = PSII_inhibition_pred, alpha = 0.2) +
  geom_line(mapping = aes(x = Stressor_A, y = fit),
            data = PSII_inhibition_pred, size = 1) +
  geom_point(mapping = aes(x = Stressor_A, y = PS_II_inhibition), size = 2, alpha = 0.5) +
  geom_vline(xintercept = coef(PSII_inhibition.drm)[4], linetype = 3) +
  geom_hline(yintercept = coef(PSII_inhibition.drm)[2] + 
               ((coef(PSII_inhibition.drm)[3] - coef(PSII_inhibition.drm)[2]) / 2), linetype = 3) +
  labs(x = "3,5-Dichlorophenol (mg/L)", 
       y = "Photosystem II inhibition (%)")

## Saving the plot
ggsave("Plots/Photosystem_II_inhibition_DRC.png", height = 5.25, width = 7, units = "in", dpi = 600, type = "cairo-png")
