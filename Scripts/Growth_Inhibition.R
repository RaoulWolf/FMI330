## This is the R script to perform a dose-response analysis of growth inhibition

## (Installing and) Loading the necessary packages
# install.packages(c("tidyverse", "drc"))
library(drc)
library(tidyverse)
library(readxl)

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
  ggplot(mapping = aes(x = Stressor_A, y = Growth_inhibition)) +
  geom_point(alpha = 0.5) +
  labs(title = expression(bold("Growth inhibition of")~bolditalic("Lemna minor")), 
       subtitle = "The raw data",
       x = expression("3,5-Dichlorophenol concentration"~("mg"/"L")), 
       y = expression("Growth inhibition"~("%"))) + 
  theme_minimal()

## Fitting a four-parametric log-logistic dose response curve
Growth_inhibition_DRM <- drm(formula = Growth_inhibition ~ Stressor_A, 
                             data = Data, 
                             fct = LL.4(fixed = c(NA, NA, NA, NA), 
                                        names = c("Slope", "Lower Limit", "Upper Limit", "EC50")),
                             type = "continuous")

Growth_inhibition_DRM %>% summary()

## Getting the EC5, EC10, EC50 and EC90 values
Growth_inhibition_DRM %>% ED(respLev = c(5, 10, 50, 90), interval = "delta")

## Creating the curve data for visualisation
Growth_inhibition_pred <- data.frame(Stressor_A = seq(from = min(Data$Stressor_A), 
                                                      to = max(Data$Stressor_A), 
                                                      length.out = 1000)) %>% 
  mutate(fit = predict(Growth_inhibition_DRM, newdata = .), 
         lwr = predict(Growth_inhibition_DRM, newdata = ., interval = "confidence", constrain = FALSE)[, 2], 
         upr = predict(Growth_inhibition_DRM, newdata = ., interval = "confidence", constrain = FALSE)[, 3]) %>% 
  as_tibble()

Growth_inhibition_pred

## Visualizing a summary of the data and the dose-response curve
Data %>% 
  group_by(Stressor_A) %>% 
  summarize(Growth_inhibition_mean = mean(Growth_inhibition), 
            Growth_inhibition_SE = sd(Growth_inhibition) / sqrt(n())) %>% 
  ggplot() +
  geom_vline(xintercept = Growth_inhibition_DRM$coefficients[4],
             linetype = 2, color = 2) +
  geom_hline(yintercept = Growth_inhibition_DRM$coefficients[2] + 
               ((Growth_inhibition_DRM$coefficients[3] - Growth_inhibition_DRM$coefficients[2]) / 2),
             linetype = 2, color = 2) +
  geom_ribbon(mapping = aes(x = Stressor_A, ymin = lwr, ymax = upr), 
              data = Growth_inhibition_pred, alpha = 0.25, fill = 3) +
  geom_line(mapping = aes(x = Stressor_A, y = fit), 
            data = Growth_inhibition_pred, size = 1, alpha = 0.5, color = 4) +
  geom_point(mapping = aes(x = Stressor_A, y = Growth_inhibition_mean)) +
  geom_errorbar(mapping = aes(x = Stressor_A, 
                              ymin = Growth_inhibition_mean - Growth_inhibition_SE,
                              ymax = Growth_inhibition_mean + Growth_inhibition_SE),
                width = 0) + 
  labs(title = expression(bold("Growth inhibition of")~bolditalic("Lemna minor")),
       subtitle = "Four-parameter log-logistic dose-response curve",
       x = expression("3,5-Dichlorophenol concentration"~("mg"/"L")), 
       y = expression("Growth inhibition"~("%"))) + 
  theme_minimal()

## Saving the plot
ggsave("Plots/Growth_inhibition_DRC.svg", height = 3.5, units = "in")