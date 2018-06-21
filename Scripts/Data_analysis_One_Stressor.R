## (Installing and) Loading the necessary packages
# install.packages(c("tidyverse", "drc"))
library(drc)
library(tidyverse)
library(readxl)

## Reading in the data and inspecting it
One_Stressor_Data_raw <- read_excel(path = "Data/Data-FMI310.xlsx", sheet = "One stressor -SM")

One_Stressor_Data_raw

## Cleaning up the data for ideal use
One_Stressor_Data <- One_Stressor_Data_raw %>% 
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

One_Stressor_Data

rm(One_Stressor_Data_raw)

## First visualisation of the raw data
One_Stressor_Data %>% 
  ggplot(mapping = aes(x = Stressor_A, y = Fronds_number)) +
  geom_point(alpha = 0.5) +
  labs(title = "The raw data", 
       x = "Stressor A", 
       y = "Fronds number") + 
  theme_bw()

## Fitting a four-parametric log-logistic dose response curve
One_Stressor_drm <- drm(formula = Fronds_number ~ Stressor_A, data = One_Stressor_Data, 
                        fct = LL.4(names = c("Slope", "Lower Limit", "Upper Limit", "ED50")),
                        type = "Poisson")

One_Stressor_drm %>% summary()

## Getting the EC5, EC10, EC50 and EC90 values
One_Stressor_drm %>% ED(respLev = c(5, 10, 50, 90), interval = "delta")

## Creating the curve data for visualisation
One_Stressor_pred <- data.frame(Stressor_A = seq(from = min(One_Stressor_Data$Stressor_A), 
                                                 to = max(One_Stressor_Data$Stressor_A), 
                                                 length.out = 1000)) %>% 
  mutate(fit = predict(One_Stressor_drm, newdata = .), 
         lwr = predict(One_Stressor_drm, newdata = ., interval = "confidence")[, 2], 
         upr = predict(One_Stressor_drm, newdata = ., interval = "confidence")[, 3]) %>% 
  as_tibble()

One_Stressor_pred

## Visualizing a summary of the data and the dose-response curve
One_Stressor_Data %>% 
  group_by(Stressor_A) %>% 
  summarize(Fronds_number_mean = mean(Fronds_number), 
            Fronds_number_SE = sd(Fronds_number) / sqrt(n())) %>% 
  ggplot() +
  geom_vline(xintercept = One_Stressor_drm$coefficients[4],
             linetype = 3, alpha = 0.5) +
  geom_hline(yintercept = ((One_Stressor_drm$coefficients[3] - One_Stressor_drm$coefficients[2]) / 2) + One_Stressor_drm$coefficients[2],
             linetype = 3, alpha = 0.5) +
  geom_ribbon(mapping = aes(x = Stressor_A, ymin = lwr, ymax = upr), 
              data = One_Stressor_pred, alpha = 0.2) +
  geom_line(mapping = aes(x = Stressor_A, y = fit), 
            data = One_Stressor_pred, size = 1, alpha = 0.5) +
  geom_point(mapping = aes(x = Stressor_A, y = Fronds_number_mean)) +
  geom_errorbar(mapping = aes(x = Stressor_A, 
                              ymin = Fronds_number_mean - Fronds_number_SE,
                              ymax = Fronds_number_mean + Fronds_number_SE),
                width = 0) + 
  # geom_point(mapping = aes(x = Stressor_A, y = Fronds_number), 
  #            data = data, alpha = 0.5) +
  labs(title = "Dose-response curve",
       subtitle = "Based on a four-parameter log-logistic function",
       x = "Stressor A", 
       y = "Fronds number") + 
  theme_bw()

## Saving the plot
# ggsave("Plots/One_Stressor_Fronds_number_DRC.pdf", height = 3.5, units = "in")
