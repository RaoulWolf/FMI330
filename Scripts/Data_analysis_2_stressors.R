# install.packages(c("tidyverse", "drc"))

library(drc)
library(tidyverse)
library(readxl)

Two_Stressors_Data_raw <- read_excel(path = "Data/Data-FMI310.xlsx", sheet = "Two stressors -SM")

Two_Stressors_Data_raw

Two_Stressors_Data <- Two_Stressors_Data_raw %>% 
  select(-`Stressor B`) %>% 
  rename(Stressor_B = `Stressor B-b`, 
         Stressor_C = `Stressor C`,
         Fronds_number = `Fronds number`,
         Growth_rate = `Growth rate (pr day)`,
         Growth_inhibition = `Growth inhibition (%)`, 
         ROS_formation = `ROS formation`, 
         ROS_formation_fold_increase = `ROS formation (fold increase)`) %>% 
  mutate(Replicate = as.factor(Replicate), 
         Stressor_C = fct_relevel(Stressor_C, "minus")) %>% 
  arrange(Stressor_B, Stressor_C, Replicate)

Two_Stressors_Data

rm(Two_Stressors_Data_raw)

Two_Stressors_Data %>% 
  ggplot(mapping = aes(x = Stressor_B, y = Fronds_number, color = Stressor_C)) +
  geom_jitter(alpha = 0.5, width = (max(Two_Stressors_Data$Stressor_B) - min(Two_Stressors_Data$Stressor_B)) / 100, 
              height = (max(Two_Stressors_Data$Fronds_number) - min(Two_Stressors_Data$Fronds_number)) / 100) +
  labs(title = "The raw data", 
       x = "Stressor B", 
       y = "Fronds number") + 
  theme_bw()

Two_Stressors_drm <- drm(formula = Fronds_number ~ Stressor_B, curveid = Stressor_C, 
                         data = Two_Stressors_Data, type = "Poisson", 
                         fct = LL.4(names = c("Slope", "Lower Limit", "Upper Limit", "ED50")))

Two_Stressors_drm %>% summary()

Two_Stressors_drm %>% ED(respLev = c(5, 10, 50, 90), interval = "delta")

Two_Stressors_pred <- expand.grid(Stressor_B = seq(from = min(Two_Stressors_Data$Stressor_B), 
                                                   to = max(Two_Stressors_Data$Stressor_B), 
                                                   length.out = 1000), 
                                  Stressor_C = unique(Two_Stressors_Data$Stressor_C)) %>% 
  mutate(fit = predict(Two_Stressors_drm, newdata = .), 
         lwr = predict(Two_Stressors_drm, newdata = ., interval = "confidence")[, 2], 
         upr = predict(Two_Stressors_drm, newdata = ., interval = "confidence")[, 3]) %>% 
  as_tibble()

Two_Stressors_pred

Two_Stressors_Data %>% 
  group_by(Stressor_B, Stressor_C) %>% 
  summarize(Fronds_number_mean = mean(Fronds_number), 
            Fronds_number_SE = sd(Fronds_number) / sqrt(n())) %>% 
  ungroup() %>% 
  ggplot() +
  geom_vline(xintercept = Two_Stressors_drm$coefficients[7], 
             linetype = 3, color = "#F8766D", alpha = 0.5) +
  geom_vline(xintercept = Two_Stressors_drm$coefficients[8], 
             linetype = 3, color = "#00BFC4", alpha = 0.5) +
  geom_hline(yintercept = ((Two_Stressors_drm$coefficients[5] - Two_Stressors_drm$coefficients[3]) / 2) + Two_Stressors_drm$coefficients[3],
             color = "#F8766D", linetype = 3, alpha = 0.5) +
  geom_hline(yintercept = ((Two_Stressors_drm$coefficients[6] - Two_Stressors_drm$coefficients[4]) / 2) + Two_Stressors_drm$coefficients[4],
             color = "#00BFC4", linetype = 3, alpha = 0.5) +
  geom_ribbon(mapping = aes(x = Stressor_B, ymin = lwr, ymax = upr, fill = Stressor_C),
              data = Two_Stressors_pred, alpha = 0.2) +
  geom_line(mapping = aes(x = Stressor_B, y = fit, color = Stressor_C), 
            data = Two_Stressors_pred, size = 1, alpha = 0.5) +
  geom_point(mapping = aes(x = Stressor_B, y = Fronds_number_mean, color = Stressor_C)) +
  geom_errorbar(mapping = aes(x = Stressor_B, color = Stressor_C,
                              ymin = Fronds_number_mean - Fronds_number_SE,
                              ymax = Fronds_number_mean + Fronds_number_SE),
                width = 0) + 
  # geom_point(mapping = aes(x = Stressor_B, y = Fronds_number),
  #            data = Two_Stressors_Data, alpha = 0.5) +
  labs(title = "Dose-response curves",
       subtitle = "Based on a four-parameter log-logistic function",
       x = "Stressor B", 
       y = "Fronds number",
       fill = "Stressor C",
       color = "Stressor C") + 
  theme_bw()

# ggsave("Plots/Two_Stressors_Fronds_number_DRC.pdf", height = 3.5, units = "in")
