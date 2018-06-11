# install.packages(c("tidyverse", "drc"))

library(drc)
library(tidyverse)
library(readxl)

data_raw <- read_excel(path = "Data/Data-FMI310.xlsx", sheet = "One stressor -SM")

data_raw

data <- data_raw %>% 
  rename(Stressor_A = `Stressor A`, 
         Fronds_number = `Fronds Number`, 
         Growth_rate = `Growth rate (pr day)`,
         Growth_inhibition = `Growth inhibition (%)`, 
         Fv_Fm = `Fv/Fm`, 
         PS_II_inhibition = `PS II inhibition (%)`, 
         ROS_formation = `ROS formation`, 
         ROS_formation_fold_increase = `ROS formation (fold increase)`) %>% 
  mutate(Replicate = as.factor(Replicate)) %>% 
  arrange(Stressor_A, Replicate)

data

rm(data_raw)

data %>% 
  ggplot(mapping = aes(x = Stressor_A, y = Fronds_number)) +
  geom_point(alpha = 0.5) +
  labs(title = "The raw data", 
       x = "Stressor A", 
       y = "Fronds number") + 
  theme_bw()

data_drm <- drm(formula = Fronds_number ~ Stressor_A, data = data, 
                fct = LL.4(names = c("Slope", "Lower Limit", "Upper Limit", "ED50")))

data_drm %>% summary()

ED(data_drm, respLev = c(5, 10, 50, 90), interval = "delta")

pred <- data.frame(Stressor_A = seq(from = min(data$Stressor_A), 
                                    to = max(data$Stressor_A), 
                                    length.out = 1000)) %>% 
  mutate(pred = predict(data_drm, newdata = .), 
         lwr = predict(data_drm, newdata = ., interval = "confidence")[, 2], 
         upr = predict(data_drm, newdata = ., interval = "confidence")[, 3]) %>% 
  as_tibble()

pred

data %>% 
  group_by(Stressor_A) %>% 
  summarize(Fronds_number_mean = mean(Fronds_number), 
            Fronds_number_SE = sd(Fronds_number) / sqrt(n())) %>% 
  ggplot() +
  geom_vline(xintercept = data_drm$coefficients[4], 
             color = "blue", linetype = 3) +
  geom_hline(yintercept = ((data_drm$coefficients[3] - data_drm$coefficients[2]) / 2) + data_drm$coefficients[2], 
             color = "blue", linetype = 3) +
  geom_ribbon(mapping = aes(x = Stressor_A, ymin = lwr, ymax = upr), 
              data = pred, alpha = 0.2) +
  geom_line(mapping = aes(x = Stressor_A, y = pred), 
            data = pred, size = 1) +
  geom_point(mapping = aes(x = Stressor_A, y = Fronds_number_mean), 
             color = "red") +
  geom_errorbar(mapping = aes(x = Stressor_A, 
                              ymin = Fronds_number_mean - Fronds_number_SE,
                              ymax = Fronds_number_mean + Fronds_number_SE),
                color = "red", width = 0) + 
  labs(title = "Dose-response curve",
       subtitle = "Based on a four-parameter log-logistic function",
       x = "Stressor A", 
       y = "Fronds number") + 
  theme_bw()

ggsave("Plots/Fronds_DRC.pdf", height = 3.5, units = "in")
