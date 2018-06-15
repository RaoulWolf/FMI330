# install.packages(c("tidyverse", "drc"))

library(drc)
library(tidyverse)
library(readxl)

data_raw <- read_excel(path = "Data/Data-FMI310.xlsx", sheet = "Two stressors -SM")

data_raw

data <- data_raw %>% 
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

data

rm(data_raw)

data %>% 
  ggplot(mapping = aes(x = Stressor_B, y = Fronds_number, color = Stressor_C)) +
  geom_jitter(alpha = 0.5, width = (max(data$Stressor_B) - min(data$Stressor_B)) / 100, 
              height = (max(data$Fronds_number) - min(data$Fronds_number)) / 100) +
  labs(title = "The raw data", 
       x = "Stressor B", 
       y = "Fronds number") + 
  theme_bw()

data_glm <- glm(formula = Fronds_number ~ Stressor_B * Stressor_C, data = data, 
                family = "poisson", contrasts = list(Stressor_C = "contr.treatment"))

data_glm %>% summary()

data_glm %>% drop1(scope = . ~ ., test = "Chisq")

data_drm <- drm(formula = Fronds_number ~ Stressor_B, Stressor_C, data = data, 
                fct = LL.4(names = c("Slope", "Lower Limit", "Upper Limit", "ED50")),
                type = "Poisson")

data_drm %>% summary()

data_drm %>% plot()

ED(data_drm, respLev = c(5, 10, 50, 90), interval = "delta")

pred <- expand.grid(Stressor_B = seq(from = min(data$Stressor_B), 
                                  to = max(data$Stressor_B), 
                                  length.out = 1000), 
                 Stressor_C = unique(data$Stressor_C)) %>% 
  mutate(fit = predict(data_drm, newdata = .), 
         lwr = predict(data_drm, newdata = ., interval = "confidence")[, 2], 
         upr = predict(data_drm, newdata = ., interval = "confidence")[, 3]) %>% 
  as_tibble()

# data_drm <- drm(formula = Fronds_number ~ Stressor_A, data = data, 
#                 fct = LL.4(names = c("Slope", "Lower Limit", "Upper Limit", "ED50")))
# 
# data_drm %>% summary()
# 
# ED(data_drm, respLev = c(5, 10, 50, 90), interval = "delta")

pred <- crossing(Stressor_B = seq(from = min(data$Stressor_B), 
                                  to = max(data$Stressor_B), 
                                  length.out = 1000), 
                 Stressor_C = unique(data$Stressor_C)) %>% 
  arrange(Stressor_C) %>% 
  mutate(fit = predict(data_glm, newdata = ., type = "response"),
         se = predict(data_glm, newdata = ., type = "response", se.fit = TRUE)$se.fit,
         lwr = fit - 1.96 * se, 
         upr = fit + 1.96 * se) %>% 
  select(-se)

pred

data %>% 
  group_by(Stressor_B, Stressor_C) %>% 
  summarize(Fronds_number_mean = mean(Fronds_number), 
            Fronds_number_SE = sd(Fronds_number) / sqrt(n())) %>% 
  ungroup() %>% 
  ggplot() +
  # geom_vline(xintercept = data_drm$coefficients[4], 
  #            color = "blue", linetype = 3) +
  # geom_hline(yintercept = ((data_drm$coefficients[3] - data_drm$coefficients[2]) / 2) + data_drm$coefficients[2], 
  #            color = "blue", linetype = 3) +
  geom_ribbon(mapping = aes(x = Stressor_B, ymin = lwr, ymax = upr, fill = Stressor_C), 
              data = pred, alpha = 0.2) +
  geom_line(mapping = aes(x = Stressor_B, y = fit, color = Stressor_C), 
            data = pred, size = 1) +
  geom_point(mapping = aes(x = Stressor_B, y = Fronds_number_mean, color = Stressor_C)) +
  geom_errorbar(mapping = aes(x = Stressor_B, color = Stressor_C,
                              ymin = Fronds_number_mean - Fronds_number_SE,
                              ymax = Fronds_number_mean + Fronds_number_SE),
                width = 0) + 
  # geom_point(mapping = aes(x = Stressor_A, y = Fronds_number), 
  #            data = data, alpha = 0.5) +
  labs(title = "Dose-response curves*",
       subtitle = "Based on a poisson-family GLM",
       x = "Stressor B", 
       y = "Fronds number") + 
  theme_bw()

# ggsave("Plots/Fronds_DRC.pdf", height = 3.5, units = "in")
