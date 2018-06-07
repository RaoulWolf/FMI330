# install.packages(c("tidyverse", "drc"))

library(drc)
library(tidyverse)
library(readxl)

data_raw <- read_excel("Data/Data-FMI310_tidy.xlsx", 
                       sheet = "One_stressor_tidy")

data_raw

data <- data_raw %>% 
  mutate(Replicate = as.factor(Replicate)) %>% 
  arrange(Stressor_A, Replicate)

data

data %>% 
  ggplot(mapping = aes(x = Stressor_A, y = Fronds_number)) +
  geom_point(alpha = 0.5) +
  labs(x = "Concentration", 
       y = "Fronds number") + 
  theme_bw()

data_drm <- drm(formula = Fronds_number ~ Stressor_A, data = data, 
                fct = LL.4(names = c("Slope", "Bottom", "Top", "EC50")))

data_drm %>% summary()

pred <- tibble(Stressor_A = seq(from = min(data$Stressor_A), 
                                  to = max(data$Stressor_A), 
                                  length.out = 1000)) %>% 
  as.data.frame() %>% 
  mutate(pred = predict(data_drm, newdata = .), 
         lwr = predict(data_drm, newdata = ., interval = "confidence")[, 2], 
         upr = predict(data_drm, newdata = ., interval = "confidence")[, 3]) %>% 
  as_tibble()

data %>% 
  ggplot() +
  geom_point(mapping = aes(x = Stressor_A, y = Fronds_number), alpha = 0.5) +
  geom_vline(xintercept = data_drm$coefficients[4], color = "red") +
  geom_hline(yintercept = ((data_drm$coefficients[3] - data_drm$coefficients[2]) / 2) + data_drm$coefficients[2], 
             color = "red") +
  geom_ribbon(mapping = aes(x = Stressor_A, ymin = lwr, ymax = upr), 
              data = pred, alpha = 0.2) +
  geom_line(mapping = aes(x = Stressor_A, y = pred), data = pred, size = 1) +
  labs(x = "Concentration", 
       y = "Fronds number") + 
  theme_bw()
