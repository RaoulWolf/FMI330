## Running the first script

source("Scripts/1) Data import and cleaning.R")

## Visualizing the data

data %>% 
  ggplot() +
  geom_point(mapping = aes(x = Concentration, y = Photosystem_II_inhibition), 
             size = 2, alpha = 0.5) +
  labs(x = "Diuron (mg/L)", 
       y = "Photosystem II inhibition (%)") +
  theme_light()

## NOEC/LOEC derivation

data %>% 
  mutate(Concentration = fct_relevel(as.character(Concentration), "0")) %>% 
  lm(formula = Photosystem_II_inhibition ~ Concentration, 
     data = ., contrasts = list(Concentration = "contr.treatment")) %>% 
  glht(linfct = mcp(Concentration = "Dunnett"), vcov = sandwich) %>% 
  summary(test = adjusted(type = "holm"))

## Dose-response modelling

photosystem_ii_inhibition.drm <- data %>% 
  drm(formula = Photosystem_II_inhibition ~ Concentration, data = .,
      fct = LL.4(names = c("Slope", "Lower Limit", "Upper Limit", "EC50")))

## Dose-response parameters

photosystem_ii_inhibition.drm %>% 
  coeftest(vcov. = sandwich)

## EC5/10/50 derivation

photosystem_ii_inhibition.drm %>% 
  ED(respLev = c(5, 10, 50), interval = "delta", vcov. = sandwich)

## Dose-response curve

data.frame(Concentration = seq(from = min(data$Concentration),
                               to = max(data$Concentration),
                               length.out = 1000)) %>% 
  mutate(fit = predict(photosystem_ii_inhibition.drm, newdata = .), 
         lwr = predict(photosystem_ii_inhibition.drm, newdata = ., interval = "confidence", vcov. = sandwich)[, 2], 
         upr = predict(photosystem_ii_inhibition.drm, newdata = ., interval = "confidence", vcov. = sandwich)[, 3]) %>% 
  ggplot() +
  geom_ribbon(mapping = aes(x = Concentration, ymin = lwr, ymax = upr), alpha = 0.2) +
  geom_line(mapping = aes(x = Concentration, y = fit), size = 1) +
  geom_point(mapping = aes(x = Concentration, y = Photosystem_II_inhibition), data = data, size = 2, alpha = 0.5) +
  labs(x = "Diuron (mg/L)", 
       y = "Photosystem II inhibition (%)") +
  theme_light()

## Saving the figure

ggsave(filename = "Figures/4) Photosystem II inhibition.png", height = 5.25, width = 7, 
       units = "in", dpi = 600, type = "cairo-png")

