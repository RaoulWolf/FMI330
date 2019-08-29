## Running the first script

source("Scripts/1) Data import and cleaning.R")

## Visualizing the data

data %>% 
  ggplot() +
  geom_point(mapping = aes(x = Concentration, y = ROS_formation), 
             size = 2, alpha = 0.5) +
  scale_y_log10() +
  labs(x = "Diuron (mg/L)", 
       y = "ROS formation (fold-change)") +
  theme_light()

## Saving the figure

ggsave(filename = "Figures/5) ROS formation.png", height = 5.25, width = 7, 
       units = "in", dpi = 600, type = "cairo-png")

## NOEC/LOEC derivation

data %>% 
  mutate(Concentration = fct_relevel(as.character(Concentration), "0")) %>% 
  lm(formula = ROS_formation ~ Concentration, 
     data = ., contrasts = list(Concentration = "contr.treatment")) %>% 
  glht(linfct = mcp(Concentration = "Dunnett"), vcov = sandwich) %>% 
  summary(test = adjusted(type = "holm"))
