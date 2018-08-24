## (Installing and) Loading the necessary packages
# install.packages(c("tidyverse", "drc"))
library(drc)
library(tidyverse)
library(readxl)

niva_colors <- c(`dark blue`   = rgb(red =   0, green =  96, blue = 169, maxColorValue = 255), 
                  aqua         = rgb(red = 170, green = 218, blue = 219, maxColorValue = 255), 
                  blue         = rgb(red =   0, green = 158, blue = 224, maxColorValue = 255), 
                 `dark teal`   = rgb(red =   0, green = 164, blue = 167, maxColorValue = 255),
                  orange       = rgb(red = 228, green = 104, blue =  11, maxColorValue = 255),
                 `dark purple` = rgb(red =  26, green =  23, blue =  27, maxColorValue = 255),
                 `light grey`  = rgb(red = 182, green = 183, blue = 185, maxColorValue = 255), 
                  white        = rgb(red = 255, green = 255, blue = 255, maxColorValue = 255))

niva_cols <- function(...) {
  cols <- c(...)
  if (is.null(cols))
    return(niva_colors)
  niva_colors[cols]
}

# install.packages("extrafont")
# extrafont::font_import()
# extrafont::loadfonts(device = "pdf", quiet = TRUE)
extrafont::loadfonts(device = "win", quiet = TRUE)
extrafont::loadfonts(device = "postscript", quiet = TRUE)

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
  geom_point(alpha = 0.5, color = niva_cols("dark blue")) +
  labs(title = "The raw data", 
       x = expression("3,5-Dichlorophenol concentration (mg/L)"), 
       y = expression("Fronds number (count)")) + 
  theme_minimal(base_family = "Calibri Light")

## Fitting a four-parametric log-logistic dose response curve
One_Stressor_drm <- drm(formula = Fronds_number ~ Stressor_A, data = One_Stressor_Data, 
                        fct = LL.4(fixed = c(NA, NA, NA, NA),
                                   names = c("Slope", "Lower Limit", "Upper Limit", "ED50")),
                        type = "Poisson")

One_Stressor_drm %>% summary()

## Getting the EC5, EC10, EC50 and EC90 values
One_Stressor_drm %>% ED(respLev = c(5, 10, 50, 90), interval = "delta") # %>% capture.output(file = "EC_values.txt")

## Creating the curve data for visualisation
One_Stressor_pred <- data.frame(Stressor_A = seq(from = min(One_Stressor_Data$Stressor_A), 
                                                 to = max(One_Stressor_Data$Stressor_A), 
                                                 length.out = 1000)) %>% 
  mutate(fit = predict(One_Stressor_drm, newdata = .), 
         lwr = predict(One_Stressor_drm, newdata = ., interval = "confidence", constrain = FALSE)[, 2], 
         upr = predict(One_Stressor_drm, newdata = ., interval = "confidence", constrain = FALSE)[, 3]) %>% 
  as_tibble()

One_Stressor_pred

## Visualizing a summary of the data and the dose-response curve
One_Stressor_Data %>% 
  group_by(Stressor_A) %>% 
  summarize(Fronds_number_mean = mean(Fronds_number), 
            Fronds_number_SE = sd(Fronds_number) / sqrt(n())) %>% 
  ggplot() +
  geom_vline(xintercept = One_Stressor_drm$coefficients[4],
             linetype = 2, color = niva_cols("orange")) +
  geom_hline(yintercept = ((One_Stressor_drm$coefficients[3] - One_Stressor_drm$coefficients[2]) / 2) + One_Stressor_drm$coefficients[2],
             linetype = 2, color = niva_cols("orange")) +
  geom_ribbon(mapping = aes(x = Stressor_A, ymin = lwr, ymax = upr), 
              data = One_Stressor_pred, alpha = 0.5, fill = niva_cols("aqua")) +
  geom_line(mapping = aes(x = Stressor_A, y = fit), 
            data = One_Stressor_pred, size = 1, alpha = 0.5, color = niva_cols("dark blue")) +
  geom_point(mapping = aes(x = Stressor_A, y = Fronds_number_mean), color = niva_cols("dark purple"), size = 0.75) +
  geom_errorbar(mapping = aes(x = Stressor_A,
                              ymin = Fronds_number_mean - Fronds_number_SE,
                              ymax = Fronds_number_mean + Fronds_number_SE),
                width = 0, color = niva_cols("dark purple")) +
  # geom_point(mapping = aes(x = Stressor_A, y = Fronds_number),
  #            data = One_Stressor_Data, color = "red", alpha = 0.25) +
  # scale_x_log10(breaks = c(0.5, 1, 2, 4, 8)) +
  labs(# title = expression(bold("Dose-response curve")),
       # subtitle = "Based on a four-parameter log-logistic function",
       x = expression("3,5-Dichlorophenol concentration (mg/L)"), 
       y = expression("Fronds number (count)")) + 
  theme_minimal(base_family = "Calibri Light")

## Saving the plot
# ggsave("Plots/One_Stressor_Fronds_number_DRC_Calibri_Light.pdf", height = 3.5, width = 7, units = "in")
# ggsave("Plots/One_Stressor_Fronds_number_DRC_Calibri_Light.svg", height = 3.5, width = 7, units = "in")

# Sys.setenv(R_GSCMD = "C:/Program Files/gs/gs9.23/bin/gswin64c.exe")
# extrafont::embed_fonts("Plots/One_Stressor_Fronds_number_DRC_Calibri_Light.pdf")
