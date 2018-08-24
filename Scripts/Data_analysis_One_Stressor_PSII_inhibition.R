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

extrafont::loadfonts(device = "postscript", quiet = TRUE)
extrafont::loadfonts(device = "win", quiet = TRUE)

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
         Fronds_number = as.integer(Fronds_number), 
         PS_II_inhibition_binomial = PS_II_inhibition / 100) %>% 
  arrange(Stressor_A, Replicate)

One_Stressor_Data

rm(One_Stressor_Data_raw)

## First visualisation of the raw data
One_Stressor_Data %>% 
  ggplot(mapping = aes(x = Stressor_A, y = PS_II_inhibition)) +
  geom_point(alpha = 0.5) +
  labs(title = "The raw data", 
       x = "Stressor A", 
       y = "Fronds number") + 
  theme_bw()

## Fitting a four-parametric log-logistic dose response curve
One_Stressor_drm <- drm(formula = PS_II_inhibition_binomial ~ Stressor_A, data = One_Stressor_Data, 
                        fct = LL.4(fixed = c(NA, NA, NA, NA), 
                                   names = c("Slope", "Lower Limit", "Upper Limit", "EC50")),
                        type = "continuous")

# One_Stressor_drm_u <- drm(formula = PS_II_inhibition_binomial ~ Stressor_A, data = One_Stressor_Data, 
#                           fct = ucedergreen(fixed = c(NA, 0, 1, NA, NA), 
#                                             names = c("Parameter 1", "Lower Limit", "Upper Limit", "Parameter 2", "Parameter 3"), 
#                                             alpha = 1),
#                           type = "continuous")

One_Stressor_drm %>% summary()

## Getting the EC5, EC10, EC50 and EC90 values
One_Stressor_drm %>% ED(respLev = c(5, 10, 50, 90), interval = "delta")

## Creating the curve data for visualisation
One_Stressor_pred <- data.frame(Stressor_A = seq(from = min(One_Stressor_Data$Stressor_A), 
                                                 to = max(One_Stressor_Data$Stressor_A), 
                                                 length.out = 1000)) %>% 
  mutate(fit = predict(One_Stressor_drm, newdata = .), 
         lwr = predict(One_Stressor_drm, newdata = ., interval = "confidence", constrain = FALSE)[, 2], 
         upr = predict(One_Stressor_drm, newdata = ., interval = "confidence", constrain = FALSE)[, 3]) %>%  
         # fit_u = predict(One_Stressor_drm_u, newdata = .)) %>% 
  as_tibble()

One_Stressor_pred

## Visualizing a summary of the data and the dose-response curve
One_Stressor_Data %>% 
  group_by(Stressor_A) %>% 
  summarize(PS_II_inhibition_mean = mean(PS_II_inhibition_binomial), 
            PS_II_inhibition_SE = sd(PS_II_inhibition_binomial) / sqrt(n())) %>% 
  ggplot() +
  geom_vline(xintercept = ED(One_Stressor_drm, respLev = 50, display = FALSE)[1],
             linetype = 2, color = niva_cols("orange")) +
  geom_hline(yintercept = 0.5,
             linetype = 2, color = niva_cols("orange")) +
  # geom_vline(xintercept = ED(One_Stressor_drm_u, respLev = 50, display = FALSE)[1],
  #            linetype = 3, alpha = 0.5, color = "darkgreen") +
  # geom_hline(yintercept = 0.5,
  #            linetype = 3, alpha = 0.5, color = "darkgreen") +
  # geom_line(mapping = aes(x = Stressor_A, y = fit_u),
  #           data = One_Stressor_pred, size = 1, alpha = 0.5, color = "darkgreen") +
  geom_ribbon(mapping = aes(x = Stressor_A, ymin = lwr, ymax = upr),
              data = One_Stressor_pred, alpha = 0.5, fill = niva_cols("aqua")) +
  geom_line(mapping = aes(x = Stressor_A, y = fit),
            data = One_Stressor_pred, size = 1, alpha = 0.5, color = niva_cols("dark blue")) +
  geom_point(mapping = aes(x = Stressor_A, y = PS_II_inhibition_mean), color = niva_cols("dark purple")) +
  geom_errorbar(mapping = aes(x = Stressor_A, 
                              ymin = PS_II_inhibition_mean - PS_II_inhibition_SE,
                              ymax = PS_II_inhibition_mean + PS_II_inhibition_SE),
                width = 0, color = niva_cols("dark purple")) + 
  # geom_point(mapping = aes(x = Stressor_A, y = PS_II_inhibition_binomial),
  #            data = One_Stressor_Data, alpha = 0.5, color = "red") +
  scale_y_continuous(breaks = c(0, 0.25, 0.5, 0.75, 1), 
                     labels = c("0", "25", "50", "75", "100")) +
  labs(title = expression(bold("Photosystem II inhibition")),
       subtitle = "Four-parameter log-logistic dose-response curve",
       x = expression("3,5-Dichlorophenol concentration"~("mg"/"L")), 
       y = expression("Photosystem II inhibition"~("%"))) + 
  theme_minimal(base_family = "Calibri Light") +
  theme(title = element_text(color = niva_cols("dark purple")), 
        axis.title = element_text(color = niva_cols("dark purple")), 
        axis.text = element_text(color = niva_cols("dark purple")))


## Saving the plot
# ggsave("Plots/One_Stressor_PSII_inhibition_DRC.svg", height = 3.5, units = "in")
