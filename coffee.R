library(tidyverse)
library(patchwork)

# set wd
setwd("~/Desktop/Github Projects/STA302")

# load the data, and we are choosing
# Species, Aroma, Flavor, Aftertaste, Acidity, and Sweetness
# y = Cupper.Points (Ratings)
# The indicator is created for species, where = 1 if is "Arabica"; = 2 otherwise
coffee_df <- read.csv(file = "merged_data_cleaned.csv") %>% 
  select(Species, Aroma, Flavor, Aftertaste, Acidity, Sweetness, Total.Cup.Points) %>% 
  rename("Ratings" = Total.Cup.Points) %>% 
  mutate(.before = 1, InSpecies = ifelse(Species == "Arabica", 1, 0)) %>% 
  select(InSpecies, Aroma, Flavor, Aftertaste, Acidity, Sweetness, Ratings)

# Q1: Fit your preliminary multiple linear model 
#and present the estimated relationship. 
model <- lm(Ratings ~ InSpecies + Aroma + Flavor + Aftertaste + Acidity, data = coffee_df)

# Assumption check
# 1. residual vs. fitted
y_hat <- fitted(model)
e_hat <- resid(model)
coffee_df <- coffee_df %>% 
  mutate("y_hat" = fitted(model)) %>% 
  mutate("e_hat" = resid(model))

# resid_fitted <- as.data.frame(cbind(y_hat, e_hat))


plot(x = y_hat, y = e_hat, main = "Residual vs Fitted",
     xlab = "Fitted", ylab = "Residuals")

plot1 <- ggplot(data = coffee_df, 
       mapping = aes(x = y_hat, y = e_hat))+
  geom_point(alpha = 0.5, size = 2)+ 
  theme_bw()+
  theme(plot.title = element_text(hjust = 0.5))+
  labs(x = "Fitted", y = "Residuals", title = "Residual vs. Fitted")
# ggsave(plot1, file = "ResidFitted.png", dpi = 600, width = 5, height = 5)

# all residual vs. predictors
# par(mfrow = c(2, 3))

# plot(x = coffee_data$InSpecies, y = e_hat, 
#                       main = "Residual vs. Species", xlab = "Species",
#                       ylab = "Residual")
# plot(x = coffee_data$Aroma, y = e_hat, 
#                     main = "Residual vs. Aroma", xlab = "Aroma",
#                     ylab = "Residual")
# plot(x = coffee_data$Flavor, y = e_hat, 
#                      main = "Residual vs. Flavor", xlab = "Flavor",
#                      ylab = "Residual")
# plot(x = coffee_data$Aftertaste, y = e_hat, 
#                     main = "Residual vs. Aftertaste", xlab = "Aftertaste",
#                     ylab = "Residual")
# plot(x = coffee_data$Acidity, y = e_hat, 
#                       main = "Residual vs. Acidity", xlab = "Acidity",
#                       ylab = "Residual")
# plot(x = coffee_data$Sweetness, y = e_hat, 
#                     main = "Residual vs. Sweetness", xlab = "Sweetness",
#                     ylab = "Residual")


# checking normality of errors
qqnorm(e_hat)
qqline(e_hat)

plot3 <- ggplot(data = coffee_df, 
                mapping = aes(x = y_hat, y = Ratings))+
  geom_point(alpha = 0.5, size = 2)+ 
  theme_bw()+
  theme(plot.title = element_text(hjust = 0.5))+
  labs(x = "Fitted", y = "Ratings", title = "Response vs. Fitted")
# ggsave(plot3, file = "ResponseFitted.png", dpi = 600, width = 5, height = 5)


pairs(coffee_df[, c(1:7)])