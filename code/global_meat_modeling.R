#R for Data Analysis - BAE 590 Module 11
#Regression and Modeling in R
#Homework 11/13/2019
#Script Author: Justine Neville
#######################################################################
#load required packages -----
library(tidyverse)
library(modelr)
library(broom)
library(corrr)

#load in required datasets----
gdp_2017 <- read_csv("data/gdp-2017.csv")
meat_production_2017 <- read_csv("data/meat-production-2017.csv")
total_pop_2017 <- read_csv("data/total-population-2017.csv")
#create and execute a function that lets you view and explore the structure of the datasets ----
explore_data <- function(dataframe){
  view(dataframe)
  str(dataframe)
  head(dataframe)
}

explore_data(gdp_2017)
explore_data(meat_production_2017)
explore_data(total_pop_2017)

#summarize meat production data for 2017----
meat_production_2017 %>%
  group_by(country) %>%
  summarize(total_meat = sum(meat_produced, na.rm=T)) ->country_meat_prod_2017
#join data to one dataframe with country, meat_produced, total_pop, and gdp----
country_meat_prod_2017%>%
  inner_join(gdp_2017, by="country")%>%
  inner_join(total_pop_2017,by="country")->global_meat_gdp_pop_2017

#plot data to visualize relationships
ggplot(global_meat_gdp_pop_2017)+
  geom_point(mapping=aes(x=gdp,y=total_meat, alpha = 0.5))

ggplot(global_meat_gdp_pop_2017)+
  geom_point(mapping=aes(x=total_population,y=total_meat, alpha = 0.5))

#normalize the data for better visualization ----
# User-defined function for normalizing
normalize <- function(x) {
  n <- (x - min(x)) / (max(x) - min(x))
  return(n)
}
# Normalize the meat_produced, gdp, and total_population columns
global_meat_gdp_pop_2017 %>%
  mutate(meat_produced = normalize(total_meat),
         gdp = normalize(gdp),
         total_population = normalize(total_population)) -> global_norm
# Check that each column in the new dataframe ranges from 0-1
summary(global_norm)


# Re-plot the meat_produced vs. gdp figure
ggplot(data = global_norm) +
  geom_point(mapping = aes(x = gdp, y = meat_produced), alpha = 0.5) +
  theme_bw()
# Re-plot the meat_produced vs. gdp figure
ggplot(data = global_norm) +
  geom_point(mapping = aes(x = gdp, y = meat_produced), alpha = 0.5) +
  lims(x = c(0,0.05),
     y = c(0, 0.15)) +
  theme_bw()


#re-plot the meat_produced vs. total_population figure
ggplot(data = global_norm) +
  geom_point(mapping = aes(x = total_population, y = meat_produced), alpha = 0.5) +
  theme_bw()

#Calculate and visualize correlations ----
global_norm %>%
  dplyr::select(-country) %>%
  correlate() %>%
  fashion()

global_norm %>%
  dplyr::select(-country) %>%
  correlate() %>%
  rplot()


#create and inspect regression models ----
#meat production vs gdp-----
#meat_production as a function of gdp
meat_gdp <- lm(meat_produced ~ gdp, data=global_norm)
tidy(meat_gdp)
glance(meat_gdp)
meat_gdp <- augment(meat_gdp,data=global_norm)

#plot fitted vs. observed meat production
ggplot(meat_gdp)+
  geom_point(mapping=aes(x=.fitted,y=meat_produced, alpha=0.5))+
  theme(legend.position = "none")+
  labs(x= "Meat Produced - Modeled", y= "Meat Produced - Observed")+
  geom_abline(intercept=0, slope=1, color = "Red")+
  xlim(0,0.15)+
  ylim(0,0.15)

#plot meat produced vs. residuals
ggplot(meat_gdp)+
  geom_ref_line(h=0)+
  geom_point(mapping=aes(x=meat_produced,y=.resid, alpha=0.5))+
  theme(legend.position = "none")+
  labs(x="Meat Produced", y= "Residuals")+
  xlim(0,0.15)+
  ylim(-0.15,0.15)
  
             
#meat production vs total population----
#meat_production as a function of gdp
meat_total_pop <- lm(meat_produced ~ total_population, data=global_norm)
tidy(meat_total_pop)
glance(meat_total_pop)
meat_total_pop <- augment(meat_total_pop,data=global_norm)

#plot fitted vs. observed meat production
ggplot(meat_total_pop)+
  geom_point(mapping=aes(x=.fitted,y=meat_produced, alpha=0.5))+
  theme(legend.position = "none")+
  labs(x= "Meat Produced - Modeled", y= "Meat Produced - Observed")+
  geom_abline(intercept=0, slope=1, color = "Red")+
  xlim(0,0.15)+
  ylim(0,0.15)

#plot meat produced vs. residuals
ggplot(meat_total_pop)+
  geom_ref_line(h=0)+
  geom_point(mapping=aes(x=meat_produced,y=.resid, alpha=0.5))+
  theme(legend.position = "none")+
  labs(x="Meat Produced", y= "Residuals")+
  xlim(0,0.15)+
  ylim(-0.15,0.15)



#meat production vs gdp, total population, and the interaction between gdp and total population----
#meat_production as a function of gdp,total pop, and interaction btwn gdp and TP
meat_gdp_total_pop <- lm(meat_produced ~  gdp*total_population, data=global_norm)
tidy(meat_gdp_total_pop )
glance(meat_gdp_total_pop )
meat_gdp_total_pop  <- augment(meat_gdp_total_pop ,data=global_norm)

#plot fitted vs. observed meat production
ggplot(meat_gdp_total_pop )+
  geom_point(mapping=aes(x=.fitted,y=meat_produced, alpha=0.5))+
  theme(legend.position = "none")+
  labs(x= "Meat Produced - Modeled", y= "Meat Produced - Observed")+
  geom_abline(intercept=0, slope=1, color = "Red")+
  xlim(0,0.15)+
  ylim(0,0.15)

#plot meat produced vs. residuals
ggplot(meat_gdp_total_pop )+
  geom_ref_line(h=0)+
  geom_point(mapping=aes(x=meat_produced,y=.resid, alpha=0.5))+
  theme(legend.position = "none")+
  labs(x="Meat Produced", y= "Residuals")+
  xlim(0,0.15)+
  ylim(-0.15,0.15)
#log meat production vs. log gdp----
#log meat prod as a function of log gdp
log_meat_gdp <- lm(log(meat_produced + 0.0001) ~ log(gdp + 0.0001),data=global_norm)
tidy(log_meat_gdp)
glance(log_meat_gdp)
log_meat_gdp <- augment(log_meat_gdp,data=global_norm)

#plot fitted vs. observed meat production
ggplot(log_meat_gdp)+
  geom_point(mapping=aes(x=.fitted,y=meat_produced, alpha=0.5))+
  theme(legend.position = "none")+
  labs(x= "Meat Produced - Modeled", y= "Meat Produced - Observed")+
  geom_abline(intercept=0, slope=1, color = "Red")+
  xlim(-10,0.15)+
  ylim(0,0.25)

#plot meat produced vs. residuals
ggplot(log_meat_gdp)+
  geom_ref_line(h=0)+
  geom_point(mapping=aes(x=meat_produced,y=.resid, alpha=0.5))+
  theme(legend.position = "none")+
  labs(x="Meat Produced", y= "Residuals")+
  xlim(0,0.15)+
  ylim(-0.15,0.15)

#log meat production vs. log total population----
#log meat prod as a function of log total population
log_meat_toal_pop <- lm(log(meat_produced + 0.0001) ~ log(total_population + 0.0001),data=global_norm)
tidy(log_meat_toal_pop)
glance(log_meat_toal_pop)
log_meat_toal_pop <- augment(log_meat_toal_pop,data=global_norm)

#plot fitted vs. observed meat production
ggplot(log_meat_toal_pop)+
  geom_point(mapping=aes(x=.fitted,y=meat_produced, alpha=0.5))+
  theme(legend.position = "none")+
  labs(x= "Meat Produced - Modeled", y= "Meat Produced - Observed")+
  geom_abline(intercept=0, slope=1, color = "Red")+
  xlim(-10,0.15)+
  ylim(0,0.25)

#plot meat produced vs. residuals
ggplot(log_meat_toal_pop)+
  geom_ref_line(h=0)+
  geom_point(mapping=aes(x=meat_produced,y=.resid, alpha=0.5))+
  theme(legend.position = "none")+
  labs(x="Meat Produced", y= "Residuals")+
  xlim(0,0.15)+
  ylim(-0.15,0.15)



#Which model performed the best? Why? ----

## Of the models, the meat_produced as a function of gdp, total_population and the interaction between gdp and total_population performed the best. The r-squared value was highest at 0.91. With regards to figures, the observed vs. modeled data plot fell closest to the 1-to-1 line and the residuals plot vs normalized meat data was centered most closely around the 0 reference line. 