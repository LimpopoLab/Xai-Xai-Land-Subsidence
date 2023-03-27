#Import libraries
library(tidyverse)
library(ggplot2)
library(lubridate)
library(dataRetrieval)
library(zoo)
library(viridis)
library(knitr)
library(rmarkdown)
library(dplyr)
library(tidyr)
library(ggplot2)

######################### Xai Xai #############################################

# Read in CSV of river height data from gauge station E-38 at XaiXai
# Data provided by ARA-SUL
data_E38 <- read.csv("C:/Users/zucco/OneDrive - University of Pittsburgh/Desktop/Masters_Research/Data/RiverGauges/Xai_Xai_WaterHeight_StationE38_RAW.csv")
data_E38 <- data_E38 %>% 
  drop_na(Mean)
data_E38$Mean <- data_E38$Mean*100

colnames(data_E38)[colnames(data_E38) == 'Hydrologic.Year'] <- 'Hydrologic_Year'

# Read in CSV of river height data from gauge station E-35 at Chokwe
# Data provided by ARA-SUL
data_E35 <- read.csv("C:/Users/zucco/OneDrive - University of Pittsburgh/Desktop/Masters_Research/Data/RiverGauges/Chokwe_WaterHeight_StationE35.csv")
data_E35 <- data_E35 %>% 
  drop_na(Media)
data_E35$Media <- data_E35$Media*100

yearly_trend_E38 <- data_E38 %>%
  group_by(Hydrologic_Year) %>%
  summarise_at(vars(Mean), list(Avg_Yearly_Height_E38 = mean))

yearly_trend_E35 <- data_E35 %>%
  group_by(Hydrologic_Year) %>%
  summarise_at(vars(Media), list(Avg_Yearly_Height_E35 = mean))

model_E38 <- lm(Avg_Yearly_Height_E38 ~ Hydrologic_Year, data = yearly_trend_E38)
#get intercept and slope value
coeff_E38<-coefficients(model_E38)          
intercept_E38<-coeff_E38[1]
slope_E38<- coeff_E38[2]

model_E35 <- lm(Avg_Yearly_Height_E35 ~ Hydrologic_Year, data = yearly_trend_E35)
#get intercept and slope value
coeff_E35<-coefficients(model_E35)          
intercept_E35<-coeff_E35[1]
slope_E35<- coeff_E35[2]

yearly_trends <- list(yearly_trend_E35, yearly_trend_E38)
yearly_trends <- yearly_trends %>% reduce(full_join, by='Hydrologic_Year')

yearly_trends <- yearly_trends %>% pivot_longer(cols=c('Avg_Yearly_Height_E38', 'Avg_Yearly_Height_E35'),
                          names_to='Gauge',
                          values_to='Height')

ggplot(yearly_trends, aes(x=Hydrologic_Year, y=Height)) + 
  geom_point()+
  geom_line() +
  xlab('Hydrologic Year')+
  ylab('Average Annual River Height (cm)')+
  facet_wrap(~Gauge)+
  stat_smooth(method = "lm",
              formula = y ~ x,
              geom = "smooth")

  

ggplot(yearly_trends, aes(x=Hydrologic_Year, y=Height)) + 
  geom_bar(stat='identity')+
  xlab('Hydrologic Year')+
  ylab('Average Annual River Height (cm)')+
    facet_wrap(~Gauge)+
  stat_smooth(method = "lm",
              formula = y ~ x,
              geom = "smooth")
