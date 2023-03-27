library(readr)
library(ggplot2)
library(dplyr)

# SEA-LEVEL RISE
s <- read_csv("/Users/davidkahler/Documents/Hydrology_and_WRM/Limpopo_Basin_Study/Land_Movement_Gabi/d181_Durban_Daily_RawData_UHSLC.csv")
t <- s %>%
     group_by(Hydrologic_Year) %>%
     summarize(mn=mean(Value, na.rm = TRUE)/1000) # initially in mm

n <- lm(t$mn~t$Hydrologic_Year)
summary(n)
confint(n)
# Estimate Std. Error t value Pr(>|t|)  
# (Intercept)       -1.0813041  0.8430176  -1.283    0.206  
# t$Hydrologic_Year  0.0011073  0.0004226   2.620    0.012 *
#      ---
#      Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 
#                          2.5 %      97.5 %
# (Intercept)       -2.7802944621 0.617686331
# t$Hydrologic_Year  0.0002555817 0.001959008

ggplot(t) +
     geom_point(aes(x=Hydrologic_Year, y=mn)) +
     geom_smooth(aes(x=Hydrologic_Year,y=mn), method = "lm", se = TRUE, color='blue') +
     xlab("Hydrologic Year") +
     ylab("Mean Sea-Level Height (m)") +
     theme(panel.background = element_rect(fill = "white", colour = "black")) + 
     theme(aspect.ratio = 1) +
     theme(axis.text = element_text(face = "plain", size = 14)) +
     theme(axis.title = element_text(face = "plain", size = 14))

######################### Xai Xai #############################################
# Read in CSV of river height data from gauge station E-38 at XaiXai
# Data provided by ARA-SUL
data_E38 <- read_csv("/Users/davidkahler/Documents/Hydrology_and_WRM/Limpopo_Basin_Study/Land_Movement_Gabi/Xai_Xai_WaterHeight_StationE38_RAW.csv")
annual_E38 <- data_E38 %>%
     rename(hydroYear=`Hydrologic Year`) %>%
     #drop_na(Mean) %>%
     group_by(hydroYear) %>%
     summarize(mn=mean(Mean, na.rm=TRUE)) # original data in m

model_E38 <- lm(annual_E38$mn ~ annual_E38$hydroYear)
# Coefficients:
#                        Estimate Std. Error t value Pr(>|t|)    
# (Intercept)          40.755880   9.701082   4.201 0.000369 ***
#      annual_E38$hydroYear -0.019414   0.004832  -4.017 0.000578 ***
#      ---
#      Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#                                 2.5 %       97.5 %
#      (Intercept)          20.63706833 60.874692430
# annual_E38$hydroYear -0.02943563 -0.009392097

E38 <- ggplot(annual_E38) +
     geom_point(aes(x=hydroYear, y=mn)) +
     geom_smooth(aes(x=hydroYear,y=mn), method = "lm", se = TRUE, color='blue') +
     xlab("Hydrologic Year") +
     ylab("Mean River Height (m)") +
     annotate("text", x=2016.5, y=2.21, label = "E38", size = 6) +
     theme(panel.background = element_rect(fill = "white", colour = "black")) + 
     theme(aspect.ratio = 0.5) +
     theme(axis.text = element_text(face = "plain", size = 14)) +
     theme(axis.title = element_text(face = "plain", size = 14))
gE38 <- ggplotGrob(E38)

######################### Chokwe #############################################
# Read in CSV of river height data from gauge station E-35 at Chokwe
# Data provided by ARA-SUL
data_E35 <- read.csv("/Users/davidkahler/Documents/Hydrology_and_WRM/Limpopo_Basin_Study/Land_Movement_Gabi/Chokwe_WaterHeight_StationE35.csv")

annual_E35 <- data_E35 %>%
     rename(hydroYear=`Hydrologic_Year`) %>%
     #drop_na(Mean) %>%
     group_by(hydroYear) %>%
     summarize(mn=mean(Media, na.rm=TRUE)) # original data in m

model_E35 <- lm(annual_E35$mn ~ annual_E35$hydroYear)
# Coefficients:
#      Estimate Std. Error t value Pr(>|t|)
# (Intercept)          37.66277   53.30639   0.707    0.489
# annual_E35$hydroYear -0.01811    0.02652  -0.683    0.504
# > confint(model_E35)
# 2.5 %       97.5 %
#      (Intercept)          -74.80388887 150.12943039
# annual_E35$hydroYear  -0.07406813   0.03783858

E35 <- ggplot(annual_E35) +
     geom_point(aes(x=hydroYear, y=mn)) +
     geom_smooth(aes(x=hydroYear,y=mn), method = "lm", se = TRUE, color='blue') +
     xlab("Hydrologic Year") +
     ylab("Mean River Height (m)") +
     annotate("text", x=2017, y=2, label = "E35", size = 6) +
     theme(panel.background = element_rect(fill = "white", colour = "black")) + 
     theme(aspect.ratio = 0.5) +
     theme(axis.text = element_text(face = "plain", size = 14)) +
     theme(axis.title = element_text(face = "plain", size = 14))
gE35 <- ggplotGrob(E35)

grid::grid.newpage()
rivers <- grid::grid.draw(rbind(gE38,gE35))
#ggsave("f02_riverheighs.eps", rivers, device = "eps", dpi = 72)


# BEITBRIDGE DISCHARGE
x <- read_csv("/Users/davidkahler/Documents/Hydrology_and_WRM/Limpopo_Basin_Study/Land_Movement_Gabi/beitbridge_discharge.csv")
y <- x %>%
     select(Year,`Height (m)`) %>%
     rename(height=`Height (m)`)

m <- lm(y$height~y$Year)
summary(m)
confint(m)
# Call:
#      lm(formula = y$height ~ y$Year)
# Residuals:
#      Min       1Q   Median       3Q      Max 
# -0.23808 -0.16895 -0.01556  0.12205  0.32043 
# Coefficients:
#      Estimate Std. Error t value Pr(>|t|)
# (Intercept)  1.3666340  7.5547394   0.181    0.858
# y$Year      -0.0004995  0.0037623  -0.133    0.895
# Residual standard error: 0.1741 on 26 degrees of freedom
# (3 observations deleted due to missingness)
# Multiple R-squared:  0.0006776,	Adjusted R-squared:  -0.03776 
# F-statistic: 0.01763 on 1 and 26 DF,  p-value: 0.8954
#                   2.5 %       97.5 %
# (Intercept) -14.16235519 16.895623166
# y$Year       -0.00823302  0.007233954

ggplot(y) +
     geom_point(aes(x=Year,y=height)) +
     geom_smooth(aes(x=Year,y=height), method = "lm", se = TRUE, color='blue') +
     xlab("Hydrologic Year") +
     ylab("Mean Gauge Height (m)") +
     theme(panel.background = element_rect(fill = "white", colour = "black")) + 
     theme(aspect.ratio = 1) +
     theme(axis.text = element_text(face = "plain", size = 14)) +
     theme(axis.title = element_text(face = "plain", size = 14))




