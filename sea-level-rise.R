# Sea-level rise
# Figure 01

library(readr)
library(ggplot2)
library(dplyr)

# SEA-LEVEL RISE
s <- read_csv("d181_Durban_Daily_RawData_UHSLC.csv")
t <- s %>%
     group_by(Hydrologic_Year) %>%
     summarize(mn=mean(Value, na.rm = TRUE)/1000) # initially in mm, now in m

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
