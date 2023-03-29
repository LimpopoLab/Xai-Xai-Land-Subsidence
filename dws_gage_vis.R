# To visualize data scraped from DWS gages near Musina
# Figure S03

library(tidyverse)
library(latex2exp)
# install_github("LimpopoLab/hydrostats")
library(hydrostats)

x <- read_csv("A7H004.csv", col_names = FALSE)
x <- x %>%
     rename(dt=X1,UNIX=X2,lev_m=X3,levQC=X4,flow_m3s=X5,flowQC=X6) # note UNIX date time is UTC

y <- read_csv("A7H008.csv", col_names = FALSE)
y <- y %>%
     rename(dt=X1,UNIX=X2,lev_m=X3,levQC=X4,flow_m3s=X5,flowQC=X6) # note UNIX date time is UTC

z <- rbind(x,y)
# Remove duplicate dates
q <- z %>%
     mutate(lev_m=replace(lev_m, which(lev_m<=0),NA)) %>%
     mutate(flow_m3s=replace(flow_m3s, which(flow_m3s<=0),NA)) %>%
     group_by(UNIX) %>%
     summarize(lev_m=mean(lev_m,na.rm=TRUE), levQC=mean(levQC), flow_m3s=mean(flow_m3s,na.rm=TRUE), flowQC=mean(flowQC)) %>%
     mutate(dt=with_tz(as_datetime(UNIX), tzone = "Africa/Johannesburg"))

ggplot(q) +
     geom_point(aes(x=dt,y=flow_m3s)) +
     ylim(c(0,10000)) +
     xlab("Date") + 
     ylab(TeX('Discharge $(m^3/s)$')) + 
     theme(panel.background = element_rect(fill = "white", colour = "black")) + 
     theme(aspect.ratio = 1) +
     theme(axis.text = element_text(face = "plain", size = 12))

## Annual data
a <- q %>%
     mutate(hydro_year=hyd.yr(dt, h = "S")) %>%
     group_by(hydro_year) %>%
     summarize(ann.min.flow=min(flow_m3s,na.rm=TRUE),ann.mean.flow=mean(flow_m3s,na.rm=TRUE),ann.max.flow=max(flow_m3s,na.rm=TRUE)) %>%
     mutate(annual.tot=ann.mean.flow*3600*24*365.25) #%>%
     #na_if(Inf) %>%
     #na_if(-Inf) %>%
     #na_if(NaN)
# it appears that the warnings, length > 1, non-missing arguments, are taken care of.

# Annual flow time series
# Average annual flow
ave.ann.flow <- 365.25 * 24 * 3600 * mean(m$mon.mean.flow)
# Found the annual mean flow to be 1,689,908,081 m^3, that is, 1.7x10^9 m^3

modBeitbridge <- lm(a$annual.tot ~ a$hydro_year)
# Coefficients:
#                     Estimate Std. Error t value Pr(>|t|)   
#      (Intercept)   1.748e+11  5.308e+10   3.293  0.00168 **
#      a$hydro_year -8.566e+07  2.669e+07  -3.210  0.00215 **
#      ---
#      Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#                         2.5 %       97.5 %
#      (Intercept)  68580428714 280986119165
#      a$hydro_year  -139060814    -32260553

ggplot(a) + #   ann_flow <- 
     geom_point(aes(x=hydro_year,y=(annual.tot/1e9))) + # data in cumecs, display in trillions of cumecs
     #geom_hline(yintercept = ave.ann.flow) +
     geom_smooth(aes(x=hydro_year,y=(annual.tot/1e9)), method = "lm", se = TRUE, color='blue') +
     xlim(c(1955,2022)) +
     ylim(c(0,30)) +
     #coord_cartesian(ylim=c(0,10000000000)) + # control y lim here!
     xlab("Hydrologic Year") + 
     ylab(TeX('Annual Total Discharge $(\\times 10^{9} m^3/y)$')) + 
     theme(panel.background = element_rect(fill = "white", colour = "black")) + 
     theme(legend.position="right") + 
     theme(aspect.ratio = 1) +
     theme(axis.text = element_text(face = "plain", size = 14)) +
     theme(axis.title = element_text(face = "plain", size = 14))
ggsave("annual_flow_tall.eps", ann_flow, device = "eps", dpi = 72)

## Annual flood
ann_flood <- ggplot(a) +
     geom_col(aes(x=hydro_year,y=ann.max.flow)) +
     xlim(c(1955,2022)) +
     ylim(c(0,10000)) +
     xlab("Hydrologic Year") + 
     ylab(TeX('Annual Maximum Discharge $(m^3/s)$')) + 
     theme(panel.background = element_rect(fill = "white", colour = "black")) + 
     theme(legend.position="right") + 
     theme(aspect.ratio = 1) +
     theme(axis.text = element_text(face = "plain", size = 12))
ggsave("annual_flood.eps", ann_flood, device = "eps", dpi = 72)

ann_flood_hist <- ggplot(a,aes(x=ann.max.flow)) +
     geom_histogram(breaks = (500*c(0:20)), color = "black", fill = "gray", na.rm = TRUE) +
     xlab(TeX('Annual Maximum Discharge $(m^3/s)$')) +
     ylab("Years") +
     theme(panel.background = element_rect(fill = "white", colour = "black")) + 
     theme(legend.position="right") + 
     theme(aspect.ratio = 1) +
     theme(axis.text = element_text(face = "plain", size = 12))
ggsave("annual_flood_hist.eps", ann_flood_hist, device = "eps", dpi = 72)

# 200 year flood
a$log_q <- log(a$ann.max.flow, base = 10) # log transform
T_R <- 200 # return period in years (in this case)
Fx <- 1 - (1/T_R)
mu <- mean(a$log_q, na.rm = TRUE)
s <- sd(a$log_q, na.rm = TRUE)
c <- skew(a$log_q) # NA removed by default
lp3.x <- pt3(c,Fx)
lp3.y <- (lp3.x * s) + mu
lp3.z <- 10^lp3.y # this is the T_R flood level 200-year flood is 13,667 m^3/s

ann_drought <- ggplot(a) +
     geom_col(aes(x=hydro_year,y=ann.min.flow)) +
     xlim(c(1955,2022)) +
     ylim(c(0,10)) +
     xlab("Hydrologic Year") + 
     ylab(TeX('Annual Minimum Discharge $(m^3/s)$')) + 
     theme(panel.background = element_rect(fill = "white", colour = "black")) + 
     theme(legend.position="right") + 
     theme(aspect.ratio = 1) +
     theme(axis.text = element_text(face = "plain", size = 12))
ggsave("annual_drought.eps", ann_drought, device = "eps", dpi = 72)

ann_drought_hist <- ggplot(a,aes(x=ann.min.flow)) +
     geom_histogram(breaks = (c(0:10)), color = "black", fill = "gray", na.rm = TRUE) +
     xlab(TeX('Annual Minimum Discharge $(m^3/s)$')) +
     ylab("Years") +
     theme(panel.background = element_rect(fill = "white", colour = "black")) + 
     theme(legend.position="right") + 
     theme(aspect.ratio = 1) +
     theme(axis.text = element_text(face = "plain", size = 12))
ggsave("annual_drought_hist.eps", ann_drought_hist, device = "eps", dpi = 72)

## Monthly averages
m <- q %>%
     mutate(mon=month(dt)) %>%
     group_by(mon) %>%
     summarize(mon.mean.flow=mean(flow_m3s,na.rm=TRUE),mon.sd.flow=stdev(flow_m3s),mon.med.flow=median(flow_m3s,na.rm=TRUE)) %>%
     na_if(Inf) %>%
     na_if(-Inf) %>%
     na_if(NaN)
m$label <- c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")
m$label <- factor(m$label, levels = c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"))

mon_mean <- ggplot(m) +
     geom_col(aes(x=label,y=mon.mean.flow)) +
     ylim(c(0,200)) +
     xlab("Month") + 
     ylab(TeX('Monthly Mean Discharge $(m^3/s)$')) + 
     theme(panel.background = element_rect(fill = "white", colour = "black")) + 
     theme(legend.position="right") + 
     theme(aspect.ratio = 1) +
     theme(axis.text = element_text(face = "plain", size = 12))
ggsave("mon_mean.eps", mon_mean, device = "eps", dpi = 72)

mon_median <- ggplot(m) +
     geom_col(aes(x=label,y=mon.med.flow)) +
     ylim(c(0,100)) +
     xlab("Month") + 
     ylab(TeX('Monthly Median Discharge $(m^3/s)$')) + 
     theme(panel.background = element_rect(fill = "white", colour = "black")) + 
     theme(legend.position="right") + 
     theme(aspect.ratio = 1) +
     theme(axis.text = element_text(face = "plain", size = 12))
ggsave("mon_med.eps", mon_median, device = "eps", dpi = 72)






