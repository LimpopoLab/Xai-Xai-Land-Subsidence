library(readr)
library(dplyr)
library(ggplot2)

allYears <- read_csv("/Users/davidkahler/Documents/Hydrology_and_WRM/Limpopo_Basin_Study/Land_Movement_Gabi/Results_AllPixels/AllYears_FinalResults.csv")
r2017 <- read_csv("/Users/davidkahler/Documents/Hydrology_and_WRM/Limpopo_Basin_Study/Land_Movement_Gabi/Results_AllPixels/2017_AllPixelsInRV.csv")
r2017 <- r2017 %>%
     select(VEL,V_STDEV,Vel_cm_yr) %>%
     mutate(year="2017")
r2018 <- read_csv("/Users/davidkahler/Documents/Hydrology_and_WRM/Limpopo_Basin_Study/Land_Movement_Gabi/Results_AllPixels/2018_AllPixelsInRV.csv")
r2018 <- r2018 %>%
     select(VEL,V_STDEV,Vel_cm_yr) %>%
     mutate(year="2018")
r2019 <- read_csv("/Users/davidkahler/Documents/Hydrology_and_WRM/Limpopo_Basin_Study/Land_Movement_Gabi/Results_AllPixels/2019_AllPixelsInRV.csv")
r2019 <- r2019 %>%
     select(VEL,V_STDEV,Vel_cm_yr) %>%
     mutate(year="2019")
r2020 <- read_csv("/Users/davidkahler/Documents/Hydrology_and_WRM/Limpopo_Basin_Study/Land_Movement_Gabi/Results_AllPixels/2020_AllPixelsInRV.csv")
r2020 <- r2020 %>%
     select(VEL,V_STDEV,Vel_cm_yr) %>%
     mutate(year="2020")
r2021 <- read_csv("/Users/davidkahler/Documents/Hydrology_and_WRM/Limpopo_Basin_Study/Land_Movement_Gabi/Results_AllPixels/2021_AllPixelsInRV.csv")
r2021 <- r2021 %>%
     select(VEL,V_STDEV,Vel_cm_yr) %>%
     mutate(year="2021")
r <- rbind(r2017,r2018,r2019,r2020,r2021)

ggplot(r) +
     geom_boxplot(aes(year,VEL)) +
     xlab("Dry Season Year") +
     ylab("Land Movement (mm/a)") +
     theme(panel.background = element_rect(fill = "white", colour = "black")) + 
     theme(aspect.ratio = 1) +
     theme(axis.text = element_text(face = "plain", size = 14)) +
     theme(axis.title = element_text(face = "plain", size = 14))

library(ggplot2)
library(formattable)

DrySea17 <- na.omit(allYears$Dry_Season_2017)
DrySea18 <- na.omit(allYears$Dry_Season_2018)
DrySea19 <- na.omit(allYears$Dry_Season_2019)
DrySea20 <- na.omit(allYears$Dry_Season_2020)
DrySea21 <- na.omit(allYears$Dry_Season_2021)

Mean17 <- formattable((mean(DrySea17)),format="f", digits=3)
Mean18 <- formattable((mean(DrySea18)),format="f", digits=3)
Mean19 <- formattable((mean(DrySea19)),format="f", digits=3)
Mean20 <- formattable((mean(DrySea20)),format="f", digits=3)
Mean21 <- formattable((mean(DrySea21)),format="f", digits=3)

Median17 <- formattable((median(DrySea17)),format="f", digits=3)
Median18 <- formattable((median(DrySea18)),format="f", digits=3)
Median19 <- formattable((median(DrySea19)),format="f", digits=3)
Median20 <- formattable((median(DrySea20)),format="f", digits=3)
Median21 <- formattable((median(DrySea21)),format="f", digits=3)

vlines17 <- data.frame("Statistics" = c("Mean", "Median"), 
                       "Value" = c(mean(DrySea17), median(DrySea17)))
vlines18 <- data.frame("Statistics" = c("Mean", "Median"), 
                       "Value" = c(mean(DrySea18), median(DrySea18)))
vlines19 <- data.frame("Statistics" = c("Mean", "Median"), 
                       "Value" = c(mean(DrySea19), median(DrySea19)))
vlines20 <- data.frame("Statistics" = c("Mean", "Median"), 
                       "Value" = c(mean(DrySea20), median(DrySea20)))
vlines21 <- data.frame("Statistics" = c("Mean", "Median"), 
                       "Value" = c(mean(DrySea21), median(DrySea21)))

##################################################################################################################

plot_drysea17 <- ggplot(data = allYears, aes(x = Dry_Season_2017)) + geom_histogram(bins = 150, na.rm = TRUE,
                                                                                     fill = "light gray",
                                                                                     color = "black") +
     theme_classic() +
     ggtitle("Dry Season 2017") +
     geom_vline(aes(xintercept = Value, color = Statistics),
                size=1, show.legend=TRUE, data = vlines17) + 
     theme(legend.position = "right") +
     theme(legend.key.size = unit(1, 'cm')) +
     theme(legend.title = element_text(size=14)) +
     theme(legend.text = element_text(size =14)) +
     theme(axis.text.x = element_text(size=14)) +
     theme(axis.text.y = element_text(size=14)) +
     theme(axis.title.x = element_text(size=14)) +
     theme(axis.title.y = element_text(size=14)) +
     theme(title = element_text(size=14)) +
     xlim(-20,10) +
     ylim(0,4800)+
     xlab("Velocity in LOS Direction (cm/year)") +
     ylab("Count") +
     annotate("text",
              x = 6,
              y = 3000, 
              label = paste("Mean =", Mean17), 
              col = "#FC717F", 
              size = 4) +
     annotate("text",
              x = 6,
              y = 2800, 
              label = paste("Median =", Median17), 
              col = "#339999", 
              size = 4) 

################################################################################################################

plot_drysea18 <- ggplot(data = allYears, aes(x = Dry_Season_2018)) + geom_histogram(bins = 150, na.rm = TRUE,
                                                                                     fill = "light gray",
                                                                                     color = "black") +
     theme_classic() +
     ggtitle("Dry Season 2018") +
     geom_vline(aes(xintercept = Value, color = Statistics),
                size=1, show.legend=TRUE, data = vlines18) + 
     theme(legend.position = "right") +
     theme(legend.key.size = unit(1, 'cm')) +
     theme(legend.title = element_text(size=12)) +
     theme(legend.text = element_text(size =12)) +
     theme(axis.text.x = element_text(size=12)) +
     theme(axis.text.y = element_text(size=12)) +
     theme(axis.title.x = element_text(size=12)) +
     theme(axis.title.y = element_text(size=12)) +
     theme(title = element_text(size=12)) +
     xlim(-20,10) +
     ylim(0,4800)+
     xlab("Velocity in LOS Direction (cm/year)") +
     ylab("Count") +
     annotate("text",
              x = 6,
              y = 3000, 
              label = paste("Mean =", Mean18), 
              col = "#FC717F", 
              size = 4) +
     annotate("text",
              x = 6,
              y = 2800, 
              label = paste("Median =", Median18), 
              col = "#339999", 
              size = 4) 

#################################################################################

plot_drysea19 <- ggplot(data = allYears, aes(x = Dry_Season_2019)) + geom_histogram(bins = 150, na.rm = TRUE,
                                                                                     fill = "light gray",
                                                                                     color = "black") +
     theme_classic() +
     ggtitle("Dry Season 2019") +
     geom_vline(aes(xintercept = Value, color = Statistics),
                size=1, show.legend=TRUE, data = vlines19) + 
     theme(legend.position = "right") +
     theme(legend.key.size = unit(1, 'cm')) +
     theme(legend.title = element_text(size=12)) +
     theme(legend.text = element_text(size =12)) +
     theme(axis.text.x = element_text(size=12)) +
     theme(axis.text.y = element_text(size=12)) +
     theme(axis.title.x = element_text(size=12)) +
     theme(axis.title.y = element_text(size=12)) +
     theme(title = element_text(size=12)) +
     xlim(-20,10) +
     ylim(0,4800)+
     xlab("Velocity in LOS Direction (cm/year)") +
     ylab("Count") +
     annotate("text",
              x = 6,
              y = 3000, 
              label = paste("Mean =", Mean19), 
              col = "#FC717F", 
              size = 4) +
     annotate("text",
              x = 6,
              y = 2800, 
              label = paste("Median =", Median19), 
              col = "#339999", 
              size = 4) 

##################################################################################

plot_drysea20 <- ggplot(data = allYears, aes(x = Dry_Season_2020)) + geom_histogram(bins = 150, na.rm = TRUE,
                                                                                     fill = "light gray",
                                                                                     color = "black") +
     theme_classic() +
     ggtitle("Dry Season 2020") +
     geom_vline(aes(xintercept = Value, color = Statistics),
                size=1, show.legend=TRUE, data = vlines20) + 
     theme(legend.position = "right") +
     theme(legend.key.size = unit(1, 'cm')) +
     theme(legend.title = element_text(size=12)) +
     theme(legend.text = element_text(size =12)) +
     theme(axis.text.x = element_text(size=12)) +
     theme(axis.text.y = element_text(size=12)) +
     theme(axis.title.x = element_text(size=12)) +
     theme(axis.title.y = element_text(size=12)) +
     theme(title = element_text(size=12)) +
     xlim(-20,10) +
     ylim(0,4800)+
     xlab("Velocity in LOS Direction (cm/year)") +
     ylab("Count") +
     annotate("text",
              x = 6,
              y = 3000, 
              label = paste("Mean =", Mean20), 
              col = "#FC717F", 
              size = 4) +
     annotate("text",
              x = 6,
              y = 2800, 
              label = paste("Median =", Median20), 
              col = "#339999", 
              size = 4) 

##################################################################################

plot_drysea21 <- ggplot(data = allYears, aes(x = Dry_Season_2021)) + geom_histogram(bins = 150, na.rm = TRUE,
                                                                                     fill = "light gray",
                                                                                     color = "black") +
     theme_classic() +
     ggtitle("Dry Season 2021") +
     geom_vline(aes(xintercept = Value, color = Statistics),
                size=1, show.legend=TRUE, data = vlines21) + 
     theme(legend.position = "right") +
     theme(legend.key.size = unit(1, 'cm')) +
     theme(legend.title = element_text(size=12)) +
     theme(legend.text = element_text(size =12)) +
     theme(axis.text.x = element_text(size=12)) +
     theme(axis.text.y = element_text(size=12)) +
     theme(axis.title.x = element_text(size=12)) +
     theme(axis.title.y = element_text(size=12)) +
     theme(title = element_text(size=12)) +
     xlim(-20,10) +
     ylim(0,4800)+
     xlab("Velocity in LOS Direction (cm/year)") +
     ylab("Count") +
     annotate("text",
              x = 6,
              y = 3000, 
              label = paste("Mean =", Mean21), 
              col = "#FC717F", 
              size = 4) +
     annotate("text",
              x = 6,
              y = 2800, 
              label = paste("Median =", Median21), 
              col = "#339999", 
              size = 4) 


ggsave("/Users/davidkahler/Documents/Hydrology_and_WRM/Limpopo_Basin_Study/Land_Movement_Gabi/manuscript/figS06_2017.eps", plot_drysea17, device = "eps", dpi = 72)
ggsave("/Users/davidkahler/Documents/Hydrology_and_WRM/Limpopo_Basin_Study/Land_Movement_Gabi/manuscript/figS08_2018.eps", plot_drysea18, device = "eps", dpi = 72)
ggsave("/Users/davidkahler/Documents/Hydrology_and_WRM/Limpopo_Basin_Study/Land_Movement_Gabi/manuscript/figS10_2019.eps", plot_drysea19, device = "eps", dpi = 72)
ggsave("/Users/davidkahler/Documents/Hydrology_and_WRM/Limpopo_Basin_Study/Land_Movement_Gabi/manuscript/figS12_2020.eps", plot_drysea20, device = "eps", dpi = 72)
ggsave("/Users/davidkahler/Documents/Hydrology_and_WRM/Limpopo_Basin_Study/Land_Movement_Gabi/manuscript/figS14_2021.eps", plot_drysea21, device = "eps", dpi = 72)








