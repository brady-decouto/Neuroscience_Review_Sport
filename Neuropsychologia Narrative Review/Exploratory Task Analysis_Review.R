library("ggplot2"); library("lme4"); 
library("dplyr"); 
library("lmerTest"); library("tidyr");
library("Rcpp"); library("sjstats"); library("pwr");
library("MuMIn"); library("stringr"); library("ggpattern")
rm(list = ls())

setwd("~/Library/CloudStorage/OneDrive-FloridaStateUniversity/Papers/Neuropsychologia/Neuroscience Review/Plotting/Soccer")
Alpha_Hem_S<-read.csv("./Alpha_Hem.csv", header = TRUE, sep=",",  
                    na.strings=c("NA","NaN"," ",""))
Beta_Hem_S<-read.csv("./Beta_Hem.csv", header = TRUE, sep=",",  
                   na.strings=c("NA","NaN"," ",""))

setwd("~/Library/CloudStorage/OneDrive-FloridaStateUniversity/Papers/Neuropsychologia/Neuroscience Review/Plotting/Volleyball")
Alpha_Hem_V<-read.csv("./Alpha_Hem.csv", header = TRUE, sep=",",  
                      na.strings=c("NA","NaN"," ",""))
Beta_Hem_V<-read.csv("./Beta_Hem.csv", header = TRUE, sep=",",  
                     na.strings=c("NA","NaN"," ",""))



Volleyball = do.call("list", mget(grep("_V", ls(), value=T)))
Soccer = do.call("list", mget(grep("S", ls(), value=T)))


Soccer <- lapply(Soccer, function(x) x %>%
                   mutate(Task = "S") %>%
                   rename(P_Avg = "P_Avg_Hem")%>%
                   select(subID, Task, Condition, Hemisphere, Epoch, Skill, P_Avg) %>%
                   filter(Epoch == "E2") %>%
                   filter(!(subID == "P05" | subID == "P27" | subID == "P06"))%>%
                   group_by(subID, Hemisphere, Task, Skill) %>%
                   summarise(P_Avg = mean(P_Avg))%>%
                   mutate(subID = as.character(str_replace(subID, 'P', 'S'))))

Volleyball <- lapply(Volleyball, function(x) x %>%
                       select(subID, Task, Condition, Hemisphere, Skill, P_Avg) %>%
                       filter(Task == "V") %>%
                       group_by(subID, Hemisphere, Task, Skill) %>%
                       summarise(P_Avg = mean(P_Avg)) %>%
                       mutate(subID = as.character(str_replace(subID, 'P', 'V'))))
                       

list2env(Soccer,globalenv())
list2env(Volleyball,globalenv())
Alpha <- rbind(Alpha_Hem_S, Alpha_Hem_V)
Beta <- rbind(Beta_Hem_S, Beta_Hem_V)

rm(list=ls()[! ls() %in% c("Alpha","Beta", "Rel")])



#plot-------------------------
Alpha1 <- mutate(Alpha, Variable = "Alpha (8-13Hz)")
Beta1 <- mutate(Beta, Variable = "Beta (14-30Hz)")
Graph <- rbind(Alpha1, Beta1)



levels(Graph$Hemisphere)
Graph$Variable <- as.factor(Graph$Variable)
Graph$Hemisphere <- as.factor(Graph$Hemisphere)
Graph$Task <- as.factor(Graph$Task)
Graph$Skill <- as.factor(Graph$Skill)


levels(Graph$Task)[levels(Graph$Task)=="S"] <- "Soccer"
levels(Graph$Task)[levels(Graph$Task)=="V"] <- "Volleyball"
levels(Graph$Skill)[levels(Graph$Skill)=="LS"] <- "Less Skilled"
levels(Graph$Skill)[levels(Graph$Skill)=="S"] <- "Skilled"
levels(Graph$Skill)[levels(Graph$Skill)=="N"] <- "Less Skilled"
levels(Graph$Skill)[levels(Graph$Skill)=="E"] <- "Skilled"


Plot <- Graph %>%
  group_by(subID, Task, Hemisphere, Variable, Skill) %>%
  summarise(P_Avg = mean(P_Avg))

ggplot((Plot)) +
  geom_boxplot(aes(Variable, P_Avg, fill = Hemisphere, middle = median(P_Avg)),
               outlier.shape = NA) +
  geom_jitter(aes(Variable, P_Avg, fill = Hemisphere), 
              position = position_jitterdodge(jitter.width = 0.2, dodge.width = 0.75), alpha = 0.8, shape = 21, colour = "black")+  
  geom_line(aes(Variable, P_Avg, group = subID), 
             position = position_dodge(width = .75), alpha = .3, color = "black") +
  ylab(expression(Parietal~Power~(µV^{"2"}))) + 
  facet_grid(Task ~ Skill) +
  scale_y_continuous(breaks = c(-1,  -.5,  0,  .5, 1)) +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(axis.text = element_text(size = 15, colour = "black"),
        strip.text = element_text(size = 16)) + 
  theme(axis.title.y = element_text(margin = margin(t = 0, r = 15, b = 0, l = 5),
                                    size = 18, face = "bold")) +
  theme(axis.title.x = element_text(margin = margin(t = 35, b = 0),
                                    size = 22, face = "bold")) +
  theme(legend.position = "bottom") +
  theme(legend.text = element_text(size = 14)) +
  theme(legend.title = element_text(size = 14, face = "bold")) +
  theme(legend.key.size = unit(1, 'cm')) +
  scale_fill_manual(values = c("#77AADD", "#44BB99"))

levels(Plot$Skill)

