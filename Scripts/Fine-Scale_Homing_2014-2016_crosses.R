#This script is used to examine the all_streams_parents_paired_14_16_cross.csv file
#This file contains offspring (2016) with BOTH parents assigned from 2014.
#Used to examine the correlation between spawning locations of known parent pairs



#Set working directiory to source file location

library(tidyverse)
library(glmmTMB)
#install_version("MuMIn",version = "1.43.17")
library(MuMIn)
library(lme4)
library(sjPlot)
library(viridis)
library(scales)

###JUST THE PO CROSS DATA

PWS_cross_data<-read.csv("../Data/all_streams_parents_paired_14_16_cross.csv") %>% filter(stream_off!="Paddy")
colorz<-viridis::plasma(5,end = 0.8)

PWS_cross_data %>% group_by(stream_off) %>% 
  dplyr::summarize(n=n(),non_na_sire = sum(!is.na(parent_id_sire)),non_na_dam = sum(!is.na(parent_id_dam))) %>% 
  ungroup() %>% dplyr::summarize(sum(non_na_sire))


model_data<-PWS_cross_data %>% filter(origin_off=="Natural") %>% 
  dplyr::select(offspring_id,parent_id_dam,parent_id_sire,stream_off,sex_off,DOY_off,length_off,distance_mouth_off,
                                             intertidal_off,origin_off,distance_mouth_dam,
                                             distance_mouth_sire,intertidal_dam,intertidal_sire) %>% 
  group_by(stream_off) %>% #mutate_at(.vars=vars(DOY_off,length_off,distance_mouth_upstream_parent),scale) %>% 
  drop_na(distance_mouth_dam,distance_mouth_sire,stream_off)

model_data %>% group_by(stream_off) %>% 
  dplyr::summarize(n=n(),non_na_sire = sum(!is.na(parent_id_sire)),non_na_dam = sum(!is.na(parent_id_dam))) %>% 
  ungroup() %>% dplyr::summarize(sum(non_na_sire))


##Boxplots:
#Scale of Mate Correlation Boxplots:

dam_sire_boxplots <- model_data %>% mutate(mate_distance = distance_mouth_dam - distance_mouth_sire) %>%  group_by(stream_off) %>% 
  ggplot(aes(x = 1, y = mate_distance, fill = stream_off))+
  geom_hline(yintercept = 0, lty = 2)+
  geom_boxplot(aes(color= stream_off),alpha= 0.6,outlier.shape = NA, show.legend = F)+
  geom_jitter(aes(color= stream_off),alpha = 0.05, size = 0.75, show.legend = F,height = 0)+
  scale_fill_discrete(type = colorz)+
  scale_color_discrete(type = colorz)+
  scale_y_continuous(breaks = c(-2000,-1500,-1000,-500,-100,100,500,1000,1500,2000)) + 
  theme_classic()+
  labs(y = "Distance Between Mates (river meter)\n(Dam - Sire)", x = "Stream", color = "Stream", fill = "Stream")+
  facet_grid(stream_off~.)+
  theme(axis.title.x = element_blank(),axis.text.x = element_blank(),
        axis.ticks.x = element_blank())

model_data %>% mutate(mate_distance = distance_mouth_dam - distance_mouth_sire) %>% group_by(stream_off) %>% 
  dplyr::summarise(mean = mean(mate_distance), sd = sd(mate_distance), median = median(mate_distance))

model_data %>% mutate(mate_distance = distance_mouth_dam - distance_mouth_sire) %>% group_by(sex_off) %>% 
  dplyr::summarise(mean = mean(mate_distance), sd = sd(mate_distance), median = median(mate_distance))


DS_distance_global<-lmer(distance_mouth_sire~distance_mouth_dam+(1|stream_off),
                         data=model_data,na.action="na.fail")
DS_distance_dredge<-dredge(DS_distance_global)

DS_AIC<-as.data.frame(DS_distance_dredge)[3:7]
DS_AIC[,2:5]<-round(DS_AIC[,2:5],2)
names(DS_AIC[1])<-"K"
DS_AIC$Model<-rownames(DS_AIC)
for(i in 1:nrow(DS_AIC)) DS_AIC$Model[i]<- as.character(formula(get.models(DS_distance_dredge,subset = T)[[i]]))[3]
#write.csv(DS_AIC,"../Figures/Dam_Sire_AIC_table.csv")

DS_distance_best<-DS_distance_global
summary(DS_distance_best)

DS_distance_pred<-plot_model(DS_distance_best,type="pred")

dam_sire_glm<-PWS_cross_data %>% ggplot(aes(y=distance_mouth_sire,x=distance_mouth_dam))+
  geom_jitter(aes(color=stream_off),alpha=0.3,height=10,width=10,size=0.8)+
  geom_line(data=data.frame(distance_mouth_dam=c(0,1100),distance_mouth_sire=c(0,1100)),color="red",lty=2,lwd=0.8)+
  guides(colour = guide_legend(override.aes = list(alpha=1)))+
  geom_ribbon(data=DS_distance_pred$distance_mouth_dam$data,
              aes(x=x,ymin=conf.low,ymax=conf.high),inherit.aes=F,alpha=0.3)+
  geom_line(data=DS_distance_pred$distance_mouth_dam$data,aes(x=x,y=predicted))+
  theme_classic()+
  theme(legend.position = "none")+
  labs(y="Inferred Spawning Location of Sires\n(river meter)",
       x="Inferred Spawning Location of Dams\n(river meter)",
       color = "Stream")+
  scale_color_discrete(type=colorz)#+
  #guides(colour = guide_legend(override.aes = list(alpha=1,size=2)))

ggpubr::ggarrange(dam_sire_glm,dam_sire_boxplots,widths=c(2.5,1),align = "v")


####Stream-specific slopes
DS_distance_global<-lm(distance_mouth_sire~distance_mouth_dam * stream_off,
                         data=model_data,na.action="na.fail")
DS_distance_dredge<-dredge(DS_distance_global)

DS_AIC<-as.data.frame(DS_distance_dredge)[5:9]
DS_AIC[,2:5]<-round(DS_AIC[,2:5],2)
names(DS_AIC[1])<-"K"
DS_AIC$Model<-rownames(DS_AIC)
for(i in 1:nrow(DS_AIC)) DS_AIC$Model[i]<- as.character(formula(get.models(DS_distance_dredge,subset = T)[[i]]))[3]
#write.csv(DS_AIC,"../Figures/Dam_Sire_AIC_table.csv")

DS_distance_best<-DS_distance_global
summary(DS_distance_best)

as.data.frame(model_data %>% group_by(stream_off) %>% dplyr::summarise(max = max(distance_mouth_dam)))
DS_distance_pred<-as.data.frame(model_data %>% group_by(stream_off) %>% dplyr::summarise(distance_mouth_dam = seq(min(distance_mouth_sire),max(distance_mouth_sire),length.out=1000)))
DS_distance_pred$distance_mouth_sire <- predict(DS_distance_best,newdata = DS_distance_pred)
DS_distance_pred$se.fit <- predict(DS_distance_best,newdata = DS_distance_pred,se.fit = T)$se.fit


dam_sire_glm<-PWS_cross_data %>% ggplot(aes(y=distance_mouth_sire,x=distance_mouth_dam))+
  geom_jitter(aes(color=stream_off),alpha=0.3,height=10,width=10,size=0.8)+
  #geom_line(data=data.frame(distance_mouth_dam=c(0,1100),distance_mouth_sire=c(0,1100)),lty=2,lwd=0.5)+
  guides(colour = guide_legend(override.aes = list(alpha=1)))+
  geom_ribbon(data=DS_distance_pred,
              aes(x=distance_mouth_dam,ymin=distance_mouth_sire-se.fit,ymax=distance_mouth_sire+se.fit,
                  fill=stream_off,color=stream_off),inherit.aes=F,alpha=0.3)+
  geom_line(data=DS_distance_pred,aes(color=stream_off))+
  theme_classic()+
  theme(legend.position = "none")+
  labs(y="Inferred Spawning Location of Sires\n(river meter)",
       x="Inferred Spawning Location of Dams\n(river meter)",
       color = "Stream")+
  scale_color_discrete(type=colorz)+
  scale_fill_discrete(type=colorz)+
guides(colour = guide_legend(override.aes = list(alpha=1,size=2)))+
  facet_grid(stream_off~.)+
  theme(strip.background = element_blank(),strip.text=element_blank())
dam_sire_glm




dam_sire_boxplots <- model_data %>% mutate(mate_distance = distance_mouth_dam - distance_mouth_sire) %>%  group_by(stream_off) %>% 
  ggplot(aes(x = 1, y = mate_distance, fill = stream_off))+
  geom_hline(yintercept = 0, lty = 2)+
  geom_boxplot(aes(color= stream_off),alpha= 0.6,outlier.shape = NA, show.legend = F)+
  geom_jitter(aes(color= stream_off),alpha = 0.05, size = 0.75, show.legend = F,height = 0)+
  scale_fill_discrete(type = colorz)+
  scale_color_discrete(type = colorz)+
  scale_y_continuous(breaks = c(-2000,-1500,-1000,-500,-100,100,500,1000,1500,2000)) + 
  theme_classic()+
  labs(y = "Distance Between Mates (river meter)\n(Dam - Sire)", x = "Stream", color = "Stream", fill = "Stream")+
  facet_grid(stream_off~.)+
  theme(axis.text.x = element_blank(),axis.title.x = element_blank(),
        axis.ticks.x = element_blank())


ggpubr::ggarrange(dam_sire_glm,dam_sire_boxplots,widths=c(2,1),align = "h")





###horizontal:
mysqrt_trans <- function() {
  trans_new("mysqrt", 
            transform = base::sqrt,
            inverse = function(x) ifelse(x<0, 0, x^2),
            domain = c(0, Inf))
}

dam_sire_glm<-model_data %>% ggplot(aes(y=distance_mouth_sire,x=distance_mouth_dam))+
  geom_jitter(aes(color=stream_off),alpha=0.15,height=10,width=10,size=0.8)+
  geom_abline(slope=1,lty = 2)+
  guides(colour = guide_legend(override.aes = list(alpha=1)))+
  geom_ribbon(data=DS_distance_pred,
              aes(x=distance_mouth_dam,ymin=distance_mouth_sire-se.fit,ymax=distance_mouth_sire+se.fit,
                  fill=stream_off,color=stream_off),inherit.aes=F,alpha=0.3)+
  geom_line(data=DS_distance_pred,aes(color=stream_off))+
  theme_classic()+
  theme(legend.position = "none")+
  labs(y="Inferred Spawning Location\nof Sires (river meter)",
       x="Inferred Spawning Location of Dams (river meter)",
       color = "Stream")+
  scale_color_discrete(type=colorz)+
  scale_fill_discrete(type=colorz)+
  guides(colour = guide_legend(override.aes = list(alpha=1,size=2)))+
  facet_grid(.~stream_off)+
  theme(strip.background = element_blank(),strip.text=element_blank())+
  scale_x_continuous(breaks = c(0,300,600,900))+
  expand_limits(x = 0)
dam_sire_glm



dam_sire_boxplots <- model_data %>% mutate(mate_distance = distance_mouth_dam - distance_mouth_sire) %>%  group_by(stream_off) %>% 
  ggplot(aes(x = 1, y = mate_distance, fill = stream_off))+
  geom_hline(yintercept = 0, lty = 2)+
  geom_boxplot(aes(color= stream_off),alpha= 0.6,outlier.shape = NA, show.legend = F,width = 0.4)+
  geom_jitter(aes(color= stream_off),alpha = 0.15, size = 0.8, show.legend = F,height = 0)+
  scale_fill_discrete(type = colorz)+
  scale_color_discrete(type = colorz)+
  scale_y_continuous(breaks = c(-200-100,100,500)) + 
  theme_classic()+
  labs(y = "Distance Between\nMates (Dam - Sire)", x = "Stream", color = "Stream", fill = "Stream")+
  facet_grid(.~stream_off)+
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank())


ggpubr::ggarrange(dam_sire_boxplots,dam_sire_glm,nrow = 2,heights=c(1,1),align = "v")

