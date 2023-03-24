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

###JUST THE PO CROSS DATA

PWS_cross_data<-read.csv("../Data/all_streams_parents_paired_14_16_cross.csv")
colorz<-viridis::plasma(5,end = 0.8)

PWS_cross_data %>% group_by(stream_off) %>% 
  summarize(n=n(),non_na_sire = sum(!is.na(parent_id_sire)),non_na_dam = sum(!is.na(parent_id_dam))) %>% 
  ungroup() %>% summarize(sum(non_na_sire))


model_data<-PWS_cross_data %>% filter(origin_off=="Natural") %>% 
  dplyr::select(offspring_id,parent_id_dam,parent_id_sire,stream_off,sex_off,DOY_off,length_off,distance_mouth_off,
                                             intertidal_off,origin_off,distance_mouth_dam,
                                             distance_mouth_sire,intertidal_dam,intertidal_sire) %>% 
  group_by(stream_off) %>% #mutate_at(.vars=vars(DOY_off,length_off,distance_mouth_upstream_parent),scale) %>% 
  drop_na(distance_mouth_dam,distance_mouth_sire,stream_off)

model_data %>% group_by(stream_off) %>% 
  summarize(n=n(),non_na_sire = sum(!is.na(parent_id_sire)),non_na_dam = sum(!is.na(parent_id_dam))) %>% 
  ungroup() %>% summarize(sum(non_na_sire))


DS_distance_global<-lmer(distance_mouth_sire~distance_mouth_dam+(1|stream_off),
                         data=model_data,na.action="na.fail")
DS_distance_dredge<-dredge(DS_distance_global)

DS_AIC<-as.data.frame(DS_distance_dredge)[3:7]
DS_AIC[,2:5]<-round(DS_AIC[,2:5],2)
names(DS_AIC[1])<-"K"
DS_AIC$Model<-rownames(DS_AIC)
for(i in 1:nrow(DS_AIC)) DS_AIC$Model[i]<- as.character(formula(get.models(DS_distance_dredge,subset = T)[[i]]))[3]
write.csv(DS_AIC,"../Figures/Dam_Sire_AIC_table.csv")

DS_distance_best<-DS_distance_global
summary(DS_distance_best)

DS_distance_pred<-plot_model(DS_distance_best,type="pred")

PWS_cross_data %>% ggplot(aes(y=distance_mouth_sire,x=distance_mouth_dam))+
  geom_jitter(aes(color=stream_off),alpha=0.3,height=10,width=10,size=0.8)+
  geom_line(data=data.frame(distance_mouth_dam=c(0,1100),distance_mouth_sire=c(0,1100)),color="red",lty=2,lwd=0.8)+
  guides(colour = guide_legend(override.aes = list(alpha=1)))+
  geom_ribbon(data=DS_distance_pred$distance_mouth_dam$data,
              aes(x=x,ymin=conf.low,ymax=conf.high),inherit.aes=F,alpha=0.3)+
  geom_line(data=DS_distance_pred$distance_mouth_dam$data,aes(x=x,y=predicted))+
  theme_classic()+
  labs(y="Inferred Spawning Location of Sires\n(river meter)",
       x="Inferred Spawning Location of Dams\n(river meter)",
       color = "Stream")+
  scale_color_discrete(type=colorz)+
  guides(colour = guide_legend(override.aes = list(alpha=1,size=2)))



