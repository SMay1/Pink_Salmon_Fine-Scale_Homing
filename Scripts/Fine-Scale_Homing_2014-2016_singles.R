#This script is used to examine the all_streams_parents_paired_14_16.csv file
#which contains offspring from 2016 with either one or two parents assigned
#and is used to examine the correlation between offspring and parent spawning locations


setwd("H:/My Drive/PWS_Empirical/Scripts")

library(tidyverse)
library(glmmTMB)
#install_version("MuMIn",version = "1.43.17")
library(MuMIn)
library(lme4)
library(sjPlot)
library(viridis)

###JUST THE PO single DATA

PWS_single_data<-read.csv("../Data/all_streams_parents_paired_14_16.csv")
colorz<-viridis::plasma(5,end = 0.8)

hist(PWS_single_data$distance_mouth_off)
hist(PWS_single_data$distance_tide_par)

PWS_single_data %>% group_by(year_off,stream_off,offspring_id) %>% 
  summarize(non_na = sum(!is.na(parent_id))) %>%
  group_by(year_off,stream_off,non_na) %>% summarize(n=n()) %>% spread(non_na,n) %>% 
  mutate(total_offspring = rowSums(across(where(is.numeric)))) %>% 
  rename(one_parent = '1',two_parents = '2') %>% 
  ungroup() %>% summarize(sum(two_parents))

model_data<-PWS_single_data %>% filter(origin_off=="Natural") %>% 
  dplyr::select(offspring_id,parent_id,stream_off,sex_off,DOY_off,length_off,distance_mouth_off,
                                             intertidal_off,origin_off,distance_mouth_par,
                                             intertidal_par) %>% 
  group_by(offspring_id) %>% filter(distance_mouth_par==max(distance_mouth_par)) %>% 
  mutate(intertidal_off=as.factor(intertidal_off)) %>% 
  #group_by(stream_off) %>% mutate_at(.vars=vars(DOY_off,length_off,distance_mouth_par),scale) %>% 
  drop_na(intertidal_off,intertidal_par,sex_off,DOY_off,length_off,stream_off)

#Need to remove the first group_by above to get this:
sample_size_table <- model_data %>% group_by(stream_off,sex_off,offspring_id) %>% 
  summarize(non_na = sum(!is.na(parent_id))) %>%
  group_by(stream_off,sex_off,non_na) %>% summarize(n=n()) %>% spread(non_na,n) %>% 
  mutate(total_offspring = rowSums(across(where(is.numeric)))) %>% 
  rename(one_parent = '1',two_parents = '2') %>% 
  mutate(total_parents = one_parent + (2*two_parents))#%>% 
  #ungroup() %>% summarize(sum(one_parent),sum(two_parents))
#write.csv(sample_size_table,"../Figures/Sample_Size_Table.csv")

#Proportion Intertidal
prop_intertidal <- model_data %>% group_by(stream_off,sex_off,intertidal_off) %>% 
  summarize(n=n()) %>% mutate(total = sum(n), prop_int = round(n/total,2)) %>% filter(intertidal_off=="Intertidal")

 #Global distance model:
global_distance<-lmer(distance_mouth_off~distance_mouth_par
                     sex_off+DOY_off+length_off+(1|stream_off),
                   data=model_data,na.action=na.fail)
summary(global_distance)

distance_dredge<-dredge(global_distance)

best_distance <- lmer(distance_mouth_off~distance_mouth_par+
                        sex_off+DOY_off+(1|stream_off),
                      data=model_data,na.action=na.fail)
summary(best_distance)


distance_AIC<-as.data.frame(distance_dredge)[6:10]
distance_AIC[,2:5]<-round(distance_AIC[,2:5],2)
names(distance_AIC[1])<-"K"
distance_AIC$Model<-rownames(distance_AIC)
for(i in 1:nrow(distance_AIC)) distance_AIC$Model[i]<- as.character(formula(get.models(distance_dredge,subset = T)[[i]]))[3]
#write.csv(distance_AIC,"../Figures/distance_AIC_table.csv")

#Parent-Offspring regression best fit line:
distance_pred<-plot_model(best_distance,type="pred")

high_tide_line<-PWS_single_data %>% group_by(stream_off) %>% summarize(htide=min(unique(high_tide_off)))

distance_pred$distance_mouth_par$data %>% 
  ggplot(aes(y=predicted,x=x))+
  #geom_vline(data=high_tide_line,aes(xintercept=htide,color=stream_off),show.legend = F,lty=2,lwd=0.8)+
  
  geom_segment(data=high_tide_line,aes(x=htide,y=-100,xend=htide,yend=-40,color=stream_off),
               lwd=1.2,show.legend = F)+
  geom_segment(data=high_tide_line,aes(x=-75,xend=-25, y=htide,yend=htide,color=stream_off),
               lwd=1.2,show.legend = F)+
  coord_cartesian(ylim = c(0, 2000),xlim=c(0,1500), clip="off") +
  
  geom_jitter(data=model_data,aes(y=distance_mouth_off,x=distance_mouth_par,
                                  color=stream_off),
              pch=16,alpha = 0.3,width = 10,height=10,size=0.8)+
  guides(colour = guide_legend(override.aes = list(alpha=1,size=2)))+
  
  geom_ribbon(aes(ymin=conf.low,ymax=conf.high),alpha=0.3)+
  geom_line(lwd=0.8)+
  #geom_abline(slope=1,intercept=0,lty=2,color="red",lwd=0.8)+
  geom_line(data=data.frame(x=c(0,1500),predicted=c(0,1500)),color="red",lty=2,lwd=0.8)+
  scale_color_discrete(type = colorz)+
  theme_classic()+
  labs(color="Stream",
       x="Inferred Spawning Location\nof Parent (river meters)",
       y = "Inferred Spawning Location\nof Offspring (river meters)")
  
  
#Intertidal/Upstream Binomial model:
global_IU<-glmer(intertidal_off~intertidal_par+
                           sex_off+DOY_off+length_off+(1|stream_off),
                         data=model_data,family = "binomial",
                 na.action=na.fail)
summary(global_IU)

IU_dredge<-dredge(global_IU)


IU_AIC<-as.data.frame(IU_dredge)[6:10]
IU_AIC[,2:5]<-round(IU_AIC[,2:5],2)
colnames(IU_AIC)[1]<-"K"
IU_AIC$Model<-rownames(IU_AIC)
for(i in 1:nrow(IU_AIC)) IU_AIC$Model[i]<- as.character(formula(get.models(IU_dredge,subset = T)[[i]]))[3]
#write.csv(IU_AIC,"../Figures/Intertidal_Upstream_AIC_table.csv")


best_fit_IU<-glmer(intertidal_off~intertidal_par+
                     sex_off+DOY_off+(1|stream_off),
                   data=model_data,family = "binomial",
                   na.action=na.fail)
summary(best_fit_IU)

IU_pred<-plot_model(best_fit_IU,type="pred")
IU_pred$intertidal_par$data$x<-c("Intertidal","Upstream")
IU_pred$intertidal_par$data %>% 
  ggplot(aes(y=predicted,x=x))+
  geom_point()+
  geom_errorbar(aes(ymin=conf.low,ymax=conf.high),width=0.2)+
  labs(y="Proportion Spawning Upstream",x="Inferred Natal Rearing Location")+
  theme_classic()

###Split by just intertidal or upstream fish:
model_data_upstream <- model_data %>% filter(intertidal_off=="Upstream")
global_distance_upstream<-lmer(distance_mouth_off~distance_mouth_par+
                        sex_off+DOY_off+length_off+(1|stream_off),
                      data=model_data_upstream,na.action=na.fail)

distance_dredge_upstream<-dredge(global_distance_upstream)

best_distance_upstream <- lmer(distance_mouth_off~distance_mouth_par+
                                 sex_off+length_off+(1|stream_off),
                               data=model_data_upstream,na.action=na.fail)
summary(best_distance_upstream)


distance_upstream_AIC<-as.data.frame(distance_dredge_upstream)[6:10]
distance_upstream_AIC[,2:5]<-round(distance_upstream_AIC[,2:5],2)
names(distance_upstream_AIC[1])<-"K"
distance_upstream_AIC$Model<-rownames(distance_upstream_AIC)
for(i in 1:nrow(distance_upstream_AIC)) distance_upstream_AIC$Model[i]<- as.character(formula(get.models(distance_dredge_upstream,subset = T)[[i]]))[3]
#write.csv(distance_upstream_AIC,"../Figures/distance_upstream_AIC_table.csv")

#Parent-Offspring regression best fit line:
distance_upstream_pred<-plot_model(best_distance_upstream,type="pred")

distance_upstream_pred$distance_mouth_par$data %>% 
  ggplot(aes(y=predicted,x=x))+
  #geom_vline(data=high_tide_line,aes(xintercept=htide,color=stream_off),show.legend = F,lty=2,lwd=0.8)+
  
  geom_segment(data=high_tide_line,aes(x=htide,y=-100,xend=htide,yend=-40,color=stream_off),
               lwd=1.2,show.legend = F)+
  geom_segment(data=high_tide_line,aes(x=-75,xend=-25, y=htide,yend=htide,color=stream_off),
               lwd=1.2,show.legend = F)+
  coord_cartesian(ylim = c(0, 2000),xlim=c(0,1500), clip="off") +
  
  geom_jitter(data=model_data,aes(y=distance_mouth_off,x=distance_mouth_par,
                                  color=stream_off),
              pch=16,alpha = 0.3,width = 10,height=10,size=0.8)+
  guides(colour = guide_legend(override.aes = list(alpha=1,size=2)))+
  
  geom_ribbon(aes(ymin=conf.low,ymax=conf.high),alpha=0.3,fill="lightblue")+
  geom_line(lwd=0.8,color="lightblue")+
  #geom_abline(slope=1,intercept=0,lty=2,color="red",lwd=0.8)+
  geom_line(data=data.frame(x=c(0,1500),predicted=c(0,1500)),color="red",lty=2,lwd=0.8)+
  scale_color_discrete(type = colorz)+
  theme_classic()+
  labs(color="Stream",
       x="Inferred Spawning Location\nof Parent (river meters)",
       y = "Inferred Spawning Location\nof Offspring (river meters)")+
  geom_line(data=distance_pred$distance_mouth_par$data,lwd=0.8)+
  geom_ribbon(data=distance_pred$distance_mouth_par$data,
              aes(ymin=conf.low,ymax=conf.high),alpha=0.3)


model_data_intertidal <- model_data %>% filter(intertidal_par=="Intertidal")
global_distance_intertidal<-lmer(distance_mouth_off~distance_mouth_par+
                                 sex_off+DOY_off+length_off+(1|stream_off),
                               data=model_data_intertidal,na.action=na.fail)

distance_dredge_intertidal<-dredge(global_distance_intertidal)

best_distance_intertidal <- lmer(distance_mouth_off~sex_off+DOY_off+(1|stream_off),
                                 data=model_data_intertidal,na.action=na.fail)
summary(best_distance_intertidal)


distance_intertidal_AIC<-as.data.frame(distance_dredge_intertidal)[6:10]
distance_intertidal_AIC[,2:5]<-round(distance_intertidal_AIC[,2:5],2)
names(distance_intertidal_AIC[1])<-"K"
distance_intertidal_AIC$Model<-rownames(distance_intertidal_AIC)
for(i in 1:nrow(distance_intertidal_AIC)) distance_intertidal_AIC$Model[i]<- as.character(formula(get.models(distance_dredge_intertidal,subset = T)[[i]]))[3]
#write.csv(distance_intertidal_AIC,"../Figures/distance_intertidal_AIC_table.csv")





