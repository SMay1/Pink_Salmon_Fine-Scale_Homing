#This script is used to examine the all_streams_parents_paired_14_16.csv file
#which contains offspring from 2016 with either one or two parents assigned
#and is used to examine the correlation between offspring and parent spawning locations


library(tidyverse)
library(glmmTMB)
#install_version("MuMIn",version = "1.43.17")
library(MuMIn)
library(lme4)
library(sjPlot)
library(viridis)
library(ggallin)

###JUST THE PO single DATA

PWS_single_data<-read.csv("../Data/all_streams_parents_paired_14_16.csv") %>% filter(stream_off!="Paddy")
colorz<-viridis::plasma(5,end = 0.8)

hist(PWS_single_data$distance_mouth_off)
hist(PWS_single_data$distance_tide_par)

PWS_single_data %>% group_by(year_off,stream_off,offspring_id) %>% 
  dplyr::summarize(non_na = sum(!is.na(parent_id))) %>%
  group_by(year_off,stream_off,non_na) %>% dplyr::summarize(n=n()) %>% spread(non_na,n) %>% 
  mutate(total_offspring = rowSums(across(where(is.numeric)))) %>% 
  rename(one_parent = '1',two_parents = '2') %>% 
  ungroup() %>% dplyr::summarize(sum(two_parents))

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
  dplyr::summarize(non_na = sum(!is.na(parent_id))) %>%
  group_by(stream_off,sex_off,non_na) %>% dplyr::summarize(n=n()) %>% spread(non_na,n) %>% 
  mutate(total_offspring = rowSums(across(where(is.numeric)))) %>% 
  rename(one_parent = '1',two_parents = '2') %>% 
  mutate(total_parents = one_parent + (2*two_parents))#%>% 
  #ungroup() %>% summarize(sum(one_parent),sum(two_parents))
#write.csv(sample_size_table,"../Figures/Sample_Size_Table.csv")

#Proportion Intertidal
prop_intertidal <- model_data %>% group_by(stream_off,sex_off,intertidal_off) %>% 
  dplyr::summarize(n=n()) %>% mutate(total = sum(n), prop_int = round(n/total,2)) %>% filter(intertidal_off=="Intertidal")

#Individual distributions:
# Transform the dataset to a long format
model_data_long <- model_data %>% 
  pivot_longer(
    cols = c(distance_mouth_par, distance_mouth_off),
    names_to = "Year",
    values_to = "distance_mouth"
  ) %>% 
  mutate(Year = recode(Year, "distance_mouth_par" = "Parents (2014)", "distance_mouth_off" = "Offspring (2016)"))

# Create a single plot with facets
collections_plot <- model_data_long %>%
  ggplot(aes(x = stream_off, y = distance_mouth, fill = stream_off)) +
  geom_jitter(aes(color = stream_off), alpha = 0.3, size = 0.75, show.legend = FALSE, height = 0) +
  scale_fill_discrete(type = colorz)+
  scale_color_discrete(type = colorz)+
  scale_y_continuous(breaks = c(0,100,500,1000,1500,2000)) +
  theme_classic() +
  geom_segment(data = high_tide_line, 
               aes(y = htide,yend = htide,x=c(1,2,3,4,1,2,3,4) - 0.50,xend = c(1,2,3,4,1,2,3,4)+0.50),
               lty=2, lwd = 1, color="black") +
  labs(y = "Sampling Locations", x = "Stream", color = "Stream", fill = "Stream") +
  facet_wrap(~Year)

collections_plot
#5 x 5.5 ish

library(patchwork)

# Combine the plots
#combined_plot <- Map.fig / collections_plot
#combined_plot <- combined_plot + plot_layout(heights = c(2, 1),widths=c(2,1))
#combined_plot

#6 x 8.5 ish? 

#Scale of Homing Boxplots:
max_dist <- model_data %>%  group_by(stream_off) %>% dplyr::summarise(max(distance_mouth_par))
 model_data %>%  group_by(stream_off) %>% dplyr::filter(distance_mouth_par == max(distance_mouth_par))

model_data %>% mutate(homing_distance = distance_mouth_par - distance_mouth_off) %>%  group_by(stream_off) %>% 
  ggplot(aes(x = stream_off, y = homing_distance, fill = stream_off))+
  geom_hline(yintercept = 0, lty = 2)+
  geom_boxplot(aes(color= stream_off),alpha= 0.6,outlier.shape = NA, show.legend = F)+
  geom_jitter(aes(color= stream_off),alpha = 0.05, size = 0.75, show.legend = F,height = 0)+
  scale_fill_discrete(type = colorz)+
  scale_color_discrete(type = colorz)+
  scale_y_continuous(breaks = c(-2000,-1500,-1000,-500,-100,100,500,1000,1500,2000)) + 
  theme_classic()+
  labs(y = "Homing Distance (river meter)\n(Parent - Offspring)", x = "Stream", color = "Stream", fill = "Stream")

model_data %>% mutate(homing_distance = distance_mouth_par - distance_mouth_off) %>%  group_by(stream_off) %>% 
  dplyr::summarise(mean = mean(homing_distance), sd = sd(homing_distance), median = median(homing_distance))

model_data %>% mutate(homing_distance = distance_mouth_par - distance_mouth_off) %>% group_by(sex_off) %>% 
  dplyr::summarise(mean = mean(homing_distance), sd = sd(homing_distance), median = median(homing_distance))

#What is the scale of sampling? Taking the mean in each stream on each day of the distance between sampling locations:
#will need to do this for 2014 and 2016 separately
sampling_data_2016<-PWS_single_data %>% 
  dplyr::select(stream_off,DOY_off,distance_mouth_off) %>% drop_na() %>%  
  distinct() %>% group_by(stream_off,DOY_off)
mean_int_2016<-sampling_data_2016 %>%
  group_by(stream_off,DOY_off) %>%
  arrange(DOY_off,distance_mouth_off,.by_group = T) %>%
  mutate(sampling_interval = distance_mouth_off - lag(distance_mouth_off, default = first(distance_mouth_off))) %>% 
  dplyr::summarize(n = n()) %>% mutate(year = 2016) %>% rename(stream = stream_off,DOY = DOY_off)
sampling_data_2014<-PWS_single_data %>% 
  dplyr::select(stream_par,DOY_par,distance_mouth_par) %>% drop_na() %>%  
  distinct() %>% group_by(stream_par,DOY_par)
mean_int<-sampling_data_2014 %>%
  group_by(stream_par,DOY_par) %>%
  arrange(DOY_par,distance_mouth_par,.by_group = T) %>%
  mutate(sampling_interval = distance_mouth_par - lag(distance_mouth_par, default = first(distance_mouth_par))) %>% 
  dplyr::summarize(n = n()) %>% mutate(year = 2014) %>% rename(stream = stream_par,DOY = DOY_par) %>% 
  rbind(mean_int_2016)
mean_int %>% ggplot(aes(x=n))+
  geom_histogram(binwidth = 1,aes(fill = stream),position="dodge",color="black")+
  geom_vline(data = mean_int %>% group_by(stream,year) %>% dplyr::summarize(mean=mean(n)), aes(xintercept = mean),lty = 2)+
  facet_grid(year~stream)+
  theme_classic()+
  labs(x = "Number of sampling\nlocations per day",y = "Count",fill = "Stream")

mean_int_table<-mean_int %>% group_by(stream,year) %>% dplyr::summarize(mean_n_sample_sites=mean(n),sd_n_sample_sites = sd(n))

by_day_sampling_intervals_2016<-sampling_data_2016 %>%
  group_by(stream_off,DOY_off) %>%
  arrange(DOY_off,distance_mouth_off,.by_group = T) %>%
  mutate(sampling_interval = distance_mouth_off - lag(distance_mouth_off, default = first(distance_mouth_off))) %>% 
  filter(sampling_interval>0) %>% dplyr::summarize(mean_sampling_interval = mean(sampling_interval),
                                                   var_sampling_interval = var(sampling_interval),
                                                   sd_sampling_interval = sd(sampling_interval),
                                                   n_sampling_sites = n()) 

by_creek_sampling_intervals_2016<-by_day_sampling_intervals_2016 %>% group_by(stream_off) %>% 
  dplyr::summarize(mean_creek_interval = mean(mean_sampling_interval),
                   sd_creek_interval = sd(mean_sampling_interval)) %>% 
  rename(stream = stream_off) %>% mutate(year = 2016)

by_day_sampling_intervals_2014<-sampling_data_2014 %>%
  group_by(stream_par,DOY_par) %>%
  arrange(DOY_par,distance_mouth_par,.by_group = T) %>%
  mutate(sampling_interval = distance_mouth_par - lag(distance_mouth_par, default = first(distance_mouth_par))) %>% 
  filter(sampling_interval>0) %>% dplyr::summarize(mean_sampling_interval = mean(sampling_interval),
                                                   var_sampling_interval = var(sampling_interval),
                                                   sd_sampling_interval = sd(sampling_interval),
                                                   n_sampling_sites = n()) 

by_creek_sampling_intervals_2014<-by_day_sampling_intervals_2014 %>% group_by(stream_par) %>% 
  dplyr::summarize(mean_creek_interval = mean(mean_sampling_interval),
                   sd_creek_interval = sd(mean_sampling_interval)) %>% 
  rename(stream = stream_par) %>% mutate(year = 2014)

rbind(by_creek_sampling_intervals_2014,by_creek_sampling_intervals_2016) %>% arrange(stream) %>% 
  left_join(mean_int_table)

 
#Global distance model:
global_distance<-lmer(distance_mouth_off~distance_mouth_par +
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

high_tide_line<-PWS_single_data %>% group_by(stream_off) %>% dplyr::summarize(htide=min(unique(high_tide_off)))

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
  




####Stream Specific Model:

#Global distance model:
global_distance<-lm(distance_mouth_off~distance_mouth_par*stream_off +
                        sex_off+DOY_off+length_off,
                      data=model_data,na.action=na.fail)
summary(global_distance)

distance_dredge<-dredge(global_distance)

best_distance <- lm(distance_mouth_off~distance_mouth_par*stream_off+
                        sex_off+DOY_off,
                      data=model_data,na.action=na.fail)
summary(best_distance)


distance_AIC<-as.data.frame(distance_dredge)[8:12]
distance_AIC[,2:5]<-round(distance_AIC[,2:5],2)
names(distance_AIC[1])<-"K"
distance_AIC$Model<-rownames(distance_AIC)
for(i in 1:nrow(distance_AIC)) distance_AIC$Model[i]<- as.character(formula(get.models(distance_dredge,subset = T)[[i]]))[3]
#write.csv(distance_AIC,"../Figures/distance_AIC_table.csv")

#Parent-Offspring regression best fit line:
distance_pred<-as.data.frame(model_data %>% group_by(stream_off) %>% 
                               dplyr::summarise(distance_mouth_par = seq(min(distance_mouth_par),max(distance_mouth_par),length.out=1000),
                                                DOY_off = seq(min(DOY_off),max(DOY_off),length.out = 1000),
                                                sex_off = "Male"))
distance_pred$distance_mouth_off <- predict(best_distance,newdata = distance_pred)
distance_pred$se.fit <- predict(best_distance,newdata = distance_pred,se.fit = T)$se.fit

high_tide_line<-PWS_single_data %>% group_by(stream_off) %>% dplyr::summarize(htide=min(unique(high_tide_off)))

par_off_glm<-distance_pred %>% 
  ggplot(aes(y=distance_mouth_off,x=distance_mouth_par,color=stream_off))+
  #geom_vline(data=high_tide_line,aes(xintercept=htide,color=stream_off),show.legend = F,lty=2,lwd=0.8)+
  
  geom_segment(data=high_tide_line,aes(x=htide,y=-100,xend=htide,yend=-20,color=stream_off),
               lwd=1.2,show.legend = F)+
  geom_segment(data=high_tide_line,aes(x=-175,xend=-20, y=htide,yend=htide,color=stream_off),
               lwd=1.2,show.legend = F)+
  coord_cartesian(ylim = c(-20, 750),xlim=c(-100,1200), clip="on") +
  
  #geom_jitter(data=model_data,aes(y=distance_mouth_off,x=distance_mouth_par,
  #                                color=stream_off),
  #            pch=16,alpha = 0.3,width = 10,height=10,size=0.8)+
  guides(colour = guide_legend(override.aes = list(alpha=1,size=2)))+
  
  geom_ribbon(aes(ymin=distance_mouth_off-se.fit,ymax=distance_mouth_off+se.fit,color = stream_off,fill=stream_off),alpha=0.3,show.legend = F)+
  geom_line(lwd=0.8,aes(color=stream_off),show.legend = F)+
  geom_abline(slope=1,intercept=0,lty=2,lwd=0.5)+
  scale_color_discrete(type = colorz)+
  scale_fill_discrete(type = colorz)+
  theme_classic()+
  theme(legend.position = "none",strip.background = element_blank(),
        strip.text = element_blank())+
  facet_grid(.~stream_off)+
  labs(color="Stream",
       x="Inferred Spawning Location of Parent (river meters)",
       y = "Inferred Spawning Location\nof Offspring (river meters)")


par_off_boxplot<-model_data %>% mutate(homing_distance = distance_mouth_par - distance_mouth_off) %>%  group_by(stream_off) %>% 
  ggplot(aes(x = 1, y = homing_distance, fill = stream_off))+
  geom_hline(yintercept = 0, lty = 2)+
  geom_boxplot(aes(color= stream_off),alpha= 0.7,
               outlier.shape = NA, show.legend = F,width = 0.4)+
  geom_jitter(aes(color= stream_off),alpha = 0.07, size = 0.8, show.legend = F,height = 0)+
  scale_fill_discrete(type = colorz)+
  scale_color_discrete(type = colorz)+
  theme_classic()+
  scale_y_continuous(breaks = c(-1000,-500,-100,100,500,1000),limits = c(-1100,1100)) + 
  theme(axis.title.x = element_blank(),axis.text.x = element_blank(),
        axis.ticks.x = element_blank())+
  facet_grid(.~stream_off)+
  labs(y = "Homing Distance\n(Parent - Offspring)", x = "Stream", color = "Stream", fill = "Stream")

ggarrange(par_off_boxplot,par_off_glm,nrow=2,heights=c(1,1.5),align = "v")
  
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

####Stream-specific:
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





