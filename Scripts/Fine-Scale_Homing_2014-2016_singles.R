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
library(ggpubr)

###JUST THE PO single DATA

PWS_single_data<-read.csv("../Data/all_streams_parents_paired_14_16.csv") %>% filter(stream_off!="Paddy",stream_off==stream_par)
PWS_metadata<- read.csv("../Data/pws_pink_pedigree_2014-2016_sample_metadata_with_location.csv") %>% filter(!silly_code%in%c("PPADDY16","PPADDY14"))

colorz<-viridis::plasma(5,end = 0.8)

length(unique(PWS_single_data$offspring_id))
length(unique(PWS_single_data$parent_id))

hist(PWS_single_data$distance_mouth_off)
hist(PWS_single_data$distance_tide_par)

PWS_single_data %>% group_by(year_off,stream_off,offspring_id) %>% 
  dplyr::summarize(non_na = sum(!is.na(parent_id))) %>%
  group_by(year_off,stream_off,non_na) %>% dplyr::summarize(n=n()) %>% spread(non_na,n) %>% 
  mutate(total_offspring = rowSums(across(where(is.numeric)))) %>% 
  rename(one_parent = '1',two_parents = '2') #%>% 
  #ungroup() %>% dplyr::summarize(sum(two_parents))

model_data<-PWS_single_data %>% 
  dplyr::select(offspring_id,parent_id,stream_off,sex_off,DOY_off,length_off,distance_mouth_off,
                intertidal_off,origin_off,distance_mouth_par,
                intertidal_par) %>% 
  group_by(offspring_id) %>% filter(distance_mouth_par==max(distance_mouth_par)) %>% distinct(.keep_all = T, offspring_id) %>% 
  mutate(intertidal_off=as.factor(intertidal_off))%>% 
  #group_by(stream_off) %>% mutate_at(.vars=vars(DOY_off,length_off,distance_mouth_par),scale) %>% 
  drop_na(intertidal_off,intertidal_par,sex_off,DOY_off,length_off,stream_off)

#Need to remove the first group_by above to get this:
sample_size_table <- model_data %>% group_by(stream_off,sex_off,offspring_id) %>% 
  dplyr::summarize(non_na = sum(!is.na(parent_id))) %>%
  group_by(stream_off,sex_off,non_na) %>% 
  dplyr::summarize(n=n()) %>% spread(non_na,n) %>% 
  mutate(total_offspring = rowSums(across(where(is.numeric)))) %>% 
  rename(one_parent = '1',two_parents = '2') %>% 
  mutate(total_parents = one_parent + (2*two_parents))#%>% 
 #ungroup() %>% summarize(sum(one_parent),sum(two_parents))

n_parents <- model_data %>% group_by(stream_off,sex_off) %>% 
  dplyr::summarize(length(unique(parent_id)))


write.csv(sample_size_table,"Sample_Size_Table.csv")

#Proportion Intertidal
prop_intertidal <- model_data %>% group_by(stream_off,intertidal_off) %>% 
  dplyr::summarize(n=n()) %>% mutate(total = sum(n), prop_int = round(n/total,2)) %>% filter(intertidal_off=="Intertidal") %>% 
  ungroup() %>% summarize(mean(prop_int),sd(prop_int))

#Individual distributions:
# Transform the dataset to a long format

model_data_long<-model_data %>% ungroup() %>% distinct(parent_id,.keep_all = T) %>% 
  select(parent_id,stream_off,distance_mouth_par) %>% 
  rename(id = parent_id,distance_mouth = distance_mouth_par) %>% 
    mutate(Year = "Parents (2014)") %>% 
  rbind(
  model_data %>% ungroup() %>% distinct(offspring_id,.keep_all = T) %>% 
    select(offspring_id,stream_off,distance_mouth_off) %>% 
    rename(id = offspring_id,distance_mouth = distance_mouth_off) %>% 
    mutate(Year = "Offspring (2016)"))

# Create a single plot with facets
collections_plot <- model_data_long %>%
  ggplot(aes(x = stream_off, y = distance_mouth, fill = stream_off)) +
  geom_jitter(aes(color = stream_off), alpha = 0.3, size = 0.4, show.legend = FALSE, height = 0) +
  scale_fill_discrete(type = colorz)+
  scale_color_discrete(type = colorz)+
  scale_y_continuous(breaks = c(0,100,500,1000,1500,2000),limits = c(0,1500)) +
  theme_classic() +
  facet_wrap(~Year)+
  geom_segment(data = high_tide_line, 
               aes(y = htide,yend = htide,x=c(1,2,3,4,1,2,3,4) - 0.50,xend = c(1,2,3,4,1,2,3,4)+0.50),
               lty=2, lwd = 1, color="black") +
  labs(y = "Sampling Locations (river meters)", x = "Stream", color = "Stream", fill = "Stream") 

collections_plot
#5 x 5.5 ish

ggsave(filename = "Collections_Fig1.png",
       plot = collections_plot,dpi = 300,width = 5, height = 2.5, units = "in")

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
  dplyr::summarise(mean = mean(homing_distance), sd = sd(homing_distance), median = median(homing_distance)) %>% View()

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
  mutate(sampling_interval = distance_mouth_off - lag(distance_mouth_off, default = first(distance_mouth_off)),
         sampling_interval = sampling_interval/2) %>% 
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
  mutate(sampling_interval = distance_mouth_par - lag(distance_mouth_par, default = first(distance_mouth_par)),
         sampling_interval = sampling_interval/2) %>% 
  filter(sampling_interval>0) %>% dplyr::summarize(mean_sampling_interval = mean(sampling_interval),
                                                   var_sampling_interval = var(sampling_interval),
                                                   sd_sampling_interval = sd(sampling_interval),
                                                   n_sampling_sites = n()) 

by_creek_sampling_intervals_2014<-by_day_sampling_intervals_2014 %>% group_by(stream_par) %>% 
  dplyr::summarize(mean_creek_interval = mean(mean_sampling_interval),
                   sd_creek_interval = sd(mean_sampling_interval)) %>% 
  rename(stream = stream_par) %>% mutate(year = 2014)

rbind(by_creek_sampling_intervals_2014,by_creek_sampling_intervals_2016) %>% arrange(stream) %>% 
  left_join(mean_int_table) %>%  View()

#Using all metadata instead of the pedigree to get the sampling interval:
by_day_sampling_intervals<-PWS_metadata %>%
  mutate(sample_date = as.Date(sample_date)) %>% 
  group_by(silly_code,sample_date) %>%
  arrange(sample_date,mouthdist,.by_group = T) %>%
  mutate(sampling_interval = mouthdist - lag(mouthdist, default = first(mouthdist)),
         sampling_interval = sampling_interval/2) %>% 
  filter(sampling_interval>0) %>% dplyr::summarize(mean_sampling_interval = mean(sampling_interval),
                                                   var_sampling_interval = var(sampling_interval),
                                                   sd_sampling_interval = sd(sampling_interval),
                                                   n_sampling_sites = n()) 

by_creek_sampling_intervals<-by_day_sampling_intervals %>% group_by(silly_code) %>% 
  dplyr::summarize(mean_creek_interval = round(mean(mean_sampling_interval),2),
                   sd_creek_interval = round(sd(mean_sampling_interval),2)) %>% 
  rename(stream = silly_code)
#by_creek_sampling_intervals %>% View()

overall_creek_sampling_intervals<-by_day_sampling_intervals %>% ungroup() %>%  
  dplyr::summarize(mean_creek_interval = round(mean(mean_sampling_interval),2),
                   sd_creek_interval = round(sd(mean_sampling_interval),2))
overall_creek_sampling_intervals %>% View()
###Proportion of sampled days?
PWS_single_data %>% group_by(stream_off) %>% dplyr::summarise(min_day = min(DOY_off),max_day = max(DOY_off),
                                                                 total_days = max_day - min_day,
                                                                 days_sampled = length(unique(DOY_off)),
                                                                 proportion_days = days_sampled/total_days)
PWS_single_data %>% group_by(stream_par) %>% dplyr::summarise(min_day = min(DOY_par),max_day = max(DOY_par),
                                                                 total_days = max_day - min_day,
                                                                 days_sampled = length(unique(DOY_par)),
                                                                 proportion_days = days_sampled/total_days)


####Stream Specific Model:

#Global distance model:
global_distance<-lm(distance_mouth_off~distance_mouth_par*stream_off +
                        sex_off+DOY_off+length_off,
                      data=model_data,na.action=na.fail)
summary(global_distance)

distance_dredge<-dredge(global_distance)

best_distance <- lm(distance_mouth_off~distance_mouth_par*stream_off+
                        sex_off+DOY_off+length_off,
                      data=model_data,na.action=na.fail)
summary(best_distance)


distance_AIC<-as.data.frame(distance_dredge)[8:12]
distance_AIC[,2:5]<-round(distance_AIC[,2:5],2)
names(distance_AIC[1])<-"K"
distance_AIC$Model<-rownames(distance_AIC)
for(i in 1:nrow(distance_AIC)) distance_AIC$Model[i]<- as.character(formula(get.models(distance_dredge,subset = T)[[i]]))[3]
write.csv(distance_AIC,"distance_AIC_table.csv")

#Parent-Offspring regression best fit line:
distance_pred<-as.data.frame(model_data %>% group_by(stream_off) %>% 
                               dplyr::summarise(distance_mouth_par = seq(min(distance_mouth_par),max(distance_mouth_par),length.out=1000),
                                                DOY_off = mean(DOY_off),
                                                length_off = mean(length_off),
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
  coord_cartesian(ylim = c(-20, 1100),xlim=c(-100,1200), clip="on") +
  
  geom_jitter(data=model_data,aes(y=distance_mouth_off,x=distance_mouth_par,
                                  color=stream_off),
              pch=16,alpha = 0.3,width = 10,height=10,size=0.3)+
  guides(colour = guide_legend(override.aes = list(alpha=1,size=2)))+
  
  geom_ribbon(aes(ymin=distance_mouth_off-(1.96*se.fit),ymax=distance_mouth_off+(1.96*se.fit),color = stream_off,fill=stream_off),alpha=0.3,show.legend = F)+
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
  labs(y = "Distance Between Parents\nand Offspring (m)", x = "Stream", color = "Stream", fill = "Stream")

#Fig2<-ggarrange(par_off_boxplot,par_off_glm,nrow=2,heights=c(1,1.5),align = "v")
#ggsave(plot = Fig2,filename = "Figure_2.png",units = "in",width = 6.5,height = 6, dpi = 300)



####Stream-specific:
#Intertidal/Upstream Binomial model:
global_IU<-glm(intertidal_off~intertidal_par*stream_off+ distance_mouth_par*stream_off +
                 sex_off+DOY_off+length_off,
                 data=model_data,family = "binomial",
                 na.action=na.fail)

IU_dredge<-dredge(global_IU)


IU_AIC<-as.data.frame(IU_dredge)[10:14]
IU_AIC[,2:5]<-round(IU_AIC[,2:5],2)
colnames(IU_AIC)[1]<-"K"
IU_AIC$Model<-rownames(IU_AIC)
for(i in 1:nrow(IU_AIC)) IU_AIC$Model[i]<- as.character(formula(get.models(IU_dredge,subset = T)[[i]]))[3]
write.csv(IU_AIC,"Intertidal_Upstream_AIC_table.csv")

best_fit_IU<-glm(intertidal_off~intertidal_par+stream_off+ distance_mouth_par + distance_mouth_par:stream_off +
                   sex_off+DOY_off,
                 data=model_data,family = "binomial",
                 na.action=na.fail)

summary(best_fit_IU)

IU_pred<-model_data %>% group_by(stream_off,intertidal_par) %>% 
  dplyr::summarize(DOY_off=mean(DOY_off),
                   length_off = mean(length_off),
                   distance_mouth_par = mean(distance_mouth_par)) %>% 
  mutate(intertidal_par = factor(intertidal_par))
IU_pred$sex_off <- "Male"
IU_pred$intertidal_off<-predict(best_fit_IU,newdata = IU_pred,type = "re")
IU_pred$se.fit<-predict(best_fit_IU,newdata = IU_pred,type = "re",se.fit=T)$se.fit

IU_pred %>% group_by(stream_off) %>% mutate(prob = intertidal_off/intertidal_off[intertidal_par=="Intertidal"])

IU_pred_plot<-IU_pred%>% 
  ggplot(aes(x=intertidal_par,y=intertidal_off,color=stream_off))+
  geom_point(size=2)+
  geom_errorbar(aes(ymin=intertidal_off-(se.fit*1.96),ymax=intertidal_off+(se.fit*1.96)),width=0.2)+
  labs(y="Proportion Spawning\nUpstream",x="Inferred Natal Rearing Location")+
  scale_color_discrete(type = colorz)+
  theme_classic()+
  facet_grid(.~stream_off)+
  theme(legend.position = "none",
        strip.text = element_blank(),strip.background = element_blank())

Fig3<-ggarrange(par_off_boxplot,par_off_glm,IU_pred_plot,nrow=3,heights=c(1,1,1),align = "v")
ggsave(plot = Fig3,filename = "Figure_3.png",units = "in",width = 6.5,height = 7, dpi = 300)



###Split by just intertidal or upstream fish:
model_data_upstream <- model_data %>% filter(intertidal_off=="Upstream")
global_distance_upstream<-lm(distance_mouth_off~distance_mouth_par*stream_off+
                        sex_off+DOY_off+length_off,
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
global_distance_intertidal<-lmer(distance_mouth_off~distance_mouth_par*stream_off+
                                 sex_off+DOY_off+length_off,
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

###Stream-Specific:

model_data_upstream <- model_data %>% filter(intertidal_off=="Upstream")
global_distance_upstream<-lm(distance_mouth_off~distance_mouth_par*stream_off+
                                 sex_off+DOY_off+length_off,
                               data=model_data_upstream,na.action=na.fail)

distance_dredge_upstream<-dredge(global_distance_upstream)

best_distance_upstream <- lm(distance_mouth_off~distance_mouth_par*stream_off+
                                 length_off,
                               data=model_data_upstream,na.action=na.fail)




model_data_intertidal <- model_data %>% filter(intertidal_par=="Intertidal")
global_distance_intertidal<-lm(distance_mouth_off~distance_mouth_par*stream_off +
                                   sex_off+DOY_off+length_off,
                                 data=model_data_intertidal,na.action=na.fail)

distance_dredge_intertidal<-dredge(global_distance_intertidal)

best_distance_intertidal <- lm(distance_mouth_off~distance_mouth_par*stream_off +
                                 sex_off+DOY_off,
                               data=model_data_intertidal,na.action=na.fail)
  
summary(best_distance_intertidal)



distance_pred_upstream<-as.data.frame(model_data_upstream %>% group_by(stream_off) %>% 
                                        dplyr::summarise(distance_mouth_par = seq(min(distance_mouth_par),max(distance_mouth_par),length.out=1000),
                                                         DOY_off = mean(DOY_off),
                                                         length_off = mean(length_off),
                                                         sex_off = "Male",
                                                         zone = "Upstream"))
distance_pred_upstream$distance_mouth_off <- predict(best_distance_upstream,newdata = distance_pred_upstream)
distance_pred_upstream$se.fit <- predict(best_distance_upstream,newdata = distance_pred_upstream,se.fit = T)$se.fit


distance_pred_intertidal<-as.data.frame(model_data_intertidal %>% group_by(stream_off) %>% 
                                        dplyr::summarise(distance_mouth_par = seq(min(distance_mouth_par),max(distance_mouth_par),length.out=1000),
                                                         DOY_off = mean(DOY_off),
                                                         length_off = mean(length_off),
                                                         sex_off = "Male",
                                                         zone = "Intertidal"))
distance_pred_intertidal$distance_mouth_off <- predict(best_distance_intertidal,newdata = distance_pred_intertidal)
distance_pred_intertidal$se.fit <- predict(best_distance_intertidal,newdata = distance_pred_intertidal,se.fit = T)$se.fit

distance_pred<-rbind(distance_pred_intertidal,distance_pred_upstream)
distance_pred<-distance_pred %>% left_join(high_tide_line,by="stream_off")%>% 
  filter((distance_mouth_par <= htide & zone == "Intertidal") | 
           (distance_mouth_par > htide & zone == "Upstream"))


high_tide_line<-PWS_single_data %>% group_by(stream_off) %>% dplyr::summarize(htide=min(unique(high_tide_off)))
model_data <- left_join(model_data, high_tide_line, by = "stream_off") %>% 
  mutate(zone = ifelse(distance_mouth_par <= htide, "Intertidal", "Upstream"))


IU_separate_plot<-distance_pred %>% 
  ggplot(aes(y=distance_mouth_off,x=distance_mouth_par,color=stream_off))+
  #geom_vline(data=high_tide_line,aes(xintercept=htide,color=stream_off),show.legend = F,lty=2,lwd=0.8)+
  
  #geom_segment(data=high_tide_line,aes(x=htide,y=-100,xend=htide,yend=-20,color=stream_off),
  #             lwd=1.2,show.legend = F)+
  #geom_segment(data=high_tide_line,aes(x=-175,xend=-20, y=htide,yend=htide,color=stream_off),
  #             lwd=1.2,show.legend = F)+
  #coord_cartesian(ylim = c(-20, 750),xlim=c(-100,1200), clip="on") +
  
  geom_jitter(data=model_data,aes(y=distance_mouth_off,x=distance_mouth_par,
                                  color=stream_off),
              pch=16,alpha = 0.3,width = 10,height=10,size=0.3)+
  guides(colour = guide_legend(override.aes = list(alpha=1,size=2)))+
  
  geom_ribbon(aes(ymin=distance_mouth_off-1.96*se.fit,ymax=distance_mouth_off+1.96*se.fit,
                  color = stream_off,fill=stream_off,
                  group = zone),alpha=0.3,show.legend = F)+
  geom_line(lwd=0.8,aes(color=stream_off,group = zone),show.legend = F)+
  geom_abline(slope=1,intercept=0,lty=2,lwd=0.5)+
  scale_color_discrete(type = colorz)+
  scale_fill_discrete(type = colorz)+
  theme_classic()+
  theme(legend.position = "none",strip.background = element_blank(),
        strip.text = element_blank())+
  facet_grid(stream_off~zone,scales="free")+
  labs(color="Stream",
       x="Inferred Spawning Location of Parent (river meters)",
       y = "Inferred Spawning Location\nof Offspring (river meters)")

IU_separate_plot



###ERB
model_data_upstream_erb <- model_data %>% filter(intertidal_off=="Upstream",stream_off=="Erb")
global_distance_upstream_erb<-lm(distance_mouth_off~distance_mouth_par +
                               sex_off+DOY_off+length_off,
                             data=model_data_upstream_erb,na.action=na.fail)

distance_dredge_upstream_erb<-dredge(global_distance_upstream_erb)

best_distance_upstream_erb <- lm(distance_mouth_off~
                                   DOY_off,
                                 data=model_data_upstream_erb,na.action=na.fail)
summary(best_distance_upstream_erb)

model_data_intertidal_erb <- model_data %>% filter(intertidal_off=="Intertidal",stream_off=="Erb")
global_distance_intertidal_erb<-lm(distance_mouth_off~distance_mouth_par +
                                   sex_off+DOY_off+length_off,
                                 data=model_data_intertidal_erb,na.action=na.fail)

distance_dredge_intertidal_erb<-dredge(global_distance_intertidal_erb)

best_distance_intertidal_erb <- lm(distance_mouth_off~distance_mouth_par +
                                     DOY_off+sex_off,
                                   data=model_data_intertidal_erb,na.action=na.fail)
summary(best_distance_intertidal_erb)


distance_pred_upstream_erb<-as.data.frame(model_data_upstream_erb %>% group_by(stream_off) %>% 
                                        dplyr::summarise(distance_mouth_par = seq(min(distance_mouth_par),max(distance_mouth_par),length.out=1000),
                                                         DOY_off = mean(DOY_off),
                                                         length_off = mean(length_off),
                                                         sex_off = "Male", distance_mouth_off = NA,se.fit=NA,
                                                         zone = "Upstream"))
distance_pred_upstream_erb$distance_mouth_off <- predict(best_distance_upstream_erb,newdata = distance_pred_upstream_erb)
distance_pred_upstream_erb$se.fit <- predict(best_distance_upstream_erb,newdata = distance_pred_upstream_erb,se.fit = T)$se.fit


distance_pred_intertidal_erb<-as.data.frame(model_data_intertidal_erb %>% group_by(stream_off) %>% 
                                          dplyr::summarise(distance_mouth_par = seq(min(distance_mouth_par),max(distance_mouth_par),length.out=1000),
                                                           DOY_off = mean(DOY_off),
                                                           length_off = mean(length_off),
                                                           sex_off = "Male", distance_mouth_off = NA,se.fit=NA,
                                                           zone = "Intertidal"))
distance_pred_intertidal_erb$distance_mouth_off <- predict(best_distance_intertidal_erb,newdata = distance_pred_intertidal_erb)
distance_pred_intertidal_erb$se.fit <- predict(best_distance_intertidal_erb,newdata = distance_pred_intertidal_erb,se.fit = T)$se.fit


###Gilmour
model_data_upstream_gilmour <- model_data %>% filter(intertidal_off=="Upstream",stream_off=="Gilmour")
global_distance_upstream_gilmour<-lm(distance_mouth_off~distance_mouth_par +
                                   sex_off+DOY_off+length_off,
                                 data=model_data_upstream_gilmour,na.action=na.fail)

distance_dredge_upstream_gilmour<-dredge(global_distance_upstream_gilmour)

best_distance_upstream_gilmour <- lm(distance_mouth_off~ 1,
                                 data=model_data_upstream_gilmour,na.action=na.fail)
summary(best_distance_upstream_gilmour)

model_data_intertidal_gilmour <- model_data %>% filter(intertidal_off=="Intertidal",stream_off=="Gilmour")
global_distance_intertidal_gilmour<-lm(distance_mouth_off~distance_mouth_par +
                                     sex_off+DOY_off+length_off,
                                   data=model_data_intertidal_gilmour,na.action=na.fail)

distance_dredge_intertidal_gilmour<-dredge(global_distance_intertidal_gilmour)

best_distance_intertidal_gilmour <- lm(distance_mouth_off~ 
                                     DOY_off,
                                   data=model_data_intertidal_gilmour,na.action=na.fail)
summary(best_distance_intertidal_gilmour)

distance_pred_upstream_gilmour<-as.data.frame(model_data_upstream_gilmour %>% group_by(stream_off) %>% 
                                            dplyr::summarise(distance_mouth_par = seq(min(distance_mouth_par),max(distance_mouth_par),length.out=1000),
                                                             DOY_off = mean(DOY_off),
                                                             length_off = mean(length_off),
                                                             sex_off = "Male", distance_mouth_off = NA,se.fit=NA,
                                                             zone = "Upstream"))
distance_pred_upstream_gilmour$distance_mouth_off <- predict(best_distance_upstream_gilmour,newdata = distance_pred_upstream_gilmour)
distance_pred_upstream_gilmour$se.fit <- predict(best_distance_upstream_gilmour,newdata = distance_pred_upstream_gilmour,se.fit = T)$se.fit


distance_pred_intertidal_gilmour<-as.data.frame(model_data_intertidal_gilmour %>% group_by(stream_off) %>% 
                                              dplyr::summarise(distance_mouth_par = seq(min(distance_mouth_par),max(distance_mouth_par),length.out=1000),
                                                               DOY_off = mean(DOY_off),
                                                               length_off = mean(length_off),
                                                               sex_off = "Male", distance_mouth_off = NA,se.fit=NA,
                                                               zone = "Intertidal"))
#distance_pred_intertidal_gilmour$distance_mouth_off <- predict(best_distance_intertidal_gilmour,newdata = distance_pred_intertidal_gilmour)
#distance_pred_intertidal_gilmour$se.fit <- predict(best_distance_intertidal_gilmour,newdata = distance_pred_intertidal_gilmour,se.fit = T)$se.fit


###Hogan
model_data_upstream_hogan <- model_data %>% filter(intertidal_off=="Upstream",stream_off=="Hogan")
global_distance_upstream_hogan<-lm(distance_mouth_off~distance_mouth_par +
                                       sex_off+DOY_off+length_off,
                                     data=model_data_upstream_hogan,na.action=na.fail)

distance_dredge_upstream_hogan<-dredge(global_distance_upstream_hogan)

best_distance_upstream_hogan <- lm(distance_mouth_off~
                                     length_off,
                                     data=model_data_upstream_hogan,na.action=na.fail)
summary(best_distance_upstream_hogan)

model_data_intertidal_hogan <- model_data %>% filter(intertidal_off=="Intertidal",stream_off=="Hogan")
global_distance_intertidal_hogan<-lm(distance_mouth_off~distance_mouth_par +
                                         sex_off+DOY_off+length_off,
                                       data=model_data_intertidal_hogan,na.action=na.fail)

distance_dredge_intertidal_hogan<-dredge(global_distance_intertidal_hogan)

best_distance_intertidal_hogan <- lm(distance_mouth_off~ 
                                         DOY_off + sex_off,
                                       data=model_data_intertidal_hogan,na.action=na.fail)
summary(best_distance_intertidal_hogan)

distance_pred_upstream_hogan<-as.data.frame(model_data_upstream_hogan %>% group_by(stream_off) %>% 
                                            dplyr::summarise(distance_mouth_par = seq(min(distance_mouth_par),max(distance_mouth_par),length.out=1000),
                                                             DOY_off = mean(DOY_off),
                                                             length_off = mean(length_off),
                                                             sex_off = "Male", distance_mouth_off = NA,se.fit=NA,
                                                             zone = "Upstream"))
distance_pred_upstream_hogan$distance_mouth_off <- predict(best_distance_upstream_hogan,newdata = distance_pred_upstream_hogan)
distance_pred_upstream_hogan$se.fit <- predict(best_distance_upstream_hogan,newdata = distance_pred_upstream_hogan,se.fit = T)$se.fit


distance_pred_intertidal_hogan<-as.data.frame(model_data_intertidal_hogan %>% group_by(stream_off) %>% 
                                              dplyr::summarise(distance_mouth_par = seq(min(distance_mouth_par),max(distance_mouth_par),length.out=1000),
                                                               DOY_off = mean(DOY_off),
                                                               length_off = mean(length_off),
                                                               sex_off = "Male", distance_mouth_off = NA,se.fit=NA,
                                                               zone = "Intertidal"))
#distance_pred_intertidal_hogan$distance_mouth_off <- predict(best_distance_intertidal_hogan,newdata = distance_pred_intertidal_hogan)
#distance_pred_intertidal_hogan$se.fit <- predict(best_distance_intertidal_hogan,newdata = distance_pred_intertidal_hogan,se.fit = T)$se.fit



###Stockdale
model_data_upstream_stockdale <- model_data %>% filter(intertidal_off=="Upstream",stream_off=="Stockdale")
global_distance_upstream_stockdale<-lm(distance_mouth_off~distance_mouth_par +
                                       sex_off+DOY_off+length_off,
                                     data=model_data_upstream_stockdale,na.action=na.fail)

distance_dredge_upstream_stockdale<-dredge(global_distance_upstream_stockdale)

best_distance_upstream_stockdale <- lm(distance_mouth_off~distance_mouth_par+
                                         length_off,
                                     data=model_data_upstream_stockdale,na.action=na.fail)
summary(best_distance_upstream_stockdale)

model_data_intertidal_stockdale <- model_data %>% filter(intertidal_off=="Intertidal",stream_off=="Stockdale")
global_distance_intertidal_stockdale<-lm(distance_mouth_off~distance_mouth_par +
                                         sex_off+DOY_off+length_off,
                                       data=model_data_intertidal_stockdale,na.action=na.fail)

distance_dredge_intertidal_stockdale<-dredge(global_distance_intertidal_stockdale)

best_distance_intertidal_stockdale <- lm(distance_mouth_off~ distance_mouth_par +
                                         DOY_off,
                                       data=model_data_intertidal_stockdale,na.action=na.fail)
summary(best_distance_intertidal_stockdale)

distance_pred_upstream_stockdale<-as.data.frame(model_data_upstream_stockdale %>% group_by(stream_off) %>% 
                                            dplyr::summarise(distance_mouth_par = seq(min(distance_mouth_par),max(distance_mouth_par),length.out=1000),
                                                             DOY_off = mean(DOY_off),
                                                             length_off = mean(length_off),
                                                             sex_off = "Male", distance_mouth_off = NA,se.fit=NA,
                                                             zone = "Upstream"))
distance_pred_upstream_stockdale$distance_mouth_off <- predict(best_distance_upstream_stockdale,newdata = distance_pred_upstream_stockdale)
distance_pred_upstream_stockdale$se.fit <- predict(best_distance_upstream_stockdale,newdata = distance_pred_upstream_stockdale,se.fit = T)$se.fit


distance_pred_intertidal_stockdale<-as.data.frame(model_data_intertidal_stockdale %>% group_by(stream_off) %>% 
                                              dplyr::summarise(distance_mouth_par = seq(min(distance_mouth_par),max(distance_mouth_par),length.out=1000),
                                                               DOY_off = mean(DOY_off),
                                                               length_off = mean(length_off),
                                                               sex_off = "Male", distance_mouth_off = NA,se.fit=NA,
                                                               zone = "Intertidal"))
distance_pred_intertidal_stockdale$distance_mouth_off <- predict(best_distance_intertidal_stockdale,newdata = distance_pred_intertidal_stockdale)
distance_pred_intertidal_stockdale$se.fit <- predict(best_distance_intertidal_stockdale,newdata = distance_pred_intertidal_stockdale,se.fit = T)$se.fit



distance_pred_IU_separate <- rbind(distance_pred_intertidal_erb, distance_pred_upstream_erb,
      distance_pred_intertidal_gilmour, distance_pred_upstream_gilmour,
      distance_pred_intertidal_hogan, distance_pred_upstream_hogan,
      distance_pred_intertidal_stockdale, distance_pred_upstream_stockdale)

distance_pred_IU_separate<-distance_pred_IU_separate %>% left_join(high_tide_line,by="stream_off")%>% 
  filter((distance_mouth_off <= htide & zone == "Intertidal") | 
           (distance_mouth_off > htide & zone == "Upstream"))

IU_model_data_separate <- model_data %>% left_join(high_tide_line,by="stream_off") %>% 
  filter((distance_mouth_off <= htide & intertidal_off == "Intertidal") | 
           (distance_mouth_off > htide & intertidal_off == "Upstream"))
IU_model_data_separate <- model_data %>% mutate(zone=intertidal_off) #%>% filter(distance_mouth_off<1200)


IU_separate_plot<-distance_pred_IU_separate %>% mutate(zone = factor(zone,levels = c("Upstream","Intertidal"))) %>% 
  ggplot(aes(y=distance_mouth_off,x=distance_mouth_par,color=stream_off))+
  #geom_vline(data=high_tide_line,aes(xintercept=htide,color=stream_off),show.legend = F,lty=2,lwd=0.8)+
  
  #geom_segment(data=high_tide_line,aes(x=htide,y=-100,xend=htide,yend=-20,color=stream_off),
  #             lwd=1.2,show.legend = F)+
  #geom_segment(data=high_tide_line,aes(x=-175,xend=-20, y=htide,yend=htide,color=stream_off),
  #             lwd=1.2,show.legend = F)+
  #coord_cartesian(ylim = c(-20, 750),xlim=c(-100,1200), clip="on") +
  
  geom_jitter(data=IU_model_data_separate,aes(y=distance_mouth_off,x=distance_mouth_par,
                                  color=stream_off),
              pch=16,alpha = 0.3,width = 10,height=10,size=0.3)+
  guides(colour = guide_legend(override.aes = list(alpha=1,size=2)))+
  
  geom_ribbon(aes(ymin=distance_mouth_off-1.96*se.fit,ymax=distance_mouth_off+1.96*se.fit,
                  color = stream_off,fill=stream_off,
                  group = zone),alpha=0.3,show.legend = F)+
  geom_line(lwd=0.8,aes(color=stream_off,group = zone),show.legend = F)+
  geom_abline(slope=1,intercept=0,lty=2,lwd=0.5)+
  scale_color_discrete(type = colorz)+
  scale_fill_discrete(type = colorz)+
  theme_classic()+
  theme(legend.position = "none")+
  facet_grid(zone~stream_off, scales = "free_y")+
  labs(color="Stream",
       x="Inferred Spawning Location of Parent (river meters)",
       y = "Inferred Spawning Location\nof Offspring (river meters)")

IU_separate_plot

ggsave(filename = "../Data/Figure4_IU_Separate.png",plot = IU_separate_plot,
       units = "in", dpi = 300, width = 6, height = 4)










# Load necessary libraries
library(MuMIn)
library(tidyverse)
library(ggplot2)

# Function to run the model
run_model <- function(data, intertidal_off_value, stream_off_value) {
  data_filtered <- data %>% 
    filter(intertidal_off == intertidal_off_value, stream_off == stream_off_value)
  
  global_model <- lm(distance_mouth_off ~ distance_mouth_par + sex_off + DOY_off + length_off,
                     data = data_filtered, na.action = na.fail)
  distance_dredge <- dredge(global_model)
  
  # Sort the models based on AICc using model.sel()
  sorted_models <- model.sel(distance_dredge)
  
  # Get the best model
  best_model <- get.models(sorted_models, subset = 1)[[1]]
  best_model_formula <- formula(best_model)
  
  best_model <- lm(best_model_formula, data = data_filtered, na.action = na.fail)
  
  # Get the predictors in the best model
  predictors <- all.vars(best_model_formula)[-1]
  
  prediction <- predict(best_model, newdata = data_filtered[predictors], se.fit = TRUE)
  
  return(data.frame(intertidal_off = intertidal_off_value,
                    stream_off = stream_off_value,
                    distance_mouth_par = ifelse("distance_mouth_par" %in% predictors, data_filtered$distance_mouth_par, NA),
                    prediction = prediction$fit,
                    se.fit = prediction$se.fit))
}

# Define the combinations of 'intertidal_off' and 'stream_off'
conditions <- expand.grid(intertidal_off = c("Intertidal", "Upstream"),
                          stream_off = unique(model_data$stream_off))

# Run models for all combinations
predictions <- map2_dfr(conditions$intertidal_off, conditions$stream_off, ~run_model(model_data, .x, .y))

IU_separate_plot<-distance_pred_IU_separate %>% 
  ggplot(aes(y=distance_mouth_off,x=distance_mouth_par,color=stream_off))+
  #geom_vline(data=high_tide_line,aes(xintercept=htide,color=stream_off),show.legend = F,lty=2,lwd=0.8)+
  
  #geom_segment(data=high_tide_line,aes(x=htide,y=-100,xend=htide,yend=-20,color=stream_off),
  #             lwd=1.2,show.legend = F)+
  #geom_segment(data=high_tide_line,aes(x=-175,xend=-20, y=htide,yend=htide,color=stream_off),
  #             lwd=1.2,show.legend = F)+
  #coord_cartesian(ylim = c(-20, 750),xlim=c(-100,1200), clip="on") +
  
  geom_jitter(data=model_data %>% filter(distance_mouth_off<1200),aes(y=distance_mouth_off,x=distance_mouth_par,
                                  color=stream_off),
              pch=16,alpha = 0.3,width = 10,height=10,size=0.3)+
  guides(colour = guide_legend(override.aes = list(alpha=1,size=2)))+
  
  geom_ribbon(aes(ymin=distance_mouth_off-1.96*se.fit,ymax=distance_mouth_off+1.96*se.fit,
                  color = stream_off,fill=stream_off,
                  group = zone),alpha=0.3,show.legend = F)+
  geom_line(lwd=0.8,aes(color=stream_off,group = zone),show.legend = F)+
  geom_abline(slope=1,intercept=0,lty=2,lwd=0.5)+
  scale_color_discrete(type = colorz)+
  scale_fill_discrete(type = colorz)+
  theme_classic()+
  theme(legend.position = "none",strip.background = element_blank(),
        strip.text = element_blank())+
  facet_grid(stream_off~zone,scales="free")+
  labs(color="Stream",
       x="Inferred Spawning Location of Parent (river meters)",
       y = "Inferred Spawning Location\nof Offspring (river meters)")

IU_separate_plot



