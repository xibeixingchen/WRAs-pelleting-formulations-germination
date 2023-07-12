# load packages
library(tidyverse)
library(ggsci)
library(ggfun)
library(agricolae)
## read data
dat = read.csv("WRAs_germination.csv") 

## set a duncan test function
duncan_test = function(data){
  model <- aov(value ~ group, data = data)
  out <- agricolae::duncan.test(model, "group")
  aa = out$groups
  aa$group = row.names(aa)
  return(aa)
}

# pivot dat to long data and Batch Duncan analysis
aov = dat %>%
  pivot_longer(-group,values_to = 'value',names_to = 'feature') %>% 
  group_by(feature) %>% 
  nest %>% 
  mutate(sig = map(data,~duncan_test(data = .))) %>% 
  unnest(sig)
## Statistical analysis
stat = dat %>% 
  pivot_longer(-group,values_to = 'value',names_to = 'feature') %>% 
  group_by(feature,group) %>% 
  get_summary_stats()
## Integrated result
aov1 = stat %>% left_join(aov,by = c('group','feature')) %>% 
  select(1,2,12,14,18) %>% 
  mutate(sig = paste(round(mean,2),'±',round(se,2),groups,sep = ''),
         id = 1:n()) %>% 
  select(2,1,6,7) %>% 
  pivot_wider(-id,names_from = group,values_from = sig) 
#view result
view(aov1)

## Plot data preparation
df = stat %>% left_join(aov,by = c('group','feature')) %>% 
  select(1,2,12,14,18)
df$group =factor(df$group,levels = c('CK0','CK1','CK2','BS0.5','BS1','BS2','HS0.5','HS1','HS2'))

##plot
ggplot(df,aes(x =group,y = mean,fill =group))+
  geom_col()+
  geom_errorbar(aes(ymax = mean + se, 
                    ymin = mean - se
  ),width =0.4,size=0.5,
  alpha = 0.7)+
  geom_text(aes(label = groups, y = mean + se + 0.1*(mean+se)),size=4)+
  facet_wrap(ggplot2::vars(feature),scales = 'free_y')+
  labs(x='WARs',y='Values',color='Legends')+
  scale_fill_npg()+
  theme_bw()+
  theme(axis.text = element_text(color = "black", size = 10),
        axis.text.x = element_text(angle = 30, vjust = 1, hjust = 1),
        legend.position = 'none',
        panel.spacing.y =unit(0,'cm'),
        panel.border = element_rect(linewidth =1),
        strip.background = element_rect(linewidth = 1),
        strip.text = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        legend.background=element_roundrect(color="gray", linetype=1))
