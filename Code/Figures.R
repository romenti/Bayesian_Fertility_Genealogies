#### Code to reproduce paper figures ####

#### Upload packages ####

source('upload_packages.R')


#### Figure 1 ####

load('Data/gen_population_pyramids.RData')

plot_pyramid = data_pyramid %>%
  filter(country %in% c('USA','SWE','ENG')) %>%
  mutate(count_gen=ifelse(gender=="female",count_gen*(-1),count_gen)) %>%
  ggplot()+  # default x-axis is age in years;
  # case data graph
  geom_col(mapping = aes(
    x = Age,
    y = count_gen,
    fill = gender),         
    colour = "white")+       # white around each bar
  geom_step(data =  data_plot_new   %>% filter(gender == "female",country %in% c('ENG','SWE','USA')), 
            aes(x = Age, y= expected, color = label),size=1) +
  geom_step(data =  data_plot_new   %>% filter(gender == "male",country %in% c('ENG','SWE','USA')), 
            aes(x = Age, y = (-1)*expected,color=label),size=1) +
  
  # flip the X and Y axes to make pyramid vertical
  coord_flip()+
  scale_fill_manual(name = "Sex",
                    values = c("female" = "#990099",
                               "male" = "#009900"),
                    labels = c("Female", "Male"))+
  scale_color_manual(values = c('true population' = "black"),name="",labels = c("Expected Population Structure"),na.translate = F)+
  ggthemes::theme_fivethirtyeight()+
  scale_y_continuous(labels = abs)+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1),
        legend.position = 'bottom',
        rect = element_rect(fill = 'white'),
        plot.background = element_rect(fill = "white", colour = "white"),
        #legend.background = element_rect(fill="lightgray", size=0.5, linetype="solid"),
        axis.title.y = element_text(face="bold"),
        axis.title.x = element_text(face="bold"),
        strip.text.x = element_text(face="bold"), 
        strip.text.y = element_text(face="bold"),
        aspect.ratio = 0.66)+
  facet_wrap(country~years,scales = "free",nrow=3)+
  ylab("Genealogy Population Size")+
  xlab("Age")

ggsave("Figures/Figure1.pdf",plot_pyramid,height = 15, width = 25, units = "cm",dpi=700)



#### Figure 2 ####

load('Data/motivation_datasets.RData')

plot_missing_age = missing_age %>%
  ggplot(aes(x = years, y = perc*100)) +
  geom_line(size = 1) +
  scale_x_continuous(breaks = c(1751, seq(1770, 1910, 20)),
                     labels = c(1751, seq(1770, 1910, 20))) +
  scale_y_continuous(labels = function(x) paste0(x, "%")) +  # <- ADD THIS LINE
  facet_wrap(~country, nrow = 2) +
  ggthemes::theme_fivethirtyeight(base_size = 18) +
  ylab("% of Children under 5\nwith Missing Mother Age") +
  xlab("Year") +
  guides(color = guide_legend(override.aes = list(size = 4))) +
  theme(
    axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, size = 14),
    axis.text.y = element_text(size = 14),
    legend.position = "bottom",
    legend.text = element_text(face = "bold", size = 16),
    legend.title = element_text(face = "bold", size = 16),
    rect = element_rect(fill = 'white'),
    plot.background = element_rect(fill = "white", colour = "white"),
    strip.text.x = element_text(face = "bold", size = 16),
    strip.text.y = element_text(face = "bold", size = 16),
    axis.title.y = element_text(face = "bold", size = 16),
    axis.title.x = element_text(face = "bold", size = 16),
    plot.title = element_text(hjust = 0.5, face = "bold")
  )

ggsave("Figures/Figure2.pdf",plot_missing_age,height = 15, width = 25, units = "cm",dpi=700)



#### Figure 4 ####

load('Results/bayesian_model_results.RData')
load('Results/indirect_TFR_results.RData')
load('Data/historical_TFR.RData')

plot_TFR = mcmc.out_TFR_final %>%
  mutate(country = factor(country,levels = c("SWE","ENG","FRA","DEN",
                                             "NOR","NLD","USA","FIN"))) %>%
  ggplot()+
  geom_line(aes(x=years,y=TFR_median,shape="bTFR*",color="bTFR*",linetype = "bTFR*"),size=1.5)+
  geom_line(data= data_iTFR,aes(x=years,y=iTFR_star,color="iTFR*",linetype="iTFR*"),size=1.5)+
  geom_line(data= data_xTFR,aes(x=years,y=xTFR_star,color="xTFR*",linetype="xTFR*"),size=1.5)+
  geom_point(data = hist_data %>% filter(years<=1910),aes(x=years,y=TFR_true,
                                                                    shape="Historical Estimates",color="Historical Estimates"),size=1.5)+
  geom_ribbon(data=mcmc.out_TFR_final ,aes(x=years,ymin = TFR_lower, ymax = TFR_upper),  linetype=2, alpha = 0.5,fill="#377EB8")+
  
  scale_x_continuous(breaks=c(1751,seq(1770,1910,20)),labels=c(1751,seq(1770,1910,20)))+
  scale_colour_manual(name = "",
                      values = c("bTFR*"="#377EB8", "Historical Estimates"="#FF0000","iTFR*"="#FFB000","xTFR*"="#4DAF4A")) +  
  scale_linetype_manual(name="",
                        values=c("bTFR*"="solid", "Historical Estimates"="dotted","iTFR*"="twodash","xTFR*"='dotted'))+
  scale_shape_manual(name = "",
                     values = c("bTFR*"=2,"Historical Estimates"=8))+
  facet_wrap(~country,nrow=2)+
  ggthemes::theme_fivethirtyeight(base_size = 18)+ 
  #coord_cartesian(ylim=c(2,9))+
  ylab("TFR Estimates")+
  xlab("Year")+
  guides(color = guide_legend(override.aes = list(size = 4))) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1,size=14),
        axis.text.y = element_text(size=14),
        legend.position = "none",
        legend.text = element_text(face="bold",size=16),
        legend.title  = element_text(face="bold",size=16),
        rect = element_rect(fill = 'white'),
        plot.background = element_rect(fill = "white", colour = "white"),
        aspect.ratio = 0.66,
        #legend.background = element_rect(fill="lightgray", size=0.5, linetype="solid"),
        strip.text.x = element_text(face="bold",size=16),
        strip.text.y = element_text(face="bold",size=16),
        axis.title.y = element_text(face="bold",size=16),
        axis.title.x = element_text(face="bold",size=16),
        plot.title = element_text(hjust=0.5,face="bold")
  ) 


legend_data <- data.frame(
  x = c(1, 2,1,2, 1, 2, 1, 1),  # Duplicate x values for line groups
  y = c(1, 1,1,1, 1, 1, 1, 1),
  type = c("iTFR*", "iTFR*","xTFR*","xTFR*" ,"bTFR*", "bTFR*", "Historical Estimates", "Historical Estimates")
)



manual_legend <- ggplot(legend_data, aes(x, y, group = type, linetype = type, color = type, shape = type)) +
  geom_line(size = 1, data = subset(legend_data, type != "Historical Estimates")) +  # Lines only for relevant types
  geom_point(size = 3, data = subset(legend_data, type == "Historical Estimates")) +  # Points for historical estimates
  scale_color_manual(name = "",
                     values = c("iTFR*" = "#FFB000",
                                "bTFR*" = "#377EB8",
                                "Historical Estimates" = "#FF0000",
                                "xTFR*"="#4DAF4A")) +
  scale_linetype_manual(name = "",
                        values = c("iTFR*" = "twodash",
                                   "bTFR*" = "solid",
                                   "Historical Estimates" = "blank",
                                   "xTFR*"="dotted")) +
  scale_shape_manual(name = "",
                     values = c("iTFR*" = NA,
                                "bTFR*" = NA,
                                "Historical Estimates" = 8,
                                "xTFR*"=NA)) +  
  theme_void() +
  theme(legend.text = element_text(face="bold", size=16),
        legend.title = element_text(face="bold", size=16),
        legend.key.width = unit(2, "cm"),
        legend.spacing.y = unit(2, "cm")
  ) +
  guides(
    color = guide_legend(nrow = 1),
    linetype = guide_legend(nrow = 1),
    shape = guide_legend(nrow = 1)
  )

legend_grob <- get_legend(manual_legend)

plot_TFR_final = plot_TFR/legend_grob+
  plot_layout(heights = c(10, 1))  

ggsave("Figures/Figure4.pdf",plot_TFR_final,height = 15, width = 25, units = "cm",dpi=700)


#### Figure 5 ####


bias_plot = mcmc.out_theta_final %>%
  ggplot()+
  geom_line(aes(x=years,y=theta_median,
                color='Bayesian Bias-Adjustement Multiplier',
                linetype='Bayesian Bias-Adjustement Multiplier'),
            size=2)+
  geom_line(data=data_multiplier,aes(x=years,y=log(multiplier),
                                     color='Indirect Bias-Adjustment Multiplier',
                                     linetype='Indirect Bias-Adjustment Multiplier'),size=2)+
  geom_hline(aes(yintercept=0),linetype='dotted',size=2)+
  #geom_point(data=canada_true_fertility,aes(x=years,y=TFR_true,shape="Historical Estimates"))+
  geom_ribbon(aes(x=years,ymin = theta_lower, ymax = theta_upper),  linetype=2, alpha = 0.5,fill="#377EB8")+
  theme_classic()+
  scale_color_manual(name='',values=c('Bayesian Bias-Adjustement Multiplier'='#377EB8',
                                      'Indirect Bias-Adjustment Multiplier'="#FFB000"))+
  scale_linetype_manual(name='',values=c('Bayesian Bias-Adjustement Multiplier'='solid',
                                         'Indirect Bias-Adjustment Multiplier'="twodash"))+
  scale_x_continuous(breaks=c(1751,seq(1770,1910,20)),labels=c(1751,seq(1770,1910,20)))+
  coord_cartesian(ylim=c(-1,0.35))+
  xlab('Year')+
  ylab('Bias-adjustment Multiplier')+
  ggthemes::theme_fivethirtyeight(base_size = 18)+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1,size=14),
        axis.text.y = element_text(size=14),
        legend.position = "bottom",
        aspect.ratio = 0.66,
        legend.text = element_text(face="bold",size=16),
        legend.title  = element_text(face="bold",size=16),
        #legend.background = element_rect(fill="lightgray", size=0.5, linetype="solid"),
        strip.text.x = element_text(face="bold",size=16),
        strip.text.y = element_text(face="bold",size=16),
        axis.title.y = element_text(face="bold",size=16),
        axis.title.x = element_text(face="bold",size=16),
        plot.title = element_text(hjust=0.5,face="bold"),
        rect = element_rect(fill = 'white'),
        plot.background = element_rect(fill = "white", colour = "white")
  ) +
  facet_wrap(~country,nrow=2)


ggsave("Figures/Figure5.pdf",bias_plot,height = 15, width = 25, units = "cm",dpi=700)

