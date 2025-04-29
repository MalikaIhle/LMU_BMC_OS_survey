  # source("Rscripts/FormatData.R")
  source(here::here("Rscripts","CurrentCriteria.R"))
  source(here::here("Rscripts","DesiredCriteria.R"))
 
# PI Current Recruitment
PIs_CurrentCriteria_for_plotting$LabelIndiv <- factor(PIs_CurrentCriteria_for_plotting$LabelIndiv, levels = rev(Criteria)) # this will determine order of the bars
  
Leftplot_PIs <- PIs_CurrentCriteria_for_plotting %>% 
    ggplot() +
    
    ### Add the stacked bar
    geom_bar(aes(x=LabelIndiv, y=(perc/100), fill=factor(Answer, level = CurrentCriteria_answers)),
             stat="identity") +
    scale_y_continuous(labels = c("100%", "75%", "50%", "25%", "0%")) +
    scale_fill_manual(values = rev(CurrentCriteria_colors), 
                      breaks = CurrentCriteria_answers, 
                      labels = CurrentCriteria_legend, 
                      drop = FALSE) +
    coord_flip() +
    theme_minimal() +
    theme(
      legend.position = "none", 
      axis.title = element_blank(),
      panel.grid.major.x = element_blank(),
      panel.grid.minor.y = element_blank(),
      legend.title=element_blank(),
      axis.text.y = element_text(size = 11, colour = "black"),
      plot.title = element_text(lineheight=.8, face="bold", hjust = 0.5)) + 
    ggtitle(PIs_title_plot_CurrentCriteria) + 
    guides(fill = guide_legend(nrow = 1, reverse = FALSE))
  
Leftplot_PIs 


## Future Recruitment

PIs_DesiredCriteria_for_plotting$LabelIndiv <- factor(PIs_DesiredCriteria_for_plotting$LabelIndiv, levels = rev(Criteria)) # this will determine order of the bars

Rightplot_PIs <- PIs_DesiredCriteria_for_plotting %>% 
  ggplot() +
  
  ### Add the stacked bar
  geom_bar(aes(x=LabelIndiv, y=(perc/100), fill=factor(Answer, level = DesiredCriteria_answers)),
           stat="identity") +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_manual(values = rev(DesiredCriteria_colors), 
                    breaks = DesiredCriteria_answers, 
                    labels = DesiredCriteria_legend, 
                    drop = FALSE) +
  scale_x_discrete(position = "top")+
  coord_flip() +
  theme_minimal() +
  theme(
    legend.position = "bottom", 
    axis.title = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    legend.title=element_blank(),
    axis.text.y = element_text(size = 11, colour = "black"),
    plot.title = element_text(lineheight=.8, face="bold", hjust = 0.5)) + 
  ggtitle(PIs_title_plot_DesiredCriteria) + 
  guides(fill = guide_legend(nrow = 1, reverse = TRUE))

Rightplot_PIs 


# PI combined
CombinedCriteria_PIs <- ggpubr::ggarrange(Leftplot_PIs,Rightplot_PIs, ncol=2, nrow=1, common.legend = TRUE, legend="bottom")
CombinedCriteria_PIs
ggsave(here::here("Figures", "CriteriaCombined_PIs.png"), width = 13, height = 7, bg = "white")


#----------- all respondents

CurrentCriteria_for_plotting$LabelIndiv <- factor(CurrentCriteria_for_plotting$LabelIndiv, levels = rev(Criteria)) # this will determine order of the bars

Leftplot <- CurrentCriteria_for_plotting %>% 
  ggplot() +
  
  ### Add the stacked bar
  geom_bar(aes(x=LabelIndiv, y=(perc/100), fill=factor(Answer, level = CurrentCriteria_answers)),
           stat="identity") +
  scale_y_continuous(labels = c("100%", "75%", "50%", "25%", "0%")) +
  scale_fill_manual(values = rev(CurrentCriteria_colors), 
                    breaks = CurrentCriteria_answers, 
                    labels = CurrentCriteria_legend, 
                    drop = FALSE) +
  coord_flip() +
  theme_minimal() +
  theme(
    legend.position = "none", 
    axis.title = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    legend.title=element_blank(),
    axis.text.y = element_text(size = 11, colour = "black"),
    plot.title = element_text(lineheight=.8, face="bold", hjust = 0.5)) + 
  ggtitle(title_plot_CurrentCriteria) + 
  guides(fill = guide_legend(nrow = 1, reverse = FALSE))

Leftplot 


## Future Recruitment

DesiredCriteria_for_plotting$LabelIndiv <- factor(DesiredCriteria_for_plotting$LabelIndiv, levels = rev(Criteria)) # this will determine order of the bars

Rightplot <- DesiredCriteria_for_plotting %>% 
  ggplot() +
  
  ### Add the stacked bar
  geom_bar(aes(x=LabelIndiv, y=(perc/100), fill=factor(Answer, level = DesiredCriteria_answers)),
           stat="identity") +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_manual(values = rev(DesiredCriteria_colors), 
                    breaks = DesiredCriteria_answers, 
                    labels = DesiredCriteria_legend, 
                    drop = FALSE) +
  scale_x_discrete(position = "top")+
  coord_flip() +
  theme_minimal() +
  theme(
    legend.position = "bottom", 
    axis.title = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    legend.title=element_blank(),
    axis.text.y = element_text(size = 11, colour = "black"),
    plot.title = element_text(lineheight=.8, face="bold", hjust = 0.5)) + 
  ggtitle(title_plot_DesiredCriteria) + 
  guides(fill = guide_legend(nrow = 1, reverse = TRUE))

Rightplot 


# combined
CombinedCriteria <- ggpubr::ggarrange(Leftplot,Rightplot, ncol=2, nrow=1, common.legend = TRUE, legend="bottom")
CombinedCriteria
ggsave(here::here("Figures", "CriteriaCombined.png"), width = 13, height = 7, bg = "white")

