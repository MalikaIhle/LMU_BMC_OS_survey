
  #source("Rscripts/FormatData.R")

Trainings <- c('Preprint/Postprint', 'RDM plan', 'FAIR Data sharing','Ethics','Data anonymisation', 'Code sharing', 'Materials sharing', 'Licences', 'Preregistration', 'Recruitment')
Training_columns <- c(expr(Q10_oa), expr(Q10_rdm), expr(Q10_fair), expr(Q10_ethics),expr(Q10_privacy),
                      expr(Q10_code),expr(Q10_material),expr(Q10_license), 
                       expr(Q10_prereg), expr(Q10_recruitment))
Training_answers <- rev(c("workshop", "written", "none_needed", "none_wanted", "unsure",  "na" ))
#Training_colors <- c("black", "#666666", "#ABDDA4", "#FFFFBF", '#FDAE61', '#D7191C')
Training_colors <- rev(c("#bdbdbd", "#666666",  "#ffffcc", "#c2e699", "#78c679", '#238443'))
Training_legend <- rev(c("Workshop", "Written guidance only", "No guidance needed", "No guidance wanted",  "Not sure",  "Not applicable                                                          " ))

# create dataset for plotting
data_Training_for_plotting <- prepare_data_for_plotting(Trainings, data_Training, Training_answers, Training_columns)
EachMeasureTraining_ss <- data.frame(data_Training_for_plotting %>% group_by(LabelIndiv) %>% summarise(N = sum(n, na.rm=TRUE)))
EachMeasureTraining_ss 

title_plot_Training <- paste ("Open research practices training needs
(all researchers, N=", as.numeric(data_Training_ss), ")", sep="")

Training_plot <- horizontal_stacked_barplot(data_Training_for_plotting, 
                                            Trainings, 
                                             Training_answers, 
                                             Training_colors, 
                                             title_plot = title_plot_Training, 
                                             Training_legend,
                                             legend_position = "bottom")
Training_plot
ggsave(here::here("Figures", "Training_plot.png"), width = 10, height = 4, bg = "white")

Training_plot_pres <- horizontal_stacked_barplot(data_Training_for_plotting, 
                                                 Trainings, 
                                                 Training_answers, 
                                                 Training_colors, 
                                                 title_plot = NULL, 
                                                 Training_legend,
                                                 legend_position = "bottom")