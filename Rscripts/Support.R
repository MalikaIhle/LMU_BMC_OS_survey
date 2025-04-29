
  #source("Rscripts/FormatData.R")

Supports <- c('Online Resources', 'Colloquium', 'Consulting', 'Mentoring', 'Support Networks')
Support_columns <- c(expr(Q11_online), expr(Q11_colloquium), expr(Q11_consulting), expr(Q11_mentoring),expr(Q11_network))
Support_answers <- rev(c("essential", "useful", "unsure", "useless"))
Support_colors <- rev(c("black", "#666666", "#6BAED6", '#08519C'))
Support_legend <- rev(c("Essential", "Useful", "Not sure", "Not useful"))

# create dataset for plotting
data_Support_for_plotting <- prepare_data_for_plotting(Supports, data_Support, Support_answers, Support_columns)
EachMeasureSupport_ss <- data.frame(data_Training_for_plotting %>% group_by(LabelIndiv) %>% summarise(N = sum(n, na.rm=TRUE)))
EachMeasureSupport_ss

title_plot_Support <- paste ("Support needs to adopt open research pratices 
(all researchers, N=", as.numeric(data_Support_ss), ")", sep="")

Support_plot <- horizontal_stacked_barplot(data_Support_for_plotting, 
                                            Supports, 
                                            Support_answers, 
                                            Support_colors, 
                                            title_plot = title_plot_Support, 
                                            Support_legend,
                                            legend_position = "bottom")
Support_plot
ggsave(here::here("Figures", "Support_plot.png"), width = 10, height = 4, bg = "white")

Support_plot_pres <- horizontal_stacked_barplot(data_Support_for_plotting, 
                                                Supports, 
                                                Support_answers, 
                                                Support_colors, 
                                                title_plot = NULL, 
                                                Support_legend,
                                                legend_position = "bottom")
