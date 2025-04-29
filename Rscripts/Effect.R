
  #source("Rscripts/FormatData.R")

Measures
Effect_columns <- c(expr(Q5_oa), expr(Q5_rdm), expr(Q5_fair), expr(Q5_code), expr(Q5_material),expr(Q5_prereg),expr(Q5_rr))
Effect_answers <- rev(c("beneficial", "neutral", "detrimental","unsure", "na"))
Effect_colors <- rev(c("#bdbdbd", "#666666", "#D95F02", "#E6AB02", "#1B9E77"))
Effect_legend <- rev(c("Beneficial", "Neutral", "Detrimental","Not sure", "Not applicable"))


# create dataset for plotting
data_Effect_for_plotting <- prepare_data_for_plotting(Measures, data_Effect, Effect_answers, Effect_columns)
EachMeasureEffect_ss <- data.frame(data_Effect_for_plotting %>% group_by(LabelIndiv) %>% summarise(N = sum(n, na.rm=TRUE)))
EachMeasureEffect_ss


title_plot_Effect <- paste ("Effect of widespread adoption of open research pratices
(all researchers, N=", as.numeric(data_Effect_ss), ")", sep="")

Effect_plot <- horizontal_stacked_barplot(data_Effect_for_plotting, 
                                             Measures, 
                                             Effect_answers, 
                                             Effect_colors, 
                                             title_plot = title_plot_Effect, 
                                             Effect_legend,
                                             legend_position = "bottom")
Effect_plot
ggsave(here::here("Figures", "Effect_plot.png"), width = 10, height = 4, bg = "white")

Effect_plot_pres <- horizontal_stacked_barplot(data_Effect_for_plotting, 
                                          Measures, 
                                          Effect_answers, 
                                          Effect_colors, 
                                          title_plot = NULL, 
                                          Effect_legend,
                                          legend_position = "bottom")
