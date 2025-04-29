
  #source("Rscripts/FormatData.R")

# Variables to define
Measures
Awareness_columns <- c(expr(Q4_oa), expr(Q4_rdm), expr(Q4_fair), expr(Q4_code), expr(Q4_material),expr(Q4_prereg),expr(Q4_rr))
Awareness_answers <- rev(c("practicing", "using", "aware",  "unsure",  "na" ))
Awareness_colors <- c("#ABDDA4", "#FFFFBF", "#FDAE61", "#D7191C", "#bdbdbd")
Awareness_legend <- rev(c("Practicing myself", "Accessing / using only", "Aware only",  "Not aware",  "Not applicable" ))

# create dataset for plotting
data_Awareness_for_plotting <- prepare_data_for_plotting(Measures, data_Awareness, Awareness_answers, Awareness_columns)
EachMeasureAwareness_ss <- data.frame(data_Awareness_for_plotting %>% group_by(LabelIndiv) %>% summarise(N = sum(n, na.rm=TRUE)))
EachMeasureAwareness_ss




title_plot_Awareness <- paste ("Awareness of open research pratices
(all researchers, N=", as.numeric(data_Awareness_ss), ")", sep="")

Awareness_plot <- horizontal_stacked_barplot(data_Awareness_for_plotting, 
                      Measures, 
                      Awareness_answers, 
                      Awareness_colors, 
                      title_plot = title_plot_Awareness, 
                      Awareness_legend,
                      legend_position = "bottom")
Awareness_plot
ggsave(here::here("Figures", "Awareness_plot.png"), width = 10, height = 4, bg = "white")

Awareness_plot_pres <- horizontal_stacked_barplot(data_Awareness_for_plotting, 
                                             Measures, 
                                             Awareness_answers, 
                                             Awareness_colors, 
                                             title_plot = NULL, 
                                             Awareness_legend,
                                             legend_position = "bottom")
Awareness_plot_pres
