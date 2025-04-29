
  #source("Rscripts/FormatData.R")

# Variables to define
Measures
Downsides_columns <- c(expr(Q7_oa), expr(Q7_rdm), expr(Q7_fair), expr(Q7_code), expr(Q7_material),expr(Q7_prereg),expr(Q7_rr))
Downsides_answers <- rev(c("no", "yes",  "unsure",  "na" ))
Downsides_colors <- rev(c("#bdbdbd", "#666666", "#D95F02", "#1B9E77"))
Downsides_legend <- rev(c("No", "Yes",  "Not sure",  "Not applicable" ))

# create dataset for plotting
data_Downsides_for_plotting <- prepare_data_for_plotting(Measures, data_Downsides, Downsides_answers, Downsides_columns)
EachMeasureDownsides_ss <- data.frame(data_Downsides_for_plotting %>% group_by(LabelIndiv) %>% summarise(N = sum(n, na.rm=TRUE)))
EachMeasureDownsides_ss 
no_downside_all <- round(summary(data_Downsides_for_plotting$perc[data_Downsides_for_plotting$Answer == "no"]),1)
unsure_downside_all <- round(summary(data_Downsides_for_plotting$perc[data_Downsides_for_plotting$Answer == "unsure"]),1)
yes_downside_all <- round(summary(data_Downsides_for_plotting$perc[data_Downsides_for_plotting$Answer == "yes"]),1)
nyes_downside_all <- sum(data_Downsides_for_plotting$n[data_Downsides_for_plotting$Answer == "yes"])

title_plot_Downsides <- paste ("Downsides of open research pratices
(all researchers, N=", as.numeric(data_Downsides_ss), ")", sep="")

Downsides_plot <- horizontal_stacked_barplot(data_Downsides_for_plotting, 
                                             Measures, 
                                             Downsides_answers, 
                                             Downsides_colors, 
                                             title_plot = title_plot_Downsides, 
                                             Downsides_legend,
                                             legend_position = "bottom")
Downsides_plot
ggsave(here::here("Figures", "Downsides_plot.png"), width = 10, height = 4, bg = "white")

