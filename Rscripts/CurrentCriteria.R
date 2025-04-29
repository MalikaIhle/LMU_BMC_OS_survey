
  #source("Rscripts/FormatData.R")

Criteria
CurrentCriteria_columns <- c(expr(Q8_nb_pub), expr(Q8_IF), expr(Q8_quality), expr(Q8_author_role),expr(Q8_citation),
                      expr(Q8_grant),expr(Q8_impact),expr(Q8_teaching),expr(Q8_supervision),expr(Q8_services),
                      expr(Q8_citizenship),expr(Q8_awards),expr(Q8_network),expr(Q8_ORPs))
CurrentCriteria_answers <- c("considerably", "moderately", "slightly", "none","unsure", "na")
CurrentCriteria_colors <- c("#bdbdbd", "#666666", "#FDE0DD",'#FA9FB5',"#F768A1",'#DD3497')
CurrentCriteria_legend <- c("Considerably", "Moderately", "Slightly", "Not at all","Not sure", "Not applicable")

# create dataset for plotting per Divisions
CurrentCriteria_for_plotting <- prepare_data_for_plotting(Criteria, data_CurrentCriteria, CurrentCriteria_answers, CurrentCriteria_columns)
EachCurrentCriteria_ss <- data.frame(CurrentCriteria_for_plotting %>% group_by(LabelIndiv) %>% summarise(N = sum(n, na.rm=TRUE)))

# plot regrouped data 
title_plot_CurrentCriteria <- paste ("Perceived current recruitment criteria
(all researchers, N=", as.numeric(data_CurrentCriteria_ss), ")", sep="")


plot_All_CurrentCriteria <- horizontal_stacked_barplot (CurrentCriteria_for_plotting, Criteria
                            , CurrentCriteria_answers, CurrentCriteria_colors
                            , title_plot_CurrentCriteria, CurrentCriteria_legend, "bottom")
plot_All_CurrentCriteria

# Subset to only PIs
PIs_CurrentCriteria_for_plotting <- prepare_data_for_plotting(Criteria, data_CurrentCriteria[data_CurrentCriteria$Q2 == "PIs",], CurrentCriteria_answers, CurrentCriteria_columns)

PIs_title_plot_CurrentCriteria <- paste ("Perceived current recruitment criteria
(group leaders, N=", as.numeric(nrow(data_CurrentCriteria[data_CurrentCriteria$Q2 == "PIs",])), ")", sep="")


plot_PIs_CurrentCriteria <- horizontal_stacked_barplot (PIs_CurrentCriteria_for_plotting, Criteria
                                                        , CurrentCriteria_answers, CurrentCriteria_colors
                                                        , PIs_title_plot_CurrentCriteria, CurrentCriteria_legend, "bottom")
plot_PIs_CurrentCriteria

# Subset to all but PIs
# nonPIs_CurrentCriteria_for_plotting <- prepare_data_for_plotting(Criteria, data_CurrentCriteria[data_CurrentCriteria$Q2 != "PIs",], CurrentCriteria_answers, CurrentCriteria_columns)
# 
# nonPIs_title_plot_CurrentCriteria <- paste ("Perceived current recruitment criteria
# (non group leaders, N=", as.numeric(nrow(data_CurrentCriteria[data_CurrentCriteria$Q2 != "PIs",])), ")", sep="")
# 
# 
# plot_nonPIs_CurrentCriteria <- horizontal_stacked_barplot (nonPIs_CurrentCriteria_for_plotting, Criteria
#                                                         , CurrentCriteria_answers, CurrentCriteria_colors
#                                                         , nonPIs_title_plot_CurrentCriteria, CurrentCriteria_legend, "bottom")
# plot_nonPIs_CurrentCriteria
