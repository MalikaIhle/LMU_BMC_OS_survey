
#source("Rscripts/FormatData.R")

Criteria
DesiredCriteria_columns <- c(expr(Q9_nb_pub), expr(Q9_IF), expr(Q9_quality), expr(Q9_author_role),expr(Q9_citation),
                             expr(Q9_grant),expr(Q9_impact),expr(Q9_teaching),expr(Q9_supervision),expr(Q9_services),
                             expr(Q9_citizenship),expr(Q9_awards),expr(Q9_network),expr(Q9_ORPs))
DesiredCriteria_answers <- rev(c("considerably", "moderately", "slightly", "none","unsure", "na"))
DesiredCriteria_colors <- rev(c("#bdbdbd", "#666666", "#FDE0DD",'#FA9FB5',"#F768A1",'#DD3497'))
DesiredCriteria_legend <- rev(c("Considerably", "Moderately", "Slightly", "Not at all","Not sure", "Not applicable"))


# create dataset for plotting per Divisions
DesiredCriteria_for_plotting <- prepare_data_for_plotting(Criteria, data_DesiredCriteria, DesiredCriteria_answers, DesiredCriteria_columns)
EachDesiredCriteria_ss <- data.frame(DesiredCriteria_for_plotting %>% group_by(LabelIndiv) %>% summarise(N = sum(n, na.rm=TRUE)))


title_plot_DesiredCriteria <- paste ("Desired future recruitment criteria
(all researchers, N=", as.numeric(data_DesiredCriteria_ss), ")", sep="")

# plot regrouped data 

plot_All_DesiredCriteria <- horizontal_stacked_barplot (DesiredCriteria_for_plotting, Criteria
                            , DesiredCriteria_answers, DesiredCriteria_colors
                            , title_plot_DesiredCriteria, DesiredCriteria_legend, "bottom")

plot_All_DesiredCriteria

# Subset to only PIs
PIs_DesiredCriteria_for_plotting <- prepare_data_for_plotting(Criteria, data_DesiredCriteria[data_DesiredCriteria$Q2 == "PIs",], DesiredCriteria_answers, DesiredCriteria_columns)

PIs_title_plot_DesiredCriteria <- paste ("Desired future recruitment criteria
(group leaders, N=", as.numeric(nrow(data_DesiredCriteria[data_DesiredCriteria$Q2 == "PIs",])), ")", sep="")


plot_PIs_DesiredCriteria <- horizontal_stacked_barplot (PIs_DesiredCriteria_for_plotting, Criteria
                                                        , DesiredCriteria_answers, DesiredCriteria_colors
                                                        , PIs_title_plot_DesiredCriteria, DesiredCriteria_legend, "bottom")
plot_PIs_DesiredCriteria

