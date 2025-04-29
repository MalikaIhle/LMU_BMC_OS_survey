  # source("Rscripts/FormatData.R")

source(here::here("Rscripts","CriteriaCombined.R"))
head(data_CurrentCriteria)
head(data_DesiredCriteria)

# recoding with numerical values: 
## means "Not sure", "Not applicable", essentially becomes "Not at all" i.e. 0 !!!!

data_CurrentCriteria[!is.na(data_CurrentCriteria) 
                     & data_CurrentCriteria == "considerably"] <- 3
data_CurrentCriteria[!is.na(data_CurrentCriteria) 
                     & data_CurrentCriteria == "moderately"] <- 2
data_CurrentCriteria[!is.na(data_CurrentCriteria) 
                     & data_CurrentCriteria == "slightly"] <- 1
data_CurrentCriteria[!is.na(data_CurrentCriteria) 
                     & (data_CurrentCriteria == "none" 
                        | data_CurrentCriteria == "unsure" 
                        | data_CurrentCriteria == "na")] <- 0

str(data_CurrentCriteria)
data_CurrentCriteria_all_num <- data_CurrentCriteria[,c(-1,-16,-17,-18)]
str(data_CurrentCriteria_all_num)
data_CurrentCriteria_all_num <- sapply(data_CurrentCriteria_all_num, as.numeric )
str(data_CurrentCriteria_all_num)


data_DesiredCriteria[!is.na(data_DesiredCriteria) 
                     & data_DesiredCriteria == "considerably"] <- 3
data_DesiredCriteria[!is.na(data_DesiredCriteria) 
                     & data_DesiredCriteria == "moderately"] <- 2
data_DesiredCriteria[!is.na(data_DesiredCriteria) 
                     & data_DesiredCriteria == "slightly"] <- 1
data_DesiredCriteria[!is.na(data_DesiredCriteria) 
                     & (data_DesiredCriteria == "none" 
                        | data_DesiredCriteria == "unsure" 
                        | data_DesiredCriteria == "na")] <- 0

# average these numbers

str(data_DesiredCriteria)
data_DesiredCriteria_all_num <- data_DesiredCriteria[,c(-1,-16,-17,-18)]
str(data_DesiredCriteria_all_num)
data_DesiredCriteria_all_num <- sapply(data_DesiredCriteria_all_num, as.numeric )
str(data_DesiredCriteria_all_num)

means_current <- colMeans(data_CurrentCriteria_all_num, na.rm = TRUE)
means_desired <- colMeans(data_DesiredCriteria_all_num, na.rm = TRUE)
delta <- means_desired-means_current
data_CriteriaDiff <- data.frame(cbind(means_current, means_desired, delta))
data_CriteriaDiff$criteria <- Criteria
data_CriteriaDiff <- data_CriteriaDiff %>% arrange(means_desired) %>% mutate(desired_order = 1:14)
data_CriteriaDiff <- data_CriteriaDiff %>% arrange(-means_current) %>% mutate(current_order = 1:14)
data_CriteriaDiff <- data_CriteriaDiff %>% arrange(-means_desired) %>% mutate(revdesired_order = 1:14)
data_CriteriaDiff <- data_CriteriaDiff %>% arrange(-delta) %>% mutate(delta_order = 1:14)
#rownames(data_CriteriaDiff) <- NULL
data_CriteriaDiff

## sorted by delta_order
{
# title_plot <- "To what extent are (●) vs should (○) the following criteria be used for recruitment?"
# 
# ranks_desired <- data.frame(
#   x = rep(-0.3,14),
#   y = data_CriteriaDiff$delta_order,
#   label = data_CriteriaDiff$revdesired_order
# )
# 
# ranks_current <- data.frame(
#   x = rep(-0.4,14),
#   y = data_CriteriaDiff$delta_order,
#   label = data_CriteriaDiff$current_order
# )
# 
# 
# data_CriteriaDiff %>% arrange (delta_order) %>%
#   ggplot() +
#   geom_point(aes(y = delta_order+0.04, x=means_current ), size = 3) +
#   geom_point(aes(y = delta_order-0.04, x=means_desired), shape= 1, size = 3)+
#   scale_y_continuous(breaks= 1:14, labels = data_CriteriaDiff$criteria[order(data_CriteriaDiff$delta_order, decreasing = FALSE)]) +
#   scale_x_continuous(limits=c(-0.5,3.2), breaks= 0:3, labels = c("Not at all", "Slightly", "Moderately", "Considerably") )+
#   theme_minimal() +
#   annotate("segment", x = data_CriteriaDiff$means_current[data_CriteriaDiff$delta_order==14]-0.04, xend = data_CriteriaDiff$means_desired[data_CriteriaDiff$delta_order==14]+0.04, y= 14, colour = "red", arrow=arrow(length = unit(0.2, "cm")))+
#   annotate("segment", x = data_CriteriaDiff$means_current[data_CriteriaDiff$delta_order==13]-0.04, xend = data_CriteriaDiff$means_desired[data_CriteriaDiff$delta_order==13]+0.04, y= 13, colour = "red", arrow=arrow(length = unit(0.2, "cm")))+
#   annotate("segment", x = data_CriteriaDiff$means_current[data_CriteriaDiff$delta_order==12]-0.04, xend = data_CriteriaDiff$means_desired[data_CriteriaDiff$delta_order==12]+0.04, y= 12, colour = "red", arrow=arrow(length = unit(0.2, "cm")))+
#   annotate("segment", x = data_CriteriaDiff$means_current[data_CriteriaDiff$delta_order==11]-0.04, xend = data_CriteriaDiff$means_desired[data_CriteriaDiff$delta_order==11]+0.04, y= 11, colour = "red", arrow=arrow(length = unit(0.2, "cm")))+
#   annotate("segment", x = data_CriteriaDiff$means_current[data_CriteriaDiff$delta_order==10]-0.04, xend = data_CriteriaDiff$means_desired[data_CriteriaDiff$delta_order==10]+0.04, y= 10, colour = "red", arrow=arrow(length = unit(0.2, "cm")))+
#   annotate("segment", x = data_CriteriaDiff$means_current[data_CriteriaDiff$delta_order==7]+0.04, xend = data_CriteriaDiff$means_desired[data_CriteriaDiff$delta_order==7]-0.04, y= 7, colour = "red", arrow=arrow(length = unit(0.2, "cm")))+
#   annotate("segment", x = data_CriteriaDiff$means_current[data_CriteriaDiff$delta_order==6]+0.04, xend = data_CriteriaDiff$means_desired[data_CriteriaDiff$delta_order==6]-0.04, y= 6, colour = "red", arrow=arrow(length = unit(0.2, "cm")))+
#   annotate("segment", x = data_CriteriaDiff$means_current[data_CriteriaDiff$delta_order==5]+0.04, xend = data_CriteriaDiff$means_desired[data_CriteriaDiff$delta_order==5]-0.04, y= 5, colour = "red", arrow=arrow(length = unit(0.2, "cm")))+
#   annotate("segment", x = data_CriteriaDiff$means_current[data_CriteriaDiff$delta_order==4]+0.04, xend = data_CriteriaDiff$means_desired[data_CriteriaDiff$delta_order==4]-0.04, y= 4, colour = "red", arrow=arrow(length = unit(0.2, "cm")))+
#   annotate("segment", x = data_CriteriaDiff$means_current[data_CriteriaDiff$delta_order==3]+0.04, xend = data_CriteriaDiff$means_desired[data_CriteriaDiff$delta_order==3]-0.04, y= 3, colour = "red", arrow=arrow(length = unit(0.2, "cm")))+
#   annotate("segment", x = data_CriteriaDiff$means_current[data_CriteriaDiff$delta_order==2]+0.04, xend = data_CriteriaDiff$means_desired[data_CriteriaDiff$delta_order==2]-0.04, y= 2, colour = "red", arrow=arrow(length = unit(0.2, "cm")))+
#   annotate("segment", x = data_CriteriaDiff$means_current[data_CriteriaDiff$delta_order==1]+0.04, xend = data_CriteriaDiff$means_desired[data_CriteriaDiff$delta_order==1]-0.04, y= 1, colour = "red", arrow=arrow(length = unit(0.2, "cm")))+
#  
#   geom_label(data=ranks_desired, aes( x=x, y=y, label=label),                 , 
#              color="#DD3497", 
#              size=3 , fontface="bold" )+
#   
#   geom_label(data=ranks_current, aes( x=x, y=y, label=label),                 , 
#              color='#FA9FB5', 
#              size=3 , fontface="bold" )+
# 
# 
#   theme(
#     legend.position = "bottom", 
#     axis.title.x = element_blank(),
#     axis.title.y = element_blank(),
#     panel.grid.minor.x = element_blank(),
#     panel.grid.minor.y = element_blank(),
#     axis.text.x = element_text(size = 11, colour = "black"),
#     axis.text.y = element_text(size = 11, colour = "black"),
#     plot.title = element_text(lineheight=.2, face="bold", hjust = 0.5, size = 12)) + 
#   ggtitle(title_plot)
}

## sorted by desired_order

title_plot_desired <- paste(expression("To what extent are (○) vs should (●) 
the following criteria be used for recruitment? 
(all researchers, n="), as.numeric(data_DesiredCriteria_ss), ")", sep="") # unfortunately the unicode symbol only print in the png not in the preview

CriteriaDiff_plot <- data_CriteriaDiff[order(data_CriteriaDiff$desired_order, decreasing = FALSE), ]   %>% 
  ggplot() +
  geom_point(aes(y = desired_order, x=means_current ), shape= 1, size = 5) +
  geom_point(aes(y = desired_order, x=means_desired), size = 5)+
  scale_y_continuous(breaks= 1:14, labels = data_CriteriaDiff$criteria[order(data_CriteriaDiff$desired_order, decreasing = FALSE)]) +
  scale_x_continuous(limits=c(0,3.2), breaks= 0:3, labels = c("Not at all", "Slightly", "Moderately", "Considerably") )+
  theme_minimal() +
   annotate("segment", x = data_CriteriaDiff$means_current[data_CriteriaDiff$desired_order==14]+0.06, xend = data_CriteriaDiff$means_desired[data_CriteriaDiff$desired_order==14]-0.06, y= 14, colour = "#DD3497", arrow=arrow(length = unit(0.2, "cm")), size = 1.5)+
   #annotate("segment", x = data_CriteriaDiff$means_current[data_CriteriaDiff$desired_order==13]+0.06, xend = data_CriteriaDiff$means_desired[data_CriteriaDiff$desired_order==13]-0.06, y= 13, colour = "#DD3497", arrow=arrow(length = unit(0.2, "cm")), size = 1.5)+
   annotate("segment", x = data_CriteriaDiff$means_current[data_CriteriaDiff$desired_order==12]+0.06, xend = data_CriteriaDiff$means_desired[data_CriteriaDiff$desired_order==12]-0.06, y= 12, colour = "#DD3497", arrow=arrow(length = unit(0.2, "cm")), size = 1.5)+
   annotate("segment", x = data_CriteriaDiff$means_current[data_CriteriaDiff$desired_order==11]+0.06, xend = data_CriteriaDiff$means_desired[data_CriteriaDiff$desired_order==11]-0.06, y= 11, colour = "#DD3497", arrow=arrow(length = unit(0.2, "cm")), size = 1.5)+
   annotate("segment", x = data_CriteriaDiff$means_current[data_CriteriaDiff$desired_order==10]-0.06, xend = data_CriteriaDiff$means_desired[data_CriteriaDiff$desired_order==10]+0.06, y= 10, colour = "#FA9FB5", arrow=arrow(length = unit(0.2, "cm")), size = 1.5)+
   annotate("segment", x = data_CriteriaDiff$means_current[data_CriteriaDiff$desired_order==9]+0.06, xend = data_CriteriaDiff$means_desired[data_CriteriaDiff$desired_order==9]-0.06, y= 9, colour = "#DD3497", arrow=arrow(length = unit(0.2, "cm")), size = 1.5)+
   annotate("segment", x = data_CriteriaDiff$means_current[data_CriteriaDiff$desired_order==8]+0.06, xend = data_CriteriaDiff$means_desired[data_CriteriaDiff$desired_order==8]-0.06, y= 8, colour = "#DD3497", arrow=arrow(length = unit(0.2, "cm")), size = 1.5)+
   annotate("segment", x = data_CriteriaDiff$means_current[data_CriteriaDiff$desired_order==7]-0.06, xend = data_CriteriaDiff$means_desired[data_CriteriaDiff$desired_order==7]+0.06, y= 7, colour = "#FA9FB5", arrow=arrow(length = unit(0.2, "cm")), size = 1.5)+
   annotate("segment", x = data_CriteriaDiff$means_current[data_CriteriaDiff$desired_order==6]-0.06, xend = data_CriteriaDiff$means_desired[data_CriteriaDiff$desired_order==6]+0.06, y= 6, colour = "#FA9FB5", arrow=arrow(length = unit(0.2, "cm")), size = 1.5)+
   annotate("segment", x = data_CriteriaDiff$means_current[data_CriteriaDiff$desired_order==5]-0.06, xend = data_CriteriaDiff$means_desired[data_CriteriaDiff$desired_order==5]+0.06, y= 5, colour = "#FA9FB5", arrow=arrow(length = unit(0.2, "cm")), size = 1.5)+
   annotate("segment", x = data_CriteriaDiff$means_current[data_CriteriaDiff$desired_order==4]+0.06, xend = data_CriteriaDiff$means_desired[data_CriteriaDiff$desired_order==4]-0.06, y= 4, colour = "#DD3497", arrow=arrow(length = unit(0.2, "cm")), size = 1.5)+
   annotate("segment", x = data_CriteriaDiff$means_current[data_CriteriaDiff$desired_order==3]+0.06, xend = data_CriteriaDiff$means_desired[data_CriteriaDiff$desired_order==3]-0.06, y= 3, colour = "#DD3497", arrow=arrow(length = unit(0.2, "cm")), size = 1.5)+
   annotate("segment", x = data_CriteriaDiff$means_current[data_CriteriaDiff$desired_order==2]-0.06, xend = data_CriteriaDiff$means_desired[data_CriteriaDiff$desired_order==2]+0.06, y= 2, colour = "#FA9FB5", arrow=arrow(length = unit(0.2, "cm")), size = 1.5)+

  theme(
    legend.position = "bottom", 
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    axis.text.x = element_text(size = 14,face = "bold"),
    axis.text.y = element_text(size = 18, colour = "black", face = "bold"),
    plot.title = element_text(lineheight=.8, face="bold", hjust = 0.5, size = 20)) + 
  ggtitle(title_plot_desired)

CriteriaDiff_plot
ggsave(here::here("Figures", "CriteriaDiff_plot.png"), width = 10, height = 6, bg = "white")



# desired order just for PIs

data_CurrentCriteria_PIs <- data_CurrentCriteria[data_CurrentCriteria$Q2 == "PIs",]
data_DesiredCriteria_PIs <- data_DesiredCriteria[data_DesiredCriteria$Q2 == "PIs",]

data_CurrentCriteria_all_num_PIs <- data_CurrentCriteria_PIs[,c(-1,-16,-17,-18)] # remove 'other' and Q2
data_CurrentCriteria_all_num_PIs <- sapply(data_CurrentCriteria_all_num_PIs, as.numeric )
data_DesiredCriteria_all_num_PIs <- data_DesiredCriteria_PIs[,c(-1,-16,-17,-18)]
data_DesiredCriteria_all_num_PIs <- sapply(data_DesiredCriteria_all_num_PIs, as.numeric )

means_current_PIs <- colMeans(data_CurrentCriteria_all_num_PIs, na.rm = TRUE)
means_desired_PIs <- colMeans(data_DesiredCriteria_all_num_PIs, na.rm = TRUE)
delta_PIs <- means_desired_PIs-means_current_PIs
data_CriteriaDiff_PIs <- data.frame(cbind(means_current_PIs, means_desired_PIs, delta_PIs))
data_CriteriaDiff_PIs$criteria <- Criteria
data_CriteriaDiff_PIs <- data_CriteriaDiff_PIs %>% arrange(means_desired_PIs) %>% mutate(desired_order = 1:14)
data_CriteriaDiff_PIs <- data_CriteriaDiff_PIs %>% arrange(-means_current_PIs) %>% mutate(current_order = 1:14)
data_CriteriaDiff_PIs <- data_CriteriaDiff_PIs %>% arrange(-means_desired_PIs) %>% mutate(revdesired_order = 1:14)
data_CriteriaDiff_PIs <- data_CriteriaDiff_PIs %>% arrange(-delta_PIs) %>% mutate(delta_order = 1:14)
#rownames(data_CriteriaDiff_PIs) <- NULL
data_CriteriaDiff_PIs


title_plot_desired_PIs <- paste ("To what extent are (○) vs should (●) 
the following criteria be used for recruitment?
(group leaders, n=", as.numeric(nrow(data_DesiredCriteria[data_DesiredCriteria$Q2 == "PIs",])), ")", sep="")


CriteriaDiff_plot_PIs <- data_CriteriaDiff_PIs[order(data_CriteriaDiff_PIs$desired_order, decreasing = FALSE), ]   %>% 
  ggplot() +
  geom_point(aes(y = desired_order, x=means_current_PIs ), shape= 1, size = 3) +
  geom_point(aes(y = desired_order, x=means_desired_PIs), size = 3)+
  scale_y_continuous(breaks= 1:14, labels = data_CriteriaDiff_PIs$criteria[order(data_CriteriaDiff_PIs$desired_order, decreasing = FALSE)]) +
  scale_x_continuous(limits=c(0,3.2), breaks= 0:3, labels = c("Not at all", "Slightly", "Moderately", "Considerably") )+
  theme_minimal() +
  annotate("segment", x = data_CriteriaDiff_PIs$means_current[data_CriteriaDiff_PIs$desired_order==14]+0.06, xend = data_CriteriaDiff_PIs$means_desired[data_CriteriaDiff_PIs$desired_order==14]-0.06, y= 14, colour = "#DD3497", arrow=arrow(length = unit(0.2, "cm")), size = 1.5)+
  annotate("segment", x = data_CriteriaDiff_PIs$means_current[data_CriteriaDiff_PIs$desired_order==12]-0.06, xend = data_CriteriaDiff_PIs$means_desired[data_CriteriaDiff_PIs$desired_order==12]+0.06, y= 12, colour = "#FA9FB5", arrow=arrow(length = unit(0.2, "cm")), size = 1.5)+
  annotate("segment", x = data_CriteriaDiff_PIs$means_current[data_CriteriaDiff_PIs$desired_order==11]+0.06, xend = data_CriteriaDiff_PIs$means_desired[data_CriteriaDiff_PIs$desired_order==11]-0.06, y= 11, colour = "#DD3497", arrow=arrow(length = unit(0.2, "cm")), size = 1.5)+
  annotate("segment", x = data_CriteriaDiff_PIs$means_current[data_CriteriaDiff_PIs$desired_order==10]+0.06, xend = data_CriteriaDiff_PIs$means_desired[data_CriteriaDiff_PIs$desired_order==10]-0.06, y= 10, colour = "#DD3497", arrow=arrow(length = unit(0.2, "cm")), size = 1.5)+
  annotate("segment", x = data_CriteriaDiff_PIs$means_current[data_CriteriaDiff_PIs$desired_order==9]-0.06, xend = data_CriteriaDiff_PIs$means_desired[data_CriteriaDiff_PIs$desired_order==9]+0.06, y= 9, colour = "#FA9FB5", arrow=arrow(length = unit(0.2, "cm")), size = 1.5)+
  annotate("segment", x = data_CriteriaDiff_PIs$means_current[data_CriteriaDiff_PIs$desired_order==7]-0.06, xend = data_CriteriaDiff_PIs$means_desired[data_CriteriaDiff_PIs$desired_order==7]+0.06, y= 7, colour = "#FA9FB5", arrow=arrow(length = unit(0.2, "cm")), size = 1.5)+
  annotate("segment", x = data_CriteriaDiff_PIs$means_current[data_CriteriaDiff_PIs$desired_order==6]+0.06, xend = data_CriteriaDiff_PIs$means_desired[data_CriteriaDiff_PIs$desired_order==6]-0.06, y= 6, colour = "#DD3497", arrow=arrow(length = unit(0.2, "cm")), size = 1.5)+
  annotate("segment", x = data_CriteriaDiff_PIs$means_current[data_CriteriaDiff_PIs$desired_order==5]-0.06, xend = data_CriteriaDiff_PIs$means_desired[data_CriteriaDiff_PIs$desired_order==5]+0.06, y= 5, colour = "#FA9FB5", arrow=arrow(length = unit(0.2, "cm")), size = 1.5)+
  annotate("segment", x = data_CriteriaDiff_PIs$means_current[data_CriteriaDiff_PIs$desired_order==4]+0.06, xend = data_CriteriaDiff_PIs$means_desired[data_CriteriaDiff_PIs$desired_order==4]-0.06, y= 4, colour = "#DD3497", arrow=arrow(length = unit(0.2, "cm")), size = 1.5)+
  annotate("segment", x = data_CriteriaDiff_PIs$means_current[data_CriteriaDiff_PIs$desired_order==3]+0.06, xend = data_CriteriaDiff_PIs$means_desired[data_CriteriaDiff_PIs$desired_order==3]-0.06, y= 3, colour = "#DD3497", arrow=arrow(length = unit(0.2, "cm")), size = 1.5)+
  annotate("segment", x = data_CriteriaDiff_PIs$means_current[data_CriteriaDiff_PIs$desired_order==2]-0.06, xend = data_CriteriaDiff_PIs$means_desired[data_CriteriaDiff_PIs$desired_order==2]+0.06, y= 2, colour = "#FA9FB5", arrow=arrow(length = unit(0.2, "cm")), size = 1.5)+
  annotate("segment", x = data_CriteriaDiff_PIs$means_current[data_CriteriaDiff_PIs$desired_order==1]+0.06, xend = data_CriteriaDiff_PIs$means_desired[data_CriteriaDiff_PIs$desired_order==1]-0.06, y= 1, colour = "#DD3497", arrow=arrow(length = unit(0.2, "cm")), size = 1.5)+
  
  theme(
    legend.position = "bottom", 
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    axis.text.x = element_text(size = 14,face = "bold"),
    axis.text.y = element_text(size = 18, colour = "black", face = "bold"),
    plot.title = element_text(lineheight=.8, face="bold", hjust = 0.5, size = 20)) +  
  ggtitle(title_plot_desired_PIs)

CriteriaDiff_plot_PIs

ggsave(here::here("Figures", "CriteriaDiff_plot_PIs.png"), width = 10, height = 6, bg = "white")




CriteriaDiff_plot_pres <- data_CriteriaDiff[order(data_CriteriaDiff$desired_order, decreasing = FALSE), ]   %>% 
  ggplot() +
  geom_point(aes(y = desired_order, x=means_current ), shape= 1, size = 5) +
  geom_point(aes(y = desired_order, x=means_desired), size = 5)+
  scale_y_continuous(breaks= 1:14, labels = data_CriteriaDiff$criteria[order(data_CriteriaDiff$desired_order, decreasing = FALSE)]) +
  scale_x_continuous(limits=c(0,3.2), breaks= 0:3, labels = c("Not at all", "Slightly", "Moderately", "Considerably") )+
  theme_minimal() +
  annotate("segment", x = data_CriteriaDiff$means_current[data_CriteriaDiff$desired_order==14]+0.06, xend = data_CriteriaDiff$means_desired[data_CriteriaDiff$desired_order==14]-0.06, y= 14, colour = "#DD3497", arrow=arrow(length = unit(0.2, "cm")), size = 1.5)+
  #annotate("segment", x = data_CriteriaDiff$means_current[data_CriteriaDiff$desired_order==13]+0.06, xend = data_CriteriaDiff$means_desired[data_CriteriaDiff$desired_order==13]-0.06, y= 13, colour = "#DD3497", arrow=arrow(length = unit(0.2, "cm")), size = 1.5)+
  annotate("segment", x = data_CriteriaDiff$means_current[data_CriteriaDiff$desired_order==12]+0.06, xend = data_CriteriaDiff$means_desired[data_CriteriaDiff$desired_order==12]-0.06, y= 12, colour = "#DD3497", arrow=arrow(length = unit(0.2, "cm")), size = 1.5)+
  annotate("segment", x = data_CriteriaDiff$means_current[data_CriteriaDiff$desired_order==11]+0.06, xend = data_CriteriaDiff$means_desired[data_CriteriaDiff$desired_order==11]-0.06, y= 11, colour = "#DD3497", arrow=arrow(length = unit(0.2, "cm")), size = 1.5)+
  annotate("segment", x = data_CriteriaDiff$means_current[data_CriteriaDiff$desired_order==10]-0.06, xend = data_CriteriaDiff$means_desired[data_CriteriaDiff$desired_order==10]+0.06, y= 10, colour = "#FA9FB5", arrow=arrow(length = unit(0.2, "cm")), size = 1.5)+
  annotate("segment", x = data_CriteriaDiff$means_current[data_CriteriaDiff$desired_order==9]+0.06, xend = data_CriteriaDiff$means_desired[data_CriteriaDiff$desired_order==9]-0.06, y= 9, colour = "#DD3497", arrow=arrow(length = unit(0.2, "cm")), size = 1.5)+
  annotate("segment", x = data_CriteriaDiff$means_current[data_CriteriaDiff$desired_order==8]+0.06, xend = data_CriteriaDiff$means_desired[data_CriteriaDiff$desired_order==8]-0.06, y= 8, colour = "#DD3497", arrow=arrow(length = unit(0.2, "cm")), size = 1.5)+
  annotate("segment", x = data_CriteriaDiff$means_current[data_CriteriaDiff$desired_order==7]-0.06, xend = data_CriteriaDiff$means_desired[data_CriteriaDiff$desired_order==7]+0.06, y= 7, colour = "#FA9FB5", arrow=arrow(length = unit(0.2, "cm")), size = 1.5)+
  annotate("segment", x = data_CriteriaDiff$means_current[data_CriteriaDiff$desired_order==6]-0.06, xend = data_CriteriaDiff$means_desired[data_CriteriaDiff$desired_order==6]+0.06, y= 6, colour = "#FA9FB5", arrow=arrow(length = unit(0.2, "cm")), size = 1.5)+
  annotate("segment", x = data_CriteriaDiff$means_current[data_CriteriaDiff$desired_order==5]-0.06, xend = data_CriteriaDiff$means_desired[data_CriteriaDiff$desired_order==5]+0.06, y= 5, colour = "#FA9FB5", arrow=arrow(length = unit(0.2, "cm")), size = 1.5)+
  annotate("segment", x = data_CriteriaDiff$means_current[data_CriteriaDiff$desired_order==4]+0.06, xend = data_CriteriaDiff$means_desired[data_CriteriaDiff$desired_order==4]-0.06, y= 4, colour = "#DD3497", arrow=arrow(length = unit(0.2, "cm")), size = 1.5)+
  annotate("segment", x = data_CriteriaDiff$means_current[data_CriteriaDiff$desired_order==3]+0.06, xend = data_CriteriaDiff$means_desired[data_CriteriaDiff$desired_order==3]-0.06, y= 3, colour = "#DD3497", arrow=arrow(length = unit(0.2, "cm")), size = 1.5)+
  annotate("segment", x = data_CriteriaDiff$means_current[data_CriteriaDiff$desired_order==2]-0.06, xend = data_CriteriaDiff$means_desired[data_CriteriaDiff$desired_order==2]+0.06, y= 2, colour = "#FA9FB5", arrow=arrow(length = unit(0.2, "cm")), size = 1.5)+
  
  theme(
    legend.position = "bottom", 
    legend.text = element_text(size=11.5, face = "bold", hjust = 0),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    axis.text.x = element_text(size = 14, colour = "black", face = "bold"),
    axis.text.y = element_text(size = 18, colour = "black",face = "bold"))


CriteriaDiff_plot_pres




CriteriaDiff_plot_PIs_pres <- data_CriteriaDiff_PIs[order(data_CriteriaDiff_PIs$desired_order, decreasing = FALSE), ]   %>% 
  ggplot() +
  geom_point(aes(y = desired_order, x=means_current_PIs ), shape= 1, size = 5) +
  geom_point(aes(y = desired_order, x=means_desired_PIs), size = 5)+
  scale_y_continuous(breaks= 1:14, labels = data_CriteriaDiff_PIs$criteria[order(data_CriteriaDiff_PIs$desired_order, decreasing = FALSE)]) +
  scale_x_continuous(limits=c(0,3.2), breaks= 0:3, labels = c("Not at all", "Slightly", "Moderately", "Considerably") )+
  theme_minimal() +
  annotate("segment", x = data_CriteriaDiff_PIs$means_current[data_CriteriaDiff_PIs$desired_order==14]+0.06, xend = data_CriteriaDiff_PIs$means_desired[data_CriteriaDiff_PIs$desired_order==14]-0.06, y= 14, colour = "#DD3497", arrow=arrow(length = unit(0.2, "cm")), size = 1.5)+
  annotate("segment", x = data_CriteriaDiff_PIs$means_current[data_CriteriaDiff_PIs$desired_order==12]-0.06, xend = data_CriteriaDiff_PIs$means_desired[data_CriteriaDiff_PIs$desired_order==12]+0.06, y= 12, colour = "#FA9FB5", arrow=arrow(length = unit(0.2, "cm")), size = 1.5)+
  annotate("segment", x = data_CriteriaDiff_PIs$means_current[data_CriteriaDiff_PIs$desired_order==11]+0.06, xend = data_CriteriaDiff_PIs$means_desired[data_CriteriaDiff_PIs$desired_order==11]-0.06, y= 11, colour = "#DD3497", arrow=arrow(length = unit(0.2, "cm")), size = 1.5)+
  annotate("segment", x = data_CriteriaDiff_PIs$means_current[data_CriteriaDiff_PIs$desired_order==10]+0.06, xend = data_CriteriaDiff_PIs$means_desired[data_CriteriaDiff_PIs$desired_order==10]-0.06, y= 10, colour = "#DD3497", arrow=arrow(length = unit(0.2, "cm")), size = 1.5)+
  annotate("segment", x = data_CriteriaDiff_PIs$means_current[data_CriteriaDiff_PIs$desired_order==9]-0.06, xend = data_CriteriaDiff_PIs$means_desired[data_CriteriaDiff_PIs$desired_order==9]+0.06, y= 9, colour = "#FA9FB5", arrow=arrow(length = unit(0.2, "cm")), size = 1.5)+
  annotate("segment", x = data_CriteriaDiff_PIs$means_current[data_CriteriaDiff_PIs$desired_order==7]-0.06, xend = data_CriteriaDiff_PIs$means_desired[data_CriteriaDiff_PIs$desired_order==7]+0.06, y= 7, colour = "#FA9FB5", arrow=arrow(length = unit(0.2, "cm")), size = 1.5)+
  annotate("segment", x = data_CriteriaDiff_PIs$means_current[data_CriteriaDiff_PIs$desired_order==6]+0.06, xend = data_CriteriaDiff_PIs$means_desired[data_CriteriaDiff_PIs$desired_order==6]-0.06, y= 6, colour = "#DD3497", arrow=arrow(length = unit(0.2, "cm")), size = 1.5)+
  #annotate("segment", x = data_CriteriaDiff_PIs$means_current[data_CriteriaDiff_PIs$desired_order==5]-0.06, xend = data_CriteriaDiff_PIs$means_desired[data_CriteriaDiff_PIs$desired_order==5]+0.06, y= 5, colour = "#FA9FB5", arrow=arrow(length = unit(0.2, "cm")), size = 1.5)+
  annotate("segment", x = data_CriteriaDiff_PIs$means_current[data_CriteriaDiff_PIs$desired_order==4]+0.06, xend = data_CriteriaDiff_PIs$means_desired[data_CriteriaDiff_PIs$desired_order==4]-0.06, y= 4, colour = "#DD3497", arrow=arrow(length = unit(0.2, "cm")), size = 1.5)+
  annotate("segment", x = data_CriteriaDiff_PIs$means_current[data_CriteriaDiff_PIs$desired_order==3]+0.06, xend = data_CriteriaDiff_PIs$means_desired[data_CriteriaDiff_PIs$desired_order==3]-0.06, y= 3, colour = "#DD3497", arrow=arrow(length = unit(0.2, "cm")), size = 1.5)+
  annotate("segment", x = data_CriteriaDiff_PIs$means_current[data_CriteriaDiff_PIs$desired_order==2]-0.06, xend = data_CriteriaDiff_PIs$means_desired[data_CriteriaDiff_PIs$desired_order==2]+0.06, y= 2, colour = "#FA9FB5", arrow=arrow(length = unit(0.2, "cm")), size = 1.5)+
  annotate("segment", x = data_CriteriaDiff_PIs$means_current[data_CriteriaDiff_PIs$desired_order==1]+0.06, xend = data_CriteriaDiff_PIs$means_desired[data_CriteriaDiff_PIs$desired_order==1]-0.06, y= 1, colour = "#DD3497", arrow=arrow(length = unit(0.2, "cm")), size = 1.5)+
  
  theme(
    legend.position = "bottom",
    legend.text = element_text(size=11.5, face = "bold", hjust = 0),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    axis.text.x = element_text(size = 14, colour = "black", face = "bold"),
    axis.text.y = element_text(size = 18, colour = "black", face = "bold"))

CriteriaDiff_plot_PIs_pres
