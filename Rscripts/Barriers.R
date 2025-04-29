
  #source("Rscripts/FormatData.R")

# Variables to define
Measures
Barriers_columns <- c(expr(Q6_oa), expr(Q6_rdm), expr(Q6_fair), expr(Q6_code), expr(Q6_material),expr(Q6_prereg),expr(Q6_rr))
Barriers_answers <- rev(c("none", "other", "policy",  "incentives","norms" , "training", "infrastructure", "unsure","na"))
Barriers_colors <- rev(c("#bdbdbd", "#666666", "#E31A1C", "#FC4E2A", "#FD8D3C", "#FEB24C", "#FED976", "#FFEDA0", "#B8E186"))
Barriers_legend <- rev(c("None", "Other", "Policy",  "Incentives","Norms" , "Training", "Infrastructure", "Not sure","Not applicable                                     ")) # adding space after NA (replacing Not applicable) so the legend doesn't get cut - this is pathetic but I already spent an hour trying to do this properly without success....

barriers_answer_map <- c(
  "none" = "None",
  "other" = "Other",
  "policy" = "Policy",
  "incentives" = "Incentives",
  "norms" = "Norms",
  "training" = "Training",
  "infrastructure" = "Infrastructure",
  "unsure" = "Not sure",
  "na" = "Not applicable"
)

# data correction !!! overwriting respondents answers !!!

## these answers should have been "not applicable"
data_Barriers[!is.na(data_Barriers$Q6_oa_comment) & data_Barriers$Q6_oa_comment == "When selected \"other\" as a barrier to adopt open research practices, I was referring to the lack of relevant data to make it publicly available. ",]

data_Barriers[!is.na(data_Barriers$Q6_oa_comment) & data_Barriers$Q6_oa_comment == "When selected \"other\" as a barrier to adopt open research practices, I was referring to the lack of relevant data to make it publicly available. ",] <- data_Barriers[!is.na(data_Barriers$Q6_oa_comment) & data_Barriers$Q6_oa_comment == "When selected \"other\" as a barrier to adopt open research practices, I was referring to the lack of relevant data to make it publicly available. ",] %>%
  mutate(across(
    .cols = !contains("_comment"),     # only non-comment columns
    .fns = ~ ifelse(. == "other", "na", .)
  ))

data_Barriers$Q6_code[!is.na(data_Barriers$Q6_oa_comment) & data_Barriers$Q6_oa_comment == "When selected \"other\" as a barrier to adopt open research practices, I was referring to the lack of relevant data to make it publicly available. "] <- "training"

data_Barriers[ data_Barriers$Q6_oa == "other",]

### these answer should have picked a logical answer (none, na, or unsure alone)

# Vector of key words to check
keywords <- c("na", "none", "unsure")

# Function to check if a cell have these words AND another
check_mixed_cell <- function(cell) {
  if (is.na(cell)) return(NA)
  # Check if the cell contains a comma (implying multiple entries)
  if (grepl(",", cell)) {
    parts <- trimws(unlist(strsplit(cell, ",")))
    # Check if at least one keyword exists and another different word exists (which could also be a keyword)
    if (length(parts[parts %in% keywords]) >= 1 && length(parts) > 1) {
      return(cell)
    }
  }
  return(NA)
}

# Apply to your data frame
mixed_cells <- as.data.frame(lapply(data_Barriers, function(col) sapply(col, check_mixed_cell)))
print(mixed_cells[!is.na(mixed_cells) & mixed_cells != FALSE])


## rewriting data manually !!!!
data_Barriers[!is.na(data_Barriers) & data_Barriers == "incentives, policy, none"] <- "incentives, policy"
data_Barriers[!is.na(data_Barriers) & data_Barriers == "training, norms, incentives, none"] <- "training, norms, incentives"
data_Barriers[!is.na(data_Barriers) & data_Barriers == "training, incentives, none"] <- "training, incentives"
data_Barriers[!is.na(data_Barriers) & data_Barriers == "norms, none"] <- "norms"
data_Barriers[!is.na(data_Barriers) & data_Barriers == "training, none"] <- "training"

data_Barriers[!is.na(data_Barriers) & data_Barriers == "training, incentives, policy, unsure"] <- "training, incentives, policy"
data_Barriers[!is.na(data_Barriers) & data_Barriers == "none, unsure"] <- "unsure"


mixed_cells <- as.data.frame(lapply(data_Barriers, function(col) sapply(col, check_mixed_cell)))
print(mixed_cells[!is.na(mixed_cells) & mixed_cells != FALSE])





# create dataset for plotting
data_Barriers_for_plotting <- prepare_barriers_data_for_plotting(Measures, data_Barriers, Barriers_answers, Barriers_columns)
EachMeasureBarriers_ss <- data.frame(data_Barriers_for_plotting %>% group_by(LabelIndiv) %>% summarise(N = sum(n, na.rm=TRUE)))
EachMeasureBarriers_ss ### many drops for prereg, rdm plan, and RR


title_plot_Barriers <- paste ("Barriers to adoption of open research pratices
(all researchers, N=",as.numeric(data_Barriers_ss),")" , sep="")


Barriers_plot_dodged <- horizontal_dodged_barplot_on_barriers(
  data = data_Barriers_for_plotting, 
  Question = Measures, 
  answers = Barriers_answers, 
  answers_colors = Barriers_colors, 
  title_plot = title_plot_Barriers, 
  legend = Barriers_legend,
  plot_ylim = 55
  )

Barriers_plot_dodged

Barriers_plot <- horizontal_stacked_barplot(
  data = data_Barriers_for_plotting, 
  Question = Measures, 
  answers = Barriers_answers, 
  answers_colors = Barriers_colors, 
  title_plot = title_plot_Barriers, 
  legend = Barriers_legend,
  legend_position = "bottom"
)

Barriers_plot

ggsave(here::here("Figures", "Barriers_plot.png"), width = 10, height = 4, bg = "white")

Barriers_plot_pres <- horizontal_stacked_barplot(
  data = data_Barriers_for_plotting, 
  Question = Measures, 
  answers = Barriers_answers, 
  answers_colors = Barriers_colors, 
  title_plot = NULL, 
  legend = Barriers_legend,
  legend_position = "bottom"
)


# creating counts of barriers

data_Barriers_for_table <- data_Barriers[, c("Q6_oa", "Q6_rdm", "Q6_fair", "Q6_code",
                                                       "Q6_material", "Q6_prereg", "Q6_rr")]

data_Barriers_for_table[data_Barriers_for_table == "none"] <- "0"

data_Barriers_for_table[] <- lapply(data_Barriers_for_table, function(column) {
  sapply(column, function(cell) {
    if (is.na(cell)) {
      return(NA)  # Keep NA as NA
    } else if (cell %in% c("0", "unsure", "na")) {
      return(cell)  # Keep special codes as they are
    } else {
      # Otherwise, count number of words separated by commas
      return(as.character(length(strsplit(cell, ",\\s*")[[1]])))
    }
  })
})

all_levels <- c("0", "1", "2", "3", "4", "5", "na", "unsure")

# For each column, create a table ensuring all levels exist
tables_list <- lapply(data_Barriers_for_table, function(column) {
  # Create a factor with all expected levels
  factor_column <- factor(column, levels = all_levels)
  table(factor_column)
})

# Combine into one data frame
pivot_for_each <- as.data.frame(do.call(rbind, tables_list))

pivot_for_each$practice <- names(tables_list)
pivot_for_each
rownames(pivot_for_each) <- NULL
pivot_for_each$n <- rowSums(pivot_for_each[,-ncol(pivot_for_each)])


for (i in 1:(ncol(pivot_for_each) - 2)) {  # Exclude the 'practice' and 'n' columns
    pivot_for_each[, i] <- (pivot_for_each[, i] / pivot_for_each$n) * 100
  }

pivot_for_each

# Calculate column means, excluding the last column (practice)
Nb_barriers <- as.data.frame(round(colMeans(pivot_for_each[, !(names(pivot_for_each) %in% c("practice", "n"))], na.rm = TRUE),0))
colnames(Nb_barriers) <- NULL

transposed2 <- t(Nb_barriers)
rownames(transposed2) <- NULL
colnames(transposed2) <- c("0", "1", "2", "3", "4", "5", "Not applicable", "Not sure")

transposed2 <- as.data.frame(transposed2)
transposed2



