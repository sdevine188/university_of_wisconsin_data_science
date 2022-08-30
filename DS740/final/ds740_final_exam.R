library(tidyverse)
library(skimr)
library(viridis)
library(caret)
library(missForest)
library(cluster)
library(factoextra)
library(dendextend)
library(ggdendro)
library(RANN)


# setwd
setwd("C:/Users/sdevine/Desktop/usaid/mcp/malign_influence")
options(scipen = 999)

#//////////////////////////////////////////////////////////////////////////////////////////////////

# load modified ggdendro functions
current_wd <- getwd()
setwd("C:/Users/sdevine/Desktop/R/clustering")
source("modified_ggdendro_functions.R")
setwd(current_wd)

#//////////////////////////////////////////////////////////////////////////////////////////////////

# load add_dummies functions
current_wd <- getwd()
setwd("C:/Users/sdevine/Desktop/R/assorted_helper_scripts")
source("add_dummies.R")
setwd(current_wd)

#//////////////////////////////////////////////////////////////////////////////////////////////////

# fmir_obj_1_indicator_clusters ####

# read final fmir data ####
fmir <- read.csv(file = "data/fmir/fmir_20220307.csv") %>% as_tibble()

# set current_obj_num
current_obj_num <- "obj_1"

# get fmir_obj_1_indicator with scaled version of indicator_standardized_values
fmir_obj_1_indicator <- fmir %>% filter(year == 2020) %>% 
        filter(mcp_grouping != "EU-15") %>%
        filter(obj_num == current_obj_num) %>%
        distinct(country, indicator_name, indicator_standardized_values) %>%
        pivot_wider(id_cols = country, names_from = indicator_name, values_from = indicator_standardized_values) %>%
        column_to_rownames(var = "country") %>%
        mutate(across(.cols = everything(), .fns = ~ scale(.x, center = TRUE, scale = TRUE)[ , 1]))

#//////////////////////////////////////////////////////////////////////////////////////////////////

# get fmir_obj_1_indicator_distance_matrix 
fmir_obj_1_indicator_distance_matrix <- dist(fmir_obj_1_indicator, method = "euclidean")

# inspect fmir_obj_1_indicator_distance_matrix_tbl
fmir_obj_1_indicator_distance_matrix_tbl <- as.matrix(fmir_obj_1_indicator_distance_matrix) %>% as_tibble() %>% 
        mutate(country = row.names(as.matrix(fmir_obj_1_indicator_distance_matrix))) %>%
        relocate(country, .before = everything())

#//////////////////////////////////////////////////////////////////////////////////////////////////

# fmir_obj_1_indicator_distance_matrix_tile_chart ####

# get country_order from fviz_dist_plot
fmir_obj_1_fviz_dist_plot <- fviz_dist(dist.obj = fmir_obj_1_indicator_distance_matrix, 
                                       gradient = list(low = "#440154FF", mid = "#21908CFF", high = "#FDE725FF"))

fmir_obj_1_country_order_tbl <- fmir_obj_1_fviz_dist_plot$data %>% as_tibble() %>% slice(1:(fmir_obj_1_indicator %>% nrow())) %>% 
        mutate(country = str_sub(string = Var1, start = 1, end = -2),
               country_order = row_number()) %>% 
        select(country, country_order)

#/////////////////////////////

# create chart
fmir_obj_1_indicator_distance_matrix_tile_chart <- fmir_obj_1_indicator_distance_matrix_tbl %>% 
        pivot_longer(cols = -country, names_to = "var", values_to = "values") %>%
        left_join(., fmir_obj_1_country_order_tbl, by = c("var" = "country")) %>%
        rename(var_order = country_order) %>%
        left_join(., fmir_obj_1_country_order_tbl, by = "country") %>%
        ggplot(data = ., mapping = aes(x = fct_reorder(.f = country, .x = country_order, .fun = min), 
                                       y = fct_reorder(.f = var, .x = var_order, .fun = min), 
                                       fill = values)) +
        geom_tile(color = "#DDDDDD") + scale_fill_viridis() +
        labs(fill = "Similiarity\n(lower is more similar)", y = NULL, x = NULL) +
        coord_fixed(ratio = 1/1.5, clip = "off") +
        theme_bw() +
        theme(
                # plot.background = element_rect(fill = "blue"),
                plot.margin = unit(c(0, 0, 0, 5), "mm"),
                plot.caption = element_text(hjust = 0, size = 11, face = "plain", family = "Calibri", 
                                            color = "#595959", margin = margin(t = 4, r = 0, b = 0, l = 0)),
                # text = element_text(family = "Calibri", size = 46, face = "plain", color = "#000000"),
                panel.grid.minor = element_blank(),
                panel.grid.major.x = element_blank(),
                # panel.grid.major.y = element_line(color = "#DDDDDD", size = .25),
                panel.grid.major.y = element_blank(),
                panel.border = element_blank(),
                # panel.grid = element_blank(),
                # line = element_blank(),
                # rect = element_blank(),
                panel.spacing.x = unit(1, "lines"),
                panel.spacing.y = unit(1, "lines"),
                strip.background = element_blank(),
                strip.text = element_text(family = "Calibri", face = "plain", size = 7, color = "#333333"),
                # axis.ticks.y = element_blank(),
                axis.ticks.y = element_line(color = "#333333", size = .25),
                # axis.ticks.x = element_blank(),
                axis.ticks.x = element_line(color = "#333333", size = .25),
                axis.ticks.length.y.left = unit(.1, "cm"),
                axis.ticks.length.x.bottom = unit(.1, "cm"),
                axis.text.x = element_text(family = "Calibri", face = "plain", size = 7, color = "#333333",
                                           margin = margin(t = 3, r = 0, b = 0, l = 0), angle = 45, hjust = 1),
                # axis.text.x = element_blank(),
                axis.text.y = element_text(family = "Calibri", face = "plain", size = 7, color = "#333333", 
                                           margin = margin(t = 0, r = 5, b = 0, l = 0)),
                # axis.line.x.bottom = element_line(color = "#333333"),
                axis.line.x.bottom = element_blank(),
                axis.line.y.left = element_blank(),
                axis.title.x = element_text(family = "Calibri", face = "plain", size = 7, color = "#333333", 
                                            margin = margin(t = 13, r = 0, b = 5, l = 0)),
                axis.title.y = element_text(family = "Calibri", face = "plain", size = 7, color = "#333333", 
                                            margin = margin(t = 0, r = 5, b = 0, l = 2)),
                plot.title = element_text(size = 7, face = "bold", hjust = .5, family = "Calibri", color = "#333333", 
                                          margin = margin(t = 0, r = 0, b = 10, l = 0, unit = "pt")),
                legend.position = "right",
                # legend.key.size = unit(2, "mm"), 
                legend.title = element_text(size = 7, family = "Calibri", face = "plain", color = "#333333", hjust = .5),
                legend.text = element_text(size = 7, family = "Calibri", margin(t = 0, r = 0, b = 0, l = 0, unit = "pt"), 
                                           hjust = .5, color = "#333333"),
                # legend.spacing.x = unit(1.0, 'cm')
                # legend.spacing.y = unit(5.5, "cm")
                # legend.key = element_rect(size = 5),
                # legend.key.size = unit(2, 'lines')
                legend.key.height = unit(1, "line")
        ) 

#//////////////////////////////////

# save chart as emf
filename <- tempfile(fileext = ".emf")
emf(file = filename)
print(fmir_obj_1_indicator_distance_matrix_tile_chart)
dev.off()

# add emf to word doc
read_docx() %>%
        body_add_img(src = filename, width = 6, height = 6) %>%
        print(target = "output/charts/fmir_obj_1_indicator_distance_matrix_tile_chart.docx")

#//////////////////////////////////////////////////////////////////////////////////////////////////

# get cluster tendency to see if data is likely to have clusters that are more meaningful/tight than uniform random sample
# https://www.datanovia.com/en/lessons/assessing-clustering-tendency/
# note that the hopkins stat uses .5 as a threshold, with higher values indicating there is a clustering tendency; 
# a hopkins stat over .75 is 90% confidence that there is clustering tendency that is real, and not random
cluster_tendency <- get_clust_tendency(fmir_obj_1_indicator, n = nrow(fmir_obj_1_indicator) - 1, graph = FALSE)

#///////////////////

# inspect
cluster_tendency
cluster_tendency$hopkins_stat

#//////////////////////////////////////////////////////////////////////////////////////////////////

# use agnes to look for clustering method with best agglomerative coefficient 
method_list <- c( "average", "single", "complete", "ward")
names(method_list) <- c( "average", "single", "complete", "ward")

# function to compute coefficient
cluster_methods <- function(current_method) {
        print(current_method)
        agnes(fmir_obj_1_indicator_distance_matrix, method = current_method)$ac
}

# run cluster_methods
# note that wards method has the highest/best agglomerative coefficient
map(.x = method_list, .f = ~ cluster_methods(current_method = .x))

#//////////////////////////////////////////////////////////////////////////////////////////////////

# use ward's method with agnes, which had highest (best) agglomerative coefficient
fmir_obj_1_indicator_agnes <- agnes(fmir_obj_1_indicator, method = "ward")

#///////////////////

# inspect optimal number of clusters
# note the gap statistic is recently developed by tibshirani et al
fviz_nbclust(fmir_obj_1_indicator, FUN = hcut, method = "wss")
fviz_nbclust(fmir_obj_1_indicator, FUN = hcut, method = "silhouette")
gap_stat <- clusGap(fmir_obj_1_indicator, FUN = hcut, K.max = 10, B = 50)
fviz_gap_stat(gap_stat)

# set optimal k
fmir_obj_1_indicator_k <- 3

#//////////////////////////////////////////////////////////////////////////////////////////////////

# use ward's method with hclust
fmir_obj_1_indicator_hclust <- hclust(d = fmir_obj_1_indicator_distance_matrix, method = "ward.D2")

#//////////////////////////////////////////////////////////////////////////////////////////////////

# cut hclust tree with cutree
fmir_obj_1_indicator_hclust_cut <- cutree(fmir_obj_1_indicator_hclust, k = fmir_obj_1_indicator_k)
fmir_obj_1_indicator_hclust_cut

#//////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

# fmir_obj_1_indicator_dendrogram ####

# get fmir_obj_1_indicator_dendrogram_data
# note that dendro_data_k() takes the uncut hclust as first argument
fmir_obj_1_indicator_dendrogram_data <- dendro_data_k(hc = fmir_obj_1_indicator_hclust, 
                                                      k = fmir_obj_1_indicator_k)

#///////////////////

# get cluster_colors
# note that colors are passed in a vector that is k + 1 in length
# the first value is the neutral color for branches above the point at which the tree has been cut up to the root
# the second/third/fourth etc values are indexed to the cluster number; 
# so cluster 1 is second value in colors vector, cluster 2 is third value in colors vector, cluster 3 is fourth value in colors vector
cluster_colors <- c("#333333", 
                    color_palette %>% slice(2) %>% pull(hex), 
                    color_palette %>% slice(7) %>% pull(hex),
                    color_palette %>% slice(10) %>% pull(hex))

# plot
fmir_obj_1_indicator_dendrogram <- plot_ggdendro(fmir_obj_1_indicator_dendrogram_data,
                                                 direction = "tb",
                                                 scale.color = cluster_colors,
                                                 label.size = 2.5,
                                                 branch.size = 0.5,
                                                 expand.y = 0) +
        labs(y = "Similarity\n(lower groupings are more similar)", x = "") +
        coord_fixed(ratio = 1 / 1.5, clip = "off") +
        theme_bw() +
        theme(
                # plot.background = element_rect(fill = "blue"),
                # plot.background = element_rect(colour = "#DDDDDD", size = .5),
                plot.margin = unit(c(0, 5, 0, 0), "mm"),
                plot.caption = element_text(hjust = 0, size = 11, face = "plain", family = "Calibri", 
                                            color = "#595959", margin = margin(t = 4, r = 0, b = 0, l = 0)),
                # text = element_text(family = "Calibri", size = 46, face = "plain", color = "#000000"),
                panel.grid.minor = element_blank(),
                panel.grid.major.x = element_blank(),
                panel.grid.major.y = element_line(color = "#DDDDDD"),
                # panel.grid.major.y = element_blank(),
                panel.border = element_blank(),
                # panel.border = element_rect(color = "#DDDDDD", size = .5),
                # panel.grid = element_blank(),
                # line = element_blank(),
                # rect = element_blank(),
                axis.ticks.y = element_blank(),
                axis.ticks.x = element_line(size = .5),
                axis.ticks.length.y.left = unit(.2, "cm"),
                axis.ticks.length.x.bottom = unit(.1, "cm"),
                axis.text.x = element_text(family = "Calibri", face = "bold", size = 7, color = "#333333", 
                                           margin = margin(t = 3, r = 0, b = 0, l = 0), angle = 0, hjust = .5),
                axis.text.y = element_text(family = "Calibri", face = "plain", size = 8, color = "#333333",
                                           margin = margin(t = 0, r = 0, b = 0, l = 0)),
                # axis.line.x.bottom = element_line(color = "#333333"),
                axis.line.x.bottom = element_blank(),
                axis.line.y.left = element_blank(),
                # axis.title.x = element_blank(),
                axis.title.x = element_text(family = "Calibri", face = "plain", size = 8, color = "#333333",
                                            margin = margin(t = 5, r = 0, b = 10, l = 0)),
                axis.title.y = element_text(family = "Calibri", face = "plain", size = 8, color = "#333333",
                                            margin = margin(t = 0, r = 15, b = 0, l = 5)),
                # axis.title.y = element_blank(),
                # axis.text.y = element_blank(),
                plot.title = element_text(size = 7, face = "plain", hjust = .5, family = "Calibri", color = "#333333", 
                                          margin = margin(t = 0, r = 0, b = 5, l = 0, unit = "pt")),
                legend.position = "bottom",
                # legend.key.size = unit(2, "mm"), 
                legend.title = element_text(size = 9, family = "Calibri", face = "plain", color = "#333333"),
                legend.text = element_text(size = 9, family = "Calibri", margin(t = 0, r = 0, b = 0, l = 0, unit = "pt"), 
                                           hjust = .5, color = "#333333")
                # legend.spacing.y = unit(5.5, "cm"),
                # legend.key = element_rect(size = 5),
                # legend.key.size = unit(2, 'lines')
        ) 

#//////////////////////////////////

# save chart as emf
filename <- tempfile(fileext = ".emf")
emf(file = filename)
print(fmir_obj_1_indicator_dendrogram)
dev.off()

# add emf to word doc
read_docx() %>%
        body_add_img(src = filename, width = 6, height = 6) %>%
        print(target = "output/charts/fmir_obj_1_indicator_dendrogram.docx")

#//////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

# fmir_obj_1_country_indicator_tile_chart ####

# get chart_data
# note this is 
chart_data <- fmir %>% filter(year == 2020) %>% 
        filter(mcp_grouping != "EU-15") %>%
        filter(obj_num == current_obj_num) %>%
        select(country, indicator_name, indicator_standardized_values) %>%
        left_join(., fmir_obj_1_indicator_hclust_cut %>% enframe() %>% rename(country = name, cluster = value),
                  by = "country") %>% 
        relocate(cluster, .after = country) %>%
        left_join(., fmir_obj_1_country_order_tbl, by = "country") %>%
        relocate(country_order, .after = cluster) %>% arrange(country_order) %>%
        pivot_wider(id_cols = c(country, cluster, country_order), 
                    names_from = indicator_name, values_from = indicator_standardized_values) %>%
        left_join(., fmir %>% 
                          filter(year == 2020,
                                 mcp_grouping != "EU-15",
                                 obj_num == current_obj_num) %>%
                          select(country, year, obj_num, sub_obj_num, indicator_name, sub_obj_avg, obj_avg) %>%
                          arrange(country) %>%
                          distinct(country, obj_num, sub_obj_num, sub_obj_avg, obj_avg) %>%
                          mutate(sub_obj_num_label = case_when(sub_obj_num == "sub_obj_1_1" ~ 
                                                                       "Sub-obj. 1.1: Checks & balances and rule of law",
                                                               sub_obj_num == "sub_obj_1_2" ~ "Sub-obj. 1.2: Civil society",
                                                               sub_obj_num == "sub_obj_1_3" ~ 
                                                                       "Sub-obj. 1.3: Reslience to electoral/political interference")) %>%
                          select(country, obj_num, obj_avg, sub_obj_num_label, sub_obj_avg) %>%
                          pivot_wider(id_cols = c(country, obj_num, obj_avg), 
                                      names_from = sub_obj_num_label, values_from = sub_obj_avg) %>%
                          rename(!!sym("Obj. 1: Democratic") := obj_avg) %>% select(-obj_num), 
                  by = "country") %>%
        pivot_longer(cols = -c(country, cluster, country_order), names_to = "var", values_to = "values") %>%
        mutate(color = case_when(cluster == 1 ~ color_palette %>% slice(2) %>% pull(hex),
                                 cluster == 2 ~ color_palette %>% slice(7) %>% pull(hex),
                                 cluster == 3 ~ color_palette %>% slice(10) %>% pull(hex)),
               country_label = str_c("Group ", cluster, ": ", country),
               var_order = case_when(var == "Obj. 1: Democratic" ~ 1,
                                     str_detect(string = var, pattern = "^Sub-obj.") ~ 2,
                                     TRUE ~ 3))

#/////////////////////////////

# create chart
fmir_obj_1_country_indicator_tile_chart <- chart_data %>%
        ggplot(data = ., mapping = aes(x = fct_reorder(.f = var, .x = var_order, .fun = min), 
                                       y = fct_reorder(.f = country_label, .x = country_order, .fun = min), 
                                       fill = values)) +
        geom_tile(color = "#DDDDDD") + scale_fill_viridis() +
        labs(fill = "Score\n(higher is better)\n", y = NULL, x = "Indicator") +
        coord_fixed(ratio = 1 / 3, clip = "off") +
        theme_bw() +
        theme(
                # plot.background = element_rect(fill = "blue"),
                plot.margin = unit(c(0, 0, 0, 5), "mm"),
                plot.caption = element_text(hjust = 0, size = 11, face = "plain", family = "Calibri", 
                                            color = "#595959", margin = margin(t = 4, r = 0, b = 0, l = 0)),
                # text = element_text(family = "Calibri", size = 46, face = "plain", color = "#000000"),
                panel.grid.minor = element_blank(),
                panel.grid.major.x = element_blank(),
                # panel.grid.major.y = element_line(color = "#DDDDDD", size = .25),
                panel.grid.major.y = element_blank(),
                panel.border = element_blank(),
                # panel.grid = element_blank(),
                # line = element_blank(),
                # rect = element_blank(),
                panel.spacing.x = unit(1, "lines"),
                panel.spacing.y = unit(1, "lines"),
                strip.background = element_blank(),
                strip.text = element_text(family = "Calibri", face = "plain", size = 7, color = "#333333"),
                # axis.ticks.y = element_blank(),
                axis.ticks.y = element_line(color = "#333333", size = .25),
                # axis.ticks.x = element_blank(),
                axis.ticks.x = element_line(color = "#333333", size = .25),
                axis.ticks.length.y.left = unit(.1, "cm"),
                axis.ticks.length.x.bottom = unit(.1, "cm"),
                axis.text.x = element_text(family = "Calibri", face = "plain", size = 5, color = "#333333",
                                           margin = margin(t = 3, r = 0, b = 0, l = 0), angle = 45, hjust = 1),
                # axis.text.x = element_blank(),
                axis.text.y = element_text(family = "Calibri", face = "plain", size = 5, 
                                           color = chart_data %>% distinct(country, color) %>% pull(color), 
                                           margin = margin(t = 0, r = 5, b = 0, l = 0)),
                # axis.line.x.bottom = element_line(color = "#333333"),
                axis.line.x.bottom = element_blank(),
                axis.line.y.left = element_blank(),
                axis.title.x = element_text(family = "Calibri", face = "plain", size = 5, color = "#333333", 
                                            margin = margin(t = 5, r = 0, b = 0, l = 0)),
                axis.title.y = element_text(family = "Calibri", face = "plain", size = 5, color = "#333333", 
                                            margin = margin(t = 0, r = 5, b = 0, l = 2)),
                plot.title = element_text(size = 7, face = "bold", hjust = .5, family = "Calibri", color = "#333333", 
                                          margin = margin(t = 0, r = 0, b = 10, l = 0, unit = "pt")),
                legend.position = "right",
                # legend.key.size = unit(2, "mm"), 
                legend.title = element_text(size = 5, family = "Calibri", face = "plain", color = "#333333", hjust = .5),
                legend.text = element_text(size = 5, family = "Calibri", margin(t = 0, r = 0, b = 0, l = 0, unit = "pt"), 
                                           hjust = .5, color = "#333333"),
                # legend.spacing.x = unit(1.0, 'cm')
                # legend.spacing.y = unit(5.5, "cm")
                # legend.key = element_rect(size = 5),
                # legend.key.size = unit(2, 'lines')
                legend.key.height = unit(1, "line")
        ) 

#//////////////////////////////////

# save chart as emf
filename <- tempfile(fileext = ".emf")
emf(file = filename)
print(fmir_obj_1_country_indicator_tile_chart)
dev.off()

# add emf to word doc
read_docx() %>%
        body_add_img(src = filename, width = 6, height = 6) %>%
        print(target = "output/charts/fmir_obj_1_country_indicator_tile_chart.docx")

#//////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

# fmir_obj_1_indicator_cluster_boxplot_chart ####

# get chart_data
# note that this uses the original ISV values from the index, not the scaled ISV used in fmir_obj_1_indicator for clustering; 
# original/unscaled ISV avg is better for plot because values are then interpretable in the framework of the index (e.g. ISV btw 0-1)
chart_data <- fmir %>% filter(year == 2020) %>% 
        filter(mcp_grouping != "EU-15") %>%
        filter(obj_num == current_obj_num) %>%
        select(country, indicator_name, indicator_standardized_values) %>%
        pivot_wider(id_cols = country, names_from = indicator_name, values_from = indicator_standardized_values) %>%
        
        
        # add clusters
        left_join(., fmir_obj_1_indicator_hclust_cut %>% enframe() %>% rename(country = name, cluster = value),
                  by = "country") %>% 
        relocate(cluster, .after = country) %>%
        
        
        # add cluster avg of sub_obj_avg 
        left_join(fmir %>% 
                          filter(year == 2020,
                                 mcp_grouping != "EU-15",
                                 obj_num == current_obj_num) %>%
                          select(country, year, obj_num, sub_obj_num, indicator_name, sub_obj_avg, obj_avg) %>%
                          arrange(country) %>%
                          select(-c(year, obj_num, indicator_name, obj_avg)) %>% 
                          distinct() %>%
                          pivot_wider(id_cols = country, names_from = sub_obj_num, values_from = sub_obj_avg) %>%
                          left_join(., fmir_obj_1_indicator_hclust_cut %>% enframe() %>% rename(country = name, cluster = value),
                                    by = "country") %>% 
                          relocate(cluster, .after = country),
                  ., by = c("country", "cluster")) %>%
        
        
        # add cluster avg of obj_avg 
        left_join(fmir %>% 
                          filter(year == 2020,
                                 mcp_grouping != "EU-15",
                                 obj_num == current_obj_num) %>%
                          select(country, year, obj_num, sub_obj_num, indicator_name, sub_obj_avg, obj_avg) %>%
                          arrange(country) %>%
                          select(-c(year, sub_obj_num, indicator_name, sub_obj_avg)) %>% 
                          distinct() %>%
                          pivot_wider(id_cols = country, names_from = obj_num, values_from = obj_avg) %>%
                          left_join(., fmir_obj_1_indicator_hclust_cut %>% enframe() %>% rename(country = name, cluster = value),
                                    by = "country") %>% 
                          relocate(cluster, .after = country),
                  ., by = c("country", "cluster")) %>%
        
        
        # get distinct values for each cluster
        pivot_longer(cols = -c(country, cluster), names_to = "var", values_to = "values") %>%
        mutate(cluster_name = str_c("Group ", cluster),
               color_bin = as.character(cluster_name),
               color = case_when(color_bin == "Group 1" ~ color_palette %>% slice(2) %>% pull(hex),
                                 color_bin == "Group 2" ~ color_palette %>% slice(7) %>% pull(hex),
                                 color_bin == "Group 3" ~ color_palette %>% slice(10) %>% pull(hex)))

# create color_list for to pass to scale_color_manual
chart_data_color_list <- chart_data %>% count(color_bin, color) %>% pull(color)
names(chart_data_color_list) <- chart_data %>% count(color_bin, color) %>% pull(color_bin)
chart_data_color_list

#//////////////////////////

# create chart
fmir_obj_1_indicator_cluster_boxplot_chart <- chart_data %>%
        ggplot(data = ., mapping = aes(x = fct_relevel(.f = var, order_obj_and_sub_obj_avg_first), 
                                       y = values, fill = color_bin)) + 
        geom_boxplot(width = .5, lwd = .01, outlier.size = .5) + facet_wrap(facets = vars(cluster_name)) +
        scale_y_continuous(breaks = seq(from = 0, to = 1, by = .2), limits = c(0, 1), expand = c(0, 0)) +
        scale_fill_manual(values = chart_data_color_list) + 
        # scale_color_manual(values = chart_data_color_list) + 
        coord_fixed(ratio = 10/.6, clip = "off") +
        labs(x = "Indicator", y = "Score (higher is better)", fill = NULL, color = NULL) +
        theme_minimal() +
        theme(
                # plot.background = element_rect(fill = "blue"),
                # plot.background = element_rect(colour = "#DDDDDD", size = .5),
                plot.margin = unit(c(0, 0, 0, 5), "mm"),
                plot.caption = element_text(hjust = 0, size = 11, face = "plain", family = "Calibri", 
                                            color = "#595959", margin = margin(t = 4, r = 0, b = 0, l = 0)),
                # text = element_text(family = "Calibri", size = 46, face = "plain", color = "#000000"),
                panel.grid.minor = element_blank(),
                panel.grid.major.x = element_blank(),
                panel.grid.major.y = element_line(color = "#DDDDDD"),
                # panel.grid.major.y = element_blank(),
                panel.border = element_blank(),
                # panel.border = element_rect(color = "#DDDDDD", size = .5),
                # panel.grid = element_blank(),
                # line = element_blank(),
                # rect = element_blank(),
                # strip.background = element_blank(),
                strip.background = element_rect(fill = "#DDDDDD", color = NA),
                strip.text = element_text(family = "Calibri", face = "plain", size = 8, color = "#333333"),
                axis.ticks.y = element_blank(),
                axis.ticks.x = element_line(color = "#333333", size = .25),
                # axis.ticks.length.y.left = unit(.2, "cm"),
                axis.ticks.length.x.bottom = unit(.1, "cm"),
                axis.text.x = element_text(family = "Calibri", face = "plain", size = 5, color = "#333333", 
                                           margin = margin(t = 3, r = 0, b = 0, l = 0), angle = 45, hjust = 1),
                axis.text.y = element_text(family = "Calibri", face = "plain", size = 7, color = "#333333",
                                           margin = margin(t = 0, r = 0, b = 0, l = 0)),
                # axis.line.x.bottom = element_line(color = "#333333"),
                axis.line.x.bottom = element_blank(),
                axis.line.y.left = element_blank(),
                axis.title.x = element_text(family = "Calibri", face = "plain", size = 7, color = "#333333", 
                                            margin = margin(t = 5, r = 0, b = -15, l = 0)),
                axis.title.y = element_text(family = "Calibri", face = "plain", size = 7, color = "#333333",
                                            margin = margin(t = 0, r = 10, b = 0, l = 0)),
                # axis.title.y = element_blank(),
                plot.title = element_text(size = 5, face = "plain", hjust = .5, family = "Calibri", color = "#333333", 
                                          margin = margin(t = 0, r = 0, b = 5, l = 0, unit = "pt")),
                legend.position = "bottom",
                legend.key.size = unit(5, "mm"),
                legend.title = element_text(size = 7, family = "Calibri", face = "plain", color = "#333333", hjust = 1,
                                            margin(t = 0, r = 0, b = 0, l = 0, unit = "pt")),
                legend.text = element_text(size = 7, family = "Calibri", margin(t = 0, r = 0, b = 0, l = 0, unit = "pt"), 
                                           hjust = .5, color = "#333333")
                # legend.spacing.y = unit(5.5, "cm"),
                # legend.key = element_rect(size = 5),
                # legend.key.size = unit(2, 'lines')
        ) 

#//////////////////////////////////

# save chart as emf
filename <- tempfile(fileext = ".emf")
emf(file = filename)
print(fmir_obj_1_indicator_cluster_boxplot_chart)
dev.off()

# add emf to word doc
read_docx() %>%
        body_add_img(src = filename, width = 6, height = 6) %>%
        print(target = "output/charts/fmir_obj_1_indicator_cluster_boxplot_chart.docx")

#//////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
#//////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
#//////////////////////////////////////////////////////////////////////////////////////////////////////////////////////


library(xgboost)
library(tidyverse)
library(skimr)
library(viridis)
library(rpart)
library(caret)
library(missForest)
library(cluster)
library(factoextra)
library(dendextend)
library(traj)
library(ggdendro)
library(forecast)
library(RANN)
library(conflicted)
conflict_scout()
conflict_prefer(name = "filter", winner = "dplyr")
conflict_prefer(name = "slice", winner = "dplyr")
conflict_prefer(name = "lag", winner = "dplyr")


# https://topepo.github.io/caret/train-models-by-tag.html#boosting
# https://rpubs.com/crossxwill/time-series-cv
# https://www.kaggle.com/code/rtatman/machine-learning-with-xgboost-in-r/notebook
# https://xgboost.readthedocs.io/en/stable/R-package/xgboostPresentation.html
# https://www.hackerearth.com/practice/machine-learning/machine-learning-algorithms/beginners-tutorial-on-xgboost-parameter-tuning-r/tutorial/
# https://www.kaggle.com/code/nagsdata/simple-r-xgboost-caret-kernel/script
# https://www.analyticsvidhya.com/blog/2016/03/complete-guide-parameter-tuning-xgboost-with-codes-python/
# https://robjhyndman.com/hyndsight/tscv/
# eta is the learning rate that discounts the forecast (residual) output of each tree, to avoid overfitting: 
# https://machinelearningmastery.com/tune-learning-rate-for-gradient-boosting-with-xgboost-in-python/
# gamma regularizes "across trees" by setting complexity cost threshold for adding additional node, based on the accuracy gain relative to 
# distribution of accuracy gain when adding nodes in other trees:
# https://stats.stackexchange.com/questions/418687/gamma-parameter-in-xgboost
# min_child_weight is threshold for stopping adding a node when the sample size in node gets small enough: 
# https://stats.stackexchange.com/questions/317073/explanation-of-min-child-weight-in-xgboost-algorithm


# https://tiantiy.people.clemson.edu/blog/2019//MissingData/random_forest.html
# https://stat.ethz.ch/education/semesters/ss2012/ams/paper/missForest_1.2.pdf
# https://rpubs.com/lmorgan95/MissForest


#//////////////////////////////////////////////////////////////////////////////////////////////////
#//////////////////////////////////////////////////////////////////////////////////////////////////
#//////////////////////////////////////////////////////////////////////////////////////////////////


# setwd
setwd("C:/Users/sdevine/Desktop/usaid/mcp/malign_influence")
options(scipen = 999)


#//////////////////////////////////////////////////////////////////////////////////////////////////
#//////////////////////////////////////////////////////////////////////////////////////////////////
#//////////////////////////////////////////////////////////////////////////////////////////////////


# load modified ggdendro functions
current_wd <- getwd()
setwd("C:/Users/sdevine/Desktop/R/clustering")
source("modified_ggdendro_functions.R")
setwd(current_wd)


#//////////////////////////////////////////////////////////////////////////////////////////////////
#//////////////////////////////////////////////////////////////////////////////////////////////////
#//////////////////////////////////////////////////////////////////////////////////////////////////


# load modified ggdendro functions
current_wd <- getwd()
setwd("C:/Users/sdevine/Desktop/R/assorted_helper_scripts")
source("add_dummies.R")
setwd(current_wd)


#//////////////////////////////////////////////////////////////////////////////////////////////////
#//////////////////////////////////////////////////////////////////////////////////////////////////
#//////////////////////////////////////////////////////////////////////////////////////////////////


# create custom color_palette ####
color_palette <- tibble(hex = c("#083D7F", "#2474B6", "#8BBFD0",
                                "#CBCBCB", "#7D7D7D",
                                "#99ba78", "#35B779FF", "#006629", 
                                "#E4DC68", "#FDA159", "#EF6712", "#CE1B1E",
                                "#8B008B", "#DA70D6"))
color_palette
color_palette %>% pull(hex) %>% show_col()


#//////////////////////////////////////////////////////////////////////////////////////////////////
#//////////////////////////////////////////////////////////////////////////////////////////////////
#//////////////////////////////////////////////////////////////////////////////////////////////////

# read obj_1_data ####
obj_1_data <- read.csv(file = "data/fmir/fmir_20220307.csv") %>% as_tibble() %>%
        select(country, mcp_grouping, year, obj_num, indicator_name, values_z_std) %>%
        filter(obj_num == "obj_1") %>%
        pivot_wider(id_cols = c(country, mcp_grouping, year, obj_num),
                    names_from = indicator_name, values_from = values_z_std) %>%
        as.data.frame()

#//////////////////////////////////////////////////////////////////////////////////////////////////

# impute missing values ####
obj_1_data_mf <- obj_1_data %>% 
        mutate(across(.cols = c(country, mcp_grouping, obj_num), .fns = as.factor)) %>%
        missForest(xmis = ., maxiter = 10, ntree = 100, variablewise = TRUE, verbose = TRUE)

#//////////////////////////////////////////////////////////////////////////////////////////////////

obj_1_data_imputed <- obj_1_data_mf$ximp %>% as_tibble() %>% 
        pivot_longer(cols = -c(country, mcp_grouping, year, obj_num),
                     names_to = "indicator_name", values_to = "values_z_std")

#//////////////////////////////////////////////////////////////////////////////////////////////////

# add concept_avg
obj_1_data_imputed <- read.csv(file = "data/fmir/fmir_20220307.csv") %>% as_tibble() %>%
        select(country, year, sub_obj_num, concept, indicator_name) %>%
        left_join(obj_1_data_imputed, ., by = c("country", "year", "indicator_name")) %>%
        group_by(country, year, concept) %>% 
        mutate(concept_avg = mean(values_z_std, na.rm = TRUE)) %>%
        ungroup()

# add sub_obj_avg ####
obj_1_data_imputed <- obj_1_data_imputed %>% distinct(country, year, sub_obj_num, concept, concept_avg) %>%
        arrange(country, year, sub_obj_num, concept) %>%
        group_by(country, year, sub_obj_num) %>% mutate(sub_obj_avg = mean(concept_avg, na.rm = TRUE)) %>%
        ungroup() %>%
        distinct(country, year, sub_obj_num, sub_obj_avg) %>%
        left_join(obj_1_data_imputed, ., by = c("country", "year", "sub_obj_num"))

# add obj_avg ####
obj_1_data_imputed <- obj_1_data_imputed %>% distinct(country, year, obj_num, sub_obj_num, sub_obj_avg) %>%
        arrange(country, year, obj_num, sub_obj_num) %>%
        group_by(country, year, obj_num) %>%
        mutate(obj_avg = mean(sub_obj_avg, na.rm = TRUE)) %>%
        ungroup() %>% select(country, year, obj_num, obj_avg) %>% distinct() %>%
        arrange(country, year, obj_num) %>%
        left_join(obj_1_data_imputed, ., by = c("country", "year", "obj_num"))

#//////////////////////////////////////////////////////////////////////////////////////////////////

# reshape data, add obj_avg_lead as response var, add lags/diffs as features ####

# get wide data 
obj_1_data_imputed <- obj_1_data_imputed %>%
        select(country, mcp_grouping, year, indicator_name, values_z_std, obj_avg) %>%
        pivot_wider(id_cols = c(country, mcp_grouping, year, obj_avg),
                    names_from = indicator_name, values_from = values_z_std) 

# save obj_avg values for 2020 to use as the final validation test
obj_avg_2020 <- obj_1_data_imputed %>% filter(year == 2020) %>% select(country, year, obj_avg)

# add obj_avg_lead as response variable
# drop most recent year which doesn't have response variable and so can't train model
# add dummies for country and mcp_grouping categorical variables
obj_1_data_imputed <- obj_1_data_imputed %>% 
        arrange(country, year) %>%
        group_by(country) %>%
        mutate(obj_avg_lead = lead(obj_avg, n = 1),
               across(.cols = c(obj_avg, matches("sub_obj_.*[^0-9]$")), .fns = ~ lag(.x, n = 1), .names = "{.col}_lag_1"),
               across(.cols = c(obj_avg, matches("sub_obj_.*[^0-9]$")), .fns = ~ lag(.x, n = 2), .names = "{.col}_lag_2"),
               across(.cols = c(obj_avg, matches("sub_obj_.*[^0-9]$")), .fns = ~ lag(.x, n = 3), .names = "{.col}_lag_3"),
               across(.cols = c(obj_avg, matches("sub_obj_.*[^0-9]$")), .fns = ~ .x - lag(.x, n = 1), .names = "{.col}_diff_1"),
               across(.cols = c(obj_avg, matches("sub_obj_.*[^0-9]$")), .fns = ~ .x - lag(.x, n = 2), .names = "{.col}_diff_2"),
               across(.cols = c(obj_avg, matches("sub_obj_.*[^0-9]$")), .fns = ~ .x - lag(.x, n = 3), .names = "{.col}_diff_3")) %>%
        ungroup() %>%
        filter(year < 2020) %>%
        relocate(c(obj_avg_lead, year, starts_with("country.")), .before = everything()) %>%
        add_dummies(vars = vars(country, mcp_grouping), drop_vars = TRUE)

# use preProcess to impute the median for lags/diffs with NA
median_imputation_model <- preProcess(x = obj_1_data_imputed %>% as.data.frame(), method = "medianImpute")
obj_1_data_imputed <- predict(object = median_imputation_model, newdata = obj_1_data_imputed)

#//////////////////////////////////////////////////////////////////////////////////////////////////

# get train_data and test_data
train_data <- obj_1_data_imputed %>% filter(year <= 2018)
test_data <- obj_1_data_imputed %>% filter(year == 2019)

#//////////////////////////////////////////////////////////////////////////////////////////////////

# create tune_grid for training xgboost model
tune_grid <- expand.grid(mtry = c(round(sqrt(train_data %>% ncol())),
                                  round(sqrt(train_data %>% ncol())) + 20))

#//////////////////////////////////////////////////////////////////////////////////////////////////

# set train_control
train_control <- trainControl(method = "timeslice",
                              initialWindow = 5,
                              horizon = 1,
                              fixedWindow = FALSE,
                              allowParallel = FALSE)

#//////////////////////////////////////////////////////////////////////////////////////////////////

# get rf model
# rf_fit_caret_1 <- train(obj_avg_lead ~ .,
#                         data = train_data,
#                         method = "rf",
#                         trControl = train_control,
#                         tuneGrid = tune_grid)

# inspect
rf_fit_caret_1
rf_fit_caret_1$bestTune
rf_fit_caret_1$finalModel
varImp(rf_fit_caret_1, scale = FALSE) %>% attributes()

var_imp_plot <- varImp(rf_fit_caret_1, scale = FALSE)$importance %>% rownames_to_column() %>% as_tibble() %>%
        arrange(desc(Overall)) %>% 
        slice(1:20) %>% mutate(fill = "#2474B6") %>%
        ggplot(data = ., mapping = aes(x = fct_reorder(.f = rowname, .x = Overall, .desc = FALSE), 
                                       y = Overall)) + geom_col() +
        labs(x = "Variable", y = "Importance") +
        theme(axis.text.x = element_text(family = "Calibri", face = "plain", size = 7, color = "#333333",
                                         margin = margin(t = 3, r = 0, b = 0, l = 0), angle = 45, hjust = 1))

predictions <- predict(object = rf_fit_caret_1, newdata = test_data)
obj_avg_2020 %>% mutate(predictions = predictions,
                        diff = predictions - obj_avg) %>% 
        summarize(root_mean_squared_error = sqrt(mean(diff^2))) # .113



