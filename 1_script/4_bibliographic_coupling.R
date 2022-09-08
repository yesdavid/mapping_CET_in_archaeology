# bibliographic coupling
library(dplyr)
library(data.table)
library(ggraph)
library(igraph) 
library(tidyverse)
library(gridExtra)
library(ggforce)
library(magrittr)

WoS_bib_included <- readRDS(file = file.path(".", "2_data", "WoS_run_7","wos_run_7_prepared.RDS"))
cr_details_per_article_tbl <- readRDS(file = file.path("3_output", "wos_run_7_cr_details_per_article_tbl.RDS"))
thesauriert_joined <- readRDS(file = file.path("3_output", "wos_run_7_thesauriert_joined_DE_TI_AB.RDS"))
thesauriert_joined_binary <- readRDS(file = file.path("3_output", "wos_run_7_thesauriert_joined_binary_DE_TI_AB.RDS"))
meta_meta_categories <- read.csv(file.path("2_data", "categories.csv"))

thesauriert_joined_binary_incidence <- thesauriert_joined_binary[,-which(colnames(thesauriert_joined_binary) == "AU")]
thesauriert_joined_binary_incidence <- as.matrix(thesauriert_joined_binary_incidence, labels = T)

#####################################################################################################################################
# cross-tab: bibliographic coupling
#####################################################################################################################################

xtab_cr_paper_paper <- 
  xtabs(data = cr_details_per_article_tbl,
        formula =  ~ mybibtex_key + CR_au1_py)

xtab_cr_paper_paper[5:10, 1:5]


#####################################################################################################################################
# https://rpubs.com/neloe/ggraph_intro
#####################################################################################################################################
# Calculate cosine distance between characters
cosine_sim <- as.dist(xtab_cr_paper_paper %*% t(xtab_cr_paper_paper) / (sqrt(rowSums(xtab_cr_paper_paper^2) %*% t(rowSums(xtab_cr_paper_paper^2)))))
# hist(cosine_sim)

# Initial look at the network 
# autograph(as.matrix(cosine_sim))

# Filter weak connections. The amount chosen here is arbitrary. Try different variations. 
cs_strong <- cosine_sim
hist(cosine_sim)
# max(cosine_sim)
cs_strong[is.na(cs_strong)] <- 0 
cs_strong[cs_strong < max(cs_strong) * 0.09] <- 0 
hist(cs_strong)

# Create an igraph object 
article_references_network <- as.matrix(cs_strong) %>% 
  graph_from_adjacency_matrix(mode = "undirected", 
                              weighted = TRUE)

Isolated <- which(degree(article_references_network)==0)
article_references_network <- igraph::delete.vertices(article_references_network, Isolated)

# only largest connected graph
article_references_network <- induced_subgraph(
  article_references_network, V(article_references_network)[components(article_references_network)$membership == which.max(components(article_references_network)$csize)]
)

# Community detection algorithm 
set.seed(123)
article_references_network_community_louvain <- cluster_louvain(article_references_network) 


# Attach communities to relevant vertices
V(article_references_network)$color <- article_references_network_community_louvain$membership

# remove clusters 10 + 11 because of their small size
# article_references_network <- igraph::delete.vertices(article_references_network, which(article_references_network_community_louvain$membership %in% c(10,11)))

# # Set size to degree centrality 
# V(article_references_network)$size = degree(article_references_network)

# Set size to times cited +1
times_cited <- WoS_bib_included$TC # Web of Science Core Collection Times Cited Count
names(times_cited) <- WoS_bib_included$mybibtex_key
V(article_references_network)$size <-
  times_cited[which(names(V(article_references_network)) %in% names(times_cited))]+1


# bibliographic_coupling_network_graphopt_plot_noLabels.png
set.seed(1)
article_references_network_louvain <- 
ggraph(delete_vertices(article_references_network,
                       which(V(article_references_network)$color == 10 | V(article_references_network)$color == 11)),
       # article_references_network,
       layout = "graphopt") +
  geom_edge_arc(strength=0.2, width=0.1, alpha=.15) + 
  geom_node_point(aes(size=size, fill=factor(color)), shape = 21) + 
  # geom_node_text(aes(label = name, size = size), repel = TRUE) +
  theme_void() +
  labs(fill = "Cluster") +
  guides(size = F)
  # theme(legend.position = "none"") #+ 

article_references_network_louvain

ggsave(article_references_network_louvain,
       filename = file.path("3_output", "Fig_6_bibliographic_coupling_network_graphopt_plot_noLabels.png"),
       device = "png",
       width = 8,
       height = 8,
       units = "in",
       bg = "white")



# bibliographic_coupling_network_graphopt_plot_linesDotsWrap.png
del_vert <- delete_vertices(article_references_network,
                            which(V(article_references_network)$color == 10 | V(article_references_network)$color == 11))

V(del_vert)$color <- paste("Cluster", V(del_vert)$color)

set.seed(1)
article_references_network_louvain_dots_lines_warp_color <-
  ggraph(del_vert,
         # article_references_network,
         layout = "graphopt") +
  geom_edge_arc(strength=0.2, width=0.1, alpha=.15) +
  geom_node_point(aes(size=size, fill=factor(color)),
                  shape = 21) +
  theme_bw() + theme(panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(),
                     axis.title=element_blank(),
                     axis.text=element_blank(),
                     axis.ticks=element_blank()) +
  theme(legend.position = "none") +
  facet_wrap(~color,
             ncol = 3)
article_references_network_louvain_dots_lines_warp_color

ggsave(article_references_network_louvain_dots_lines_warp_color,
       filename = file.path("3_output", "SI_Fig_2_bibliographic_coupling_network_graphopt_plot_linesDotsWrap.png"),
       device = "png",
       width = 8,
       height = 8,
       units = "in",
       bg = "white")

##############################################################################################################
# calculate centrality measures
##############################################################################################################

article_references_network_node_df <- tibble::as_tibble(igraph::as_data_frame(article_references_network, "vertices"))

article_references_network_node_df <- 
  dplyr::left_join(article_references_network_node_df,
                   unique(cr_details_per_article_tbl[,c("mybibtex_key", "PY")]),
                   by = c("name" = "mybibtex_key"))


# eigenvector
article_references_network_node_df <- dplyr::left_join(article_references_network_node_df,
                            tibble::enframe(scales::rescale(igraph::eigen_centrality(article_references_network)$vector,
                                                            to = c(0,1)),
                                            value = "eigen_centrality"),
                            by = "name")

# betweenness
article_references_network_node_df <- dplyr::left_join(article_references_network_node_df,
                            tibble::enframe(scales::rescale(igraph::betweenness(article_references_network), 
                                                            to = c(0,1)),
                                            value = "betweenness_centrality"),
                            by = "name")

# closeness
article_references_network_node_df <- dplyr::left_join(article_references_network_node_df,
                            tibble::enframe(scales::rescale(igraph::closeness(article_references_network),
                                                            to = c(0,1)),
                                            value = "closeness_centrality"),
                            by = "name")
mean_median_closeness_centrality_list <- list()
for(i in unique(article_references_network_node_df$color)){
  current_cluster <- subset(article_references_network_node_df, color == i)
  mean_median_closeness_centrality_list[[i]] <- 
  data.frame(cluster = i,
             mean_closeness = round(mean(current_cluster$closeness_centrality), digits = 3),
             median_closeness = round(median(current_cluster$closeness_centrality), digits = 3))
}
mean_median_closeness_centrality <- do.call(rbind.data.frame, mean_median_closeness_centrality_list)
readr::write_csv(mean_median_closeness_centrality,
                 file = file.path("3_output", "bibliographic_coupling_network_articles_with_netw_measures_w_clusters_mean_median_closeness_centrality.csv"))

# degree
article_references_network_node_df <- dplyr::left_join(article_references_network_node_df,
                            tibble::enframe(scales::rescale(igraph::degree(article_references_network),
                                                            to = c(0,1)),
                                            value = "degree_centrality"),
                            by = "name")


readr::write_csv(article_references_network_node_df,
                 file = file.path("3_output", "bibliographic_coupling_network_articles_with_netw_measures_w_clusters.csv"))

# run anovas and pairwise t tests
anova_list <- list()
anova_res_list <- list()
pairwise_t_list <- list()
for(i in c("eigen_centrality","betweenness_centrality","closeness_centrality","degree_centrality")){
  anova_list[[i]] <- aov(article_references_network_node_df[[i]] ~ factor(article_references_network_node_df$color))
  anova_res_list[[i]] <- summary(anova_list[[i]])
  pairwise_t_list[[i]] <- pairwise.t.test(article_references_network_node_df[[i]], 
                                          article_references_network_node_df$color, 
                                          p.adjust.method = "holm")
}

# modularity
modularity <- igraph::modularity(article_references_network,
                                 article_references_network_community_louvain$membership)
modularity

# network density for whole network
density <- igraph::edge_density(article_references_network)
density

# transitivity
transitivity <- igraph::transitivity(article_references_network)
transitivity

# small world index; crashes R
# sw <- qgraph::smallworldness(article_references_network, B = 1000)


# 
network_centralities_plots <- 
article_references_network_node_df %>% 
  group_by(color) %>% 
  dplyr::select(# eigen_centrality, 
                # betweenness_centrality, 
                # degree_centrality, 
                closeness_centrality) %>% 
  subset(., !(color %in% c(10,11))) %>% 
  pivot_longer(cols = colnames(.)[2:ncol(.)]) %>% 
ggplot() +
  geom_boxplot(aes(x = color,
                   y = value,
                   group = color,
                   fill = as.factor(color)))+
  theme_bw(base_size = 18) +
  theme(legend.position = "none") +
  xlab("Cluster") +
  ylab("Scaled closeness centrality") +
  scale_x_continuous(breaks = c(1:max(article_references_network_node_df$color))) #+
  # facet_wrap(~name) 

network_centralities_plots

ggsave(network_centralities_plots,
       filename = file.path("3_output", "Fig_7_bibliographic_coupling_network_closenessCentrality_plot.png"),
       device = "png",
       width = 7,
       height = 7,
       bg = "white",
       units = "in")


##############################################################################################################
# subgraph measures
##############################################################################################################

graph_1 <- subgraph(graph=article_references_network, v=which(V(article_references_network)$color==1))
graph_2 <- subgraph(graph=article_references_network, v=which(V(article_references_network)$color==2))
graph_3 <- subgraph(graph=article_references_network, v=which(V(article_references_network)$color==3))
graph_4 <- subgraph(graph=article_references_network, v=which(V(article_references_network)$color==4))
graph_5 <- subgraph(graph=article_references_network, v=which(V(article_references_network)$color==5))
graph_6 <- subgraph(graph=article_references_network, v=which(V(article_references_network)$color==6))
graph_7 <- subgraph(graph=article_references_network, v=which(V(article_references_network)$color==7))
graph_8 <- subgraph(graph=article_references_network, v=which(V(article_references_network)$color==8))
graph_9 <- subgraph(graph=article_references_network, v=which(V(article_references_network)$color==9))
graph_10 <- subgraph(graph=article_references_network, v=which(V(article_references_network)$color==10))
graph_11 <- subgraph(graph=article_references_network, v=which(V(article_references_network)$color==11))

article_references_network_cluster_densities <- 
  data.frame(cluster = c(1:11),
             edge_density = c(round(igraph::edge_density(graph_1), digits = 3),
                              round(igraph::edge_density(graph_2), digits = 3),
                              round(igraph::edge_density(graph_3), digits = 3),
                              round(igraph::edge_density(graph_4), digits = 3),
                              round(igraph::edge_density(graph_5), digits = 3),
                              round(igraph::edge_density(graph_6), digits = 3),
                              round(igraph::edge_density(graph_7), digits = 3),
                              round(igraph::edge_density(graph_8), digits = 3),
                              round(igraph::edge_density(graph_9), digits = 3),
                              round(igraph::edge_density(graph_10), digits = 3),
                              round(igraph::edge_density(graph_11), digits = 3)))

readr::write_csv(article_references_network_cluster_densities,
                 file = file.path("3_output", "bibliographic_coupling_network_clusters_w_densities.csv"))

##############################################################################################################
# Chi-Squared Test
##############################################################################################################
library("graphics")

WoS_bib_included_w_clusters <- dplyr::left_join(WoS_bib_included,
                                                article_references_network_node_df[,c("name", "color")],
                                                by = c("mybibtex_key" = "name"))


blubb2 <- as.data.frame(thesauriert_joined_binary_incidence) #
blubb2$mybibtex_key <- rownames(blubb2)

blubb2 <- dplyr::left_join(blubb2, article_references_network_node_df[,c("name", "color")],
                          by = c("mybibtex_key" = "name"))

rownames(blubb2) <- blubb2$mybibtex_key
blubb <- blubb2[,-which(colnames(blubb2) == "mybibtex_key")]

genemerge_assoc <-
  blubb%>%
  pivot_longer(-color, names_to = "category") %>%
  na_if(0) %>%
  na.omit %>%
  select(color, category)

chisq_test_res <- 
as_tibble(table(genemerge_assoc)) %>% 
  pivot_wider(names_from = color, values_from = n) %>% 
  as.data.frame() %>% 
  tibble::column_to_rownames('category') %>% 
  select(-c("10","11")) %>%
  chisq.test()

chisq_test_res

dummy_df <- 
  data.frame(cluster = 1:9,
             GEOGR_LOCATION = "NA",
             GEOGR_LOCATION_FINE = "NA",
             MATERIAL_CULTURE = "NA",
             CET = "NA",
             PERIOD = "NA",
             METHODS = "NA",
             TOPIC = "NA")

cluster_keyword_list <- list()
for (cluster_index in 1:9) {

  current_cluster <- data.frame(cluster = cluster_index,
                                category = names(chisq_test_res$stdres[,cluster_index]),
                                value = chisq_test_res$stdres[,cluster_index])

  current_cluster_pos <- subset(current_cluster, value >=2)


  
  current_cluster_per_meta_cat_list <- list()
  wordcloud_cluster_per_meta_cat_list <- list()
  for(meta_cat_index in unique(meta_meta_categories$meta_category)){
    current_cluster_per_meta_cat_list[[meta_cat_index]] <-
    subset(current_cluster_pos, category %in% subset(meta_meta_categories, meta_category == meta_cat_index)$category) %>%
      arrange(desc(value)) %>%
      mutate(meta_category = meta_cat_index)

    dummy_df[cluster_index, meta_cat_index] <- paste0(current_cluster_per_meta_cat_list[[meta_cat_index]]$category,
                                                      collapse = ", ")
    
  }

  cluster_keyword_list[[cluster_index]] <- current_cluster_per_meta_cat_list
  
}
dummy_df

readr::write_csv(dummy_df,
                 file = file.path("3_output", "chiSq_residuals_keywords_clusters.csv"))


# library(corrplot)
# png(file.path("3_output", "keyword_cluster_chisq_geogrLoc.png"),
#     width =8,height = 8,units = "in", bg = "white", res = 72)
# corrplot(chisq_test_res$stdres[which(rownames(chisq_test_res$stdres) %in% subset(meta_meta_categories, meta_category == "GEOGR_LOCATION")$category),], is.cor = FALSE,method = "square")
# dev.off()
# 
# png(file.path("3_output", "keyword_cluster_chisq_geogrLocFine.png"),
#     width =8,height = 8,units = "in", bg = "white", res = 72)
# corrplot(chisq_test_res$stdres[which(rownames(chisq_test_res$stdres) %in% subset(meta_meta_categories, meta_category == "GEOGR_LOCATION_FINE")$category),], is.cor = FALSE,method = "square")
# dev.off()
# 
# png(file.path("3_output", "keyword_cluster_chisq_MaterialCult.png"),
#     width =8,height = 8,units = "in", bg = "white", res = 72)
# corrplot(chisq_test_res$stdres[which(rownames(chisq_test_res$stdres) %in% subset(meta_meta_categories, meta_category == "MATERIAL_CULTURE")$category),], is.cor = FALSE,method = "square")
# dev.off()
# 
# png(file.path("3_output", "keyword_cluster_chisq_CET.png"),
#     width =8,height = 8,units = "in", bg = "white", res = 72)
# corrplot(chisq_test_res$stdres[which(rownames(chisq_test_res$stdres) %in% subset(meta_meta_categories, meta_category == "CET")$category),], is.cor = FALSE,method = "square")
# dev.off()
# 
# png(file.path("3_output", "keyword_cluster_chisq_periods.png"),
#     width =8,height = 8,units = "in", bg = "white", res = 72)
# corrplot(chisq_test_res$stdres[which(rownames(chisq_test_res$stdres) %in% subset(meta_meta_categories, meta_category == "PERIOD")$category),], is.cor = FALSE,method = "square")
# dev.off()
# 
# png(file.path("3_output", "keyword_cluster_chisq_methods.png"),
#     width =8,height = 8,units = "in", bg = "white", res = 72)
# corrplot(chisq_test_res$stdres[which(rownames(chisq_test_res$stdres) %in% subset(meta_meta_categories, meta_category == "METHODS")$category),], is.cor = FALSE,method = "square")
# dev.off()
# 
# png(file.path("3_output", "keyword_cluster_chisq_Topic.png"),
#     width =8,height = 16,units = "in", bg = "white", res = 72)
# corrplot(chisq_test_res$stdres[which(rownames(chisq_test_res$stdres) %in% subset(meta_meta_categories, meta_category == "TOPIC")$category),], is.cor = FALSE,method = "square")
# dev.off()

# Contribution in percentage (%)
contrib <- 100*chisq_test_res$residuals^2/chisq_test_res$statistic

most_important_categories <- data.frame(category = rownames(contrib),
                                        contrib = rowSums(contrib))
arrange(most_important_categories, desc(contrib))

# # Visualize the contribution
# library(corrplot)
# corrplot(contrib, 
#          is.cor = FALSE,
#          method = "square")
# corrplot(contrib[which(rownames(contrib) %in% subset(meta_meta_categories, meta_category == "CET")$category),], 
#          is.cor = FALSE,
#          method = "square")

##############################################################################################################
# log odds
##############################################################################################################
library(tidylo)

table(genemerge_assoc)

log_odds <- 
genemerge_assoc %>% 
  subset(., color %in% c(1:9)) %>% 
  group_by(color) %>% 
  count(category, sort = T) %>% 
  ungroup() %>% 
  tidylo::bind_log_odds(color, category, n) 
  
log_odds %>% 
  # filter(exam_type == "bastardy") %>%
  top_n(60, n) %>% 
  ggplot(aes(n, log_odds_weighted, label = category, color = as.factor(color))) +
  geom_hline(yintercept = 0, lty = 2,
             color = "gray50", alpha = 0.5, size = 1.2) +
  ggrepel::geom_text_repel() +
  geom_point() +
  scale_x_log10()

log_odds %>% 
  group_by(color) %>%
  top_n(20, log_odds_weighted) %>%
  ungroup()  %>%
  mutate(category = tidytext::reorder_within(category, log_odds_weighted, color))  %>%
  ggplot(aes(category, log_odds_weighted, fill = color)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~color, scales = "free") +
  coord_flip() +
  tidytext::scale_x_reordered()  +
  scale_y_continuous(expand = c(0,0)) +
  labs(y = "Weighted log odds ratio", x = NULL, title="Most distinctive keyword categories in each cluster")






#########################################################################
# Analysis per time bin
#########################################################################


articles_PY <- 
  cr_details_per_article_tbl %>% 
  select(mybibtex_key, PY) %>% 
  unique() 


articles_1981_2000 <- 
  articles_PY%>% 
  subset(., PY >= 1981 & PY <= 2000) %>% 
  pull(mybibtex_key)

articles_per_timeframe <- 
  data.frame(mybibtex_key = articles_1981_2000,
             timeframe = "1981 to 2000")


articles_2001_2005 <- 
  articles_PY%>% 
  subset(., PY >= 2001 & PY <= 2005) %>% 
  pull(mybibtex_key)

articles_per_timeframe <- 
  rbind(articles_per_timeframe,
        data.frame(mybibtex_key = articles_2001_2005,
                   timeframe = "2001 to 2005"))


articles_2006_2010 <- 
  articles_PY%>% 
  subset(., PY >= 2006 & PY <= 2010) %>% 
  pull(mybibtex_key)

articles_per_timeframe <- 
  rbind(articles_per_timeframe,
        data.frame(mybibtex_key = articles_2006_2010,
                   timeframe = "2006 to 2010"))


articles_2011_2015 <- 
  articles_PY%>% 
  subset(., PY >= 2011 & PY <= 2015) %>% 
  pull(mybibtex_key)

articles_per_timeframe <- 
  rbind(articles_per_timeframe,
        data.frame(mybibtex_key = articles_2011_2015,
                   timeframe = "2011 to 2015"))

articles_2015_2021 <- 
  articles_PY%>% 
  subset(., PY >= 2016 & PY <= 2021) %>% 
  pull(mybibtex_key)

articles_per_timeframe <- 
  rbind(articles_per_timeframe,
        data.frame(mybibtex_key = articles_2015_2021,
                   timeframe = "2016 to 2021"))

article_references_network_node_df_articles_per_timeframe <- 
  dplyr::left_join(article_references_network_node_df, 
                   articles_per_timeframe,
                   by = c("name" = "mybibtex_key"))

article_references_network_node_df_articles_per_timeframe <- 
  subset(article_references_network_node_df_articles_per_timeframe,
         !(color %in% c(10,11)))


# add timeframe info to igraph
df <- igraph::as_data_frame(article_references_network, 'both')

df$vertices <- 
  df$vertices %>%
  subset(., 
         !(color %in% c(10,11))) %>% 
  left_join(article_references_network_node_df_articles_per_timeframe[,c("name", "timeframe")],
            by = "name") %>% 
  na.omit()

updated_g <- igraph::graph_from_data_frame(subset(df$edges, from %in% df$vertices$name & to %in% df$vertices$name),
                                           directed = F,
                                           vertices = df$vertices)

set.seed(1)
article_references_network_node_df_articles_per_timeframe_plot <- 
  ggraph(updated_g, layout = "graphopt") +
  geom_node_point(aes(size=size, fill=factor(color)), shape = 21) + 
  theme_bw() + theme(panel.grid.major = element_blank(), 
                     panel.grid.minor = element_blank(),
                     axis.title=element_blank(),
                     axis.text=element_blank(),
                     axis.ticks=element_blank()) +
  labs(fill = "Cluster") +
  guides(size = FALSE) +
  facet_nodes(~timeframe,
              ncol = 2,
              drop = T,
              shrink = T)
article_references_network_node_df_articles_per_timeframe_plot

ggsave(article_references_network_node_df_articles_per_timeframe_plot,
       filename = file.path("3_output", "SI_Fig_3_article_references_network_node_df_articles_per_timeframe_plot.png"),
       device = "png",
       width = 8,
       height = 10,
       units = "in",
       bg = "white")

############
number_of_articles_per_cluster_per_timeframe_plot <- 
  article_references_network_node_df_articles_per_timeframe %>% 
  group_by(color, timeframe) %>% 
  tally() %>% 
  na.omit() %>% 
  subset(!(color %in% c(10,11))) %>% 
  mutate(cluster = paste("Cluster", color)) %>% 
  ggplot(aes(x = timeframe, y = n, fill = factor(cluster),
             group = factor(cluster)), color = black) +
  geom_col(size = 2) +
  facet_wrap(~cluster) +
  theme_bw() +
  theme(legend.position = "none") +
  xlab("Time bins") +
  ylab("Number of publications")

number_of_articles_per_cluster_per_timeframe_plot

ggsave(number_of_articles_per_cluster_per_timeframe_plot,
       filename = file.path("3_output", "number_of_articles_per_cluster_per_timeframe_plot.png"),
       device = "png",
       width = 15,
       height = 8,
       units = "in",
       bg = "white")

articles_1981_2000_graph <- igraph::delete_vertices(updated_g,
                                                    which(!(names(igraph::components(updated_g)$membership) %in% articles_1981_2000)))

articles_2001_2005_graph <- igraph::delete_vertices(updated_g,
                                                    which(!(names(igraph::components(updated_g)$membership) %in% articles_2001_2005)))

articles_2006_2010_graph <- igraph::delete_vertices(updated_g,
                                                    which(!(names(igraph::components(updated_g)$membership) %in% articles_2006_2010)))

articles_2011_2015_graph <- igraph::delete_vertices(updated_g,
                                                    which(!(names(igraph::components(updated_g)$membership) %in% articles_2011_2015)))
articles_2016_2021_graph <- igraph::delete_vertices(updated_g,
                                                    which(!(names(igraph::components(updated_g)$membership) %in% articles_2015_2021)))

graphs_over_time <- list("1981 to 2000" = articles_1981_2000_graph,
                         "2001 to 2005" = articles_2001_2005_graph,
                         "2006 to 2010" = articles_2006_2010_graph,
                         "2011 to 2015" = articles_2011_2015_graph,
                         "2016 to 2021" = articles_2016_2021_graph)

current_graph_df_list_list <- list()
overall_density_list <- list()
for(current_graph_name in names(graphs_over_time)){
  
  current_graph <- graphs_over_time[[current_graph_name]]
  
  overall_density_list[[current_graph_name]] <- data.frame(timeframe = current_graph_name,
                                                           density = igraph::edge_density(current_graph))
  
  current_graph_df_list <- list() 
  for(current_cluster in as.integer(names(which(table(V(current_graph)$color) > 2)))){
    if(current_cluster != 10 & current_cluster != 11){
      current_cluster_current_graph <- subgraph(graph=current_graph, 
                                                v=which(V(current_graph)$color==current_cluster))
      current_graph_df_list[[current_cluster]] <- 
        data.frame(timeframe = current_graph_name,
                   cluster = current_cluster,
                   cluster_density = igraph::edge_density(current_cluster_current_graph))
    }
    
  }
  
  current_graph_df_list_list[[current_graph_name]] <- do.call(rbind.data.frame, current_graph_df_list)
}


density_over_time_per_cluster_plot <- 
  do.call(rbind.data.frame, current_graph_df_list_list) %>% 
  mutate(cluster = paste("Cluster", cluster)) %>% 
  ggplot(aes(x = timeframe, y = cluster_density, color = factor(cluster), fill = factor(cluster),
             group = factor(cluster))) +
  geom_line() +
  geom_point(size = 2) +
  facet_wrap(~cluster,
             ncol = 3) +
  theme_bw() +
  theme(legend.position = "none") +
  xlab("Time bins") +
  ylab("Density")

density_over_time_per_cluster_plot

ggsave(density_over_time_per_cluster_plot,
       filename = file.path("3_output", "density_over_time_per_cluster_plot.png"),
       device = "png",
       width = 15,
       height = 8,
       units = "in",
       bg = "white")


density_over_time_plot <- 
  do.call(rbind.data.frame, overall_density_list) %>% 
  ggplot(aes(x = timeframe, y = density)) +
  # geom_line() +
  geom_point(size = 5) +
  theme_bw() +
  xlab("Time bins") +
  ylab("Density")
density_over_time_plot

ggsave(density_over_time_plot,
       filename = file.path("3_output", "Fig_8_density_over_time_plot.png"),
       device = "png",
       width = 6,
       height = 5,
       units = "in",
       bg = "white")




# density_over_time_per_cluster_plot + number_of_articles_per_cluster_per_timeframe_plot
pubs_per_cluster_per_timebin_plus_density <-
  dplyr::left_join(
    article_references_network_node_df_articles_per_timeframe %>% 
      group_by(color, timeframe) %>% 
      tally() %>% 
      na.omit() %>% 
      subset(!(color %in% c(10,11))) %>% 
      mutate(cluster = paste("Cluster", color)),
    
    do.call(rbind.data.frame, current_graph_df_list_list) %>% 
      mutate(cluster = paste("Cluster", cluster))
  )

pubs_per_cluster_per_timebin_plus_density_plot <- 
pubs_per_cluster_per_timebin_plus_density %>% 
  ggplot() +
  geom_col(aes(x = timeframe, 
               y = n, 
               fill = factor(cluster),
               group = factor(cluster)),
           size = 2) +
  geom_line(aes(x = timeframe, y = cluster_density*100, 
                group = factor(cluster)),
            color = "black") +
  geom_point(aes(x = timeframe, y = cluster_density*100, 
                 group = factor(cluster)),
             size = 2) +
  scale_y_continuous("Number of publications", sec.axis = sec_axis(~./100, name = "Density")) +
  facet_wrap(~cluster) +
  theme_bw() +
  theme(legend.position = "none") +
  xlab("Time bins") +
  ylab("Number of publications")

pubs_per_cluster_per_timebin_plus_density_plot

ggsave(pubs_per_cluster_per_timebin_plus_density_plot,
       filename = file.path("3_output", "Fig_9_pubs_per_cluster_per_timebin_plus_density_plot.png"),
       device = "png",
       width = 15,
       height = 8,
       units = "in",
       bg = "white")


















