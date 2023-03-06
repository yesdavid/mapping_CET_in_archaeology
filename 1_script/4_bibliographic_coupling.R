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
        formula =  ~ mybibtex_key + CR_full)

xtab_cr_paper_paper[xtab_cr_paper_paper > 1] <- 1

# xtab_cr_paper_paper[5:10, 1:5]
# table(xtab_cr_paper_paper)


#####################################################################################################################################
# Calculate cosine (similarity) distance between characters
cosine_sim <- as.dist(xtab_cr_paper_paper %*% t(xtab_cr_paper_paper) / (sqrt(rowSums(xtab_cr_paper_paper^2) %*% t(rowSums(xtab_cr_paper_paper^2)))))
# hist(cosine_sim)
# summary(cosine_sim)

cosine_sim[is.na(cosine_sim)] <- 0 
# summary(cosine_sim)

# autograph(as.matrix(cosine_sim))

# Create an igraph object 
article_references_network <- as.matrix(cosine_sim) %>% 
  graph_from_adjacency_matrix(mode = "undirected", 
                              weighted = TRUE)

# only largest connected graph
article_references_network <- induced_subgraph(
  article_references_network, V(article_references_network)[components(article_references_network)$membership == which.max(components(article_references_network)$csize)]
)

# Community detection algorithm 
set.seed(123)
article_references_network_community_louvain <- cluster_louvain(article_references_network) 

table(article_references_network_community_louvain$membership)

# Attach communities to relevant vertices
V(article_references_network)$color <- article_references_network_community_louvain$membership

# Fig_5_bibliographic_coupling_network_graphopt_plot_noLabels.png
set.seed(1)
article_references_network_louvain <- 
ggraph(article_references_network,
       layout = "igraph",
       algorithm = "fr") +
  geom_edge_arc(strength=0.2, width=0.1, alpha=.15) + 
  geom_node_point(aes(fill=factor(color)), 
                  shape = 21) + 
  theme_void() +
  labs(fill = "Cluster") +
  guides(size = F)

article_references_network_louvain

ggsave(article_references_network_louvain,
       filename = file.path("3_output", "Fig_5_bibliographic_coupling_network_graphopt_plot_noLabels.png"),
       device = "png",
       width = 8,
       height = 8,
       units = "in",
       bg = "white")


##############################################################################################################
# calculate centrality measures
##############################################################################################################

article_references_network_node_df <- tibble::as_tibble(igraph::as_data_frame(article_references_network, "vertices"))
table(article_references_network_node_df$color)

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
mean_median_closeness_centrality
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

n_articles_per_cluster <- data.frame(cluster = c(1:length(as.vector(table(article_references_network_node_df$color)))),
                                        n = as.vector(table(article_references_network_node_df$color)))

####
Articles_with_cluster_association_and_closeness_and_degree_centrality <- 
WoS_bib_included %>% 
  select(mybibtex_key,
         AU,
         TI,
         SO,
         DI) %>% 
  mutate(AU = tolower(AU),
         TI = tolower(TI),
         SO = tolower(SO),
         DI = tolower(DI)) %>% 
  dplyr::left_join(., article_references_network_node_df %>% 
                     select(name, color, PY, size, closeness_centrality, degree_centrality),
                   by = c("mybibtex_key" = "name")) %>% 
  rename(., Cluster = color,
         "Year of publication" = PY,
         "Author(s)" = AU,
         Title = TI,
         Outlet = SO,
         DOI = DI) %>% 
  arrange(., "Cluster") 

readr::write_csv(Articles_with_cluster_association_and_closeness_and_degree_centrality,
                   file = file.path("3_output", "Articles_with_cluster_association_and_closeness_and_degree_centrality.csv"))

####
top_5_outlets_per_cluster <- 
Articles_with_cluster_association_and_closeness_and_degree_centrality %>% 
  group_by(Cluster) %>% 
  select(Cluster, Outlet) %>% 
  table() %>% 
  melt(., id.vars=c("cluster", "Outlet")) %>% 
  filter(value != 0 & !(Cluster %in% c(6,7))) %>%
  group_by(Cluster) %>% 
  arrange(desc(value)) %>%
  filter(value >= 1) %>% 
  top_n(5) %>%
  ungroup() %>%
  mutate(Outlet = tidytext::reorder_within(Outlet, value, Cluster)) %>% 
  ggplot(aes(x = value, y = Outlet)) +
  geom_col() +
  tidytext::scale_y_reordered()  +
  facet_wrap(~Cluster, 
             scales = "free_y", 
             nrow = 6)+
  theme_bw() +
  xlab("n")

top_5_outlets_per_cluster

ggsave(top_5_outlets_per_cluster,
       filename = file.path("3_output", "top_5_outlets_per_cluster.png"),
       device = "png",
       width = 8,
       height = 10,
       units = "in",
       bg = "white")
  

##################################
# run anovas and pairwise t tests
##################################

anova_list <- list()
anova_res_list <- list()
pairwise_t_list <- list()
for(i in c("eigen_centrality","betweenness_centrality","closeness_centrality","degree_centrality")){
  anova_list[[i]] <- aov(article_references_network_node_df[[i]] ~ factor(article_references_network_node_df$color))
  anova_res_list[[i]] <- summary(anova_list[[i]])
  pairwise_t_list[[i]] <- pairwise.t.test(article_references_network_node_df[[i]], 
                                          article_references_network_node_df$color, 
                                          p.adjust.method = "bonferroni")
}

anova_list$closeness_centrality
anova_res_list$closeness_centrality
pairwise_t_list$closeness_centrality


# modularity
modularity <- igraph::modularity(article_references_network,
                                 article_references_network_community_louvain$membership)
round(modularity, digits = 3)

# network density for whole network
density <- igraph::edge_density(article_references_network)
round(density, digits = 3)

# transitivity
transitivity <- igraph::transitivity(article_references_network)
round(transitivity, digits = 3)

# small world index; crashes R
# sw <- qgraph::smallworldness(article_references_network, B = 1000)


# CLOSENESS BOXPLOT
network_centralities_plots <- 
article_references_network_node_df %>% 
  group_by(color) %>% 
  dplyr::select(# eigen_centrality, 
                # betweenness_centrality, 
                # degree_centrality, 
                closeness_centrality) %>% 
  subset(., !(color %in% c(6,7))) %>%
  pivot_longer(cols = colnames(.)[2:ncol(.)]) %>% 
ggplot(aes(x = color,
           y = value,
           group = color,
           fill = as.factor(color))) +
  geom_boxplot()+
  theme_bw(base_size = 18) +
  theme(legend.position = "none") +
  xlab("Cluster") +
  ylab("Scaled closeness centrality") #+
  # scale_x_continuous(breaks = c(1:5)) #+
  # facet_wrap(~name) 

network_centralities_plots

ggsave(network_centralities_plots,
       filename = file.path("3_output", "Fig_6_bibliographic_coupling_network_closenessCentrality_plot.png"),
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

article_references_network_cluster_densities <- 
  data.frame(cluster = c(1:5),
             edge_density = c(round(igraph::edge_density(graph_1), digits = 3),
                              round(igraph::edge_density(graph_2), digits = 3),
                              round(igraph::edge_density(graph_3), digits = 3),
                              round(igraph::edge_density(graph_4), digits = 3),
                              round(igraph::edge_density(graph_5), digits = 3)))

readr::write_csv(article_references_network_cluster_densities,
                 file = file.path("3_output", "bibliographic_coupling_network_clusters_w_densities.csv"))


# #############################################################################################################
# # Chi-Squared Test
# #############################################################################################################

WoS_bib_included_w_clusters <- dplyr::left_join(WoS_bib_included,
                                                article_references_network_node_df[,c("name", "color")],
                                                by = c("mybibtex_key" = "name"))


thesauriert_joined_binary_incidence_df2 <- as.data.frame(thesauriert_joined_binary_incidence) #
thesauriert_joined_binary_incidence_df2$mybibtex_key <- rownames(thesauriert_joined_binary_incidence_df2)

thesauriert_joined_binary_incidence_df2 <- dplyr::left_join(thesauriert_joined_binary_incidence_df2, article_references_network_node_df[,c("name", "color")],
                          by = c("mybibtex_key" = "name"))

rownames(thesauriert_joined_binary_incidence_df2) <- thesauriert_joined_binary_incidence_df2$mybibtex_key
thesauriert_joined_binary_incidence_df <- thesauriert_joined_binary_incidence_df2[,-which(colnames(thesauriert_joined_binary_incidence_df2) == "mybibtex_key")]

merge_assoc <-
  thesauriert_joined_binary_incidence_df%>%
  pivot_longer(-color, names_to = "category") %>%
  na_if(0) %>%
  na.omit %>%
  select(color, category) %>% 
  filter((color %in% c(1:5)))
table(merge_assoc)

chisq_test_res <-
as_tibble(table(merge_assoc)) %>%
  pivot_wider(names_from = color, values_from = n) %>%
  as.data.frame() %>%
  tibble::column_to_rownames('category') %>%
  # select(-c("10","11")) %>%
  chisq.test()

chisq_test_res

dummy_df <-
  data.frame(cluster = 1:5,
             GEOGR_LOCATION = "NA",
             GEOGR_LOCATION_FINE = "NA",
             MATERIAL_CULTURE = "NA",
             CET = "NA",
             PERIOD = "NA",
             METHODS = "NA",
             TOPIC = "NA")

cluster_keyword_list <- list()
for (cluster_index in 1:5) {

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


n_articles_per_cluster %>% 
  dplyr::left_join(.,
                   mean_median_closeness_centrality,
                   by = "cluster") %>% 
  dplyr::filter(cluster %in% c(1:5)) %>% 
  dplyr::select(-median_closeness) %>% 
  dplyr::left_join(.,
                   article_references_network_cluster_densities,
                   by = "cluster") %>% 
  dplyr::left_join(.,
                   dummy_df,
                   by = "cluster") %>% 
  dplyr::arrange(desc(mean_closeness)) %>% 
  readr::write_csv(.,
                   file = file.path("3_output", "chiSq_residuals_keywords_clusters.csv"))
  

# ##############################################################################################################
# # log odds
# ##############################################################################################################

# based on binary paper incidence

library(tidylo)

table(merge_assoc)

log_odds <-
merge_assoc %>%
  group_by(color) %>%
  count(category, sort = T) %>%
  ungroup() %>%
  tidylo::bind_log_odds(color, category, n)

weighted_log_odds_1_plot <-
log_odds %>%
    group_by(color) %>%
    arrange(desc(log_odds_weighted)) %>%
    slice(1:10, (n()-4):n(), with_ties = FALSE) %>% # top 10 and bottom 5
    ungroup() %>%
  mutate(category = recode(category, "African_Stone_Age" = "African Stone Age",
                           "Projectile_Points" = "Projectile points",
                           "Symbolic_Communication" = "Symbolic communication",
                           "Central_Northern_Asia" = "Central/Northern Asia",
                           "Maritime_Lacustrine_Riverine" = "Maritime/lacustrine/riverine",	
                           "Absolute_Dating" ="Absolute dating" ,	
                           "Ethics_Morality" = "Ethics/morality" ,
                           "Human_Evolution" = "Human evolution",
                           "Climate_and_Catastrophes" = "Climate and catastrophes",	
                           "Sites_Settlement" = "Sites and settlements",	
                           "East_Asia" = "East Asia",	
                           "Evolutionary_Psychology" = "Evolutionary psychology",	
                           "Bronze_Age" = "Bronze Age",	
                           "Typology_Taxonomy" ="Typology taxonomy" ,	
                           "North_America" = "North America" ,
                           "Oceania_Polynesia" ="Oceania/Polynesia" ,	
                           "Iron_age" = "Iron Age",	
                           "Socio_political" = "Socio-political" ,
                           "In_equality_Prestige" = "(In-)equality/prestige",	
                           "Cultural_Transmission"= "Cultural transmission" ,
                           "Lithic_Tools_Technology"="Lithic tools/technology",
                           "Material_culture"="Material culture" ,
                           "Health_Medicine" ="Health/medicine" ,	
                           "Australia_Tasmania" = "Australia/Tasmania",	
                           "Religion_Ritual_Tradition"="Religion/ritual/tradition" ,	
                           "Meso_South_America"="Meso/South America" ,	
                           "Behav_Ecology"="Behavioural ecology" ,	
                           "Gene_Culture_CoEvol"="Gene-culture co-evolution" ,	
                           "Play_objects"="Play objects" ,	
                           "Geochronological_terms" ="Geochronological terms",
                           "Theory_of_science"="Theory of science" ,
                           "Lithic_Raw_Material" ="Lithic raw material",	
                           "Cross_Cultural"="Cross-cultural" ,
                           "Theoretical_approaches"="Theoretical approaches")) %>% 
  ggplot(aes(n, log_odds_weighted, 
             label = category,
             color = as.factor(color))) +
  geom_hline(yintercept = 0, lty = 2,
             color = "gray50", alpha = 0.5, size = 1.2) +
  ggrepel::geom_text_repel(force=8) +
  geom_point() +
  theme_bw() +
  theme(legend.position = "none") +
  facet_wrap(~color,
             scales = "free_x",
             ncol =2) +
  labs(y = "Weighted log odds ratio", 
       x = "Keyword frequency in cluster")
weighted_log_odds_1_plot

weighted_log_odds_2_plot <-
  log_odds %>%
  group_by(color) %>%
  arrange(desc(log_odds_weighted)) %>%
  top_n(20) %>% 
  ungroup() %>%
  mutate(category = tidytext::reorder_within(category, log_odds_weighted, color))  %>%
  ggplot(aes(category, log_odds_weighted, fill = color)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~color, scales = "free") +
  coord_flip() +
  tidytext::scale_x_reordered()  +
  scale_y_continuous(expand = c(0,0)) +
  theme_bw() +
  theme(legend.position = "none") +
  labs(y = "Weighted log odds ratio", 
       x = NULL, title="Most distinctive keyword categories in each cluster")
weighted_log_odds_2_plot


ggsave(weighted_log_odds_1_plot,
       filename = file.path("3_output", "Fig_8_weighted_log_odds_BINARY_1_plot.png"),
       device = "png",
       width = 10,
       height = 10,
       bg = "white",
       units = "in")
ggsave(weighted_log_odds_2_plot,
       filename = file.path("3_output", "Supplementary_Fig_S1_weighted_log_odds_BINARY_2_plot.png"),
       device = "png",
       width = 14,
       height = 7,
       bg = "white",
       units = "in")


#########################################################################
# Analysis per time bin
#########################################################################

articles_PY <- 
  cr_details_per_article_tbl %>% 
  select(mybibtex_key, PY) %>% 
  unique() 

articles_1981_2001 <- 
  articles_PY%>% 
  subset(., PY >= 1981 & PY <= 2001) %>% 
  pull(mybibtex_key)
articles_per_timeframe <- 
  data.frame(mybibtex_key = articles_1981_2001,
             timeframe = "1981 to 2001")

articles_2002_2006 <- 
  articles_PY%>% 
  subset(., PY >= 2002 & PY <= 2006) %>% 
  pull(mybibtex_key)
articles_per_timeframe <- 
  rbind(articles_per_timeframe,
        data.frame(mybibtex_key = articles_2002_2006,
                   timeframe = "2002 to 2006"))

articles_2007_2011 <- 
  articles_PY%>% 
  subset(., PY >= 2007 & PY <= 2011) %>% 
  pull(mybibtex_key)
articles_per_timeframe <- 
  rbind(articles_per_timeframe,
        data.frame(mybibtex_key = articles_2007_2011,
                   timeframe = "2007 to 2011"))

articles_2012_2016 <- 
  articles_PY%>% 
  subset(., PY >= 2012 & PY <= 2016) %>% 
  pull(mybibtex_key)
articles_per_timeframe <- 
  rbind(articles_per_timeframe,
        data.frame(mybibtex_key = articles_2012_2016,
                   timeframe = "2012 to 2016"))

articles_2017_2021 <- 
  articles_PY%>% 
  subset(., PY >= 2017 & PY <= 2021) %>% 
  pull(mybibtex_key)
articles_per_timeframe <- 
  rbind(articles_per_timeframe,
        data.frame(mybibtex_key = articles_2017_2021,
                   timeframe = "2017 to 2021"))

article_references_network_node_df_articles_per_timeframe <- 
  dplyr::left_join(article_references_network_node_df, 
                   articles_per_timeframe,
                   by = c("name" = "mybibtex_key"))

article_references_network_node_df_articles_per_timeframe <- 
  subset(article_references_network_node_df_articles_per_timeframe,
         !(color %in% c(6,7)))

# add timeframe info to igraph
df <- igraph::as_data_frame(article_references_network, 'both')

df$vertices <- 
  df$vertices %>%
  subset(.,
         color %in% c(1:5)) %>%
  left_join(article_references_network_node_df_articles_per_timeframe[,c("name", "timeframe")],
            by = "name") %>% 
  na.omit()

updated_g <- igraph::graph_from_data_frame(subset(df$edges, 
                                                  from %in% df$vertices$name & to %in% df$vertices$name),
                                           directed = F,
                                           vertices = df$vertices)

# Supplementary_Fig_S4_article_references_network_node_df_articles_per_timeframe_plot.png
set.seed(1)
article_references_network_node_df_articles_per_timeframe_plot <- 
  ggraph(updated_g,
         layout = "igraph",
         algorithm = "fr") +
  geom_node_point(aes(fill=factor(color)), shape = 21) + 
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
       filename = file.path("3_output", "Supplementary_Fig_S4_article_references_network_node_df_articles_per_timeframe_plot.png"),
       device = "png",
       width = 8,
       height = 10,
       units = "in",
       bg = "white")


#### 

articles_1981_2001_graph <- igraph::delete_vertices(updated_g,
                                                    which(!(names(igraph::components(updated_g)$membership) %in% articles_1981_2001)))

articles_2002_2006_graph <- igraph::delete_vertices(updated_g,
                                                    which(!(names(igraph::components(updated_g)$membership) %in% articles_2002_2006)))

articles_2007_2011_graph <- igraph::delete_vertices(updated_g,
                                                    which(!(names(igraph::components(updated_g)$membership) %in% articles_2007_2011)))

articles_2012_2016_graph <- igraph::delete_vertices(updated_g,
                                                    which(!(names(igraph::components(updated_g)$membership) %in% articles_2012_2016)))
articles_2017_2021_graph <- igraph::delete_vertices(updated_g,
                                                    which(!(names(igraph::components(updated_g)$membership) %in% articles_2017_2021)))

graphs_over_time <- list("1981 to 2001" = articles_1981_2001_graph,
                         "2002 to 2006" = articles_2002_2006_graph,
                         "2007 to 2011" = articles_2007_2011_graph,
                         "2012 to 2016" = articles_2012_2016_graph,
                         "2017 to 2021" = articles_2017_2021_graph)

current_graph_df_list_list <- list()
overall_density_list <- list()
for(current_graph_name in names(graphs_over_time)){
  
  current_graph <- graphs_over_time[[current_graph_name]]
  
  overall_density_list[[current_graph_name]] <- data.frame(timeframe = current_graph_name,
                                                           density = igraph::edge_density(current_graph))
  
  current_graph_df_list <- list() 
  for(current_cluster in as.integer(names(which(table(V(current_graph)$color) > 2)))){
    if(current_cluster != 6 & current_cluster != 7){
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
round(do.call(rbind.data.frame, overall_density_list)$density, digits = 3)

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


density_over_time_data <- 
do.call(rbind.data.frame, overall_density_list)

density_over_time_plot <- 
  density_over_time_data %>% 
  ggplot(aes(x = timeframe, y = density)) +
  # geom_line() +
  geom_point(size = 5) +
  theme_bw() +
  xlab("Time bins") +
  ylab("Density")
density_over_time_plot

ggsave(density_over_time_plot,
       filename = file.path("3_output", "Supplementary_Fig_S5_density_over_time_plot.png"),
       device = "png",
       width = 6,
       height = 5,
       units = "in",
       bg = "white")

density_over_time_data$density <- round(density_over_time_data$density, digits = 3)
density_over_time_data


# density_over_time_per_cluster_plot + number_of_articles_per_cluster_per_timeframe_plot
pubs_per_cluster_per_timebin_plus_density <-
  dplyr::left_join(
    article_references_network_node_df_articles_per_timeframe %>% 
      group_by(color, timeframe) %>% 
      tally() %>% 
      na.omit() %>% 
      subset(!(color %in% c(6,7)))
  )

pubs_per_cluster_per_timebin_plus_density_plot <- 
pubs_per_cluster_per_timebin_plus_density %>% 
  filter(!(color %in% c(6,7))) %>% 
  dplyr::mutate(timeframe = gsub(" to ", "-", timeframe)) %>% 
  ggplot() +
  geom_col(aes(x = timeframe, 
               y = n, 
               fill = factor(color),
               group = factor(color)),
           size = 2) +
  geom_line(aes(x = timeframe, y = cluster_density*100, 
                group = factor(color)),
            color = "black") +
  geom_point(aes(x = timeframe, y = cluster_density*100, 
                 group = factor(color)),
             size = 2) +
  scale_y_continuous("Number of publications", sec.axis = sec_axis(~./100, name = "Density")) +
  facet_wrap(~color,
             ncol = 2) +
  theme_bw() +
  theme(legend.position = "none") +
  xlab("Time bins") +
  ylab("Number of publications")

pubs_per_cluster_per_timebin_plus_density_plot

ggsave(pubs_per_cluster_per_timebin_plus_density_plot,
       filename = file.path("3_output", "Fig_7_pubs_per_cluster_per_timebin_plus_density_plot.png"),
       device = "png",
       width = 9,
       height = 8,
       units = "in",
       bg = "white")


















