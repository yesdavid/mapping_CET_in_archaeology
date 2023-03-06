# Script to compare the effect of different thresholds to the structure of the network and the position of the clusters
# The script shows that the clusters remain coherent entities even if we introduce a threshold.

# requires that lines 1-63 from script 4_bibliographic_coupling.R have been run.

color_palette <- unique(ggplot_build(article_references_network_louvain)$data[[2]][c("fill", "group")])


# ####### 0.01
# # apply threshold
# thresh_0.01 <- cosine_sim
# thresh_0.01[which(thresh_0.01 < max(thresh_0.01)*0.01)] <- 0
# # Create an igraph object 
# article_references_network_thresh_0.01 <- as.matrix(thresh_0.01) %>% 
#   graph_from_adjacency_matrix(mode = "undirected", 
#                               weighted = TRUE)
# # only largest connected graph
# article_references_network_thresh_0.01 <- induced_subgraph(
#   article_references_network_thresh_0.01, V(article_references_network_thresh_0.01)[components(article_references_network_thresh_0.01)$membership == which.max(components(article_references_network_thresh_0.01)$csize)]
# )
# # Attach communities to relevant vertices
# 
# new_names_df <- data.frame(names = V(article_references_network_thresh_0.01)$name)
# clusters_no_thresh_df <- data.frame(names = article_references_network_community_louvain$name,
#                                     color = article_references_network_community_louvain$membership)
# V(article_references_network_thresh_0.01)$color <- dplyr::left_join(new_names_df,
#                                                                     clusters_no_thresh_df,
#                                                                     by = "names")$color
# # Plot network with threshold
# set.seed(1)
# article_references_network_thresh_0.01_plot <- 
#   ggraph(article_references_network_thresh_0.01,
#          layout = "igraph",
#          algorithm = "fr") +
#   geom_edge_arc(strength=0.2, width=0.1, alpha=.15) + 
#   geom_node_point(aes(fill=factor(color)), 
#                   shape = 21) + 
#   theme_void() +
#   labs(fill = "Cluster") +
#   guides(size = F) + scale_fill_manual(values = color_palette$fill) +
#   ggtitle("Threshold = 0.01")
# # article_references_network_thresh_0.01_plot


####### 0.02
# apply threshold
thresh_0.02 <- cosine_sim
thresh_0.02[which(thresh_0.02 < max(thresh_0.02)*0.02)] <- 0
# Create an igraph object 
article_references_network_thresh_0.02 <- as.matrix(thresh_0.02) %>% 
  graph_from_adjacency_matrix(mode = "undirected", 
                              weighted = TRUE)
# only largest connected graph
article_references_network_thresh_0.02 <- induced_subgraph(
  article_references_network_thresh_0.02, V(article_references_network_thresh_0.02)[components(article_references_network_thresh_0.02)$membership == which.max(components(article_references_network_thresh_0.02)$csize)]
)
# Attach communities to relevant vertices

new_names_df <- data.frame(names = V(article_references_network_thresh_0.02)$name)
clusters_no_thresh_df <- data.frame(names = article_references_network_community_louvain$name,
                                    color = article_references_network_community_louvain$membership)
V(article_references_network_thresh_0.02)$color <- dplyr::left_join(new_names_df,
                                                                    clusters_no_thresh_df,
                                                                    by = "names")$color
# Plot network with threshold
set.seed(1)
article_references_network_thresh_0.02_plot <- 
  ggraph(article_references_network_thresh_0.02,
         layout = "igraph",
         algorithm = "fr") +
  geom_edge_arc(strength=0.2, width=0.1, alpha=.15) + 
  geom_node_point(aes(fill=factor(color)), 
                  shape = 21) + 
  theme_void() +
  labs(fill = "Cluster") +
  guides(size = F) + scale_fill_manual(values = color_palette$fill) +
  ggtitle("Threshold = 0.02")
# article_references_network_thresh_0.02_plot

####### 0.04
# apply threshold
thresh_0.04 <- cosine_sim
thresh_0.04[which(thresh_0.04 < max(thresh_0.04)*0.04)] <- 0
# Create an igraph object 
article_references_network_thresh_0.04 <- as.matrix(thresh_0.04) %>% 
  graph_from_adjacency_matrix(mode = "undirected", 
                              weighted = TRUE)
# only largest connected graph
article_references_network_thresh_0.04 <- induced_subgraph(
  article_references_network_thresh_0.04, V(article_references_network_thresh_0.04)[components(article_references_network_thresh_0.04)$membership == which.max(components(article_references_network_thresh_0.04)$csize)]
)
# Attach communities to relevant vertices

new_names_df <- data.frame(names = V(article_references_network_thresh_0.04)$name)
clusters_no_thresh_df <- data.frame(names = article_references_network_community_louvain$name,
                                    color = article_references_network_community_louvain$membership)
V(article_references_network_thresh_0.04)$color <- dplyr::left_join(new_names_df,
                                                                    clusters_no_thresh_df,
                                                                    by = "names")$color
# Plot network with threshold
set.seed(1)
article_references_network_thresh_0.04_plot <- 
  ggraph(article_references_network_thresh_0.04,
         layout = "igraph",
         algorithm = "fr") +
  geom_edge_arc(strength=0.2, width=0.1, alpha=.15) + 
  geom_node_point(aes(fill=factor(color)), 
                  shape = 21) + 
  theme_void() +
  labs(fill = "Cluster") +
  guides(size = F) + scale_fill_manual(values = color_palette$fill) +
  ggtitle("Threshold = 0.04")
# article_references_network_thresh_0.04_plot

####### 0.08
# apply threshold
thresh_0.08 <- cosine_sim
thresh_0.08[which(thresh_0.08 < max(thresh_0.08)*0.08)] <- 0
# Create an igraph object 
article_references_network_thresh_0.08 <- as.matrix(thresh_0.08) %>% 
  graph_from_adjacency_matrix(mode = "undirected", 
                              weighted = TRUE)
# only largest connected graph
article_references_network_thresh_0.08 <- induced_subgraph(
  article_references_network_thresh_0.08, V(article_references_network_thresh_0.08)[components(article_references_network_thresh_0.08)$membership == which.max(components(article_references_network_thresh_0.08)$csize)]
)
# Attach communities to relevant vertices

new_names_df <- data.frame(names = V(article_references_network_thresh_0.08)$name)
clusters_no_thresh_df <- data.frame(names = article_references_network_community_louvain$name,
                                    color = article_references_network_community_louvain$membership)
V(article_references_network_thresh_0.08)$color <- dplyr::left_join(new_names_df,
                                                                    clusters_no_thresh_df,
                                                                    by = "names")$color
# Plot network with threshold
set.seed(1)
article_references_network_thresh_0.08_plot <- 
  ggraph(article_references_network_thresh_0.08,
         layout = "igraph",
         algorithm = "fr") +
  geom_edge_arc(strength=0.2, width=0.1, alpha=.15) + 
  geom_node_point(aes(fill=factor(color)), 
                  shape = 21) + 
  theme_void() +
  labs(fill = "Cluster") +
  guides(size = F) + scale_fill_manual(values = color_palette$fill) +
  ggtitle("Threshold = 0.08")
# article_references_network_thresh_0.08_plot



# ####### 0.10
# # apply threshold
# thresh_0.10 <- cosine_sim
# thresh_0.10[which(thresh_0.10 < max(thresh_0.10)*0.10)] <- 0
# # Create an igraph object 
# article_references_network_thresh_0.10 <- as.matrix(thresh_0.10) %>% 
#   graph_from_adjacency_matrix(mode = "undirected", 
#                               weighted = TRUE)
# # only largest connected graph
# article_references_network_thresh_0.10 <- induced_subgraph(
#   article_references_network_thresh_0.10, V(article_references_network_thresh_0.10)[components(article_references_network_thresh_0.10)$membership == which.max(components(article_references_network_thresh_0.10)$csize)]
# )
# # Attach communities to relevant vertices
# 
# new_names_df <- data.frame(names = V(article_references_network_thresh_0.10)$name)
# clusters_no_thresh_df <- data.frame(names = article_references_network_community_louvain$name,
#                                     color = article_references_network_community_louvain$membership)
# V(article_references_network_thresh_0.10)$color <- dplyr::left_join(new_names_df,
#                                                                     clusters_no_thresh_df,
#                                                                     by = "names")$color
# # Plot network with threshold
# set.seed(1)
# article_references_network_thresh_0.10_plot <- 
#   ggraph(article_references_network_thresh_0.10,
#          layout = "igraph",
#          algorithm = "fr") +
#   geom_edge_arc(strength=0.2, width=0.1, alpha=.15) + 
#   geom_node_point(aes(fill=factor(color)), 
#                   shape = 21) + 
#   theme_void() +
#   labs(fill = "Cluster") +
#   guides(size = F) + scale_fill_manual(values = color_palette$fill) +
#   ggtitle("Threshold = 0.10")
# # article_references_network_thresh_0.10_plot


###################################################
# Supplementary_Fig_S6_comparison_of_thresholds.png
###################################################

plot_grid_all_versions <- 
cowplot::plot_grid(article_references_network_louvain +
                     ggtitle("No threshold") +
                     theme(plot.title = element_text(hjust = 0.5),
                           legend.position = "top") + 
                     guides(fill = guide_legend(nrow = 1)),
                   
                   article_references_network_thresh_0.02_plot +
                     theme(plot.title = element_text(hjust = 0.5),
                           legend.position = "top") + 
                     guides(fill = guide_legend(nrow = 1)),
                   
                   article_references_network_thresh_0.04_plot +
                     theme(plot.title = element_text(hjust = 0.5),
                           legend.position = "top") + 
                     guides(fill = guide_legend(nrow = 1)),
                   
                   article_references_network_thresh_0.08_plot +
                     theme(plot.title = element_text(hjust = 0.5),
                           legend.position = "top") + 
                     guides(fill = guide_legend(nrow = 1)))

ggsave(plot_grid_all_versions,
       filename = file.path("3_output", "Supplementary_Fig_S6_comparison_of_thresholds.png"),
       device = "png",
       width = 10,
       height = 12,
       units = "in",
       bg = "white")




