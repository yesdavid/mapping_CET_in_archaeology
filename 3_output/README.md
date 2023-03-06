# Output

- _Articles_with_cluster_association_and_closeness_and_degree_centrality.csv_: All articles contained in this study with additional, derived information. It contains information about the authors, the article titles, outlets, DOIs, years of publication -- derived from WoS --, and the cluster labels assigned by the _Louvain_ algorithm, as well as the articles' closeness and degree centralities derived from the bibliographic coupling analysis. 
- _bibliographic_coupling_network_articles_with_netw_measures_w_clusters.csv_ contains the same data as above, although most of the WoS information was removed and only the network data has been kept.


## Main text

### Numbers

- _chiSq_residuals_keywords_clusters.csv_: Results of the chi-Squared test (table 1)  for each cluster and each meta-category. Only the categories (first-order thesaurified keywords) significantly associated with each cluster are shown. The table is sorted by the clusters’ mean closness centrality in descending order.

- _results_bibliometrix_biblioAnalysis.txt_: Output of the `bibliometrix::biblioAnalysis()` function. It summarises the articles contained in the WoS derived _.bib_ file. It also produces table 2 from the main text.

### Figures

1. _Fig_1_Mesoudi_et_al_2006:Fig1.png_: Comparison of major subdivisions within evolutionary biology and corresponding disciplines currently or potentially employed in the study of cultural evolution (right side). Figure redrawn after Mesoudi et al. (2006:Figure 1).

2. _Fig_2_number_of_pubs_per_year_plot.png_: Number of yearly archaeological and anthropological publications within the field of cultural evolution and cultural transmission theory available on the WoS database.

2. _Fig_3_period_keyword_occ_per_year.png_: Number of archaeological period keyword occurrences per year.

2. _Fig_4_METHODS_keyword_occ_per_year.png_: Relative appearance of keywords related to methods used in the whole corpus per year. The y-axis is scaled dynamically for each subplot. A comparison of trends can only be done within categories but not between categories.
2. _Fig_5_bibliographic_coupling_network_graphopt_plot_noLabels.png_: Bibliographic coupling network of the 630 connected articles. The clusters retrieved by the Louvain algorithm are differentiated by colour. The graph was visualised using the Fruchterman-Reingold method in the ggraph package (Pedersen 2021). As explained below, we characterised the first five clusters as follows: cluster 1: “Niche construction theory|psychology|gene-culture co-evolution” (n=148), cluster 2: “Climate change|social adaptations|population density|Neolithic” (n=92), cluster 3: “(Foundational) cultural evolutionary theory|methods” (n=150), cluster 4: “Complex human behaviour| cultural transmission|Early Stone Age“ (n=154), cluster 5: “Ethnoarchaeology|cultural complexity|chiefdoms|early states” (n=81).
2. _Fig_6_bibliographic_coupling_network_closenessCentrality_plot.png_: Boxplots of the scaled closeness centrality per retrieved bibliographic coupling cluster. Closeness centrality is defined as the average length of the shortest path between the node of interest and all other nodes. Here, closeness centrality is used to identify core elements of the network.
2. _Fig_7_pubs_per_cluster_per_timebin_plus_density_plot.png_: The number of publications per time bin per cluster are represented as columns. The bibliographic coupling network density (ratio of existing edges to all possible edges) for each time bin for each cluster are represented as point-and-line plots. The network density works as a proxy for the conformity/diversity within the individual clusters of the network.
2. _Fig_8_weighted_log_odds_BINARY_1_plot.png_: Plot of the weighted log odds ratio for clusters 1-5. The y-axis shows the weighted log odds ratio, whereas the x-axis shows the frequency of the respective keywords in each cluster. High values on the y-axis represent strong cluster association.


## Supplementary information

### Numbers

- _SI_table_2_ttest_closeness_centrality.csv_: The result of the pairwise comparisons of the mean closeness centrality between clusters using a t-tests with pooled standard deviation and Bonferroni adjustment method.

### Figures

1. _Supplementary_Fig_S1_weighted_log_odds_BINARY_2_plot.png_: Most distinctive thesaurified keywords per cluster in the order of their weighted log odds ratios.
1. _Supplementary_Fig_S2_CET_keyword_occ_per_year.png_: Relative appearance of keywords related to the thesaurified Cultural Evolutionary Theory (CET) meta-category  treated in the whole corpus per year. 
1. _Supplementary_Fig_S3_TOPIC_keyword_occ_per_year.png_: Relative appearance of keywords related to thesaurified topics treated in the whole corpus per year. The y-axis is scaled dynamically for each subplot. A comparison of trends can only be done within categories but not between categories.
1. _Supplementary_Fig_S4_article_references_network_node_df_articles_per_timeframe_plot.png_: Bibliographic coupling network split into each of the five separate time bins. The clusters are coded in the same individual colours as Fig. 5 in the main text and were retrieved from the complete data set (N=629) without time binning. The graph was visualised using the Fruchterman-Reingold algorithm in the ggraph package (Pedersen 2021).
1. _Supplementary_Fig_S5_density_over_time_plot.png_: Bibliographic coupling network density (ratio of existing edges to all possible edges) for each of the separate time bins. Here, network density is used as a proxy for the conformity/diversity within the network.





