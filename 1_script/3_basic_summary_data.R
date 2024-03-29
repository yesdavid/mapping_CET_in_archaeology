# basic descriptive measures of data set
library(bibliometrix)
library(ggplot2)
library(magrittr)
library(dplyr)

WoS_bib_included <- readRDS(file = file.path(".", "2_data", "WoS_run_7", "wos_run_7_prepared.RDS"))

WoS_bib_included_dt <- data.table::as.data.table(subset(WoS_bib_included, PY < 2022)) # since at the time of the analysis, the year 2022 had only just begun.
nrow(WoS_bib_included_dt)

thesauriert_joined <- readRDS(file.path("3_output", "wos_run_7_thesauriert_joined_DE_TI_AB.RDS"))
thesauriert_joined_binary <- readRDS(file.path("3_output", "wos_run_7_thesauriert_joined_binary_DE_TI_AB.RDS"))

thesauriert_joined_binary_incidence <- thesauriert_joined_binary[,-which(colnames(thesauriert_joined_binary) == "AU")]
thesauriert_joined_binary_incidence <- as.matrix(thesauriert_joined_binary_incidence, labels = T)

meta_meta_categories <- read.csv(file.path("2_data", "categories.csv"))


# bibliometrix
bib_analysis <- bibliometrix::biblioAnalysis(subset(WoS_bib_included, PY < 2022))
summary(bib_analysis)


# number of publications per year
number_of_pubs_per_year <- table(subset(WoS_bib_included, PY < 2022)$PY)
number_of_pubs_per_year_df <- data.frame(Year = as.numeric(names(number_of_pubs_per_year)),
                                         Count = as.numeric(number_of_pubs_per_year))

number_of_pubs_per_year_plot <- 
ggplot(data = number_of_pubs_per_year_df,
       aes(x = Year,
           y = Count)) +
  geom_point() +
  geom_smooth(method = "loess",
              fill = "grey50",
              color = "grey50",
              span = 0.75,
              se = T) + 
  ggpubr::yscale("log2") +
  theme_bw() +
  ylab("N (log2)")

number_of_pubs_per_year_plot


publications_per_year_bar_plot <- 
  ggplot(data = number_of_pubs_per_year_df,
         aes(x = Year,
             y = Count)) + 
  geom_col() +
  theme_bw() +
  ylab("Number of articles") +
  xlab("Year of publication")

publications_per_year_bar_plot

ggsave(publications_per_year_bar_plot,
       filename = file.path("3_output", "Fig_2_number_of_pubs_per_year_plot.png"),
       device = "png",
       width = 8,
       height = 6,
       units = "in",
       bg = "white")


# how many items do we have?
hm_items <- nrow(WoS_bib_included_dt) 
hm_items # 674

# span what time period?
time_range <- range(as.integer(WoS_bib_included_dt$PY)) # 1981 2021
time_range



# themes per year
binary_incidence_of_thesaurified_keyw <- as.data.frame(thesauriert_joined_binary_incidence)
binary_incidence_of_thesaurified_keyw$mybibtex_key <- rownames(binary_incidence_of_thesaurified_keyw)

no_Period <- 
  binary_incidence_of_thesaurified_keyw$mybibtex_key[which(rowSums(binary_incidence_of_thesaurified_keyw[,c("Palaeolithic", "Neolithic", "Palaeoindian", "Bronze_Age", "African_Stone_Age", "Iron_age", "Mesolithic")]) == 0)]

no_Period_df <- 
  data.frame(mybibtex_key = no_Period,
             Period = "none",
             value = 1)

Period <- 
  binary_incidence_of_thesaurified_keyw[,c("Palaeolithic", "Neolithic", "Palaeoindian", "Bronze_Age", "African_Stone_Age", "Iron_age", "Mesolithic", 
           "mybibtex_key")] %>% 
  tidyr::pivot_longer(-mybibtex_key, names_to = "Period") %>% 
  na_if(0) %>% 
  na.omit

Period <- dplyr::full_join(Period,
                           no_Period_df)

Period$PY <- 
as.integer(
  sapply(Period$mybibtex_key, function(x){
    strsplit(x, split = "_")[[1]][length(strsplit(x, split = "_")[[1]])]
  })
)

Period$Period <- gsub("Bronze_Age", "Bronze Age", Period$Period)
Period$Period <- gsub("African_Stone_Age", "African Stone Age", Period$Period)
Period$Period <- gsub("Iron_age", "Iron Age", Period$Period)
Period$Period <- gsub("none", "Not specified", Period$Period)


period_keyword_occ_per_year_data <-
  Period %>% 
  subset(PY < 2022) %>% 
  group_by(PY) %>%
  count(Period) %>% 
  ungroup() %>% 
  dplyr::left_join(., number_of_pubs_per_year_df,
                   by = c("PY"= "Year")) %>% 
  mutate(perc = n/Count) %>% 
  mutate(label = if_else(PY == max(PY), as.character(Period), NA_character_))

period_keyword_occ_per_year_data$Period <- factor(period_keyword_occ_per_year_data$Period,
                                                  levels = levels(forcats::fct_reorder(as.data.frame(table(Period$Period))$Var1,
                                                                                       as.data.frame(table(Period$Period))$Freq,
                                                                                       .desc = T)))



period_keyword_occ_per_year_plot <- 
ggplot(data = period_keyword_occ_per_year_data,
       aes(x = PY,
           y = n,
           group = Period,
           color = Period
)) +
  geom_line() +
  geom_point() +
  theme_bw() +
  xlab("Year of publication") +
  ylab("Number of articles containing keyword") +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 10)) +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_color_manual(values = c("Not specified" = "grey60",
                                "Palaeolithic" = "#191970",
                                "Neolithic" = "#ff0000",          
                                "Bronze Age" = "#ffd700",  
                                "Mesolithic" = "#00ffff",  
                                "African Stone Age" = "#ff00ff",      
                                "Iron Age" = "#4169e1",
                                "Palaeoindian" = "#00ff00")) 

period_keyword_occ_per_year_plot

ggsave(period_keyword_occ_per_year_plot,
       filename = file.path("3_output", "Fig_3_period_keyword_occ_per_year.png"),
       device = "png",
       width = 6,
       height = 6,
       units = "in",
       bg = "white")


####

METHODS_data <- 
  binary_incidence_of_thesaurified_keyw[,c(which(colnames(binary_incidence_of_thesaurified_keyw) %in% subset(meta_meta_categories, meta_category == "METHODS")$category), which(colnames(binary_incidence_of_thesaurified_keyw) == "mybibtex_key"))] %>% 
    tidyr::pivot_longer(-mybibtex_key, names_to = "METHODS") %>% 
    na_if(0) %>% 
    na.omit %>% 
    dplyr::left_join(., WoS_bib_included[,which(colnames(WoS_bib_included) %in% c("mybibtex_key", "PY"))]) %>% 
    subset(PY > 1999) %>% 
    subset(PY < 2022) %>% 
    group_by(PY) %>%
    count(METHODS) %>% 
    ungroup() %>% 
    dplyr::left_join(., number_of_pubs_per_year_df,
                     by = c("PY"= "Year")) %>% 
    mutate(perc = n/Count) %>% 
    mutate(METHODS = gsub("Absolute_Dating", "Absolute dating", METHODS)) %>% 
  mutate(METHODS = gsub("Phylogenetic_Cladistic_Methods", "Phylogenetic/cladistic methods", METHODS)) %>% 
  mutate(METHODS = gsub("Quantitative_Methods", "Quantitative methods", METHODS)) %>% 
  mutate(METHODS = gsub("Scientific_methods", "Scientific methods", METHODS)) %>% 
  mutate(METHODS = gsub("Modelling_Simulating", "Modelling/simulating", METHODS)) %>% 
  mutate(METHODS = gsub("Typology_Taxonomy", "Typology/taxonomy", METHODS)) %>% 
  mutate(METHODS = gsub("Theoretical_approaches", "Theoretical approaches", METHODS))
  
METHODS_relevance <- 
  METHODS_data %>% 
  group_by(METHODS) %>%
  summarise(summed_perc = sum(perc)) %>% 
  ungroup() %>% 
  arrange(desc(summed_perc))  

METHODS_data_relevance <- 
  dplyr::left_join(METHODS_data,
                   METHODS_relevance)

METHODS_plot <- 
  METHODS_data_relevance %>% 
    ggplot(aes(x = PY, y = perc*100)) +
    geom_smooth(span = 5) +
    geom_point() +
    facet_wrap(~forcats::fct_reorder(METHODS, desc(summed_perc), max), ncol = 5,
               scales = "free_y") +
    theme_bw() +
    theme(legend.position = "none") +
  xlab("Year of publication") +
  ylab("Relative occurrence of each keyword per year (%)")

METHODS_plot

ggsave(METHODS_plot,
       filename = file.path("3_output", "Fig_4_METHODS_keyword_occ_per_year.png"),
       device = "png",
       width = 12,
       height = 6,
       units = "in",
       bg = "white")


# TOPIC_plot <- 
TOPIC_data <- 
  binary_incidence_of_thesaurified_keyw[,c(which(colnames(binary_incidence_of_thesaurified_keyw) %in% subset(meta_meta_categories, meta_category == "TOPIC")$category), which(colnames(binary_incidence_of_thesaurified_keyw) == "mybibtex_key"))] %>% 
  tidyr::pivot_longer(-mybibtex_key, names_to = "TOPIC") %>% 
  na_if(0) %>% 
  na.omit %>% 
  dplyr::left_join(., WoS_bib_included[,which(colnames(WoS_bib_included) %in% c("mybibtex_key", "PY"))]) %>% 
  subset(PY > 1999) %>% 
  subset(PY < 2022) %>% 
  group_by(PY) %>%
  count(TOPIC) %>% 
  ungroup() %>% 
  dplyr::left_join(., number_of_pubs_per_year_df,
                   by = c("PY"= "Year")) %>% 
  mutate(perc = n/Count) %>% 
  mutate(TOPIC = gsub("Human_Evolution", "Human evolution", TOPIC)) %>% 
  mutate(TOPIC = gsub("Evolutionary_Terms", "Evolutionary terms", TOPIC)) %>% 
  mutate(TOPIC = gsub("Tool_Use", "Tool use", TOPIC)) %>% 
  mutate(TOPIC = gsub("Symbolic_Communication", "Symbolic communication", TOPIC)) %>% 
  mutate(TOPIC = gsub("HunterGatherer_Forager", "Hunter-gatherers/foragers", TOPIC)) %>% 
  mutate(TOPIC = gsub("Climate_and_Catastrophes", "Climate and catastrophes", TOPIC)) %>% 
  mutate(TOPIC = gsub("Macro_scale", "Macro scale", TOPIC)) %>% 
  mutate(TOPIC = gsub("Cross_Cultural", "Cross cultural", TOPIC)) %>% 
  mutate(TOPIC = gsub("Ecology_Environment", "Ecology/environment", TOPIC)) %>% 
  mutate(TOPIC = gsub("Theory_of_science", "Theory of science", TOPIC)) %>% 
  mutate(TOPIC = gsub("Critical_Controversial", "Critical/controversial", TOPIC)) %>% 
  mutate(TOPIC = gsub("Inter_Multidisciplinary", "Inter-/multidisciplinary", TOPIC)) %>% 
  mutate(TOPIC = gsub("Ethics_Morality", "Ethics and morality", TOPIC)) %>% 
  mutate(TOPIC = gsub("Socio_political", "Socio-political", TOPIC)) %>% 
  mutate(TOPIC = gsub("In_equality_Prestige", "(In-)equality/prestige", TOPIC)) %>% 
  mutate(TOPIC = gsub("Religion_Ritual_Tradition", "Religion, ritual and tradition", TOPIC)) %>% 
  mutate(TOPIC = gsub("Sites_Settlement", "Sites/settlements", TOPIC)) %>% 
  mutate(TOPIC = gsub("Health_Medicine", "Health/medicine", TOPIC))

TOPIC_relevance <- 
  TOPIC_data %>% 
  group_by(TOPIC) %>%
  summarise(summed_perc = sum(perc)) %>% 
  ungroup() %>% 
  arrange(desc(summed_perc))  

TOPIC_data_relevance <- 
  dplyr::left_join(TOPIC_data,
                   TOPIC_relevance)

TOPIC_plot <- 
TOPIC_data_relevance %>% 
mutate(TOPIC_factor = forcats::fct_reorder(TOPIC, summed_perc)) %>% 
  
  ggplot(aes(x = PY, y = perc*100)) +
  geom_smooth(span = 5) +
  geom_point() +
  facet_wrap(~forcats::fct_reorder(TOPIC, desc(summed_perc), max), ncol = 4,
             scales = "free_y") +
  theme_bw() +
  theme(legend.position = "none") +
  xlab("Year of publication") +
  ylab("Relative occurrence of each keyword per year (%)")

TOPIC_plot

ggsave(TOPIC_plot,
       filename = file.path("3_output", "Supplementary_Fig_S3_TOPIC_keyword_occ_per_year.png"),
       device = "png",
       width = 10,
       height = 12,
       units = "in",
       bg = "white")


####

CET_data <- 
  binary_incidence_of_thesaurified_keyw[,c(which(colnames(binary_incidence_of_thesaurified_keyw) %in% subset(meta_meta_categories, meta_category == "CET")$category), which(colnames(binary_incidence_of_thesaurified_keyw) == "mybibtex_key"))] %>% 
  tidyr::pivot_longer(-mybibtex_key, names_to = "CET") %>% 
  na_if(0) %>% 
  na.omit %>% 
  dplyr::left_join(., WoS_bib_included[,which(colnames(WoS_bib_included) %in% c("mybibtex_key", "PY"))]) %>% 
  subset(PY > 1999) %>% 
  subset(PY < 2022) %>% 
  group_by(PY) %>%
  count(CET) %>% 
  ungroup() %>% 
  dplyr::left_join(., number_of_pubs_per_year_df,
                   by = c("PY"= "Year")) %>% 
  mutate(perc = n/Count) %>% 
  mutate(CET = gsub("Cultural_Evolution", "Cultural evolution", CET)) %>% 
  mutate(CET = gsub("Niche_Construction", "Niche construction", CET)) %>% 
  mutate(CET = gsub("Cultural_Transmission", "Cultural transmission", CET)) %>% 
  mutate(CET = gsub("Social_learning", "Social learning", CET)) %>% 
  mutate(CET = gsub("Gene_Culture_CoEvol", "Gene-culture coevolution", CET)) %>% 
  mutate(CET = gsub("Evolutionary_Psychology", "Evolutionary psychology", CET)) %>% 
  mutate(CET = gsub("Behav_Ecology", "Behavioral ecology", CET))

CET_relevance <- 
  CET_data %>% 
  group_by(CET) %>%
  summarise(summed_perc = sum(perc)) %>% 
  ungroup() %>% 
  arrange(desc(summed_perc))  

CET_data_relevance <- 
  dplyr::left_join(CET_data,
                   CET_relevance)

CET_plot <- 
  CET_data_relevance %>% 
  ggplot(aes(x = PY, y = perc*100)) +
  geom_smooth(span = 5) +
  geom_point() +
  facet_wrap(~forcats::fct_reorder(CET, desc(summed_perc), max), ncol = 5) +
  theme_bw() +
  theme(legend.position = "none") +
  xlab("Year of publication") +
  ylab("Relative occurrence of each keyword per year (%)")


CET_plot

ggsave(CET_plot,
       filename = file.path("3_output", "Supplementary_Fig_S2_CET_keyword_occ_per_year.png"),
       device = "png",
       width = 12,
       height = 6,
       units = "in",
       bg = "white")



