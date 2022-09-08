library(bibliometrix)
library(dplyr)

WoS_bib_included <- readRDS(file = file.path(".", "2_data", "WoS_run_7", "wos_run_7_prepared.RDS"))

# Author keyword co-occurrences
DE <- cocMatrix(WoS_bib_included, Field = "DE", sep = ";")
DE_df <- as.data.frame(as.matrix(DE))
DE_df$AU <- WoS_bib_included$SR


# get keywords from abstracts (AB) and titles (TI) as 1-gram and 2-gram
# and transform them into author-keyword co-occurrences 
TI_TM_1gram <-
  cocMatrix(bibliometrix::termExtraction(WoS_bib_included,
                               Field = "TI",
                               stemming = F,
                               ngrams = 1,
                               verbose = F),
            Field = "TI_TM", sep = ";")

AB_TM_1gram <-
  cocMatrix(bibliometrix::termExtraction(WoS_bib_included,
                               Field = "AB",
                               stemming = F,
                               ngrams = 1,
                               verbose = F), 
            Field = "AB_TM", sep = ";")
TI_TM_2gram <-
  cocMatrix(bibliometrix::termExtraction(WoS_bib_included,
                               Field = "TI",
                               stemming = F,
                               ngrams = 2,
                               verbose = F),
            Field = "TI_TM", sep = ";")
AB_TM_2gram <-
  cocMatrix(bibliometrix::termExtraction(WoS_bib_included,
                               Field = "AB",
                               stemming = F,
                               ngrams = 2,
                               verbose = F), 
            Field = "AB_TM", sep = ";")



TI_TM_1gram_df <- as.data.frame(as.matrix(TI_TM_1gram))[,which(colSums(as.data.frame(as.matrix(TI_TM_1gram))) >= floor(nrow(WoS_bib_included)*0.01))] # only keep n-grams that appear in >= 1% of the articles
TI_TM_1gram_df$AU <- rownames(TI_TM_1gram_df)
TI_TM_2gram_df <- as.data.frame(as.matrix(TI_TM_2gram))[,which(colSums(as.data.frame(as.matrix(TI_TM_2gram))) >= floor(nrow(WoS_bib_included)*0.01))]
TI_TM_2gram_df$AU <- rownames(TI_TM_2gram_df)

AB_TM_1gram_df <- as.data.frame(as.matrix(AB_TM_1gram))[,which(colSums(as.data.frame(as.matrix(AB_TM_1gram))) >= floor(nrow(WoS_bib_included)*0.01))]
AB_TM_1gram_df$AU <- rownames(AB_TM_1gram_df)
AB_TM_2gram_df <- as.data.frame(as.matrix(AB_TM_2gram))[,which(colSums(as.data.frame(as.matrix(AB_TM_2gram))) >= floor(nrow(WoS_bib_included)*0.01))]
AB_TM_2gram_df$AU <- rownames(AB_TM_2gram_df)

joined_DE_TI_AB <- 
  plyr::rbind.fill(DE_df,
                   TI_TM_1gram_df) %>%
  plyr::rbind.fill(.,
                   TI_TM_2gram_df) %>%
  plyr::rbind.fill(.,
                   AB_TM_1gram_df) %>%
  plyr::rbind.fill(.,
                   AB_TM_2gram_df) %>%
  group_by(AU) %>%
  mutate_each(funs(sum(., na.rm = TRUE))) %>%
  unique() %>% 
  ungroup()

joined_DE_TI_AB <- as.data.frame(joined_DE_TI_AB)

joined_DE_TI_AB_mybibtexkey <- 
dplyr::left_join(WoS_bib_included[,c(which(colnames(WoS_bib_included) %in% c("SR", "mybibtex_key")))],
                 joined_DE_TI_AB,
                 by = c("SR" = "AU"))
rownames(joined_DE_TI_AB_mybibtexkey) <- joined_DE_TI_AB_mybibtexkey$mybibtex_key

joined_DE_TI_AB_mybibtexkey <- subset(joined_DE_TI_AB_mybibtexkey, select = -c(SR, V1, mybibtex_key))



########### load thesaurus function from other script
source(file.path("1_script", "thesaurus_keywords_ID_DE_030322.R")) #thesaurus_keywords_ID_D.R
thesauriert_joined <- thesaurus_function(joined_DE_TI_AB_mybibtexkey)
rownames(thesauriert_joined) <- thesauriert_joined$AU

saveRDS(thesauriert_joined, file = file.path("3_output", "wos_run_7_thesauriert_joined_DE_TI_AB.RDS"))

##############################
# binarize the occurences within the categories
thesauriert_joined_binary <- 
  thesauriert_joined %>% mutate_if(is.numeric, ~1 * (. != 0))
saveRDS(thesauriert_joined_binary, file = file.path("3_output", "wos_run_7_thesauriert_joined_binary_DE_TI_AB.RDS"))
##############################

# meta_meta_categories <- read.csv(file.path("2_data", "categories.csv"))



############################################## ############################################## 
############################################## ############################################## 
# create tibble including all references for each article in prepared format (i.e., for bibliographic coupling)
############################################## ############################################## 
############################################## ############################################## 

WoS_bib_included_dt <- data.table::as.data.table(WoS_bib_included)

long_list <- list()
for(i in 1:nrow(WoS_bib_included_dt)){
  # for each row/article
  current_row <- WoS_bib_included_dt[i,]
  current_row$AU_1st <- strsplit(current_row$AU, split = ";")[[1]][1]
  
  # single out the full references cited in this row/article
  cited_references_df <- 
    data.frame(mybibtex_key = current_row$mybibtex_key,
               AU_1st = current_row$AU_1st,
               AU_1st_1init = paste(strsplit(current_row$AU_1st, split = " ")[[1]][length(strsplit( current_row$AU_1st, split = " ")[[1]])-1],
                                    strsplit(strsplit( current_row$AU_1st, split = " ")[[1]][length(strsplit( current_row$AU_1st, split = " ")[[1]])], "")[[1]][1]),
               PY = current_row$PY,
               CR_full = strsplit(current_row$CR,split = "; ")[[1]])
  
  cr_details_per_article_list <- list()
  for(ii in 1:nrow(cited_references_df)){
    # for each row/article
    current_row_CR <- cited_references_df[ii,]
    
    # separate the information of the references cited (author name, publication year, journal/source)
    cr_details_per_article_list[[ii]] <- 
      data.frame(CR_full = current_row_CR$CR_full,
                 CR_au = strsplit(current_row_CR$CR_full,split = ", ")[[1]][1],
                 CR_au1 = paste(strsplit(strsplit(current_row_CR$CR_full,split = ", ")[[1]][1], " ")[[1]][1],
                                strsplit(strsplit(strsplit(current_row_CR$CR_full,split = ", ")[[1]][1], " ")[[1]][2], "")[[1]][1]),
                 CR_py = strsplit(current_row_CR$CR_full,split = ", ")[[1]][2],
                 CR_so = strsplit(current_row_CR$CR_full,split = ", ")[[1]][3],
                 CR_au_py = paste(strsplit(current_row_CR$CR_full,split = ", ")[[1]][1],
                                  strsplit(current_row_CR$CR_full,split = ", ")[[1]][2],
                                  sep = ", "),
                 CR_au1_py = paste(paste(strsplit(strsplit(current_row_CR$CR_full,split = ", ")[[1]][1], " ")[[1]][1],
                                         strsplit(strsplit(strsplit(current_row_CR$CR_full,split = ", ")[[1]][1], " ")[[1]][2], "")[[1]][1]),
                                   strsplit(current_row_CR$CR_full,split = ", ")[[1]][2],
                                   sep = ", "))
  }
  long_list[[i]] <- 
    dplyr::left_join(cited_references_df,
                     do.call(rbind.data.frame, cr_details_per_article_list),
                     by = "CR_full")
}

long_list_df <- na.omit(do.call(rbind.data.frame, long_list))


cr_details_per_article <- na.omit(subset(long_list_df, CR_au1 %in% names(which(table(long_list_df$CR_au1) > 5)))) # only keep AUTHORS that get cited 6 or more times in total
cr_details_per_article <- subset(cr_details_per_article, !(CR_au1 %in% c("[ANONYMOUS] NA", "ANONYMOUS NA", "[ANONYMOUS] A")))


cr_details_per_article_tbl <- tibble::as_tibble(cr_details_per_article)


saveRDS(cr_details_per_article_tbl,
        file = file.path("3_output", "wos_run_7_cr_details_per_article_tbl.RDS"))



















