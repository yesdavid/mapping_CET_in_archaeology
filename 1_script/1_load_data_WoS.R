library(bibliometrix)
library(dplyr)
library(tidytext)
library(stringr)
library(magrittr)


data("stop_words")
# load the complete WoS search result
WoS_file <- file.path(".", "2_data", "WoS_run_7", "wos_run_7.bib")
WoS_bib <- bibliometrix::convert2df(file = WoS_file, dbsource = "wos", format = "bibtex")

WoS_bib2df <- bib2df::bib2df(file = WoS_file) 

WoS_bib2df_subset <- 
  WoS_bib2df %>% 
  select("BIBTEXKEY", "TITLE", "AUTHOR", "YEAR")


# I create the betterbibtex key for the WoS_bib-file manually.
# a betterbibtex key looks like this: lastenameFirstAuthor_firstWordOfTheTitleThatIsNotAStopWord_year (actually not all stop words in general but "the", "on", "a", "an", "from")
## in some cases lastnames can be separated by a "-" or a "_", if it is a double name. "-" can also occur in the article title.

WoS_bib2df_subset

betterbibtex_to_WOS_list <- list()
for(i in 1:nrow(WoS_bib2df_subset)){
  # get first author's name
  lastname_firstauthor <- strsplit(tolower(WoS_bib2df_subset$AUTHOR[[i]])[1], split = ",")[[1]][1]
  lastname_firstauthor <- paste0(strsplit(lastname_firstauthor, " ")[[1]], collapse = "_") # in case it's a double name with empty space in between
  lastname_firstauthor <- paste0(strsplit(lastname_firstauthor, split = "[^A-Za-z0-9\\_\\-]")[[1]], collapse = "") # in case there are some special characters "attached" to the beginning/ending of the word (like ",", or "`"`)
  
  title_words <- strsplit(tolower(WoS_bib2df_subset$TITLE[[i]]), " ")[[1]]
  first_word <- title_words[1]
  if(first_word == "the" | first_word ==  "on" | first_word ==  "a" | first_word ==  "an" | first_word ==  "from"){ # in case the title starts with one of these words
    first_word <- title_words[2]
    
    if(first_word == "the" | first_word ==  "on" | first_word ==  "a" | first_word ==  "an" | first_word ==  "from"){ # in case the same applies to the second word of the title, too
      first_word <- title_words[3]
    }
  }
  no_special_chars <- paste0(strsplit(first_word, split = "[^A-Za-z0-9\\-]")[[1]], collapse = "") # in case there are some special characters "attached" to the beginning/ending of the word (like ",", or "`", however, not "-")
  first_word <- no_special_chars[length(no_special_chars)]
  # first_word
  
  betterbibtex_to_WOS_list[[i]] <- data.frame(mybibtex_key = paste0(lastname_firstauthor, "_", first_word, "_", WoS_bib2df_subset$YEAR[[i]]),
                                              UT = paste0("WOS", strsplit(WoS_bib2df_subset$BIBTEXKEY[[i]], split = ":")[[1]][2])
  )
}

betterbibtex_to_WOS <- do.call(rbind.data.frame, betterbibtex_to_WOS_list)


# add betterbibtex key
WoS_bib_included <- dplyr::left_join(WoS_bib, betterbibtex_to_WOS, by = "UT")



######## ######## ######## ######## ######## ######## ######## 
######## consitency check 1st authors of papers
AUthors <- WoS_bib_included$AU
authors_df_list <- list()
for (i in 1:length(AUthors)) {
  
  current_AU <- AUthors[i] # authors of current paper
  author_vector <- strsplit(current_AU, split = ";")[[1]] # authors are separated by ";"
  
  paper_authors <- list()
  for(ii in 1:length(author_vector)){ # single out authors
    current_author <- author_vector[ii]
    current_author_strsplit <- strsplit(current_author, split = " ")[[1]]
    
    paper_authors[[ii]] <- data.frame(paper_number = i,
                                      author_number = ii,
                                      author_fullname = current_author,
                                      author_lastname = current_author_strsplit[1],
                                      author_firstname = current_author_strsplit[2])
    
  }
  authors_df_list[[i]] <- do.call(rbind.data.frame, paper_authors)
  
}
authors_df <- do.call(rbind.data.frame, authors_df_list)

author_lastnames_unique <- unique(authors_df[,"author_lastname"])


author_lastnames_different_firstnames_list <- list()
for (current_lastname in author_lastnames_unique) {
  current_lastname_subset <- subset(authors_df, author_lastname == current_lastname)
  
  unique_firstname_versions <- unique(current_lastname_subset$author_firstname)
  length_firstname_versions <- length(unique_firstname_versions)
  
  if (length_firstname_versions > 1) {
    author_lastnames_different_firstnames_list[[current_lastname]] <- current_lastname_subset # to control list
    
  }
  
}

author_lastnames_different_firstnames_list # control list -> inconsistent initials?



######## ######## ######## ######## ######## ######## ######## 
######## consitency check 1st authors of **CITED** papers
CR_AUthors <- WoS_bib_included$CR
cr_author_df_list <- list()
for (i in 1:length(CR_AUthors)) {
  
  current_CR_AU <- CR_AUthors[i]
  author_vector <- strsplit(current_CR_AU, split = "; *")[[1]]
  
  paper_cr_author <- list()
  for(ii in 1:length(author_vector)){
    current_author <- author_vector[ii]
    current_author_strsplit <- strsplit(current_author, split = ", *")[[1]]
    current_author_strsplit_name <- strsplit(current_author_strsplit[1], " ")[[1]]
    
    author_lastname <- current_author_strsplit_name[1]
    
    # reduce full firstname to initials
    for(firstname_index in 2:length(current_author_strsplit_name)){
      current_firstname <- current_author_strsplit_name[firstname_index]
      current_firstname_strsplit <- strsplit(current_author_strsplit_name[firstname_index], "")[[1]]
      if(length(current_firstname_strsplit) > 2 & current_firstname_strsplit[2] != "." & current_firstname_strsplit[3] != "."){
        current_author_strsplit_name[firstname_index] <- current_firstname_strsplit[1]
      }
    }
    
    author_firstname_new <- paste0(current_author_strsplit_name[2:length(current_author_strsplit_name)], collapse = "")
    author_firstname_new <- gsub(x = author_firstname_new,
                             pattern = "[.]",
                             replacement = "")
    
    paper_cr_author[[ii]] <- data.frame(paper_number = i,
                                      author_number = ii,
                                      author_fullname_year_original = paste0(current_author_strsplit[1], ", ", current_author_strsplit[2]),
                                      author_fullname_year_new = paste0(author_lastname,
                                                                        " ",
                                                                        author_firstname_new,
                                                                        ", ",
                                                                        current_author_strsplit[2]),
                                      author_fullname = current_author_strsplit[1],
                                      year = current_author_strsplit[2],
                                      author_lastname = author_lastname,
                                      author_firstname_old = current_author_strsplit_name[2],
                                      author_firstname_new = author_firstname_new)
    
  }
  cr_author_df_list[[i]] <- do.call(rbind.data.frame, paper_cr_author)
  
}
cr_author_df <- do.call(rbind.data.frame, cr_author_df_list)

author_lastnames_unique <- unique(cr_author_df[,"author_lastname"])


# entries with same lastname but different original initials
author_lastnames_different_firstnames_list <- list()
for (current_lastname in author_lastnames_unique) {
  current_lastname_subset <- subset(cr_author_df, author_lastname == current_lastname)
  
  unique_firstname_versions <- unique(current_lastname_subset$author_firstname_old)
  length_firstname_versions <- length(unique_firstname_versions)
  
  if (length_firstname_versions > 1) {
    author_lastnames_different_firstnames_list[[current_lastname]] <- current_lastname_subset # to control list
    
  }
  
}

author_lastnames_different_firstnames_list # control list -> inconsistent initials?

length(author_lastnames_different_firstnames_list)


# entries with same lastname but different **NEW** initials
author_lastnames_different_NEW_firstnames_list <- list()
for (current_lastname in author_lastnames_unique) {
  current_lastname_subset <- subset(cr_author_df, author_lastname == current_lastname)
  
  unique_firstname_versions <- unique(current_lastname_subset$author_firstname_new)
  length_firstname_versions <- length(unique_firstname_versions)
  
  if (length_firstname_versions > 1) {
    author_lastnames_different_NEW_firstnames_list[[current_lastname]] <- current_lastname_subset # to control list
    
  }
  
}

author_lastnames_different_NEW_firstnames_list # control list -> inconsistent initials?
length(author_lastnames_different_NEW_firstnames_list)

lastnames_with_still_inconsistent_new_firstname_initials <- names(author_lastnames_different_NEW_firstnames_list)


# make initials consistent, ie
# cavallisforza LL 
# and 
# cavallisforza L
# should be the same
# if the first initials match, replace the shorter one with the longer one/the less common one with the more common one     
for(current_lastname_index in lastnames_with_still_inconsistent_new_firstname_initials){
  current_subset <- author_lastnames_different_firstnames_list[[current_lastname_index]]
  
  a <- summary(current_subset[,c("author_firstname_new")])
  b <- a[a>0]
  # b <- c( "A" =  1, "AB" = 3, "CA" = 5, "CF" = 2)
  
  c <- sapply(names(b), function(x){
    strsplit(x, split = "")[[1]][1]
  })
  c2 <- sapply(names(b), function(x){
    strsplit(x, split = "")[[1]][2]
  })
  
  initials_df_list <- list()
  for (i in 1:length(c)) {
    
    i_vector <- 1:length(c)
    ii_vector <- i_vector[-i]
    
    initials_df_list_ii <- list()
    for (ii in ii_vector) {
      
      if(i<length(c)){
        from_index <- i
        to_index <- ii
        result <- c[[from_index]] == c[[to_index]]
        result2 <- c2[[from_index]] == c2[[to_index]]
      } else {
        from_index <- i
        to_index <- ii
        result <- c[[from_index]] == c[[to_index]]
        result2 <- c2[[from_index]] == c2[[to_index]]
      }
      
      initials_df_list_ii[[ii]] <- data.frame(
        from_index = i,
        to_index = ii,
        from_initial = names(c)[from_index],
        to_initial = names(c)[to_index],
        first_initial_same = result,
        second_initial_same_or_NA = result2)
    }
    initials_df_list[[i]] <- do.call(rbind.data.frame, initials_df_list_ii)
    
    
    }
  initials_df <- do.call(rbind.data.frame, initials_df_list)
  initials_resolved_df <- initials_df %>% 
    subset(first_initial_same == TRUE) %>% 
    subset(!(second_initial_same_or_NA %in% FALSE)) %>% 
    slice(n = -(1:(nrow(.)/2))) # just retain one half to avoid duplicates
  
  # nchar(c(as.character(initials_resolved_df$from_initial), as.character(initials_resolved_df$to_initial)))
  
  if(nrow(initials_resolved_df) >0){
    for (row_index in 1:nrow(initials_resolved_df)) {
      current_initials_resolved_df <- initials_resolved_df[row_index,]
      
      column_names <- c("from_initial", "to_initial")
      longer_intials_index <- which.max(nchar(c(as.character(current_initials_resolved_df$from_initial), as.character(current_initials_resolved_df$to_initial))))
      longer_initials <- column_names[longer_intials_index]
      shorter_intials <- column_names[-longer_intials_index]
      
      current_subset <- mutate_if(current_subset, is.factor, as.character)
      current_initials_resolved_df <- mutate_if(current_initials_resolved_df, is.factor, as.character)
      
      current_subset$author_firstname_new[which(current_subset$author_firstname_new == as.character(current_initials_resolved_df[[shorter_intials]]))] <- 
        as.character(current_initials_resolved_df[[longer_initials]])
    }
    current_subset$author_fullname_year_new <- paste0(current_subset$author_lastname, " ", current_subset$author_firstname_new, ", ", current_subset$year)
    
    author_lastnames_different_firstnames_list[[current_lastname_index]] <- current_subset
  }
}


author_lastnames_different_firstnames_df <- do.call(rbind.data.frame, author_lastnames_different_firstnames_list)
author_lastnames_different_firstnames_df_essential <- author_lastnames_different_firstnames_df[,c("paper_number",
                                                                                                  "author_number",
                                                                                                  "author_fullname_year_original",
                                                                                                  "author_fullname_year_new")]

# 

paper_number_unique <- unique(author_lastnames_different_firstnames_df_essential$paper_number)

for (i in paper_number_unique) {
  
  current_df <- subset(author_lastnames_different_firstnames_df_essential, 
                       paper_number == i)
  
  author_number_unique <- unique(current_df$author_number)
  
  current_CRs <- strsplit(WoS_bib_included[i,"CR"], "; *")[[1]]
  
  for(ii in author_number_unique){
    current_CRs[ii] <- gsub(pattern = as.character(subset(current_df, author_number == ii)$author_fullname_year_original),
                            replacement = as.character(subset(current_df, author_number == ii)$author_fullname_year_new),
                            x = current_CRs[ii])
    
  }
  
  WoS_bib_included[i,"CR"] <- paste0(current_CRs, collapse = "; ")
  
}

WoS_bib_included$AU_1st <- sapply(WoS_bib_included$AU, function(x){strsplit(x, split = ";")[[1]][1]})
  
# there is two papers with the bibtex key "mesoudi_cultural_2008" -> change the second one manually
WoS_bib_included$mybibtex_key[which(WoS_bib_included$mybibtex_key == "mesoudi_cultural_2008")[2]] <- "mesoudi_cultural2_2008"

nrow(WoS_bib_included)


saveRDS(WoS_bib_included,
        file = file.path(".", "2_data", "WoS_run_7", "wos_run_7_prepared.RDS"))

write.csv(WoS_bib_included,
          file = file.path(".", "2_data", "WoS_run_7", "wos_run_7_prepared.csv"))




