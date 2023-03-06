# Data

This folder contains both the raw data as well as the derived data for the analyses.

## WoS_run_7

- _wos_run_7.bib_: the raw data as downloaded from Clarivate's Web of Science (WoS; www.webofscience.com).  
- _wos_run_7_query.txt_: the WoS search query to arrive at the raw data used in this article. Note that the _.bib_ file contains 677 publications while the article only uses 674. This is because three publications within this data set come from outside of our specified time frame.
- _wos_run_7_prepared.*_: the raw data after preparing it using the _1_script/1_load_data_WoS.R_-script to clean author names etc.

## categories.csv

This table (table 1 in the article) contains the thesaurus categories (first-order thesaurified keywords) with their overarching second-order meta-categories.

## thesaurus_ID_DE_030322.csv 

The thesaurus used for the analysis. For all articles contained in _./WoS_run_7/wos_run_7.bib_, the author-assigned keywords, the titles, and the abstracts were extracted. For the titles and abstracts, uni- and bi-grams were created and those that appeared in >= 1% of the articles were kept. The author-assigned keywords, uni- and bi-grams  were used to create a thesaurus which combines them into first-order categories, which again, were assigned to broader second-order categories (s. _categories.csv_ and table 1 in the article). 

