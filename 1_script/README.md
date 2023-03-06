# Scripts

To __reproduce the derived data__, scripts _1_load_data_WoS.R_ and _2_get_keywords_use_thesaurus.R_ have to be run in the order of their numberings. Script _thesaurus_keywords_ID_DE_030322.R_ is called through a command in script 2 and does not need to be opened manually. It contains the rules for the thesaurus.

To __reproduce the analyses__, script _3_basic_summary_data.R_ for the data-based results, and script _4_bibliographic_coupling.R_ for the network based results have to be run. If the whole research compendium is downloaded as a whole, it is not necessary to run scripts 1 and 2 in order to run scripts 3 and 4, as the data needed is available in folder _2\_data_.
