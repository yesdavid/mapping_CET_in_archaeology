## Research compendium for 'Mapping the field of cultural evolutionary theory and methods in archaeology using bibliometric methods' 

### Compendium DOI:

[![DOI](https://zenodo.org/badge/DOI/.svg)](https://doi.org/)

The files at the URL above will generate the results as found in the publication. The files hosted at <https://github.com/yesdavid/mapping_CET_in_archaeology> are the development versions and may have changed since the paper was published.

### Maintainer of this repository:

[![ORCiD](https://img.shields.io/badge/ORCiD-0000--0001--7349--5401-green.svg)](http://orcid.org/0000-0001-7349-5401) David N. Matzig (<david.matzig@cas.au.dk>) 

### Published in:

David N. Matzig, Clemens Schmid, Felix Riede (__accepted__) Mapping the field of cultural evolutionary theory and methods in archaeology using bibliometric methods, _Humanities & Social Sciences Communications_, [![DOI](https://zenodo.org/badge/DOI/.svg)](https://doi.org/) 

### Abstract:

Bibliometrics offers powerful means of visualising and understanding trends within research domains. We here present a first exploratory bibliometric analysis of cultural evolutionary theory and attendant methods as applied specifically within archaeology across the last four decades (1981-2021). Bibliographic coupling network analysis shows that there exists a broadly successive series of author clusters making up the core of this research domain. A broader vernacular version of cultural evolution is also commonly used in thematic or regional research traditions that fall outside of cultural evolutionary studies in the strict sense. Our bibliometric networks trace the development of evolutionary archaeology over the last four decades and while they demonstrate the centrality of computational models, they also suggest a stagnation in the application of precisely that suite of methods – phylogenetics – that is central to evolutionary archaeology’s biological counterpart palaeontology. Recent methodological innovations in palaeobiology are, however, offering new ways of integrating artefact shape data directly with phylogenetic applications. This development may usher in a renaissance in artefact phylogenetics and appropriately marco-scale applications of cultural evolutionary theory in archaeology.

### Keywords: 

Cultural evolution; bibliometrics; literature review; archaeology; phylogenetics; morphometrics

### Overview of contents and how to reproduce:

This repository contains code (`1_script`) data (`2_data`) and  for the paper. After downloading, the results can be reproduced using `systematic_literature_review.Rproj` and the existing folder structure. All analyses and visualisations presented in this paper were prepared in R version 4.2.1 (2022-06-23) under Ubuntu 18.04.5 LTS (64-bit).

### Required dependencies:

As the data and code in this repository are complete and self-contained, it can be reproduced with only an R environment (tested for R v4.2.1). The necessary package dependencies are documented in the DESCRIPTION file and can be installed manually or automatically with

```
if(!require("remotes")) install.packages("remotes")
remotes::install_github("yesdavid/mapping_CET_in_archaeology", repos = "https://mran.microsoft.com/snapshot/2022-01-31")
```

This will install the relevant package dependency versions from January 2022, thanks to Microsoft's [CRAN Time Machine](https://mran.microsoft.com/timemachine).

### Licenses:

Code: MIT <http://opensource.org/licenses/MIT> year: 2023, copyright holder: David Nicolas Matzig

Data: The data has been compiled from Clarivate's Web of Science Core Collection (WoS; www.webofscience.com)
