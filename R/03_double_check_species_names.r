
# Double-check Eucalyptus names -------------------------------------------

librarian::shelf(tidyverse, taxize, DesiQuintans/taxizehelper)
library(tidyverse)
library(taxize)
install.packages("devtools")
devtools::install_github("DesiQuintans/taxizehelper")
library(taxizehelper)
# Import CCA species ------------------------------------------------------


cca_raw <- read.csv("output/CCA.dataframe.all.species.csv", header =T, sep = ",")
view(cca_raw)
?read.csv

# Check names with taxize -------------------------------------------------

# Some species (e.g. Eucalyptus setosa) are not actually Eucs because they've 
# since been renamed.

spp_names <- 
    cca_raw %>% 
    pull(species) %>% 
    search_gnr()

spp_names %>% 
    filter(str_detect(binomial, "Eucalyptus") == FALSE) %>% 
    View()

