
# Double-check Eucalyptus names -------------------------------------------

librarian::shelf(tidyverse, taxize, DesiQuintans/taxizehelper)
library(tidyverse)
library(taxize)
install.packages("devtools")
devtools::install_github("DesiQuintans/taxizehelper")
library(taxizehelper)
# Import CCA species ------------------------------------------------------


cca_raw <- read.csv("output/CCA.dataframe.all.species.csv", header =TRUE, sep = ",")
view(cca_raw)
?read.csv
class(cca_raw$species)
glimpse(cca_raw)
cca_raw %>% mutate_if(is.factor, as.character) -> cca_raw

# Check names with taxize -------------------------------------------------

# Some species (e.g. Eucalyptus setosa) are not actually Eucs because they've 
# since been renamed.

spp_names <- 
    cca_raw %>% 
    pull(species) %>% 
    search_gnr()

spp_names %>% 
    filter(str_detect(binomial, "Eucalyptus") == TRUE) %>% 
    View()

old.and.cleaned.taxo
