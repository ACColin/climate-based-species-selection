
# Double-check Eucalyptus names -------------------------------------------

librarian::shelf(tidyverse, taxize, DesiQuintans/taxizehelper)
library(tidyverse)
library(taxize)
install.packages("devtools")
devtools::install_github("DesiQuintans/taxizehelper")
library(taxizehelper)
# Import CCA species ------------------------------------------------------


cca_raw <- read.csv("output/CCA.dataframe.all.species.csv", header =TRUE, sep = ",")
DNTaxonomyCleaned <- read_xlsx("data/DNTaxonomyCleaned.xlsx", skip = 1, na = "-")
view(cca_raw)
view(DNTaxonomyCleaned)

class(cca_raw$species)
glimpse(cca_raw)
cca_raw %>% mutate_if(is.factor, as.character) -> cca_raw

# Check names in CCA.df with taxize -------------------------------------------------

# Some species (e.g. Eucalyptus setosa) are not actually Eucs because they've 
# since been renamed.

spp_names <- 
    cca_raw %>% 
    pull(species) %>% 
    search_gnr()

spp_names %>% 
    filter(str_detect(binomial, "Eucalyptus") == TRUE) %>% 
    View()

# Check names in DN.taxonomy with taxize -------------------------------------------------

# Some species (e.g. Eucalyptus setosa) are not actually Eucs because they've 
# since been renamed.

spp_names2 <- 
    DNTaxonomyCleaned %>% 
    pull(Binomial) %>% 
    search_gnr()

spp_names2 %>% 
    filter(str_detect(binomial, "Eucalyptus") == TRUE) %>% 
    View()

spp_names3 <-
    DNTaxonomyCleaned %>%
    get_tsn
    
# Conclusion: it is better to use the old taxonomy for now and attach the cleaned taxonomy to it in order to find species with both. If only a few species are concerned, do it manually otherwise can use EUCLID for a command line based option...
