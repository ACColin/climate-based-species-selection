
# Double-check Eucalyptus names -------------------------------------------

librarian::shelf(tidyverse, taxize, DesiQuintans/taxizehelper)



# Import CCA species ------------------------------------------------------

cca_raw <- read_csv("output/cca_filtered_trait.csv")



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
