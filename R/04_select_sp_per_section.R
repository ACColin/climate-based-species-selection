library(tidyverse)

cca_worldclim <- read.csv("output/CCA_worldclim_df.csv", header=T, na = "-")
cca_meta <- read.csv("data/CCA_meta.csv", header=T, na = "-")
cca_filtered_trait <- read.csv("output/cca_filtered_trait.csv", header=T)

#filter that df by selection only create a CSV for it as an output to select across series

#filter maidenaria
cca_filtered_maidenaria <- 
  cca_filtered_trait %>% 
  filter(Section %in% c("Maidenaria"))
view(cca_filtered_maidenaria)

write.csv(cca_filtered_maidenaria, "output/voucher.ID.taxonomic.bioclim.year.maidenaria.csv")

#filter exsertaria
cca_filtered_exsertaria <- 
  cca_filtered_trait %>% 
  filter(Section %in% c("Exsertaria"))
#view(cca_filtered_exsertaria)
write.csv(cca_filtered_exsertaria, "output/voucher.ID.taxonomic.bioclim.year.exsertaria.csv")

#filter adnataria
cca_filtered_adnataria <- 
  cca_filtered_trait %>% 
  filter(Section %in% c("Adnataria"))
#view(cca_filtered_adnataria)
write.csv(cca_filtered_adnataria, "output/voucher.ID.taxonomic.bioclim.year.adnataria.csv")

#filter eucalyptus
cca_filtered_eucalyptus <- 
  cca_filtered_trait %>% 
  filter(Section %in% c("Eucalyptus"))
#view(cca_filtered_eucalyptus)
write.csv(cca_filtered_eucalyptus, "output/voucher.ID.taxonomic.bioclim.year.eucalyptus.csv")

#plot for each section


#for each section add the trees based on unique_ID
merged <- left_join(data1, data2, by = c("Collector's #" = "unique_ID"))

