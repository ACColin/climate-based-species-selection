#librarian::shelf(sp,raster,stats,nycflights13,tidyverse,plyr,readxl) NOT WORKING
library(sp)
library(raster)
library(stats)
library(nycflights13)
library(tidyverse)
library(plyr)
library(readxl)
cca_worldclim<-read.csv("output/CCA_worldclim_df.csv",header=T,na = "-")
cca_meta<-read.csv("data/CCA_meta.csv",header=T,na = "-")
view(cca_worldclim)
view(cca_meta)

#create subset of CCA meta with only taxa / collectors#
colnames(cca_meta)[1]<-"species"
colnames(cca_meta)[3]<-"unique_ID"
view(cca_meta)
sp_id<-select(cca_meta,"species","unique_ID")
view(sp_id)
nrow(sp_id)

#merge sub CCA meta and CCA worldclim by ID/collectors# to have the species name for each ID
u_sp_id<-unique(sp_id)
view(u_sp_id)
nrow(u_sp_id)
nrow(cca_worldclim)
final_sp_id<-drop_na(u_sp_id)
nrow(final_sp_id)
view(final_sp_id)
final_sp_id_worldclim<-merge(final_sp_id,cca_worldclim, by="unique_ID")
view(final_sp_id_worldclim)
write.csv(final_sp_id_worldclim,"output/species_ID_worldclim.csv",row.names = F)

#merge and filter for taxonomic info using Desi's Rmd........... TO ADAPT TO THE CURRENT SCRIPT
taxa_raw <- read_xlsx("data/DNTaxonomyCleaned.xlsx", skip = 1, na = "-")
id_worldclim_taxonomy <- left_join(final_sp_id_worldclim, taxa_raw, by = c("species" = "Binomial"))
view(id_worldclim_taxonomy)

iris %>% 
  group_by(Species) %>% 
  sample_n(1) %>% 
  View

cca_filtered <- 
  id_worldclim_taxonomy %>% 
  # Keep only 4 sections
  filter(Section %in% c("Maidenaria", "Eucalyptus", "Exsertaria", "Adnataria"),
         between(YearPlanted, 1994, 2000)) %>% 
  # For every series
  group_by(Section, Series) %>%
  identity()


#output: a csv file with species / unique ID / bioclim data / taxonomic info
#filter that df by selection maidenaria only create a CSV for it as an output
#do the same for each section
#plot for each section

#next step .... adding the other rasters

