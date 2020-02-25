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

#create subset of CCA meta with only taxa / ID / year for filter later
colnames(cca_meta)[1]<-"species"
colnames(cca_meta)[3]<-"unique_ID"
colnames(cca_meta)[18]<-"year"
view(cca_meta)
sp_id<-select(cca_meta,"species","unique_ID","year")
view(sp_id)
nrow(sp_id)

#working with df from worldclim_Extract_Eucs to filter
merged.dataframe<-read.csv("output/CCA_merged_df.csv")
colnames(merged.dataframe)[22]<-"unique_ID"
view(merged.dataframe)
all.df<-merge(merged.dataframe,sp_id, by='unique_ID')
view(all.df)

#merge all df: year, ID, Long, taxo, bioclim
taxa_raw <- read_xlsx("data/DNTaxonomyCleaned.xlsx", skip = 1, na = "-")
view(taxa_raw)
full.df <- left_join(all.df, taxa_raw, by = c("species" = "Binomial"))
view(full.df)
nrow(full.df)

#filter unique
u_full.df<-
  full.df %>% distinct(full.df,unique_ID,.keep_all = T)
view(u_full.df)
colnames(u_full.df)[3]<-"Lat"
u_full.df[[23]]<-NULL
nrow(u_full.df)
min(u_full.df$Long)



view(u_full.df)
nrow(u_full.df)

#filter for each section, year window and plot
cca_filtered <- 
  u_full.df %>% 
  # Keep only 4 sections
  filter(Section %in% c("Maidenaria", "Eucalyptus", "Exsertaria", "Adnataria"),
         between(year, 1994, 2000))
view(cca_filtered)
#output: a csv file with species / unique ID / bioclim data / taxonomic info
write.csv(cca_filtered, "output/voucher.ID.taxonomic.bioclim.year.sections.info.csv")
#cca_filtered is a dataframe which displays each CCA voucher tree unique_ID with the Lat and Longs of sample site and the bioclim variables associated with those sample sites. In the dataframe are only selected the mother trees within the Exsertaria, Maidenaria, Eucalyptus and Adnataria sections and when the seeds were planted. Only the seeds planted between 1994-2000 are shown to select trees that were planted at CCA during a general same period of time, at least 6 years before the drought of 2006 to ensure the trees were strong enough to endure the dry period of 2006. The dataframe doesn't show all trees at CCA, only the mother tree.
#By voucher tree it also means that we have the list of species already considering that the trees planted from the voucher tree will be the same species

#plot means
plot.mean.section<-ggplot(data = cca_filtered, mapping = aes(x = BIO1, y = BIO12)) + 
  geom_point(mapping = aes(color = Section))

print(plot.mean.section)
dev.print(pdf, 'figs/plot_bioclim_mean_CCA_sections.pdf')

#plot extremes
plot.extreme.section<-ggplot(data = cca_filtered, mapping = aes(x = BIO5, y = BIO14)) + 
  geom_point(mapping = aes(color = Section))+
  facet_grid(Section ~ .)

print(plot.extreme.section)

plot.extreme<-ggplot(data = cca_filtered, mapping = aes(x = BIO5, y = BIO14)) + 
  geom_point(mapping = aes(color = Section))

print(plot.extreme)
dev.print(pdf, 'figs/plot_bioclim_extreme_CCA.pdf')


#####adding traits
#euc_trait_table<-read.csv("data/euc_trait_table.csv", header = T)
#view(euc_trait_table)
#colnames(euc_trait_table)[2]<-"Species"
#cca_filtered<-read.csv("output/voucher.ID.taxonomic.bioclim.year.sections.info.csv", header = T)
#view(cca_filtered)
#subset.euc_trait_table<-euc_trait_table[euc_trait_table$Species %in% cca_filtered$Species,]   # will keep data in euc_trait_table
#view(subset.euc_trait_table)
#cca_filtered_trait <- left_join(cca_filtered, subset.euc_trait_table, by ="Species")
#view(cca_filtered_trait)
sp.presence <- cca_filtered[31]
view(sp.presence)
sp.query <- euc_trait_table[2]
view(sp.query) 
mutate(a, result = a$col_A %in% b$col_B)
<- sapply(cca_filtered, function(x) sp.presence %in% euc_trait_table$species, <=x )


#filter that df by selection only create a CSV for it as an output to select across series

#filter maidenaria
cca_filtered_maidenaria <- 
  cca_filtered %>% 
  filter(Section %in% c("Maidenaria"))
view(cca_filtered_maidenaria)
write.csv(cca_filtered_maidenaria, "output/voucher.ID.taxonomic.bioclim.year.maidenaria.csv")

#filter exsertaria
cca_filtered_exsertaria <- 
  cca_filtered %>% 
  filter(Section %in% c("Exsertaria"))
view(cca_filtered_exsertaria)
write.csv(cca_filtered_exsertaria, "output/voucher.ID.taxonomic.bioclim.year.exsertaria.csv")

#filter adnataria
cca_filtered_adnataria <- 
  cca_filtered %>% 
  filter(Section %in% c("Adnataria"))
view(cca_filtered_adnataria)
write.csv(cca_filtered_adnataria, "output/voucher.ID.taxonomic.bioclim.year.adnataria.csv")

#filter eucalyptus
cca_filtered_eucalyptus <- 
  cca_filtered %>% 
  filter(Section %in% c("Eucalyptus"))
view(cca_filtered_eucalyptus)
write.csv(cca_filtered_eucalyptus, "output/voucher.ID.taxonomic.bioclim.year.eucalyptus.csv")


#do the same for each section
#plot for each section




#next step .... adding the other filters

