####        STEP 1: WORLDCLIM DATA INPUT

# (copy of script 01_worldclim_extract_eucalypts.R)


library(raster)
library(sp)
library(stats)


r <- getData("worldclim",var="bio",res=2.5)
r <- r[[c(1:19)]]
names(r) <- c("BIO1","BIO2","BIO3","BIO4","BIO5","BIO6",
              "BIO7","BIO8","BIO9","BIO10","BIO11","BIO12",
              "BIO13","BIO14","BIO15","BIO16","BIO17","BIO18","BIO19")

#   As a reminder..
# BIOCLIM are the Worldclim bioclimatic variables.
# BIOCLIM Variables:
# BIO1 = Annual Mean Temperature
# BIO2 = Mean Diurnal Range (Mean of monthly (max temp - min temp))
# BIO3 = Isothermality (BIO2/BIO7) (* 100)
# BIO4 = Temperature Seasonality (standard deviation *100)
# BIO5 = Max Temperature of Warmest Month
# BIO6 = Min Temperature of Coldest Month
# BIO7 = Temperature Annual Range (BIO5-BIO6)
# BIO8 = Mean Temperature of Wettest Quarter
# BIO9 = Mean Temperature of Driest Quarter
# BIO10 = Mean Temperature of Warmest Quarter
# BIO11 = Mean Temperature of Coldest Quarter
# BIO12 = Annual Precipitation
# BIO13 = Precipitation of Wettest Month
# BIO14 = Precipitation of Driest Month
# BIO15 = Precipitation Seasonality (Coefficient of Variation)
# BIO16 = Precipitation of Wettest Quarter
# BIO17 = Precipitation of Driest Quarter
# BIO18 = Precipitation of Warmest Quarter
# BIO19 = Precipitation of Coldest Quarter

coords <- read.csv("data/CCA_meta.csv", header=T)
head(coords)

coords$LatD <- as.numeric(as.character(coords$LatD))
coords$LatM <- as.numeric(as.character(coords$LatM))
coords$LatS <- as.numeric(as.character(coords$LatS))

coords$LongD <- as.numeric(as.character(coords$LongD))
coords$LongM <- as.numeric(as.character(coords$LongM))
coords$LongS <- as.numeric(as.character(coords$LongS))


coords$Lat <- as.numeric(coords$LatD) + as.numeric(coords$LatM/60) + as.numeric(coords$LatS/60^2)
coords$Long <- as.numeric(coords$LongD) + as.numeric(coords$LongM/60) + as.numeric(coords$LongS/60^2)
head(coords)
coords$Lat <- coords$Lat*-1 # make the southern lats negative
head(coords)

coords.all <- coords[,c("Collector.s..",'Lat','Long')]
coords <- na.omit(coords)
head(coords.all)

coords.ll <- na.omit(coords.all)
coords.ll <- coords.ll[,c(2,3)]
plot(coords.ll$Long, coords.ll$Lat)

coords.sub <- subset(coords.ll, Long > 100) # remove the weird numbers in the data set (keeping only samples in Australia)
min(coords.sub$Long)
plot(coords.sub$Long, coords.sub$Lat)
#view(coords.sub)
coords.sub <- coords.sub[, c(2, 1)]

# getting our decimal degress points ready for spatial projection
points <- SpatialPoints(unique(coords.sub), proj4string = r@crs)
plot(points)
head(points)

values <- raster::extract(r, points) #  extract bioclim of sampling site. voucher tree (sampling site) climate of origin 

# merging & cleaning... remove duplicates 
all.sites <- as.data.frame(values)
head(all.sites)
nrow(all.sites)

nrow(coords.sub)
nrow(unique(coords.sub))
head(coords.sub)
head(all.sites)

unique_sites.dataframe <- cbind(unique(coords.sub),all.sites)
head(unique_sites.dataframe)


# now that the two dataframes are merged we have a dataframe with unique points with the coords and the bioclim data
# but we don't know which trees they are, we need the voucher unique ID for each point...
all.dataframe<- na.omit(coords.all)
head(all.dataframe)

# keeping unique ID, removing duplicates
# merging bioclim data and tree ID data by long variable
nrow(unique(all.dataframe))
u.all.dataframe <- unique(all.dataframe)
merged.dataframe <- merge(unique_sites.dataframe, u.all.dataframe, by="Long") # by.x="Long, by.y="Long"
head(merged.dataframe)
write_csv(merged.dataframe, "output/CCA_merged_df.csv")

# Laty = Lat x. removing Lat y and naming Lat x = Lat
# naming collector.s = unique_ID
# data cleaned!
final.dataframe <- merged.dataframe[,c(22,1:21)]
head(final.dataframe)
colnames(final.dataframe)[1] <- "unique_ID"
colnames(final.dataframe)[3] <- "Lat"
write.table(final.dataframe, "output/CCA_worldclim_df.txt", quote = FALSE, row.names = FALSE)
write.csv(final.dataframe, "output/CCA_worldclim_df.csv")




# VISUALIZING DATA

# Visual of climate of origin of voucher tree (final.dataframe)

# Plot by climate extemes:
#   - BIO5: Max Temperature of Warmest Month
#   - BIO14: Precipitation of Driest Month

plot.extreme <- with(final.dataframe, plot(BIO5, BIO14, cex=1, pch=16, col="grey46"))
print(plot.extreme)
dev.print(pdf, 'figs/plot_bioclim_extreme_CCA_trees_origin.pdf')

# Plot by climate means:
#   - BIO1: Mean Annual Temperature
#   - BIO12: Annual Precipitation
plot.mean <- with(final.dataframe, plot(BIO1, BIO12, cex=1, pch=16, col="grey46"))
print(plot.mean)
dev.print(pdf, 'figs/plot_bioclim_mean_CCA_trees_origin.pdf')

# end of script 01_worldclim_extract_eucalypts.R


############################################

####        STEP 2: TAXONOMY DATA INPUT & FILTERING

# (copy of script 02_tidy_merge_eucalypts.R)

library(sp)
library(raster)
library(stats)
library(nycflights13)
library(plyr)
library(readxl)
library(tidyverse)

cca_worldclim<-read.csv("output/CCA_worldclim_df.csv",header=T,na = "-")
cca_meta<-read.csv("data/CCA_meta.csv",header=T,na = "-")
#view(cca_worldclim)
#view(cca_meta)

# create subset of CCA meta with only taxa / unique_ID / offspring tree reps year of plantation for subsequent filtering

colnames(cca_meta)[1]<-"species"
colnames(cca_meta)[3]<-"unique_ID"
colnames(cca_meta)[18]<-"year"
#view(cca_meta)

sp_id <- select(cca_meta,"species","unique_ID","year")

#view(sp_id)
nrow(sp_id)

# merging df from STEP 1 above with sp_id for year of plantation
merged.dataframe<-read.csv("output/CCA_merged_df.csv")
colnames(merged.dataframe)[22]<-"unique_ID"
#view(merged.dataframe)
all.df<-merge(merged.dataframe,sp_id, by='unique_ID')
#view(all.df)

# merge all df: adding the full taxonomy description to previous df
taxa_raw <- read_xlsx("data/DNTaxonomyCleaned.xlsx", skip = 1, na = "-")
#view(taxa_raw)
full.df <- left_join(all.df, taxa_raw, by = c("species" = "Binomial"))
#view(full.df)
nrow(full.df)

# filter unique_ID again
u_full.df<-
  full.df %>% distinct(unique_ID,.keep_all = T)
#view(u_full.df)
colnames(u_full.df)[3]<-"Lat"
u_full.df[[23]]<-NULL
nrow(u_full.df)
min(u_full.df$Long)



#view(u_full.df)
nrow(u_full.df)

# Filter for:
#     - Sections: Maidenaria, Exsertaria, Adnataria & Eucalyptus
#     - Year window: 1994 - 2000 (planted roughly at the same time, around 6 years before first drought at CCA to ensure good implantation and growth prior drought) (1994 - 2000 also include the biggest blocks, 1993 and 2001 can be included if needed -> check CCA blocks map)

cca_filtered <- 
  u_full.df %>% 
  filter(Section %in% c("Maidenaria", "Eucalyptus", "Exsertaria", "Adnataria"),
         between(year, 1994, 2000))
#view(cca_filtered)


# output: a csv file with species / unique ID / bioclim data / taxonomic info
write.csv(cca_filtered, "output/voucher.ID.taxonomic.bioclim.year.sections.info.csv")

# More thorough description of the df... cca_filtered is a dataframe which displays:
#       - each CCA voucher tree (unique_ID)
#       - with the Lat and Longs of sample site
#       - and the bioclim variables associated with the sample sites.
# In the dataframe are only selected the mother trees within the Exsertaria, Maidenaria, Eucalyptus and Adnataria sections and when the seeds were planted.
# Only the seeds planted between 1994-2000 are shown to select trees that were planted at CCA during a general same period of time, at least 6 years before 2006's drought to ensure the trees were strong enough to endure the dry period.
# The dataframe doesn't show all trees (replicates) at CCA, only the mother tree.


#       DATA VISUALS

#plot climate means
plot.mean.section<-ggplot(data = cca_filtered, mapping = aes(x = BIO1, y = BIO12)) + 
  geom_point(mapping = aes(color = Section))

print(plot.mean.section)
dev.print(pdf, 'figs/plot_bioclim_mean_CCA_sections.pdf')

#plot climate extremes
plot.extreme.section<-ggplot(data = cca_filtered, mapping = aes(x = BIO5, y = BIO14)) + 
  geom_point(mapping = aes(color = Section))+
  facet_grid(Section ~ .)

print(plot.extreme.section)

plot.extreme<-ggplot(data = cca_filtered, mapping = aes(x = BIO5, y = BIO14)) + 
  geom_point(mapping = aes(color = Section))

print(plot.extreme)
dev.print(pdf, 'figs/plot_bioclim_extreme_CCA.pdf')


##### Adding flora traits
# Flora traits extracted from bibliography by Desi Quintans (PhD student in Paul's lab group)
euc_trait_table <- 
  read.csv("data/euc_trait_table.csv", header = T) %>% 
  mutate(species_name = 
           paste(binomen, "subsp.", subsp, "var.", variety) %>% 
           str_remove_all(" (var|subsp)\\. NA"))

cca_filtered_trait <- left_join(cca_filtered, euc_trait_table, by = c("species" = "species_name"))
write_csv(cca_filtered_trait, "output/cca_filtered_trait.csv")

# end of script 02_tidy_merge_eucalypts.R


############################################

####        STEP 3: SELECTION PER SECTION

# (copy of script 05_trees_selection_with_species_list_sec.R)

library(tidyverse)
library(plotly)
library(viridis)

cca_filtered_trait <- read_csv("output/cca_filtered_trait.csv")
#in order to compare with NSW threatened eucs list...
#List obtained with BioNet Atlas on NSW website (48sp.)
NSW.threatened.species.full<-read.csv("data/NSW.threatened.eucs.species.list.csv")
view(NSW.threatened.species.full)

######### Plots for species selection with mean/extreme climate variables

# BIO1; mean annual temperature
# BIO5: highest temperature of the warmest month
# BIO12: mean annual precipitation
# BIO14: precipitation of the wetest month

# Section Adnataria
cca_Adna$unique_ID=as.factor(cca_Adna$unique_ID)
Adna_mean <- plot_ly(data=cca_Adna,x=~BIO1,y=~BIO12,size=20,color=~unique_ID,symbol=~Series,symbols=c(10:18))
print(Adna_mean)
Adna_ext <- plot_ly(data=cca_Adna,x=~BIO5,y=~BIO14,size=20,color=~unique_ID,symbol=~Series,symbols=c(10:18))
print(Adna_ext)


# Section Eucalytpus
cca_Euca=cca_filtered_trait %>% 
  filter(Section=="Eucalyptus")
cca_Euca$unique_ID=as.factor(cca_Euca$unique_ID)
plot_ly(data=cca_Euca,x=~BIO1,y=~BIO12,size=20,color=~unique_ID,symbol=~Series,symbols=c(4:18))
plot_ly(data=cca_Euca,x=~BIO5,y=~BIO14,size=20,color=~unique_ID,symbol=~Series,symbols=c(4:18))

print(plot.extreme)
dev.print(pdf, 'figs/plot_bioclim_extreme_CCA.pdf')


# Section Maidenaria
cca_Maid=cca_filtered_trait %>% 
  filter(Section=="Maidenaria")
cca_Maid$unique_ID=as.factor(cca_Maid$unique_ID)
plot_ly(data=cca_Maid,x=~BIO1,y=~BIO12,size=20,color=~unique_ID,symbol=~Series,symbols=c(4:18))
plot_ly(data=cca_Maid,x=~BIO5,y=~BIO14,size=20,color=~unique_ID,symbol=~Series,symbols=c(4:18))

print(plot.extreme)
dev.print(pdf, 'figs/plot_bioclim_extreme_CCA.pdf')


# Section Exsertaria
cca_Exser=cca_filtered_trait %>% 
  filter(Section=="Exsertaria")
cca_Exser$unique_ID=as.factor(cca_Exser$unique_ID)
plot_ly(data=cca_Exser,x=~BIO1,y=~BIO12,size=20,color=~unique_ID,symbol=~Series,symbols=c(10:18))
plot_ly(data=cca_Exser,x=~BIO5,y=~BIO14,size=20,color=~unique_ID,symbol=~Series,symbols=c(10:18))

print(plot.extreme)
dev.print(pdf, 'figs/plot_bioclim_extreme_CCA.pdf')
