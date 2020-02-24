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

#working with df from worldclim_Extract_Eucs to filter using Long>100
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

#filter Long >100, unique
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
dev.print(pdf, 'figs/plot_bioclim_extreme_CCA_sections.pdf')



require(maps)
require(viridis)
theme_set(
  theme_void()
)
aus<-c("Australia")
aus.map <- map_data("world", region = aus)

region.lab.data <- aus.map %>%
  group_by(region) %>%
  summarise(long = mean(long), lat = mean(lat))
  
ggplot(aus.map, aes(x = long, y = lat)) +
  geom_polygon(aes( group = group, fill = region))+
  geom_text(aes(label = region), data = region.lab.data,  size = 3, hjust = 0.5)+
  scale_fill_viridis_d()+
  theme_void()+
  theme(legend.position = "none")

# For every series
  group_by(Section, Series) %>%
  identity()



#############merge by unique_ID then select values (unique_ID,year,Long) then subset by Long > 100, check min and nrow







  # For every series
  group_by(Section, Series) %>%
  identity()


#output: a csv file with species / unique ID / bioclim data / taxonomic info
#filter that df by selection maidenaria only create a CSV for it as an output
#do the same for each section
#plot for each section




#next step .... adding the other rasters

