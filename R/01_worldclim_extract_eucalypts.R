####        STEP 1: WORLDCLIM DATA INPUT


library(raster)
library(sp)
library(stats)

r <- getData("worldclim",var="bio",res=2.5)
?getData
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

#dms2dd(dd, mm, ss, ns)

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
?subset
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

# Plot by climate means:
#   - BIO1: Mean Annual Temperature
#   - BIO12: Annual Precipitation
plot.mean <- with(final.dataframe, plot(BIO1, BIO12, cex=1, pch=16, col="grey46"))

print(plot.extreme)
dev.print(pdf, 'figs/plot_bioclim_extreme_CCA_trees_origin.pdf')
print(plot.mean)
dev.print(pdf, 'figs/plot_bioclim_mean_CCA_trees_origin.pdf')


#####################################################
