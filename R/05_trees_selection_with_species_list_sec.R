####        STEP 3: SELECTION PER SECTION


library(tidyverse)
cca_filtered_trait <- read_csv("output/cca_filtered_trait.csv")
#in order to compare with NSW threatened eucs list...
NSW.threatened.species<-read.csv("data/ProfileSearch", sep=",")
view(NSW.threatened.species)

#List obtained with BioNet Atlas on NSW website (48sp.)
NSW.threatened.species.full<-read.csv("data/NSW.threatened.eucs.species.list.csv")
view(NSW.threatened.species.full)
#model=lm(BIO1~species,data=cca_filtered_trait)
#summary(model)
#install.packages('multcomp')
#library(multcomp)

### To keep but not used here..
#modelAdna=lm(BIO1~species,data=cca_Adna) 
#summary(modelAdna)
#Adna=aov(cca_Adna$BIO1~cca_Adna$species)
#anova(Adna)
#TukeyHSD(x=Adna, cca_Adna$species,conf.level = .95)
#multcomp_Adna=glht(model=modelAdna,linfct=mcp(species='Tukey'))
#summary(multcomp_Adna)
#library(emmeans)
#test=emmeans(modelAdna,"species")
#a=pairs(test)
#options(max.print=1000000)
#print(a)

#eff_size(test,sigma=sigma(modelAdna),edf=50)
#plot(test,comparisons=TRUE)
#pwpp(test,method='pairwise',side='=')


######### Plots for species selection with mean/extreme climate variables

# BIO1; mean annual temperature
# BIO5: highest temperature of the warmest month
# BIO12: mean annual precipitation
# BIO14: precipitation of the wetest month

cca_filtered_trait$species=as.factor(cca_filtered_trait$species)
cca_Adna=cca_filtered_trait %>% 
  filter(Section=="Adnataria")

#df=cca_Adna[,c(4,15,8,17)]
#PCA=prcomp(df)
#library(ggfortify)
#autoplot(prcomp(df),loadings=TRUE,loadings.colour='black',loadings.label=TRUE,loadings.label.size=4,data=cca_Adna,colour='species',size=8)


######### Plots for species selection with mean/extreme climate variables

library(plotly)

# Section Adnataria
cca_Adna$unique_ID=as.factor(cca_Adna$unique_ID)
library(viridis)
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
