rm(list=ls()) #To clean R environment before starting

##Creating distribution maps

install.packages("raster") #Installing the appropriate packages to create the maps
install.packages("sf")
install.packages("ggplot2")
install.packages("RColorBrewer")

library(sf) #For bird distributions
library(raster) #For creating and working with raster data
library(ggplot2) #For plotting the heatmap
library(RColorBrewer) #For colour palettes

maps <- st_read("/Users/AllyLeather/Dropbox/BOTW2/BOTW.gdb", layer="All_Species") #This reads in the birdlife species list
setwd("~/Dropbox") #Setting the working directory to be able to access the dataset
edge <- read.csv("Insectivore_edge.csv") #To read in the dataset to get species and threat level

r <- raster(ncols=2160, nrows = 900, ymn = -60) #To set up empty raster using the same resolution and extents as the bioclim variables - climatic data rasters
raster_stack <- r #To set up empty raster stack. Will set it the same as r to project species distribution onto r and then layer each raster into the raster_stack

#Using a loop to create a raster for each species from their distribution polygon
for (i in 1:nrow(edge)) {
  print(i) #To print the row to keep track of which species it's on
  s <- as.character(edge$Birdlife_Name[i]) #To select the species from my data
  map_i <- subset(maps, maps$SCINAME == s) #To select the map that corresponds with the species
  
  for (j in 1:length(map_i$Shape)) { 
    try(map_i$Shape[[j]] <- st_cast(map_i$Shape[[j]], 'MULTIPOLYGON')) #To reformat each map into multiploygon which is needed to create the raster
  }
  try(map_i$Shape <- st_cast(map_i$Shape, 'MULTIPOLYGON')) #Putting in a try loop to prevent an error halting the loop. Will skip the species if it doesn't work
  
  polygon <- as_Spatial(st_combine(map_i$Shape)) #To combine the maps and convert them into a spatial polygon
  raster_i <- rasterize(polygon, r) #To project the polygon onto the raster
  rastercells <- which(getValues(raster_i) > 0)  #To select the rastercells which get given data
  raster_i[rastercells] <- edge$Edge_score[i] #Fill the raster with the threat score of that species
  raster_stack <- addLayer(raster_stack, raster_i) 
  if (i %in% c(500,1000,1500,2000,2500,3000,3500,4000,4500,5000,nrow(edge))) { #To condense the raster stacks into raster bricks to conserve memory
    raster_stack <- brick(raster_stack)
    print(paste0("Bricked_",i))
  }
}

#Create a raster_layer where each grid cell is the average of all the values in the layers of the raster_stack
av_full <- stackApply(raster_stack, indices = rep(1, nrow(edge)), fun = mean, na.rm = T) 
writeRaster(av_full, "Insectivore_distribution", format = "raster")
r<- raster("Insectivore_distribution")

edge <- as.data.frame(r, xy=TRUE) %>% drop_na() #To convert the raster data into a dataframe which is required to plot

#Recalling the map dataframes for plotting
rich<- read.csv("SR_dataframe.csv", header = TRUE)
colnames(rich) <- c("Longitude", "Latitude", "Species") #Renaming the columns so the axes are named appropriately

rich[rich==0]<-NA   #Making all empty raster cells NA

ri<- na.omit(rich) #Removing NAs from the data
write.csv(ri, "SR_df.csv", row.names = TRUE) #Creating a new dataset with the changes

#Doing the same changes for the remainning map data
agri<- read.csv("agriculture_dataframe.csv", header=TRUE)
colnames(agri) <- c("Longitude", "Latitude", "Severity")

agri[agri==0]<-NA
ag<- na.omit(agri)
write.csv(ag, "agriculture_df.csv", row.names = TRUE)

climate<- read.csv("climate_dataframe.csv", header = TRUE)
colnames(climate) <- c("Longitude", "Latitude", "Severity")

climate[climate==0]<-NA
clim<- na.omit(climate)
write.csv(clim, "climate_df.csv", row.names = TRUE)


setwd("~/Documents/Imperial College/Thesis/dataframes") #Setting the working directory to the folder with the map dataframes

#Plotting species richness map
rich<- read.csv("SR_df.csv", header = TRUE)
View(rich)
quantile(rich$Species, prob = c(0,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9)) #Checking the quartiles at each inteval to create accurate gradient

richplot <- ggplot() + 
  borders("world", colour = "grey", fill = "grey", ylim = c(-60,90)) +  #Plotting world as a grey border as a template for the rasters
  geom_raster(aes(x=Longitude,y=Latitude,fill= Species), data=rich, interpolate = TRUE) + #Filling the map with species rasters
  scale_fill_gradientn(colors = c("white", "khaki1", "seagreen2", "green4", "yellow1","darkorange1","firebrick4"), values = c(0, 0.01, 0.15,0.21,0.35,0.45,0.6, 1)) + #Assigning colours to each value range
  theme_classic() + 
  theme(legend.position="bottom") + 
  theme(legend.key.size = unit(0.3,"cm")) +
  theme(legend.text = element_text(colour="blue", size=6.5, face="bold"), legend.title = element_text(size=9)) +
  scale_y_continuous(breaks = seq(-60, 90, by = 20)) #Altering the y axis range and breaks

#Plotting EDGE score map
quantile(edge$EDGE, prob = c(0,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9)) #Checking the quartiles at each inteval to create accurate gradient

edgeplot <- ggplot() + 
  borders("world", colour = "grey", fill = "grey", ylim = c(-60,90)) + 
  geom_raster(aes(x=Longitude,y=Latitude,fill= EDGE), data=edge, interpolate = TRUE) + 
  scale_fill_gradientn(colors = c("dodgerblue", "skyblue2", "gold2", "tomato", "orangered","red3","firebrick4"), values = c(0, 0.017, 0.026, 0.039, 0.059, 0.089, 4)) + 
  theme_classic() + 
  theme(legend.position="bottom") + 
  theme(legend.key.size = unit(0.3,"cm")) +
  theme(legend.text = element_text(colour="blue", size=6.5, face="bold"), legend.title = element_text(size=9)) +
  scale_y_continuous(breaks = seq(-60, 90, by = 20))

#Plotting the agriculture severity map
agri<- read.csv("agriculture_df.csv", header=TRUE)

quantile(agri$Severity, prob = c(0,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9)) 

agriplot <- ggplot() + # plot the heatmap#
  borders("world", colour = "grey", fill = "grey", ylim = c(-60,90)) + 
  geom_raster(aes(x=Longitude,y=Latitude,fill= Severity), data=agri, interpolate = TRUE) + 
  scale_fill_gradientn(colors = c("white", "skyblue2", "darkorchid1", "darkorchid3", "purple3","purple4","firebrick4"), values = c(0, 0.01, 0.15,0.33,0.35,0.45, 1)) + 
  theme_classic() + 
  theme(legend.position="bottom") + 
  theme(legend.key.size = unit(0.3,"cm")) +
  theme(legend.text = element_text(colour="blue", size=6.5, face="bold"), legend.title = element_text(size=9)) +
  scale_y_continuous(breaks = seq(-60, 90, by = 20))

#Plotting the climate change severity map
climate<- read.csv("climate_df.csv", header = TRUE)

quantile(climate$Severity, prob = c(0,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9)) #check the quartiles at each inteval to create accurate gradient

climplot <- ggplot() + # plot the heatmap#
  borders("world", colour = "grey", fill = "grey", ylim = c(-60,90)) + 
  geom_raster(aes(x=Longitude,y=Latitude,fill= Severity), data=climate, interpolate = TRUE) + 
  scale_fill_gradientn(colors = c("white", "lightcyan2","gold", "goldenrod", "orange2","red2","grey0"), values = c(0, 0.1, 0.18,0.22,0.30,0.4,0.5, 1)) + 
  theme_classic() + 
  theme(legend.position="bottom") + 
  theme(legend.key.size = unit(0.3,"cm")) +
  theme(legend.text = element_text(colour="blue", size=6.5, face="bold"), legend.title = element_text(size=9)) +
  scale_y_continuous(breaks = seq(-60, 90, by = 20))

install.packages("pdp") #Installing package for partial depence plot
library(pdp)

grid.arrange(richplot, edgeplot, agriplot, climplot, ncol=2) #Using grid arrange so all maps can go on the same plot

##Merging trait and threat datasets for modelling

setwd("~/Documents/Imperial College/Thesis")#Changing working directory

traits<-read.csv("Insectivore_map.csv") #Reading in trait data
threats<-read.csv("BL_Threats_2019.csv") #Reading in threat data

insectivores<-merge(traits,threats, by="Birdlife_Name") #Merging the trait and threat data by their shared column name
write.csv(insectivores, "Insectivores.csv") #Creating new csv file for merged data

install.packages("dplyr") #For data wrangling
library(dplyr)
t1<-read.csv("Insectivores.csv", header = T)
t1<-unique(t1) #Removing any repeated species in merged dataset so it is all unique data

write.csv(t1, "Insectivore_final.csv") #Creating final dataset that can be used for modelling

##Modelling

install.packages("lme4") #For glmms
install.packages("ordinal") #For clmms

library(lme4)
library(ordinal)

Insectivore<- read.csv("Insectivore_final.csv", header = TRUE)

#Converting necessary variables into factors
Insectivore$Migration <- factor(Insectivore$Migration, levels = c(1,2,3), labels = c("Sedentary", "Partial Migrant", "Migratory"))

Insectivore$Habitat <- factor(Insectivore$Habitat, levels = c(1,2,3), labels = c("Dense", "Semi-Open", "Open"))

Insectivore$Foraging_strata <- factor(Insectivore$Foraging_strata, levels = c("Generalist","Lower Strata", "Upper Strata"), labels = c("Generalist","Lower Strata", "Upper Strata"))

#Modelling Edge scores to predict endangerment for agriculture
agri<- Insectivore %>% filter(Threat=="Agriculture") #Filtering data for threat type

agri$Edge_score <- as.factor(agri$Edge_score)

a1 <- clmm2(Edge_score~HW_Index+Bill_Width+Mass+Migration+Range_size+GenLength+Habitat+Foraging_strata+Centroid_Lat, data=agri) #Modelling predictors against edge score
summary(a1)

a2 <- clmm2(Edge_score~HW_Index+Bill_Width+Mass+Migration+Range_size+GenLength+Habitat+Foraging_strata+Centroid_Lat, link = "logistic", data=agri)
summary(a2) #adding link does nothing

install.packages("lmtest") #To test which model is most suitable
library(lmtest) #Using likelihood ratio test to see if adding a link to the model improves the model
lrtest(a2,a1)  #Not significantly significant as logliklihood is barely changed and 0.29 is not a significant so therefore use more simple model


#Calculating the odds ratio to plot results with
a1$coefficients
exp(a1$coefficients)
#Getting the confidence intervals
exp(confint(a1))

install.packages("sjPlot") #To plot model output
library(sjPlot)

set_theme(base = theme_classic()) #To remove grey background on plots 

plot_model(a1, rm.terms = c("1|2","2|3","3|4","4|5")) #To remove unwanted rows

a_mod<-plot_model(a1, rm.terms = c("1|2","2|3","3|4","4|5"),show.values = TRUE, show.p = TRUE,value.offset = 0.4, show.legend = TRUE, legend.inside = TRUE) + ggtitle("Agriculture EDGE")+geom_hline(yintercept = 1, linetype = "dashed") #Plotting odds ratios for traits

#Modelling Edge scores to predict endangerment for logging
log <- Insectivore %>% filter(Threat=="Logging")

log$Edge_score <- as.factor(log$Edge_score)
l1 <- clmm2(Edge_score~HW_Index+Bill_Width+Mass+Migration+Range_size+GenLength+Habitat+Foraging_strata+Centroid_Lat, data=log)
summary(l1)

l2 <- lmer(Edge_score~HW_Index+Bill_Width+Mass+Migration+Range_size+GenLength+Habitat+Foraging_strata+Centroid_Lat+(1|Family)+(1|Order),data=log)
summary(l2)

lrtest(l1,l2) #Not significant 0.98 p value, stick with l1

#Calculating the odds ratio
l1$coefficients
exp(l1$coefficients)
#Getting the confidence intervals
exp(confint(l1))

#Creating plot for edge score logging
set_theme(base = theme_classic())
l_mod<-plot_model(l1, rm.terms = c("1|2","2|3","3|4","4|5"),show.values = TRUE, show.p = TRUE,value.offset = 0.4, show.legend = TRUE, legend.inside = TRUE) + ggtitle("Logging EDGE")+geom_hline(yintercept = 1, linetype = "dashed")

#Modelling Edge scores to predict endangerment for climate change
clim <- Insectivore %>% filter(Threat=="Climate change")

c1 <- clim$Edge_score <- as.factor(clim$Edge_score)
c1 <- clmm2(Edge_score~HW_Index+Bill_Width+Mass+Migration+Range_size+GenLength+Habitat+Foraging_strata+Centroid_Lat, data=clim)
summary(c1)

c2 <- lmer(Edge_score~HW_Index+Bill_Width+Mass+Migration+Range_size+GenLength+Habitat+Foraging_strata+Centroid_Lat+(1|Family)+(1|Order),data=clim)
summary(c2)

lrtest(c1,c2) #P value is 1 so there is no difference between models so no need for family

c3<- lm(Edge_score~HW_Index+Bill_Width+Mass+Migration+Range_size+GenLength+Habitat+Foraging_strata+Centroid_Lat, data=clim)
summary(c3)

anova(c1,c3) #Significantly says that lmm is better as p value below 0.001

#Calculating the odds ratio
c1$coefficients
exp(c1$coefficients)
#Getting the confidence intervals
exp(confint(c1))

#Creating plot for edge score climate change
set_theme(base = theme_classic())
c_mod<-plot_model(c1, rm.terms = c("1|2","2|3","3|4","4|5"),show.values = TRUE, show.p = TRUE,value.offset = 0.4, show.legend = TRUE, legend.inside = TRUE) + ggtitle("Climate Change EDGE")+geom_hline(yintercept = 1, linetype = "dashed")

#Modelling Edge scores to predict endangerment for huamn disturbance
hum <- Insectivore %>% filter(Threat=="Human disturbance")

hum$Edge_score <- as.factor(hum$Edge_score)
h1 <- clmm2(Edge_score~HW_Index+Bill_Width+Mass+Migration+Range_size+GenLength+Habitat+Foraging_strata+Centroid_Lat, data=hum)
summary(h1)

h2 <- lmer(Edge_score~HW_Index+Bill_Width+Mass+Migration+Range_size+GenLength+Habitat+Foraging_strata+Centroid_Lat+(1|Family)+(1|Order),data=hum)
summary(h2)

lrtest(h1,h2) #0.42, not significant

h3 <- lm(Edge_score~HW_Index+Bill_Width+Mass+Migration+Range_size+GenLength+Habitat+Foraging_strata+Centroid_Lat, data=hum)
summary(h3)

anova(h1,h3) #not significant (use simpler model?) 0.22

#Calculating the odds ratio
h1$coefficients
exp(h1$coefficients)
#Getting the confidence intervals
exp(confint(h1))

#Creating plot for edge score human disturbance
set_theme(base = theme_classic())
h_mod<-plot_model(h1, rm.terms = c("1|2","2|3","3|4","4|5"),show.values = TRUE, show.p = TRUE,value.offset = 0.4, show.legend = TRUE,legend.inside = TRUE) + ggtitle("Human Disturbance EDGE")+geom_hline(yintercept = 1, linetype = "dashed")

grid.arrange(a_mod, l_mod, c_mod, h_mod, ncol=2) #To plot all graphs on same plot window

#GLMMs

#Modelling sensitivy to agriculture
agri_bi<- read.csv("Binary_agri.csv", header =TRUE)

agri_bi$Migration <- factor(agri_bi$Migration, levels = c(1,2,3), labels = c("Sedentary", "Partial Migrant", "Migratory"))

agri_bi$Habitat <- factor(agri_bi$Habitat, levels = c(1,2,3), labels = c("Dense", "Semi-Open", "Open"))

#Gradually adding more predictors to observe how adding more variable impacts model and how they act upon each other
ab1 <- glmer(Agri_threat~HW_Index+(1|Family),family = binomial(link = "logit"),data =agri_bi)
summary(ab1)

ab2 <- glmer(Agri_threat~HW_Index+Bill_Width+(1|Family),family = binomial(link = "logit"),data =agri_bi)
summary(ab2)

ab3 <- glmer(Agri_threat~HW_Index+Bill_Width+Range_size+Migration+Habitat+(1|Family),family = binomial(link = "logit"),data =agri_bi)
summary(ab3)

ab4 <- glmer(Agri_threat~HW_Index+Bill_Width+Mass+Migration+Range_size+GenLength+Habitat+Foraging_strata+Centroid_Lat+(1|Family),family = binomial(link = "logit"),data =agri_bi)
summary(ab4)

ab5 <- glmer(Agri_threat~HW_Index+Bill_Width+Mass+Migration+Range_size+GenLength+Habitat+Foraging_strata+Centroid_Lat+(1|Family)+(1|Order),family = binomial(link = "logit"),data =agri_bi)
summary(ab5)

lrtest(ab4,ab5) #Adding order as an additional random effect made no significant difference to the results so keep only family as random effect

#Calculating the odds ratio
ab4$coefficients
exp(ab4$coefficients)
#Getting the confidence intervals
exp(confint(ab4))

library(oddsratio) #####Do I add this?
or_glm(data= agri_bi, model= ab4, ci=0.95)

set_theme(base = theme_classic())
a_mod2<-plot_model(ab4, rm.terms = c("1|2","2|3","3|4","4|5"),show.values = TRUE, show.p = TRUE,value.offset = 0.4, show.legend = TRUE, legend.inside = TRUE) + ggtitle("Agriculture")+geom_hline(yintercept = 1, linetype = "dashed")

#Modelling sensitivy to logging
log_bi <- read.csv("Binary_logging.csv", header =TRUE)

log_bi$Migration <- factor(log_bi$Migration, levels = c(1,2,3), labels = c("Sedentary", "Partial Migrant", "Migratory"))

log_bi$Habitat <- factor(log_bi$Habitat, levels = c(1,2,3), labels = c("Dense", "Semi-Open", "Open"))

lb1 <- glmer(Logging_threat~HW_Index+Bill_Width+Mass+Migration+Range_size+GenLength+Habitat+Foraging_strata+Centroid_Lat+(1|Family),family = binomial(link = "logit"),data =log_bi)
summary(lb1)

#Calculating the odds ratio
lb1$coefficients
exp(lb1$coefficients)
#Getting the confidence intervals
exp(confint(lb1))

set_theme(base = theme_classic())
l_mod2<-plot_model(lb1, rm.terms = c("1|2","2|3","3|4","4|5"),show.values = TRUE, show.p = TRUE,value.offset = 0.4, show.legend = TRUE) + ggtitle("Logging")+geom_hline(yintercept = 1, linetype = "dashed")

#Modelling sensitivy to climate change
clim_bi <- read.csv("Binary_climate.csv", header =TRUE)

clim_bi$Migration <- factor(clim_bi$Migration, levels = c(1,2,3), labels = c("Sedentary", "Partial Migrant", "Migratory"))

clim_bi$Habitat <- factor(clim_bi$Habitat, levels = c(1,2,3), labels = c("Dense", "Semi-Open", "Open"))

cb1 <- glmer(Climate_threat~HW_Index+Bill_Width+Mass+Migration+Range_size+GenLength+Habitat+Foraging_strata+Centroid_Lat+(1|Family),family = binomial(link = "logit"),data =clim_bi)
summary(cb1)

#Calculating the odds ratio
cb1$coefficients
exp(cb1$coefficients)
#Getting the confidence intervals
exp(confint(cb1))

set_theme(base = theme_classic())
c_mod2<-plot_model(cb1, rm.terms = c("1|2","2|3","3|4","4|5"),show.values = TRUE, show.p = TRUE,value.offset = 0.4, show.legend = TRUE, legend.inside = TRUE) + ggtitle("Climate Change")+geom_hline(yintercept = 1, linetype = "dashed")

#Modelling sensitivy to human disturbance
hum_bi <- read.csv("Binary_human.csv", header =TRUE)

hum_bi$Migration <- factor(hum_bi$Migration, levels = c(1,2,3), labels = c("Sedentary", "Partial Migrant", "Migratory"))

hum_bi$Habitat <- factor(hum_bi$Habitat, levels = c(1,2,3), labels = c("Dense", "Semi-Open", "Open"))

hb1 <- glmer(Human_threat~HW_Index+Bill_Width+Mass+Migration+Range_size+GenLength+Habitat+Foraging_strata+Centroid_Lat+(1|Family),family = binomial(link = "logit"),data =hum_bi)
summary(hb1)

#Calculating the odds ratio
hb1$coefficients
exp(hb1$coefficients)
#Getting the confidence intervals
exp(confint(hb1))

set_theme(base = theme_classic())
h_mod2<-plot_model(hb1, rm.terms = c("1|2","2|3","3|4","4|5"),show.values = TRUE, show.p = TRUE,value.offset = 0.4, show.legend = TRUE,legend.inside = TRUE) + ggtitle("Human Disturbance")+geom_hline(yintercept = 1, linetype = "dashed")

grid.arrange(a_mod2, l_mod2, c_mod2, h_mod2, ncol=2) #Plotting glmm figures on the same plot space

#Severity CLMMs

#Modelling severity of agriculture 
agri_sev<- read.csv("sev_agri.csv", header =TRUE)

agri_sev$Migration <- factor(agri_sev$Migration, levels = c(1,2,3), labels = c("Sedentary", "Partial Migrant", "Migratory"))

agri_sev$Habitat <- factor(agri_sev$Habitat, levels = c(1,2,3), labels = c("Dense", "Semi-Open", "Open"))

agri_sev$Severity <- as.factor(agri_sev$Severity)

as1 <- clmm2(Severity~HW_Index+Bill_Width+Mass+Migration+Range_size+GenLength+Habitat+Foraging_strata+Centroid_Lat, data=agri_sev)
summary(as1)

as2 <- clmm(Severity~HW_Index+Bill_Width+Mass+Migration+Range_size+GenLength+Habitat+Foraging_strata+Centroid_Lat+(1|Family), data=agri_sev)
summary(as2)

as3 <- clm(Severity~HW_Index+Bill_Width+Mass+Migration+Range_size+GenLength+Habitat+Foraging_strata+Centroid_Lat, data=agri_sev)
summary(as2)

as4 <- clm2(Severity~HW_Index+Bill_Width+Mass+Migration+Range_size+GenLength+Habitat+Foraging_strata+Centroid_Lat, data=agri_sev)
summary(as4)

anova(as2,as3) #anova indicates that as2 is a better model with the random effect (p value < 0.001)

anova(as1,as4) #Gives value of 1 so they are exactly the same? Should just use simpler clm model then?

#Calculating the odds ratio
as1$coefficients
exp(as1$coefficients)
#Getting the confidence intervals
exp(confint(as1))

set_theme(base = theme_classic())
a_mod3<-plot_model(as1, rm.terms = c("0|1","1|2","2|3","3|4","4|5"),show.values = TRUE, show.p = TRUE,value.offset = 0.4, show.legend = TRUE, legend.inside = TRUE) + ggtitle("Agriculture Severity")+geom_hline(yintercept = 1, linetype = "dashed")

#Modelling severity of logging
log_sev<- read.csv("sev_log.csv", header =TRUE)

log_sev$Migration <- factor(log_sev$Migration, levels = c(1,2,3), labels = c("Sedentary", "Partial Migrant", "Migratory"))

log_sev$Habitat <- factor(log_sev$Habitat, levels = c(1,2,3), labels = c("Dense", "Semi-Open", "Open"))

log_sev$Severity <- as.factor(log_sev$Severity)

ls1 <- clmm2(Severity~HW_Index+Bill_Width+Mass+Migration+Range_size+GenLength+Habitat+Foraging_strata+Centroid_Lat, data=log_sev)
summary(ls1)

#Calculating the odds ratio
ls1$coefficients
exp(ls1$coefficients)
#Getting the confidence intervals
exp(confint(ls1))

set_theme(base = theme_classic())
l_mod3<-plot_model(ls1, rm.terms = c("0|1","1|2","2|3","3|4","4|5"),show.values = TRUE, show.p = TRUE,value.offset = 0.4, show.legend = TRUE) + ggtitle("Logging Severity")+geom_hline(yintercept = 1, linetype = "dashed")

#Modelling severity of climate change
clim_sev<- read.csv("sev_clim.csv", header =TRUE)

clim_sev$Migration <- factor(clim_sev$Migration, levels = c(1,2,3), labels = c("Sedentary", "Partial Migrant", "Migratory"))

clim_sev$Habitat <- factor(clim_sev$Habitat, levels = c(1,2,3), labels = c("Dense", "Semi-Open", "Open"))

clim_sev$Severity <- as.factor(clim_sev$Severity)

cs1 <- clmm2(Severity~HW_Index+Bill_Width+Mass+Migration+Range_size+GenLength+Habitat+Foraging_strata+Centroid_Lat, data=clim_sev)
summary(cs1)

#Calculating the odds ratio
cs1$coefficients
exp(cs1$coefficients)
#Getting the confidence intervals
exp(confint(cs1))

set_theme(base = theme_classic())
c_mod3<-plot_model(cs1, rm.terms = c("0|1","1|2","2|3","3|4","4|5"),show.values = TRUE, show.p = TRUE,value.offset = 0.4, show.legend = TRUE, legend.inside = TRUE) + ggtitle("Climate Change Severity")+geom_hline(yintercept = 1, linetype = "dashed")

#Modelling severity of human disturbance
hum_sev<- read.csv("sev_human.csv", header =TRUE)

hum_sev$Migration <- factor(hum_sev$Migration, levels = c(1,2,3), labels = c("Sedentary", "Partial Migrant", "Migratory"))

hum_sev$Habitat <- factor(hum_sev$Habitat, levels = c(1,2,3), labels = c("Dense", "Semi-Open", "Open"))

hum_sev$Severity <- as.factor(hum_sev$Severity)

hs1 <- clmm2(Severity~HW_Index+Bill_Width+Mass+Migration+Range_size+GenLength+Habitat+Foraging_strata+Centroid_Lat, data=hum_sev)
summary(hs1)

#Calculating the odds ratio
hs1$coefficients
exp(hs1$coefficients)
#Getting the confidence intervals
exp(confint(hs1))

set_theme(base = theme_classic())
h_mod3<-plot_model(hs1, rm.terms = c("0|1","1|2","2|3","3|4","4|5"),show.values = TRUE, show.p = TRUE,value.offset = 0.4, show.legend = TRUE,legend.inside = TRUE) + ggtitle("Human Disturbance Severity")+geom_hline(yintercept = 1, linetype = "dashed")

grid.arrange(a_mod3, l_mod3, c_mod3, h_mod3, ncol=2) #Plotting severity figures in same plot space




 