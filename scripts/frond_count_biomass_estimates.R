### HAKAI INSTITUTE ###
## Nearshore Kelp Monitoring Program - Macrocystis project ##
#Frond count based biomass estimates for Macrocystis canopy on Central Coast of BC

#Written by Ondine Pontier
#Last update made November 2024

#This script summarizes macrocystis biomass at each site within a region of the Central Coast BC, based on average plant weight across densities within an permanently delineated plot within the site (step 3).
## Plant density is estimated around 3 transect lines (2m swath by 20m length each) at each site (step 2).
## Plant weights estimates are based on the relationship between frond count at 1m and plant weight measured from harvested plants (step 1).

#Set Up
rm(list = ls())
#install.packages("")
lapply(c("tidyr", "plyr", "dplyr", "ggplot2", "magrittr", 
         "lubridate", "knitr", "tidyverse", "reshape2", 
         "ggpubr", "ggpp", "ggpmisc"), library, character.only = T)

### SET UP and FUNCTION ### -----------------------------

#set working directory

# import files using their relative paths within the project folder
data <- read_csv("macro_density.csv") %>%
  filter(fronds_1m >=1)
harvest_raw <- read_csv("macro_harvest.csv")

data$year <- year(data$date)#create column for year (separate from month)
data$month <- lubridate::month(data$date, label = TRUE)#create column for month (separate from year)

str(data)#check for mishaps


### MACROCYSTIS HARVEST (step 1 - cleaning and merging) ### ---------------------

harvest_raw$year <- as.factor(year(harvest_raw$date))
harvest_raw$month <- lubridate::month(harvest_raw$date, label = TRUE)

harvest_raw <- harvest_raw[! harvest_raw$year %in% c('2014'),]
#removes 2014 data becasue plants were harvested from the surface, not entire plants!!! 

# Reformating Harvest Data

# harvest1 - isolates data where fronds were not seperated into surface and subsurface sections (nor between blade vs stipe material)
harvest1 <- subset(harvest_raw, frond_length >=1) %>%
  #removes fronds shorter than 1m 
  dplyr::select(year, month, site, date, tag, frond, frond_length, frond_weight) %>%
  #selects data where only frond weight and frond length parameters were collected
  drop_na(frond_weight)
  #removes fronds that do not have weights (missing data)

# harvest2 - combines data where fronds were separated into 2 sections (sub-surface and surface) 
harvest2 <- harvest_raw %>%
  drop_na(section_weight) %>%
  group_by(year, month, site, date, tag, frond) %>% 
  summarise(frond_length = sum(length_m),
            frond_weight = sum(section_weight)) %>%
  subset(frond_length >=1) 
  #removes fronds shorter than 1m 

harvest <- full_join(harvest1, harvest2)
# join back data together into one dataset

### MACROCYSTIS HARVEST (step 2 - morphometric relationships) ### -------------------------


# Frond length to wet weight 
#' Relation between frond length and frond wet weight for the region across years

frondLW <-lm(frond_weight ~ frond_length + 0, data = harvest)
summary(lm (frond_weight ~ frond_length + 0, data = harvest))

ggplot(harvest, aes(x = frond_length , y = frond_weight)) + 
  geom_point () +
  theme_bw()+
  theme(panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        axis.text.x=element_text(angle=90),
        text=element_text(size=12))+
  coord_cartesian(xlim = c(0, 20), ylim = c(0, 6))+
  stat_poly_line(method = lm, formula = y~x+0)+ 
  stat_poly_eq(formula = y~x+0, aes(label = paste(after_stat(eq.label),after_stat(rr.label), sep = "*\", \"*")))

ggplot(harvest, aes(x = frond_length , y = frond_weight)) + 
  geom_point () +
  theme_bw()+
  theme(panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        axis.text.x=element_text(angle=90),
        text=element_text(size=12))+
  coord_cartesian(xlim = c(0, 20), ylim = c(0, 6))+
  stat_poly_line(method = lm, formula = y~x+0)+ 
  stat_poly_eq(formula = y~x+0, aes(label = paste(after_stat(eq.label),after_stat(rr.label), sep = "*\", \"*")))+
  facet_wrap(.~site)

# Plant length to wet weight 

## Summarizing frond data into plant parameters : a. cumulative plant length (all fronds combined), b. frond count per plant
harvest_plant <- ddply (harvest, c("site", "year", "month", "tag"), summarise, 
                        #Tag = ID for individual plants
                        plant_weight = sum(frond_weight), 
                        #total plant weight, all fronds combined
                        plant_length = sum(frond_length), 
                        #cumulative plant length, all fronds combined
                        frond_count = length(frond_length), 
                        #number of fronds (per plant) 
                        max_length = max(frond_length)) 
                        #longest frond (per plant)

#harvest_plant <- harvest_plant %>% 
  #filter(plant_weight <40) %>%
  #filter(plant_length <150)
#removes 2 outliers

#' Relation between plant cummulative length and plant wet weight for the region across years
ggplot(harvest_plant, aes(x = plant_length , y = plant_weight))+#, col = Site)) + 
  geom_point () +
  stat_poly_line(method = lm, formula = y~x+0)+
  stat_poly_eq(formula = y~x+0, aes(label = paste(after_stat(eq.label),after_stat(rr.label), sep = "*\", \"*")))+
  theme_bw()+
  theme(panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        axis.text.x=element_text(angle=90),
        text=element_text(size=12))

plantWL <-lm(plant_weight ~ plant_length + 0, data = harvest_plant)
summary(plantWL)
#lm regression for above plot, plant weight as a function of plant length (aka cumulative length across all fronds)

#' Relation between plant length and plant wet weight for each site
ggplot(harvest_plant, aes(x = plant_length , y = plant_weight))+#, col = Site)) + 
  geom_point () +
  stat_poly_line(method = lm, formula = y~x+0)+
  stat_poly_eq(formula = y~x+0, aes(label = paste(after_stat(eq.label),after_stat(rr.label), sep = "*\", \"*")))+
  theme_bw()+
  theme(panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        axis.text.x=element_text(angle=90),
        text=element_text(size=12))+
facet_wrap(.~site)

plantWL_site <-lm(plant_weight ~ plant_length * site + 0, data = harvest_plant)
summary(plantWL_site)
#lm regression for above plot, plant weight as a function of plant length (aka cumulative length across all fronds)

lm_plant <- function( site ) { 
  plant <- lm(plant_weight ~ plant_length +0,
            site == site, data = harvest_plant[harvest_plant$site == site, ])
}
# sets up function to create  plant length to weight regression 
sites <- unique(harvest_plant$site)
# creates an object that lists all sites possible 
coeff_plant <- matrix(ncol = 3, nrow = length(sites), dimnames = list(sites, c("coeff", "r_sqr", "site")))
# creates a matrix with 2 collumns the length of sites
for (i in 1:length(sites)) {
  plant <- lm_plant(sites[[i]])
  coeff_plant[[i, 1]] <- plant$coeff
  coeff_plant[[i, 2]] <- summary(plant)$r.squared
  coeff_plant[[i, 3]] <- sites[[i]]
}
# creates loop that fills in matrix with desired parameters from the regression and 
# adds region specifc coefficient and r-squared values to dataframe
site_plant_length_plant_weight_coeff <- data.frame(coeff_plant)%>%
  rename(coeff_PS = coeff, r_sqr_PS = r_sqr) %>%
  mutate(
    coeff_PS = as.numeric(as.character(coeff_PS)),
    r_sqr_PS = as.numeric(as.character(r_sqr_PS)),
    coeff_PR = coef(plantWL),
    r_sqr_PR = summary(plantWL)$r.squared)
str(site_plant_length_plant_weight_coeff)


#' Relation between frond count and plant wet weight for the region across years
ggplot(harvest_plant, aes(x = frond_count , y = plant_weight)) + 
  geom_point () +
  labs(x="Frond count per plant", y="Plant weight (kg)")+
  stat_poly_line(method = lm, formula = y~x+0)+
  stat_poly_eq(formula = y~x+0, aes(label = paste(after_stat(eq.label),after_stat(rr.label), sep = "*\", \"*")))+
  theme_bw()+
  theme(panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        axis.text.x=element_text(angle=90),
        text=element_text(size=12))

frondWC <-lm(plant_weight ~ frond_count + 0, data = harvest_plant)
summary(frondWC)
#lm regression for above plot, plant weight as a function of frond count (at 1m)

#' Relation between frond count and plant wet weight for each site
ggplot(harvest_plant, aes(x = frond_count , y = plant_weight)) + 
  stat_poly_line(method = lm, formula = y~x+0)+
  stat_poly_eq(formula = y~x+0, aes(label = paste(after_stat(eq.label), sep = "*\", \"*")))+ #,after_stat(rr.label)
  geom_point() +
  facet_wrap(.~ site)+
  theme_bw()+
  theme(panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        axis.text.x=element_text(angle=90),
        text=element_text(size=12))

#' creates a data frame with coefficients and r-squared values
lm_frond <- function( site ) { 
  frond <- lm(plant_weight ~ frond_count +0,
            site == site, data = harvest_plant[harvest_plant$site == site, ])
}
# sets up function to create  plant length to weight regression 
sites <- unique(harvest_plant$site)
# creates an object that lists all Sites possible 
coeff_frond <- matrix(ncol = 3, nrow = length(sites), dimnames = list(sites, c("coeff", "r_sqr", "site")))
# creates a matrix with 2 collumns the length of sites
for (i in 1:length(sites)) {
  frond <- lm_frond(sites[[i]])
  coeff_frond[[i, 1]] <- frond$coeff
  coeff_frond[[i, 2]] <- summary(frond)$r.squared
  coeff_frond[[i, 3]] <- sites[[i]]
}
# creates loop that fills in matrix with desired parameters from the regression
site_frond_count_plant_weight_coeff <- data.frame(coeff_frond)%>%
  rename(coeff_FS = coeff, r_sqr_FS = r_sqr) %>%
  mutate(
    coeff_FS = as.numeric(as.character(coeff_FS)),
    r_sqr_FS = as.numeric(as.character(r_sqr_FS)),
    coeff_FR = coef(frondWC),
    r_sqr_FR = summary(frondWC)$r.squared)

str(site_frond_count_plant_weight_coeff)

site_coefficients <- merge(site_frond_count_plant_weight_coeff, site_plant_length_plant_weight_coeff)
#write.csv(site_coefficients, "plant_weight_site_coeff.csv")

#model statistics

### MACROCYSTIS TRANSECTS (step 3 - summarizing at transects and plot level) ### -----------------------------------------

data$fronds_1m <- case_when(
  data$C_P_T == "P"~ data$fronds_1m/2, 
  TRUE ~ data$fronds_1m)
#partial plants are treated as though only half of their fronds fall within the transect

str(site_frond_count_plant_weight_coeff)

canopy <- join(data, site_frond_count_plant_weight_coeff) %>% 
  #joins entire in-situ macro dataset (data) with site specific coefficient (between frond 
#creates new variable for the region specific coefficient
  mutate(TWFR = fronds_1m*coeff_FR) %>%
#calculates Total Weight based on Frond count using Region specific coefficient
  mutate(TWFS = fronds_1m *coeff_FS)
#calculates Total Weight based on Frond count using Site specific coefficient

#write.csv(canopy, "frond_count_macro_clean.csv")

## Summary by Transect 
# transects are 2m wide by their lengths may varies
frond_count_biomass_transect <- canopy %>% 
  dplyr::group_by (site, period, transect, year, month, date) %>% 
  dplyr::summarise (Transect_length = unique(transect_length), 
                    Biomass_m2_FR = sum(TWFR)/(Transect_length*2),
                    #2 here indicated 2m wide or both side of transect
                    Biomass_m2_FS = sum(TWFS)/(Transect_length*2),
                    Biomass_total = sum(TWFR),
                    Frond_total = sum(fronds_1m), 
                    Frond_m2 = Frond_total/(Transect_length*2),
                    Plant_total = length(fronds_1m),
                    Plant_m2 = Plant_total/(Transect_length*2))
#write.csv(frond_count_biomass_transect, "3_derived_data/2024/frond_count_transect_estimates.csv")


#summarize by Site
frond_count_biomass_plot <-frond_count_biomass_transect %>% 
  dplyr::group_by (site, period, year, month, date) %>% 
  dplyr::summarise (mean_biomass_m2 = mean(Biomass_m2_FR,na.rm=TRUE), 
                    stdev_biomass_m2 = sd(Biomass_m2_FR,na.rm=TRUE), 
                    mean_biomass_m2_FS = mean(Biomass_m2_FS,na.rm=TRUE), 
                    stdev_biomass_m2_FS = sd(Biomass_m2_FS,na.rm=TRUE), 
                    mean_frond_density_m2 = mean(Frond_m2,na.rm=TRUE),
                    stdev_frond_density_m2 = sd(Frond_m2),
                    mean_plant_density_m2 = mean(Plant_m2),               
                    stdev_plant_density_m2 = sd(Plant_m2))
#write.csv(frond_count_biomass_plot, "frond_count_plot_estimates.csv")
#summarize by Site

### MACROCYSTIS SUMMARY FIGURES (step 4 - plots) ### -----------------------------------------

## BIOMASS annual
ggplot(frond_count_biomass_plot %>%
  filter(site %in% c("Golden", "Stryker", "Simmonds", "Meay", 
                     "Womanley", "Triquet", "Westbeach")) %>% 
    #selects permanent sites with repetitive sampling 
    filter(month %in% c("Jul", "Aug"))%>% #selects summer sampling interval
    filter(!(site == "Womanley" & period == "Aug/2017"), 
           !(site == "Westbeach" & period == "Aug/2017"),
           !(site == "Triquet" & period == "Aug/2017"), 
           !(site == "Womanley" & period == "Aug/2018"),
           !(site == "Westbeach" & period == "Aug/2018"), 
           !(site == "Triquet" & period == "Aug/2018")))+  
  #removes the Aug survey when July was also surveyed that year
  geom_point(aes(x=date, y=mean_biomass_m2_FR, 
                 group=site, col=site, size = 1.5))+
  geom_line(aes(x=date, y=mean_biomass_m2_FR, 
                group=site, col=site))+
  geom_errorbar(aes(y=mean_biomass_m2_FR,x=date, 
                    ymin=mean_biomass_m2_FR-stdev_biomass_m2_FR, 
                    ymax=mean_biomass_m2_FR+stdev_biomass_m2_FR), 
                size=0.3, width=0)+
  theme_bw()+
  theme(panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        axis.text.x=element_text(angle=90),
        text=element_text(size=12))+
  labs(x="", y="Biomass (kg/m^2)")#+
  #facet_grid(~year, scale = "free")#, space = "free")
  

## FROND DENSITY annual
ggplot(frond_count_biomass_plot %>%
         filter(site %in% c("Golden", "Stryker", "Simmonds", "Meay", 
                            "Womanley", "Triquet", "Westbeach")) %>% 
         #selects permanent sites with repetitive sampling 
         filter(month %in% c("Jul", "Aug"))%>% #selects summer sampling interval
         filter(!(site == "Womanley" & period == "Aug/2017"), 
                !(site == "Westbeach" & period == "Aug/2017"),
                !(site == "Triquet" & period == "Aug/2017"), 
                !(site == "Womanley" & period == "Aug/2018"),
                !(site == "Westbeach" & period == "Aug/2018"), 
                !(site == "Triquet" & period == "Aug/2018")))+  
  #removes the Aug survey when July was also surveyed that year
  geom_point(aes(x=date, y=mean_frond_density_m2, 
                 group=site, col=site, size = 1.5))+
  geom_line(aes(x=date, y=mean_frond_density_m2, 
                group=site, col=site))+
  geom_errorbar(aes(y=mean_frond_density_m2,x=date, 
                    ymin=mean_frond_density_m2-stdev_frond_density_m2, 
                    ymax=mean_frond_density_m2+stdev_frond_density_m2), 
                size=0.3, width=0)+
  theme_bw()+
  theme(panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        axis.text.x=element_text(angle=90),
        text=element_text(size=12))+
  labs(x="", y="Frond Density /m^2)")#+
  facet_grid(~year, scale = "free")#, space = "free")

## PLANT DENSITY annual 
ggplot(frond_count_biomass_plot %>%
         filter(site %in% c("Golden", "Stryker", "Simmonds", "Meay", 
                            "Womanley", "Triquet", "Westbeach")) %>% 
         #selects permanent sites with repetitive sampling 
         filter(month %in% c("Jul", "Aug"))%>% #selects summer sampling interval
         filter(!(site == "Womanley" & period == "Aug/2017"), 
                !(site == "Westbeach" & period == "Aug/2017"),
                !(site == "Triquet" & period == "Aug/2017"), 
                !(site == "Womanley" & period == "Aug/2018"),
                !(site == "Westbeach" & period == "Aug/2018"), 
                !(site == "Triquet" & period == "Aug/2018")))+  
  #removes the Aug survey when July was also surveyed that year
  geom_point(aes(x=date, y=mean_plant_density_m2, 
                 group=site, col=site, size = 1.5))+
  geom_line(aes(x=date, y=mean_plant_density_m2, 
                group=site, col=site))+
  geom_errorbar(aes(y=mean_plant_density_m2,x=date, 
                    ymin=mean_plant_density_m2-stdev_plant_density_m2, 
                    ymax=mean_plant_density_m2+stdev_plant_density_m2), 
                size=0.3, width=0)+
  theme_bw()+
  scale_y_continuous(limits = c(0,.75))+
  theme(panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        axis.text.x=element_text(angle=90),
        text=element_text(size=12))+
  labs(x="", y="Plant Density /m^2)")#+
  facet_grid(~year, scale = "free")#, space = "free")

  #' ### BIOMASS seasonal (2016-2018 only, Westbeach, Meay, Triquet, Womanley)
  ggplot(frond_count_biomass_plot %>%
           filter(site %in% c("Meay", 
                              "Womanley", "Triquet", "Westbeach")) %>%
           #selects permanent sites with repetitive sampling 
           filter(year %in% c("2016", "2017", "2018")))+
    geom_point(aes(x=date, y=mean_biomass_m2_FR, 
                   group=site, col=site, size = 1.5))+
    geom_line(aes(x=date, y=mean_biomass_m2_FR, 
                  group=site, col=site))+
    geom_errorbar(aes(y=mean_biomass_m2_FR,x=date, 
                      ymin=mean_biomass_m2_FR-stdev_biomass_m2_FR, 
                      ymax=mean_biomass_m2_FR+stdev_biomass_m2_FR), 
                  size=0.3, width=0)+
    theme_bw()+
    theme(panel.grid.minor = element_blank(),
          panel.grid.major = element_blank(),
          axis.text.x=element_text(angle=90),
          text=element_text(size=12))+
    scale_x_date(date_labels = "%b",
                 date_breaks = "1 month",
                 )+
    labs(x="", y="Biomass (kg/m^2)")+
    facet_grid(~year, scale = "free")#, space = "free")

