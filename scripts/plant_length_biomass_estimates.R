### HAKAI INSTITUTE ###
## Nearshore Kelp Monitoring Program - Macrocystis project ##
#Plant cummulated length based biomass estimates for Macrocystis canopy on Central Coast of BC

#Written by Kira Krumhansl and Ondine Pontier
#Last Update made Nov 2024

#' ## MACROCYSTIS CANOPY BIOMASS ESTIMATES

#The following script uses slightly modified methods to the Santa Barbara LTER to calculate individual plant weight based on 3 key measurements : number of frond at 1m above the holdfast, number of fronds at the surface and max length for the longest frond. 

#These 3 key measurements are than used in three distinct equations that parse out biomass in 3 categories : biomass on the surface, biomass below surface (that reaches the surface) and biommass in the water collumn that does not break the surface. 

#The script than averages plant size density around each transects (n= 3 per plot) to calculate biomass per meter square in each plot (or site)

#The script also compares 4 different estimates of plant weight (biomass) across methods and scales
## TWPR - the total wet weight for a plant based on its cummulative frond length and region specific length to weight coefficient
## TWPS - the total wet weight for a plant based on its cummulative frond length and site specific length to weight coefficient
## TWFR - the total wet weight for a plant based on its frond count and region specific length to weight coefficient
## TWFS - the total wet weight for a plant based on its frond count and site specific length to weight coefficient


### Set Up ---------------------------
rm(list = ls())
#install.packages("ggpubr")
lapply(c("tidyr", "plyr", "dplyr", "ggplot2", "magrittr", "ggpmisc",
         "lubridate", "knitr", "tidyverse", "reshape2", "ggpubr"), library, character.only = T)


data <- read_csv("1_raw_data/2025/macro_metrics.csv", na = c("", "NA", "na"))
site_coeff <- read_csv("3_derived_data/2025/plant_weight_site_coeff.csv")%>%
  rename(Site= site)

#' ##Functions
impute <-function(a,a.impute){ifelse(is.na(a), a.impute, a)}
# creates if else function that impute the estimted values into a dataframe 
# when NA is present, used to estimate missing measurements for tangled plants





data$Date<-as.Date(data$Date, "%d/%m/%YYYY")
data$year <- year(data$Date)
data$month <- lubridate::month(data$Date, label = TRUE)
#creates collumns for year and month

str(data)
data$Fixed_depth <- as.numeric(data$Fixed_depth, na.rm = TRUE)
data$Depth <- as.numeric(data$Depth, na.rm = TRUE)

data$depth <- coalesce(data$Depth, data$Fixed_depth)
#combines both depth collumn into one

str(data)
#check for mishaps

#'## Subset Core Sites
canopy <- data %>%
  filter(Site %in% c("Golden", "Stryker", "Simmonds", "Meay", 
                     "Womanley", "Triquet", "Westbeach", "Triqster",
                     "Goose_HIRMD", "McMullins_north_HIRMD")) %>%
  filter(Transect %in% c("1","2","3")) %>%
  filter(Fronds_1m >=1)
           

### Structuring ---------------------------------------
#' The following code seperate individuals into different 
#' dataframes based on missing variables that will need to be estimated

canopy <- canopy[!is.na(canopy$Fronds_1m),] 
# removes individuals without Fronds_1m counts 
# (may be tagged individuals listed that 
# weren't retrieved at the next interval)
canopy_Fronds_SFC <- canopy %>% filter(! is.na(Fronds_SFC))
canopy_Fronds_SFC$Fronds_SFC_corr1<-canopy_Fronds_SFC$Fronds_SFC 
# moves measured Fronds_SFC values (for non-tangled, non-partial plants) 
# into the Fronds_SFC_corr1 column to match up with predicted values 
# for tangled and partial plants later
canopy_SFC_long <- canopy %>% filter(! is.na(SFC_Long))


### Missing Surface Frond Counts ----------------------------------
#' This code is used to predict the number of fronds at the 
#' surface for plants where "Fronds_SFC" measurements 
#' were unsuccessful in the field, using the relationship (coefficient)
#' between Fronds_1m and Fronds_SFC at each each site for non-tangled/partial plants.  

fg <-lm(Fronds_SFC~Fronds_1m+0, 
        subset=canopy_Fronds_SFC$Site=="Golden", data=canopy_Fronds_SFC)
# establishes linear regression between y = Fronds_SFC and 
# x = Fronds_1m for Golden
fsim <-lm(Fronds_SFC~Fronds_1m+0, 
          subset=canopy_Fronds_SFC$Site=="Simmonds", data=canopy_Fronds_SFC)
fstr <-lm(Fronds_SFC~Fronds_1m+0, 
          subset=canopy_Fronds_SFC$Site=="Stryker", data=canopy_Fronds_SFC)
fm <-lm(Fronds_SFC~Fronds_1m+0, 
        subset=canopy_Fronds_SFC$Site=="Meay", data=canopy_Fronds_SFC)
ft <-lm(Fronds_SFC~Fronds_1m+0, 
        subset=canopy_Fronds_SFC$Site=="Triquet", data=canopy_Fronds_SFC)
fw <-lm(Fronds_SFC~Fronds_1m+0, 
        subset=canopy_Fronds_SFC$Site=="Westbeach", data=canopy_Fronds_SFC)
fwom <-lm(Fronds_SFC~Fronds_1m+0, 
          subset=canopy_Fronds_SFC$Site=="Womanley", data=canopy_Fronds_SFC)
fq <-lm(Fronds_SFC~Fronds_1m+0, 
          subset=canopy_Fronds_SFC$Site=="Triqster", data=canopy_Fronds_SFC)
fmc <-lm(Fronds_SFC~Fronds_1m+0, 
        subset=canopy_Fronds_SFC$Site=="McMullins_north_HIRMD", data=canopy_Fronds_SFC)
fgoo <-lm(Fronds_SFC~Fronds_1m+0, 
          subset=canopy_Fronds_SFC$Site== "Goose_HIRMD", data=canopy_Fronds_SFC)

fall <- lm(Fronds_SFC~Fronds_1m+0, data=canopy_Fronds_SFC)
ggplot(data=canopy_Fronds_SFC,  
       aes(x = Fronds_1m , y = Fronds_SFC)) + #, col = Site
  geom_point ()+
  #stat_cor(hjust = -.5)+
  #stat_regline_equation(formula = y~x+0)+
  stat_smooth(method = lm, se = FALSE, formula = y~x+0, size = .5, show.legend = TRUE)+
  #labs(title = paste("R2 = ",signif(summary(fall)$adj.r.squared),
               #      "Slope =",signif(fall$coef)))+
  stat_poly_eq(formula = y~x+0, aes(label = paste(after_stat(eq.label), sep = "*\", \"*")))+ #,after_stat(rr.label)
  facet_wrap(.~Site)
  # linear regressions between frond count at 1m and surface frond counts used to predict 
# missing surface frond counts for tangled plants by site


macrogol <- canopy %>% #macroNP
  filter(is.na(Fronds_SFC) & Site == "Golden") #C_P_T == "T" & 
# creates new dataframe for plants with missing surface frond count at Golden
pred.1 <-predict(fg, macrogol)
# creates an object with surface frond count estimates 
# based on linear regression coefficient (established in previous step) 
Fronds_SFC_corr1 <-impute(macrogol$Fronds_SFC, pred.1)
# creates an objects that combines above estimates and measured values when measurements was possible 
# using the "impute" function created at teh start of document
macroGOL <-cbind(macrogol, Fronds_SFC_corr1) 
# creates new dataframe by binding above object with original dataframe for tangled plants at Golden

macrosim <- canopy %>% #macroNP
  filter(is.na(Fronds_SFC) & Site == "Simmonds") #C_P_T == "T" & 
pred.2 <-predict(fsim, macrosim)
Fronds_SFC_corr1 <-impute(macrosim$Fronds_SFC, pred.2)
macroSIM <-cbind(macrosim, Fronds_SFC_corr1)
# replaces NAs with values calculated using the lm for Simonds

macrostr <- canopy %>% #macroNP
  filter(is.na(Fronds_SFC) & Site == "Stryker") #C_P_T == "T" & 
pred.3 <-predict(fstr, macrostr)
Fronds_SFC_corr1 <-impute(macrostr$Fronds_SFC, pred.3)
macroSTR <-cbind(macrostr, Fronds_SFC_corr1)
# replaces NAs with values calculated using the lm for Stryker

macromeay <- canopy %>% #macroNP
  filter(is.na(Fronds_SFC) & Site == "Meay") #C_P_T == "T" & 
pred.4 <-predict(fm, macromeay)
Fronds_SFC_corr1 <-impute(macromeay$Fronds_SFC, pred.4)
macroMEAY <-cbind(macromeay, Fronds_SFC_corr1)
# replaces NAs with values calculated using the lm for Meay

macrotriq <- canopy %>% #macroNP
  filter(is.na(Fronds_SFC) & Site == "Triquet") #C_P_T == "T" & 
pred.5 <-predict(ft, macrotriq)
Fronds_SFC_corr1 <-impute(macrotriq$Fronds_SFC, pred.5)
macroTRIQ <-cbind(macrotriq, Fronds_SFC_corr1)
# replaces NAs with values calculated using the lm for Triquet

macrowest<- canopy %>% #macroNP
  filter(is.na(Fronds_SFC) & Site == "Westbeach") #C_P_T == "T" & 
pred.6 <-predict(fw, macrowest)
Fronds_SFC_corr1 <-impute(macrowest$Fronds_SFC, pred.6)
macroWEST <-cbind(macrowest, Fronds_SFC_corr1)
# replaces NAs with values calculated using the lm for 


macrowom<- canopy %>% #macroNP
  filter(is.na(Fronds_SFC) & Site == "Womanley") #C_P_T == "T" & 
pred.7 <-predict(fw, macrowom)
Fronds_SFC_corr1 <-impute(macrowom$Fronds_SFC, pred.7)
macroWOM <-cbind(macrowom, Fronds_SFC_corr1)
# replaces NAs with values calculated using the lm for 


macrotris <- canopy %>% #macroNP
  filter(is.na(Fronds_SFC) & Site == "Triqster") #C_P_T == "T" & 
pred.8 <-predict(fq, macrotris)
Fronds_SFC_corr1 <-impute(macrotris$Fronds_SFC, pred.8)
macroTRIS <-cbind(macrotris, Fronds_SFC_corr1)
# replaces NAs with values calculated using the lm for Triquet

macrogoo <- canopy %>% 
  filter(is.na(Fronds_SFC) & Site == "Goose_HIRMD")
pred.9 <-predict(fgoo, macrogoo)
Fronds_SFC_corr1 <-impute(macrogoo$Fronds_SFC, pred.9)
macroGOO <-cbind(macrogoo, Fronds_SFC_corr1)

macromc <- canopy %>% 
  filter(is.na(Fronds_SFC) & Site == "McMullins_north_HIRMD")
pred.10 <-predict(fmc, macromc)
Fronds_SFC_corr1 <-impute(macromc$Fronds_SFC, pred.10)
macroMC <-cbind(macromc, Fronds_SFC_corr1)


macro_clean <-rbind(macroTRIQ, macroSIM, macroGOL, macroSTR, 
                    macroMEAY, macroWEST, macroWOM, macroTRIS, 
                    macroMC, macroGOO, canopy_Fronds_SFC)

# AT THIS POINT CHECK FOR NAs !!!! #



### Missing Surface Surface Length -------------------------------------
#' This code is used to calculate the longuest frond length of 
#' plants where "SFC_Long" measurements were unsuccesful 
#' in the field.

fg2 <-lm(SFC_Long~log(Fronds_SFC+1)+0, 
           subset=canopy_SFC_long$Site=="Golden", data=canopy_SFC_long) #macroNPTZ)
# establishes regression between y = SFC_Long and 
# x = Fronds_SFC for Golden
fsim2 <-lm(SFC_Long~log(Fronds_SFC+1)+0, 
          subset=canopy_SFC_long$Site=="Simmonds", data=canopy_SFC_long) #macroNPTZ)
fstr2 <-lm(SFC_Long~log(Fronds_SFC+1)+0, 
          subset=canopy_SFC_long$Site=="Stryker", data=canopy_SFC_long) #macroNPTZ)
fm2 <-lm(SFC_Long~log(Fronds_SFC+1)+0, 
        subset=canopy_SFC_long$Site=="Meay", data=canopy_SFC_long) #macroNPTZ)
ft2<-lm(SFC_Long~log(Fronds_SFC+1)+0,
       subset=canopy_SFC_long$Site=="Triquet", data=canopy_SFC_long) #macroNPTZ)
fw2<-lm(SFC_Long~log(Fronds_SFC+1)+0, 
       subset=canopy_SFC_long$Site=="Westbeach", data=canopy_SFC_long) #macroNPTZ)
fwo2<-lm(SFC_Long~log(Fronds_SFC+1)+0, 
       subset=canopy_SFC_long$Site=="Womanley", data=canopy_SFC_long) #macroNPTZ)
fq2<-lm(SFC_Long~log(Fronds_SFC+1)+0,
        subset=canopy_SFC_long$Site=="Triqster", data=canopy_SFC_long) #macroNPTZ)
fgoo2<-lm(SFC_Long~log(Fronds_SFC+1)+0,
        subset=canopy_SFC_long$Site=="Goose_HIRMD", data=canopy_SFC_long) #macroNPTZ)
fmc2<-lm(SFC_Long~log(Fronds_SFC+1)+0,
        subset=canopy_SFC_long$Site=="McMullins_north_HIRMD", data=canopy_SFC_long) #macroNPTZ)
fall2 <- lm(SFC_Long~log(Fronds_SFC+1)+0, data=canopy_SFC_long) #macroNPTZ)

ggplot(data=canopy_SFC_long, #macroNPTZ), 
       aes(x = log(Fronds_SFC+1) , y = SFC_Long)) + #, col = Site
  geom_point ()+
  #stat_cor(hjust = -.5)+
  #stat_regline_equation(formula = y~x+0)+
  stat_smooth(method = lm, se = FALSE, formula = y~x+0, size = .5, show.legend = TRUE)+
  labs(title = paste("R2 = ",signif(summary(fall2)$adj.r.squared),
                     "Slope =",signif(fall2$coef)))
# regression between frond count at surface and max frond length used to predict 
# missing frond max length data for tangled plants by site

names(macro_clean)[names(macro_clean)=="Fronds_SFC"] <- "Fronds_SFC_measured"
#renames collunm 
names(macro_clean)[names(macro_clean)=="Fronds_SFC_corr1"] <- "Fronds_SFC"
#renames collumn


macrogolL <-macro_clean %>% #macroNP
  filter(is.na(SFC_Long) & Site == "Golden") #C_P_T == "T" & 
# creates new dataframe for tangled plants at Golden using the newly established dataframe
pred.1.2 <-predict(fg2, macrogolL)
# creates an object with maximum surface lengths estimates 
# based on regression coefficient (established in previous step) 
SFC_Long_corr1 <-impute(macrogolL$SFC_Long, pred.1.2)
# creates an objects that combines above estimates and measured values when measurements was possible 
# using the "impute" function created at teh start of documnent
macroGOLL <-cbind(macrogolL, SFC_Long_corr1)
# creates new dataframe by binding above object with original dataframe for tangled plants at Golden

macrosimL<-macro_clean %>% #macroNP
  filter(is.na(SFC_Long)  & Site == "Simmonds") #C_P_T == "T" & 
pred.2.2<-predict(fsim2, macrosimL)
SFC_Long_corr1<-impute(macrosimL$SFC_Long, pred.2.2)
macroSIML<-cbind(macrosimL, SFC_Long_corr1)

macrostrL<-macro_clean %>% #macroNP
  filter(is.na(SFC_Long)  & Site == 'Stryker') #C_P_T == "T" & 
pred.3.2<-predict(fstr2, macrostrL)
SFC_Long_corr1<-impute(macrostrL$SFC_Long, pred.3.2)
macroSTRL<-cbind(macrostrL, SFC_Long_corr1)

macromeayL<-macro_clean %>% #macroNP
  filter(is.na(SFC_Long) & Site == 'Meay') #C_P_T == "T" & 
pred.4.2<-predict(fm2, macromeayL)
SFC_Long_corr1<-impute(macromeayL$SFC_Long, pred.4.2)
macroMEAYL<-cbind(macromeayL, SFC_Long_corr1)

macrotriqL<-macro_clean %>% #macroNP
  filter(is.na(SFC_Long)  & Site == 'Triquet') #C_P_T == "T" & 
pred.5.2<-predict(ft2, macrotriqL)
SFC_Long_corr1<-impute(macrotriqL$SFC_Long, pred.5.2)
macroTRIQL<-cbind(macrotriqL, SFC_Long_corr1)

macrowestL<-macro_clean %>% #macroNP
  filter(is.na(SFC_Long)  & Site == 'Westbeach') #C_P_T == "T" & 
pred.6.2<-predict(fw2, macrowestL)
SFC_Long_corr1<-impute(macrowestL$SFC_Long, pred.6.2)
macroWESTL<-cbind(macrowestL, SFC_Long_corr1)

macrowomL<-macro_clean %>% #macroNP
  filter(is.na(SFC_Long)  & Site == 'Womanley') #C_P_T == "T" & 
pred.7.2<-predict(fwo2, macrowomL)
SFC_Long_corr1<-impute(macrowomL$SFC_Long, pred.7.2)
macroWOML<-cbind(macrowomL, SFC_Long_corr1)

macrotrisL<-macro_clean %>% #macroNP
  filter(is.na(SFC_Long)  & Site == 'Triqster') #C_P_T == "T" & 
pred.8.2<-predict(fq2, macrotrisL)
SFC_Long_corr1<-impute(macrotrisL$SFC_Long, pred.8.2)
macroTRISL<-cbind(macrotrisL, SFC_Long_corr1)

macrogooL<-macro_clean %>% #macroNP
  filter(is.na(SFC_Long)  & Site == 'Goose_HIRMD') #C_P_T == "T" & 
pred.9.2<-predict(fgoo2, macrogooL)
SFC_Long_corr1<-impute(macrogooL$SFC_Long, pred.9.2)
macroGOOL<-cbind(macrogooL, SFC_Long_corr1)

macromcL<-macro_clean %>% #macroNP
  filter(is.na(SFC_Long)  & Site == 'McMullins_north_HIRMD') #C_P_T == "T" & 
pred.10.2<-predict(fmc2, macromcL)
SFC_Long_corr1<-impute(macromcL$SFC_Long, pred.10.2)
macroMCL<-cbind(macromcL, SFC_Long_corr1)

macro_clean_SFC_long <- macro_clean %>% 
  filter(! is.na(SFC_Long)) %>% 
  mutate (SFC_Long_corr1 = SFC_Long)
#macro_clean_nT<-macro_clean[! macro_clean$C_P_T %in% c('T'),]
#macro_clean_nT$SFC_Long_corr1<-macro_clean_nT$SFC_Long
macro_clean_C<-rbind(macroTRIQL, macroSIML, macroGOLL, macroWESTL,
                     macroSTRL, macroMEAYL, macroWOML, macroTRISL, 
                     macroGOOL, macroMCL, macro_clean_SFC_long)
  #macro_clean_nT)
# write.csv(macro_clean_C, file="macro_clean.csv")

##### AT THIS POINT CHECK FOR NAs !!!!
### Individual Total Length ---------------------------------------
#' This code calculates the cumulative length of all fronds for every plant in the data set
#' The plants are separated into 3 sections: material bellow subsurface that doesn't reach surface (SSL), material above surface that reaches the surface (WCL) and material on the surface (CL). Cummulative frond length is estimated differently based on these 3 sections and than summed for a total cummutive length per plant. 


## PARTIAL PLANTS 
macro_clean_C$Fronds_1m <- case_when(
  macro_clean_C$C_P_T == "P"~ macro_clean_C$Fronds_1m/2, 
  TRUE ~ macro_clean_C$Fronds_1m)
macro_clean_C$Fronds_SFC <- case_when(
  macro_clean_C$C_P_T == "P"~ macro_clean_C$Fronds_SFC/2, 
  TRUE ~ macro_clean_C$Fronds_SFC)
#partial plants are treated as though only half of their fronds fall within the transect



## SUBSURFACE PLANTS (plants that do not reach surface)

macro_clean_C_SSP<-filter(macro_clean_C, -Fronds_SFC == 0)
# subsets subsurface plants, none of the fronds reached the surface 

macro_clean_C_SSP$SSL<-(macro_clean_C_SSP$Mean_F*
                          macro_clean_C_SSP$Fronds_1m)

macro_clean_C_SSP$WCL<-0
macro_clean_C_SSP$CL<-0
# uses Mean_F as subsurface lenght for kelps 1m<SFC 

## SURFACE PLANTS (plants with fronds that reach the surface)
macro_clean_C_SP <- filter(macro_clean_C,Fronds_SFC > 0)
# subsets plants that reach the surface 
macro_clean_C_SP$SSL <-((macro_clean_C_SP$Fronds_1m-macro_clean_C_SP$
                           Fronds_SFC)*(1+0.5*(macro_clean_C_SP$depth-1)))

macro_clean_C_SP$WCL <-(macro_clean_C_SP$Fronds_SFC)*
  (macro_clean_C_SP$depth)

macro_clean_C_SP$CL <-(macro_clean_C_SP$Fronds_SFC)*
  (0.75*macro_clean_C_SP$SFC_Long_corr1)
# pulls out canopy plants (>SFC), those that reach the surface and 
# calculates the total length of each component, 
# Equations from Rassweiler et al. 2008

#' ### combines subsurface and surface plants 
macro_clean_Cc<-rbind(macro_clean_C_SSP, 
                      macro_clean_C_SP)
macro_clean_Cc$Total_Length<-(macro_clean_Cc$SSL+macro_clean_Cc$WCL+
                                macro_clean_Cc$CL)
# re-combines <1m plants, 1m<SFC plants, and >SFC plants 
# calculates total length per plant 

### Individual Total Weight ------------------------
#' The following code converts the plant's Total_length (all fronds combined) into a weight based on the relationship between the cumulative frond length to plant weight established from harvested plants
macro_clean_Cc <- merge(macro_clean_Cc, site_coeff) %>% 
  #joins entire in-situ macro dataset (data) with site specific coefficient (between frond 
  #creates new variable for the region specific coefficient
  mutate(TWPR = Total_Length*coeff_PR) %>%
  #calculates Total Weight (TW) based on Plant cummulative length (P) using Region specific coefficient (R)
  mutate(TWPS = Total_Length *coeff_PS) %>%
  #calculates Total Weight (TW) based on Plant cummulative length (P) using Site specific coefficient(S)
  mutate(TWFR = Fronds_1m *coeff_FR) %>%
  #calculates Total Weight (TW) based on Frond count (F) using Region specific coefficient (R)
  mutate(TWFS = Fronds_1m *coeff_FS)
  #calculates Total Weight (TW) based on Frond count (F) using Site specific coefficient (S)


#' 0.2 is the up-to-date region coefficient between the cumulative frond length to plant weight from harvested plants (harvest.csv), using all plant sections, site/sampling events combined 
#' coefficient used to be 0.24727 - Kira K. 2016
#write.csv(macro_clean_Cc, "3_derived_data/2024/plant_length_macro_clean.csv")

# AT THIS POINT CHECK FOR NAs !!!! 

### Summary and Averages ### ---------------------

#' ## Biomass per m2 
#' By transects
lter_transect_biomass <- macro_clean_Cc %>% 
  dplyr::group_by (Site, Interval, Transect, Transect_length, year, month, Date) %>% 
  dplyr::reframe (Biomass_PR = sum(TWPR),
                  Biomass_PS = sum(TWPS),
                  Biomass_FR = sum(TWFR),
                  Biomass_FS = sum(TWFS),
             Biomass_m2_PR = Biomass_PR/(Transect_length*2),
             Biomass_m2_PS = Biomass_PS/(Transect_length*2),
             Biomass_m2_FS = Biomass_FS/(Transect_length*2),
             Biomass_m2_FR = Biomass_FR/(Transect_length*2),
             Frond_density_total = sum(Fronds_1m), 
             Frond_density_m2 = Frond_density_total/(Transect_length*2),
             Surface_Frond_density_total = sum(Fronds_SFC), 
             Surface_Frond_density_m2 = Surface_Frond_density_total/(Transect_length*2),
             Plant_total = length(Fronds_1m),
             Plant_density_m2 = Plant_total/(Transect_length*2),
             Cummulative_length = sum(Total_Length), 
             Cummulative_length_m2 = Cummulative_length/(Transect_length*2))
#write.csv(lter_transect_biomass, "3_derived_data/2024/plant_length_transect_estimates.csv")
#summary table per transect line

#' By plots
plot_based_biomass <-lter_transect_biomass %>% 
  dplyr::group_by (Site, Interval, year, month, Date) %>%
  dplyr::summarise (mean_biomass_m2_PR = mean(Biomass_m2_PR,na.rm=TRUE), 
                    stdev_biomass_m2_PR = sd(Biomass_m2_PR,na.rm=TRUE), 
                    mean_biomass_m2_PS = mean(Biomass_m2_PS,na.rm=TRUE), 
                    stdev_biomass_m2_PS = sd(Biomass_m2_PS,na.rm=TRUE), 
                    mean_biomass_m2_FS = mean(Biomass_m2_FS,na.rm=TRUE), 
                    stdev_biomass_m2_FS = sd(Biomass_m2_FS,na.rm=TRUE), 
                    mean_biomass_m2_FR = mean(Biomass_m2_FR,na.rm=TRUE), 
                    stdev_biomass_m2_FR = sd(Biomass_m2_FR,na.rm=TRUE), 
                    mean_frond_density_m2 = mean(Frond_density_m2,na.rm=TRUE),
                    stdev_frond_density_m2 = sd(Frond_density_m2),
                    mean_plant_density_m2 = mean(Plant_density_m2),               
                    stdev_plant_density_m2 = sd(Plant_density_m2))
write.csv(plot_based_biomass, "3_derived_data/2025/macro_biomass_plot_plant_length.csv")
#summary table for plots/sites


### Temporal trends -----------------
#' ### BIOMASS
ggplot(plot_based_biomass)+ 
  geom_point(aes(x=Date, y=mean_biomass_m2_PR, 
                 group=Site, col=Site, size = 2))+
  geom_line(aes(x=Date, y=mean_biomass_m2_PR, 
                group=Site, col=Site))+
  geom_errorbar(aes(y=mean_biomass_m2_PR,x=Date, 
                    ymin=mean_biomass_m2_PR-stdev_biomass_m2_PR, 
                    ymax=mean_biomass_m2_PR+stdev_biomass_m2_PR), 
                size=0.3, width=0)+
  theme_bw()+
  theme(panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        axis.text.x=element_text(angle=90),
        text=element_text(size=12))+
  labs(x="Date", y="Biomass (kg/m^2)")+
  facet_grid(~year, scale = "free")#, space = "free")

#' ### FROND DENSITY
ggplot(plot_based_biomass)+ 
  geom_point(aes(x=Date, y=mean_frond_density_m2, 
                 group=Site, col=Site, size = 1.5))+
  geom_line(aes(x=Date, y=mean_frond_density_m2, 
                group=Site, col=Site))+
  geom_errorbar(aes(y=mean_frond_density_m2,x=Date, 
                    ymin=mean_frond_density_m2-stdev_frond_density_m2, 
                    ymax=mean_frond_density_m2+stdev_frond_density_m2), 
                size=0.3, width=0)+
  theme_bw()+
  theme(panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        axis.text.x=element_text(angle=90),
        text=element_text(size=12))+
  labs(x="Date", y="Frond Density /m^2)")+
  facet_grid(~year, scale = "free")#, space = "free")

#' ### PLANT DENSITY
ggplot(plot_based_biomass)+ 
  geom_point(aes(x=Date, y=mean_plant_density_m2, 
                 group=Site, col=Site, size = 1.5))+
  geom_line(aes(x=Date, y=mean_plant_density_m2, 
                group=Site, col=Site))+
  geom_errorbar(aes(y=mean_plant_density_m2,x=Date, 
                    ymin=mean_plant_density_m2-stdev_plant_density_m2, 
                    ymax=mean_plant_density_m2+stdev_plant_density_m2), 
                size=0.3, width=0)+
  theme_bw()+
  scale_y_continuous(limits = c(0,.75))+
  theme(panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        axis.text.x=element_text(angle=90),
        text=element_text(size=12))+
  labs(x="Date", y="Plant Density /m^2)")+
  facet_grid(~year, scale = "free")#, space = "free")



  
  
  
  
### METHOD COMPARISON ------------------------

# comparing method estimates in realtion to frond density
ggplot(plot_based_biomass)+
  geom_point(aes(x = mean_frond_density_m2, y = mean_biomass_m2_PS), size = 3, colour = "red") +
  geom_smooth(aes(x = mean_frond_density_m2, y = mean_biomass_m2_PS), method = 'lm', se = FALSE, colour = "red")+
  
  geom_point(aes(x = mean_frond_density_m2, y = mean_biomass_m2_PR), size = 3, colour = "green") +
  geom_smooth(aes(x = mean_frond_density_m2, y = mean_biomass_m2_PR), method = 'lm', se = FALSE, colour = "green")+
  
  geom_point(aes(x = mean_frond_density_m2, y = mean_biomass_m2_FR), size = 3, colour = "blue") +
  geom_smooth(aes(x = mean_frond_density_m2, y = mean_biomass_m2_FR), method = 'lm', se = FALSE, colour = "blue")+
  
  geom_point(aes(x = mean_frond_density_m2, y = mean_biomass_m2_FS), size = 3, colour = "yellow") +
  geom_smooth(aes(x = mean_frond_density_m2, y = mean_biomass_m2_FS), method = 'lm', se = FALSE, colour = "yellow")

#' How frond density and cummulative length vary from one another at various scales 

#Plant cumulative length to frond count relationship for every plant
ggplot(macro_clean_Cc, aes(x = Fronds_1m , y = Total_Length)) + 
  geom_point () +
  stat_poly_line(method = lm, formula = y~x+0)+
  stat_poly_eq(formula = y~x+0, aes(label = paste(after_stat(eq.label),after_stat(rr.label), sep = "*\", \"*")))
ggplot(macro_clean_Cc, aes(x = Fronds_1m , y = Total_Length)) + 
  geom_abline(slope = 7.21, intercept = 0, linetype = "dashed", color = "red")+
  geom_point () +
  stat_poly_line(method = lm, formula = y~x+0)+
  stat_poly_eq(formula = y~x+0, aes(label = paste(after_stat(eq.label),after_stat(rr.label), sep = "*\", \"*")))+
  facet_wrap(.~Site)

#Plant cumulative length to frond count relationship for every transect
ggplot(lter_transect_biomass, aes(x = Frond_density_m2 , y = Cummulative_length_m2)) + 
  geom_point () +
  stat_poly_line(method = lm, formula = y~x+0)+
  stat_poly_eq(formula = y~x+0, aes(label = paste(after_stat(eq.label),after_stat(rr.label), sep = "*\", \"*")))
ggplot(lter_transect_biomass, aes(x = Frond_density_m2 , y = Cummulative_length_m2)) + 
  geom_point () +
  stat_poly_line(method = lm, formula = y~x+0)+
  geom_abline(slope = 6.72, intercept = 0, linetype = "dashed", color = "red")+ 
  stat_poly_eq(formula = y~x+0, aes(label = paste(after_stat(eq.label),after_stat(rr.label), sep = "*\", \"*")))+
  facet_wrap(.~Site)

#Biomass based on plant cumulative length and biomass based on frond count relationship for every plot (regional estimates)
ggplot(plot_based_biomass, aes(x = mean_biomass_m2_FR , y = mean_biomass_m2_PR)) + 
  geom_point () +
  stat_poly_line(method = lm, formula = y~x+0)+
  stat_poly_eq(formula = y~x+0, aes(label = paste(after_stat(eq.label),after_stat(rr.label), sep = "*\", \"*")))
ggplot(plot_based_biomass, aes(x = mean_biomass_m2_FR , y = mean_biomass_m2_PR)) +
  geom_point () +
  stat_poly_line(method = lm, formula = y~x+0)+
  geom_abline(slope = 1.06, intercept = 0, linetype = "dashed", color = "red")+ 
  stat_poly_eq(formula = y~x+0, aes(label = paste(after_stat(eq.label),after_stat(rr.label), sep = "*\", \"*")))+
  facet_wrap(.~Site)

### plant weight estimates comparison ---------------
#plot1
ggplot(macro_clean_Cc, aes(x = TWFS , y = TWPS)) + 
  geom_point () +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red")+  # 1:1 line
  stat_poly_line(method = lm, formula = y~x+0)+
  #stat_poly_eq(formula = y~x+0, aes(label = paste(after_stat(eq.label),after_stat(rr.label), sep = "*\", \"*"))+
  labs(x="Site specific and Count based - 
       Plant Wet Weight (kg)", y="Site specific and Length based -
       Plant Wet Weight (kg)")+
  coord_cartesian(xlim = c(0, 100), ylim = c(0, 100))+
  theme_bw()+
  theme(panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        axis.text.x=element_text(angle=90),
        text=element_text(size=12))
ggplot(macro_clean_Cc, aes(x = TWFS , y = TWPS, colour = Site)) + 
  geom_point ( color = "grey") +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red")+  # 1:1 line
  stat_poly_line(method = lm, formula = y~x+0)+
  #stat_poly_eq(formula = y~x+0, aes(label = paste(after_stat(eq.label),after_stat(rr.label), sep = "*\", \"*"))+
  labs(x="Site specific and Count based - 
       Plant Wet Weight (kg)", y="Site specific and Length based -
       Plant Wet Weight (kg)")+
  scale_color_brewer(palette = "Paired")+
  coord_cartesian(xlim = c(0, 100), ylim = c(0, 100))+
  theme_bw()+
  theme(panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        axis.text.x=element_text(angle=90),
        text=element_text(size=12))

#plot2
ggplot(macro_clean_Cc, aes(x = TWFR , y = TWPR)) + 
  geom_point ( ) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red")+  # 1:1 line
  stat_poly_line(method = lm, formula = y~x+0)+
  #stat_poly_eq(formula = y~x+0, aes(label = paste(after_stat(eq.label),after_stat(rr.label), sep = "*\", \"*")))+
  labs(x="Region specific and Count based - 
       Plant Wet Weight (kg)", y="Region specific and Length based -
       Plant Wet Weight (kg)")+
  coord_cartesian(xlim = c(0, 100), ylim = c(0, 100))+
  theme_bw()+
  theme(panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        axis.text.x=element_text(angle=90),
        text=element_text(size=12))

ggplot(macro_clean_Cc, aes(x = TWFR , y = TWPR, colour = Site)) + 
  geom_point ( color = "grey") +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red")+  # 1:1 line
  stat_poly_line(method = lm, formula = y~x+0)+
  #stat_poly_eq(formula = y~x+0, aes(label = paste(after_stat(eq.label),after_stat(rr.label), sep = "*\", \"*")))+
  labs(x="Region specific and Count based - 
       Plant Wet Weight (kg)", y="Region specific and Length based -
       Plant Wet Weight (kg)")+
  scale_color_brewer(palette = "Paired")+
  coord_cartesian(xlim = c(0, 100), ylim = c(0, 100))+
  theme_bw()+
  theme(panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        axis.text.x=element_text(angle=90),
        text=element_text(size=12))

#plot3
ggplot(macro_clean_Cc, aes(x = TWPR , y = TWPS)) + 
  geom_point () +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red")+  
  stat_poly_line(method = lm, formula = y~x+0)+
  #stat_poly_eq(formula = y~x+0, aes(label = paste(after_stat(eq.label),after_stat(rr.label), sep = "*\", \"*")))+
  labs(x="Region specific and Length based - 
       Plant Wet Weight (kg)", y="Site specific and Length based -
       Plant Wet Weight (kg)")+
  coord_cartesian(xlim = c(0, 100), ylim = c(0, 100))+
  theme_bw()+
  theme(panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        axis.text.x=element_text(angle=90),
        text=element_text(size=12))

ggplot(macro_clean_Cc, aes(x = TWPR , y = TWPS, colour = Site)) + 
  geom_point ( color = "grey") +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red")+  
  stat_poly_line(method = lm, formula = y~x+0)+
  #stat_poly_eq(formula = y~x+0, aes(label = paste(after_stat(eq.label),after_stat(rr.label), sep = "*\", \"*")))+
  labs(x="Region specific and Length based - 
       Plant Wet Weight (kg)", y="Site specific and Length based -
       Plant Wet Weight (kg)")+
  scale_color_brewer(palette = "Paired")+
  coord_cartesian(xlim = c(0, 100), ylim = c(0, 100))+
  theme_bw()+
  theme(panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        axis.text.x=element_text(angle=90),
        text=element_text(size=12))

#plot4
ggplot(macro_clean_Cc, aes(x = TWFR , y = TWFS)) + 
  geom_point () +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red")+  # 1:1 line
  stat_poly_line(method = lm, formula = y~x+0)+
  stat_poly_eq(formula = y~x+0, aes(label = paste(after_stat(eq.label),after_stat(rr.label), sep = "*\", \"*")))+
  labs(x="Region specific and Count based - 
       Plant Wet Weight (kg)", y="Site specific and Count based -
       Plant Wet Weight (kg)")+
  coord_cartesian(xlim = c(0, 100), ylim = c(0, 100))+
  theme_bw()+
  theme(panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        axis.text.x=element_text(angle=90),
        text=element_text(size=12))

ggplot(macro_clean_Cc, aes(x = TWFR , y = TWFS, color = Site)) + 
  geom_point (color = "grey" ) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red")+  # 1:1 line
  stat_poly_line(method = lm, formula = y~x+0)+
  stat_poly_eq(formula = y~x+0, aes(label = paste(after_stat(eq.label),after_stat(rr.label), sep = "*\", \"*")))+
  labs(x="Region specific and Count based - 
       Plant Wet Weight (kg)", y="Site specific and Count based -
       Plant Wet Weight (kg)")+
  scale_color_brewer(palette = "Paired")+
  coord_cartesian(xlim = c(0, 100), ylim = c(0, 100))+
  theme_bw()+
  theme(panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        axis.text.x=element_text(angle=90),
        text=element_text(size=12))

# mean plant weight 
macro_clean_Cc_long <- melt(macro_clean_Cc, id.vars=c("Site", "Interval", "Transect", "Transect_length", "year", "month", "Date", "Fronds_1m", "Fronds_SFC_measured", "SFC_Long")) %>%
  filter(variable %in% c("TWPS", "TWPR", "TWFR", "TWFS")) #%>%
  #mutate(variable = fct_recode(variable, 
  #                             "cummulative_length_region" = "TWPR",
  #                             "cummulative_length_site" = "TWPS", 
  #                             "frond_count_region" = "TWFR",
  #                             "frond_count_site" = "TWFS"))

macro_clean_Cc_long$value <- as.numeric(macro_clean_Cc_long$value)

ggplot(macro_clean_Cc_long, aes(x=variable, y=value, group = variable))+
  geom_boxplot()+
  #geom_point(position = position_jitter(w = 0.1, h = 0), col = "grey")+
  theme_bw()+
  theme(panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        text=element_text(size=12))+
  facet_wrap(.~Site, scale = "free")+
  labs(x="", y="Plant Weight Estimates (kg)")


plot_based_biomass_long <- melt(plot_based_biomass, id.vars=c("Site", "Interval",  "year", "month", "Date")) %>%
  filter(variable %in% c("mean_biomass_m2_PR", "mean_biomass_m2_PS", "mean_biomass_m2_FR", "mean_biomass_m2_FS")) %>%
  mutate(variable = fct_recode(variable, 
                               "LR" = "mean_biomass_m2_PR", 
                               "LS"= "mean_biomass_m2_PS",
                               "CR"="mean_biomass_m2_FR", 
                               "CS"="mean_biomass_m2_FS"))

plot_based_biomass_long$value <- as.numeric(plot_based_biomass_long$value)

ggplot(plot_based_biomass_long, aes(x=variable, y=value, group = variable))+
  geom_boxplot()+
  #geom_point(position = position_jitter(w = 0.1, h = 0), col = "grey")+
  theme_bw()+
  theme(panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        text=element_text(size=12))+
  facet_grid(.~Site, scale = "free")+
  labs(x="", y="Plot Biomass Estimates (kg/m2)")








