#Capture Data, Sturgeon movements
#Author: Brianna Cahill
#Purpose: Generate abacus and directionality plots for tagged sturgeon in the rockaways burrows areas used by the Army COE for dredging
#Input: detections_ACOE_20231228.csv, data from the acoustic telemetry downloads
#Output: Figures summarizing presence/absence


# establishing relative pathways & packages -------------------------------
library(rstudioapi)
setwd(dirname(rstudioapi::getSourceEditorContext()$path)) #Sets wd to where code is
setwd('../Output') #Sets wd to where output file is
owd <- getwd() #Names output file as object owd
setwd('../Data') #Sets wd to where data is
wd <- getwd() #Names data file as object wd

#Packages
library(ggplot2)
library(plyr)
library(dplyr)
library(tidyr)
library(lubridate)
library(tidyverse)
library(stringr)
library(ggResidpanel)
library(scales)
library(glatos)
library(janitor)
library(gt)
library(sf)
library(ggspatial)

# data prep ---------------------------------------------------------------

#gotta take all vrl files, import them into vue, export and then import into R
detections <- read.csv ("detections_ACOE_20231228.csv", header = TRUE) 
detections <- detections[c("Date.and.Time..UTC.","Receiver","Transmitter", "Station.Name", "Latitude", "Longitude")]

receivers <- read.csv ("BurrowRockaways_ReceiverLocations.csv", header = TRUE) %>% 
  filter(Status == "Active") %>% 
  as.data.frame()

detectionsFiltered <- detections %>% 
  mutate(Transmitter2 = Transmitter) %>%
  separate(Transmitter2, c('a', 'b', "transmitter_id"), "-") %>%
  mutate(transmitter_codespace = paste(a, b, sep = "-")) %>%
  separate(Receiver, c("receiver_type", "receiver_sn"), "-") %>%
  mutate(detection_timestamp_utc = Date.and.Time..UTC.)

detectionsFiltered <- detectionsFiltered %>%
  dplyr::select(c(detection_timestamp_utc, Transmitter, transmitter_codespace, transmitter_id,
                  receiver_type, receiver_sn, Station.Name, Latitude, Longitude))

detectionsFiltered$detection_timestamp_utc <- as.POSIXct(detectionsFiltered$detection_timestamp_utc, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
detectionsFiltered$EST <- with_tz(detectionsFiltered$detection_timestamp_utc, "America/New_York")
detectionsFiltered$receiver_type <- as.factor(detectionsFiltered$receiver_type)
detectionsFiltered$receiver_sn <- as.factor(detectionsFiltered$receiver_sn)
detectionsFiltered$Station.Name <- as.factor(detectionsFiltered$Station.Name)
detectionsFiltered$Transmitter <- as.factor(detectionsFiltered$Transmitter) 
names(detectionsFiltered)[names(detectionsFiltered) == "Transmitter"] <- "transmitterID"

#removing false detections 
detectionsFiltered <- false_detections(det=detectionsFiltered,
                                       tf=3600, #3600 s time threshold
                                       show_plot=TRUE)
detectionsFiltered <- detectionsFiltered %>%
  filter(passed_filter == 1)

#make calendar month section for each detection date
detectionsFiltered$dateEST <- as.Date(detectionsFiltered$EST)
detectionsFiltered$Month <- format(as.Date(detectionsFiltered$dateEST), "%B")
detectionsFiltered$MonthYR <- format(as.Date(detectionsFiltered$dateEST), "%B %Y")
detectionsFiltered$MonthYR <- as.factor(detectionsFiltered$MonthYR)
detectionsFiltered$MonthYR <- factor(detectionsFiltered$MonthYR, levels = c("December 2021", "January 2022", "February 2022", "March 2022", "April 2022", 
                                                                      "May 2022", "June 2022", "July 2022", "August 2022", "September 2022")) #redo levels for monthYR

#importing all known orphan tags
orphanTags <- read.csv ("NonPetersonAcousticTags.xlsx - NonSBU_KnownTags_20240109.csv", header = TRUE)
orphanTags$tag_id <- as.factor(orphanTags$tag_id)
orphanTags$transmitter <- as.factor(orphanTags$transmitter)
names(orphanTags)[names(orphanTags) == "transmitter"] <- "transmitterID"
orphanTags$scientific_name <- as.factor(orphanTags$scientific_name)
orphanTags$common_name <- as.factor(orphanTags$common_name)
orphanTags$Type <- "Animal"
orphanTags$Type <- as.factor(orphanTags$Type)

#importing all Peterson lab OTN metadata
orstedTags <- read.csv ("Sunrise_Orsted_otn_metadata_tagging.xlsx - Tag Metadata.csv", header = TRUE)
orstedTags <- clean_names(orstedTags)
orstedTags <- subset(orstedTags, select = -c(transmitter))
orstedTags$project <- "Orsted"
llabTags <- read.csv ("LLab_otn_metadata_tagging.xlsx - Tag Metadata.csv", header = TRUE)
llabTags <- clean_names(llabTags)
llabTags <- subset(llabTags, select = -c(harvest_date))
llabTags$project <- "Landscape Lab"
liarsTags <- read.csv ("NYSDEC_otn_metadata_tagging.xlsx - Tag Metadata.csv", header = TRUE)
liarsTags <- clean_names(liarsTags)
liarsTags <- subset(liarsTags, select = -c(harvest_date))
liarsTags$project <- "NYS Artificial Reef"
petersonTags <- rbind(orstedTags, llabTags, liarsTags)
petersonTags$Type <- "Animal"
#adjusting the column headers that way they match what is being used for the orphan tags
petersonTagsColChange <- petersonTags %>% 
  mutate(transmitterID = paste(tag_code_space, tag_id_code, sep = "-")) %>%
  mutate(tag_id = animal_id_floy_tag_id_pit_tag_code_etc) %>%
  mutate(common_name = common_name_e) %>%
  mutate(ReleaseDate = utc_release_date_time) %>%
  mutate(tag_expected_life_time_days = est_tag_life) %>%
  mutate(tag_status = "TRUE") %>%
  mutate(measurement = length_m) %>%
  mutate(organization = tag_owner_organization) %>%
  mutate(contact = tag_owner_pi) %>%
  mutate(email = "bradley.peterson@stonybrook.edu") %>%
  mutate(proj_id = project) %>%
  mutate(email_confirmation = "Fall 2023") 

#pulling the common columns and rbinding the rows and those columns
common_cols <- intersect(colnames(orphanTags), colnames(petersonTagsColChange))
knownTags <- rbind(
  orphanTags[, common_cols], 
  petersonTagsColChange[, common_cols])

#create consistent common name naming scheme, might need grepl for species like sandbar sharks with multiple entries
knownTags$common_name[which(knownTags$common_name == "Dusky")] <- "Dusky shark"
knownTags$common_name[which(knownTags$common_name == "Black Sea Bass")] <- "Black sea bass"
knownTags$common_name[which(knownTags$common_name == "Sand Tiger")] <- "Sand tiger shark"
knownTags$common_name[which(knownTags$common_name == "Sandbar")] <- "Sandbar shark"
knownTags$common_name[which(knownTags$common_name == "Smooth Dogfish")] <- "Smooth dogfish"
knownTags$common_name[which(knownTags$common_name == "Smooth Hammerhead")] <- "Smooth hammerhead shark"
knownTags$common_name[which(knownTags$common_name == "Winter Skate")] <- "Winter skate"

#----- adding in sync tag information -----#
syncTags <- read.csv ("Receiever Internal Tag IDs.xlsx - Sheet1.csv", header = TRUE)
syncTags$Project <- as.factor(syncTags$Project)
syncTags$Serial.Number <- as.factor(syncTags$Serial.Number)
syncTags$Transmit.ID <- as.factor(syncTags$Transmit.ID)
names(syncTags)[names(syncTags) == "Transmit.ID"] <- "transmitterID"
syncTags$Type <- "Sync"
syncTags$Type <- as.factor(syncTags$Type)

knownSyncTags <- dplyr::bind_rows(knownTags, syncTags)

#----- merging the two dataframes -----#
mergedDetectionsTags <- merge(detectionsFiltered, unique(knownSyncTags)[, c("tag_id", "transmitterID", "scientific_name", "common_name", "Type", "contact", "email")], by="transmitterID", all.x=TRUE)
#mergedTags$transmitterID2 <- mergedTags$transmitterID
#mergedTags <- mergedTags %>%
  #separate(transmitterID2, c("X", "Y", "tagID"), "-")
mergedDetectionsTags <- subset(mergedDetectionsTags, Type == "Animal" | is.na(Type))
mergedDetectionsTags$Station.Name <- as.factor(mergedDetectionsTags$Station.Name)
#mergedTags$Station.Name[which(mergedTags$receiverID == "485517")] <- "RB1"
#mergedTags$Station.Name[which(mergedTags$receiverID == "488981")] <- "BurE_4"


mergedDetectionsTags$station <- with(mergedDetectionsTags, 
                              ifelse(mergedDetectionsTags$Station.Name == "BurE_1", "EB1", 
                                     ifelse(mergedDetectionsTags$Station.Name == "BurE_3", "EB3", 
                                            ifelse(mergedDetectionsTags$Station.Name == "BurE_4", "EB4", 
                                                   ifelse(mergedDetectionsTags$Station.Name == "BurE_5", "EB5", 
                                                          ifelse(mergedDetectionsTags$Station.Name == "BurW_1", "WB1", 
                                                                 ifelse(mergedDetectionsTags$Station.Name == "BurW_2", "WB2", 
                                                                        ifelse(mergedDetectionsTags$Station.Name == "BurW_3", "WB3", 
                                                                               ifelse(mergedDetectionsTags$Station.Name == "BurW_4", "WB4", 
                                                                                      ifelse(mergedDetectionsTags$Station.Name == "BurW_6", "WB6", 
                                                                                             ifelse(mergedDetectionsTags$Station.Name == "RB1", "RB1", "CHECK")))))))))))
    

head(mergedDetectionsTags)
mergedDetectionsTags <- mergedDetectionsTags %>%
  mutate(tag_id = coalesce(tag_id, transmitterID)) #replace tag_id with transmitterID info if NA


testTest <- mergedDetectionsTags %>%
  distinct %>% 
  sample_n(100) %>%
  mutate(tag_id = coalesce(tag_id, transmitterID))


# summary info ------------------------------------------------------------

summarySturgeon <- mergedDetectionsTags %>%
  filter(common_name == "Atlantic sturgeon") %>%
  group_by(tag_id) %>%
  summarise(count = n()) #individuals = length(unique(tagID)))
summary(summarySturgeon$count)


sturgeonDetections <- mergedDetectionsTags %>%
  filter(common_name == "Atlantic sturgeon")

uniqueTags <- mergedDetectionsTags %>%
  group_by(common_name) %>%
  summarise(N = length(unique(tag_id)))

summaryDetectionsSpecies<- mergedDetectionsTags %>%
  group_by(common_name, transmitterID, tag_id) %>%
  summarise(count = n(),
            nIndiv = n_distinct(tag_id))

table1<- mergedDetectionsTags %>%
  group_by(common_name) %>% #can also sort by scientific_name
  summarise(count = n(),
            nIndiv = n_distinct(tag_id)) %>%
  gt()
table1

table2 <- mergedDetectionsTags %>%
  filter(common_name == "Atlantic sturgeon") %>%
  group_by(Station.Name, tag_id) %>%
  summarise(countDetect = n()) %>% 
  group_by(Station.Name) %>% 
  summarise(indiv = n_distinct(tag_id),
            av = mean(countDetect), 
            SD = sd(countDetect), 
            SE = SD/sqrt(indiv)) %>%
  mutate_at(vars(av, SD, SE), list(~ round(., 1))) %>%
  gt()
table2

#----- general information for sturgeon detections and residency -----#
#this is to get time difference between first detection and last detection included from these VRL files
dateSpan <- mergedDetectionsTags %>%
  summarise(minDate = min(dateEST), 
            maxDate = max(dateEST))
timeDifference <- difftime(strptime(dateSpan$maxDate, format = "%Y-%m-%d"),
         strptime(dateSpan$minDate, format = "%Y-%m-%d"), units="days")
timeDifference <- as.numeric(timeDifference)
timeDifference <- round(timeDifference, digits = 0)

genInfo <- mergedDetectionsTags %>%
  filter(common_name == "Atlantic sturgeon") %>%
  group_by(tag_id, dateEST) %>%
  summarise(countDay = n(), 
            res = ifelse(countDay == 1, 0, 1)) %>%
  ungroup() %>%
  group_by(tag_id) %>%
  summarise(detections = sum(countDay), 
            resDays = sum(res)) %>%
  mutate(propResidency = (resDays/timeDifference)*100)

sturgDetectStats <- genInfo %>%
  summarise(detMean = mean(detections), 
            detSD = sd(detections),
            detSE = detSD/sqrt(length(unique(tag_id))),
            resMin = min(resDays),
            resMean = mean(resDays),
            resMax = max(resDays),
            resSD = sd(resDays), 
            resSE = resSD/sqrt(length(unique(tag_id)))) %>%
  mutate_at(vars(detMean, detSD, detSE, resMin, resMean, resMax, resSD, resSE), list(~ round(., 1))) %>%
  gt()
sturgDetectStats

# map stuff ---------------------------------------------------------------

#----- map: number of sturgeon detections for each receiver and number of sturgeon detected -----#
sturgeonMapDetections <- mergedDetectionsTags %>%
  filter(common_name == "Atlantic sturgeon") %>%
  group_by(Station.Name, Latitude, Longitude) %>%
  summarise(countDetect = n(), 
            countIndiv = n_distinct(tag_id), 
            detectSturgeon = countDetect/countIndiv) %>%
  mutate(Latitude = as.numeric(as.character(Latitude)), 
         Longitude = as.numeric(as.character(Longitude)))
sturgeonMapDetections <- as.data.frame(sturgeonMapDetections)

write.csv(sturgeonMapDetections, paste0(owd, "/", "sturgeonMapDetections.csv"))  
  #gonna have to copy this over into a new excel doc with the "general" formatting option because arcgis pro is a big ole bitch

# figures & shit ----------------------------------------------------------

########################################### FUTURE BRI FIGURE GOALS ###########################################

  #want to work on gamm smoothing plots to look at presence over time (ordinal day), moonphase and decimal hour
  #create network analyses using bipartite graphs (using the igraph package in R) -> look at highways between nodes
  #maybe break COA into time of day to see if this habitat use shifts throughout the day?
    #check to see if 15 minutes is the usual binned timeframe for COA



#----- figure 2: dot plot with total detections for each species, sturgeon is largest -----#
test2 <- mergedDetectionsTags %>%
  group_by(common_name) %>%
  summarise(count = n_distinct(tag_id))

#mergedDetectionsTags$common_name[which(mergedDetectionsTags$common_name == "Unknown fish (teleost/elasmobranch)")] <- "Unknown fish"

test <- mergedDetectionsTags %>%
  dplyr::mutate(dummy = "Unknown") %>%
  #mutate(dupCommonName = if_else(as.character(common_name == "Unknown fish (teleost/elasmobranch)", "Unknown fish", as.character(common_name))))
  #recode_factor(common_name, "Unknown fish (teleost/elasmobranch)" = "Unknown fish") %>%
  #mutate(b = forcats::fct_recode(common_name, "Unknown fish" = "Unknown fish (teleost/elasmobranch)"), 
         #b = as.factor(as.character(b))) %>%
  #mutate(b = as.factor(if_else(common_name == "Unknown fish (teleost/elasmobranch)", "Unknown fish", as.character(common_name)))) %>%
  mutate(commonName2 = coalesce(common_name, dummy)) %>% #
  dplyr::select(-dummy) %>%
  left_join(mergedDetectionsTags %>% 
              group_by(common_name) %>% 
              summarise(N = length(unique(tag_id)))) %>%
  mutate(Label = factor(paste0(commonName2,' (n = ',N,')'))) %>%
  group_by(common_name, commonName2, Label, dateEST) %>%
  summarise(count = n()) %>%
  mutate(common_name = if_else(is.na(as.character(common_name)), 'Unknown', as.character(common_name)))
  #filter(Label != "Unknown (n = 1)")
test<- as.data.frame(test)
#test$common_name[which(is.na(test$common_name))] <- "Unknown"

sturgeonDetections <- ggplot() +
  geom_point(data=(mergedDetectionsTags %>%
                     group_by(dateEST) %>%
                     summarise(count = n())), 
             aes(x = dateEST, y = count), 
             color="grey", size=2, alpha=0.5) +
  geom_point(data = test %>% filter(common_name == "Atlantic sturgeon"), 
             aes(x = dateEST, y = count, color=common_name), 
             color="red", size=3.5) +
  scale_x_date(date_breaks = "2 month", date_minor_breaks = "1 week", date_labels ="%b\n%Y") +
  labs(title= "", x ="", y ="") + 
  facet_wrap(~Label)+
  theme_bw() +
  theme(plot.title = element_text(size = 20, face = "bold", hjust = 0.5,),
        axis.title.x = element_text(size= 16, face = "bold"), 
        axis.text.x=element_text(size=12, color="black", angle = 0, hjust = 0.5),
        axis.title.y = element_text(size= 16, face = "bold"),
        axis.text.y=element_text(size=12, color="black"),
        legend.title = element_text(size = 12, face = "bold"),
        legend.text = element_text(size = 12, face = "bold"),
        legend.position = "none", 
        legend.key.size = unit(2, "cm"),
        strip.text.x = element_text(size = 14, face = "bold",), 
        panel.grid = element_blank())
sturgeonDetections  

otherDetections <- ggplot(data=test %>% filter(common_name != "Atlantic sturgeon"), aes(x=dateEST, y=count)) +
  geom_point(data=(mergedDetectionsTags %>%
                     group_by(dateEST) %>%
                     summarise(count = n())), 
             aes(x = dateEST, y = count), 
             color="grey", size=0.5, alpha=0.5) +
  geom_point( aes(color=common_name), color="red", size=1.2 ) +
  scale_color_viridis(discrete = TRUE) +
  scale_x_date(date_breaks = "4 month", date_minor_breaks = "1 week", date_labels ="%b\n%y") +
  labs(title= "", x ="", y ="") + 
  facet_wrap(~Label) +
  theme_bw() +
  theme(plot.title = element_text(size = 20, face = "bold", hjust = 0.5,),
        axis.title.x = element_text(size= 16, face = "bold"), 
        axis.text.x=element_text(size=12, color="black", angle = 0, hjust = 0.5),
        axis.title.y = element_text(size= 16, face = "bold"),
        axis.text.y=element_text(size=12, color="black"),
        legend.title = element_text(size = 12, face = "bold"),
        legend.text = element_text(size = 12, face = "bold"),
        legend.position = "none", 
        legend.key.size = unit(2, "cm"),
        strip.text.x = element_text(size = 10, face = "bold",), 
        panel.grid = element_blank())
otherDetections

allSpeciesDetections <- ggarrange(sturgeonDetections, otherDetections,
                                  ncol = 2, nrow = 1)
allSpeciesDetections
annotate_figure(allSpeciesDetections, left = text_grob("Total detections", rot = 90, size = 20, face = "bold"),
                bottom = text_grob("Month", size = 20, face = "bold"))
ggsave(paste0(owd,"/","SpeciesDetections_DotPlot.png"), width = 19, height = 10) 

#----- figure 3: dot plot with unique transmitters for each species, sturgeon is largest -----#
specDetectPlot <- mergedDetectionsTags %>%
  dplyr::mutate(dummy = "Unknown") %>%
  mutate(commonName2 = coalesce(common_name, dummy)) %>% #
  dplyr::select(-dummy) %>%
  group_by(common_name, commonName2, dateEST) %>%
  summarise(indiv = length(unique(tag_id))) %>%
  mutate(common_name = if_else(is.na(as.character(common_name)), 'Unknown', as.character(common_name)))
specDetectPlot <- as.data.frame(specDetectPlot)  
specDetectPlot$common_name[which(is.na(specDetectPlot$common_name))] <- "Unknown"

specDetectPlot <- as.data.frame(specDetectPlot)
specDetectPlot$common_name[which(is.na(specDetectPlot$common_name))] <- "Unknown"
specDetectPlot$commonName2[which(is.na(specDetectPlot$commonName2))] <- "Unknown"

sturgeonTransmitters <- ggplot() +
  geom_point(data=(mergedDetectionsTags %>%
                     group_by(dateEST) %>%
                     summarise(indiv = n_distinct(tag_id))), 
             aes(x = dateEST, y = indiv), 
             color="grey", size=2, alpha=0.5) +
  geom_point(data = specDetectPlot %>% filter(common_name == "Atlantic sturgeon"), 
             aes(x = dateEST, y = indiv, color=common_name), 
             color="red", size=3.5) +
  scale_x_date(date_breaks = "2 month", date_minor_breaks = "1 week", date_labels ="%b\n%Y") +
  labs(title= "", x ="", y ="") + 
  facet_wrap(~common_name)+
  theme_bw() +
  theme(plot.title = element_text(size = 20, face = "bold", hjust = 0.5,),
        axis.title.x = element_text(size= 16, face = "bold"), 
        axis.text.x=element_text(size=12, color="black", angle = 0, hjust = 0.5),
        axis.title.y = element_text(size= 16, face = "bold"),
        axis.text.y=element_text(size=12, color="black"),
        legend.title = element_text(size = 12, face = "bold"),
        legend.text = element_text(size = 12, face = "bold"),
        legend.position = "none", 
        legend.key.size = unit(2, "cm"),
        strip.text.x = element_text(size = 14, face = "bold",), 
        panel.grid = element_blank())
sturgeonTransmitters  

otherTransmitters <- ggplot(data=specDetectPlot %>% filter(common_name != "Atlantic sturgeon"), aes(x=dateEST, y=indiv)) +
  geom_point(data=(mergedDetectionsTags %>%
                     group_by(dateEST) %>%
                     summarise(indiv = n_distinct(tag_id))), 
             aes(x = dateEST, y = indiv), 
             color="grey", size=0.5, alpha=0.5) +
  geom_point( aes(color=common_name), color="red", size=1.2 )+
  scale_color_viridis(discrete = TRUE) +
  scale_x_date(date_breaks = "4 month", date_minor_breaks = "1 week", date_labels ="%b\n%y") +
  labs(title= "", x ="", y ="") + 
  facet_wrap(~common_name)+
  theme_bw() +
  theme(plot.title = element_text(size = 20, face = "bold", hjust = 0.5,),
        axis.title.x = element_text(size= 16, face = "bold"), 
        axis.text.x=element_text(size=12, color="black", angle = 0, hjust = 0.5),
        axis.title.y = element_text(size= 16, face = "bold"),
        axis.text.y=element_text(size=12, color="black"),
        legend.title = element_text(size = 12, face = "bold"),
        legend.text = element_text(size = 12, face = "bold"),
        legend.position = "none", 
        legend.key.size = unit(2, "cm"),
        strip.text.x = element_text(size = 12, face = "bold",), 
        panel.grid = element_blank())
otherTransmitters

allUniqueTransmitters <- ggarrange(sturgeonTransmitters, otherTransmitters,
                                   ncol = 2, nrow = 1)
allUniqueTransmitters
annotate_figure(allUniqueTransmitters, left = text_grob("Unique transmitters", rot = 90, size = 20, face = "bold"),
                bottom = text_grob("Month", size = 20, face = "bold"))
ggsave(paste0(owd,"/","SpeciesIndiv_DotPlot.png"), width = 19, height = 10) 

#----- figure 5: abacus plot of atlantic sturgeon presence at the 10 stations -----#
stationAbacus <- mergedDetectionsTags %>%
  filter(common_name == "Atlantic sturgeon") %>%
  group_by(Station.Name) %>%
  mutate(site = ifelse(Station.Name == "BurE_1", "EB1",
                       ifelse(Station.Name == "BurE_3", "EB3",
                              ifelse(Station.Name == "BurE_4", "EB4",
                                     ifelse(Station.Name == "BurE_5", "EB5",
                                            ifelse(Station.Name == "BurW_1", "WB1",
                                                   ifelse(Station.Name == "BurW_2", "WB2",
                                                          ifelse(Station.Name == "BurW_3", "WB3",
                                                                 ifelse(Station.Name == "BurW_4", "WB4",
                                                                        ifelse(Station.Name == "BurW_6", "WB6",
                                                                               ifelse(Station.Name == "RB1", "RB1",NA))))))))))) %>% 
  mutate(site = factor(site, levels = c("RB1", "EB1", "EB3", "EB4", "EB5", "WB1", "WB2", "WB3", "WB4", "WB6"))) %>% 
  ungroup() %>%
  ggplot() + 
  geom_point(aes(x=EST, y=as.factor(site)), size = 4) + #col = horizDirection, , data = subset(mergedTags, mergedTags$common_name == "Atlantic Sturgeon"
  geom_vline(aes(xintercept = as.POSIXct("2022-09-20 00:00:00")), linetype ="solid", color = "black", linewidth = 1) +
  labs(title= "", x ="Date", y ="Borrow station") + #making the label the same for shape and color merged the legend
  scale_colour_manual(values = c("red", "#440154ff")) + #"#fde725ff"
  theme_bw() +
  scale_x_datetime(date_breaks = "2 month", date_labels ="%b\n%Y") +
  scale_y_discrete(limits=rev) +
  #scale_colour_viridis() +
  theme(plot.title = element_text(size = 20, face = "bold", hjust = 0.5,),
        axis.title.x = element_text(size=20, face = "bold"), 
        axis.text.x=element_text(size=18, color="black", hjust = 0.5),
        axis.title.y = element_text(size= 20, face = "bold"),
        axis.text.y=element_text(size=16, color="black"),
        legend.title = element_text(size = 16, face = "bold"),
        legend.text = element_text(size = 14, face = "bold"),
        legend.position = "bottom", 
        legend.key.width=unit(1,"cm"))
stationAbacus
ggsave(paste0(owd,"/","StationAbacus.png"), width = 19, height = 10)  

#----- figure 6: abacus plots of tagged sturgeon -----#
sturgeonAbacus <- mergedDetectionsTags %>%
  mutate(borrowSide = case_when(
    grepl("BurE", Station.Name) ~ "Borrow East",
    grepl("BurW", Station.Name) ~ "Borrow West",
    grepl("RB1", Station.Name) ~ "Borrow West")) %>%
  filter(common_name == "Atlantic sturgeon") %>%
  #sample_n(., 1000) %>%
  #mutate(longitude = as.numeric(as.character(longitude))) %>% 
  group_by(tag_id) %>%
  #mutate(horizDirection = longitude - lag(longitude, default = first(longitude))) %>%
  ungroup() %>%
  ggplot() + 
  geom_point(aes(x=EST, y=as.factor(tag_id), col = borrowSide, shape = borrowSide), size = 4) + #col = horizDirection, , data = subset(mergedTags, mergedTags$common_name == "Atlantic Sturgeon"
  #guides(col = guide_legend(reverse=T)) +
  geom_vline(aes(xintercept = as.POSIXct("2022-09-20 00:00:00")), linetype ="solid", color = "black", linewidth = 1) +
  labs(title= "", x ="Date", y ="Atlantic Sturgeon Transmitter ID", shape = "Detected Location", color = "Detected Location") + #making the label the same for shape and color merged the legend
  scale_colour_manual(values = c("red", "#440154ff")) + #"#fde725ff"
  theme_bw() +
  scale_x_datetime(date_breaks = "2 month", date_labels ="%b\n%Y") +
  scale_shape_manual(values = c(15, 16)) +
  #scale_colour_viridis() +
  theme(plot.title = element_text(size = 20, face = "bold", hjust = 0.5,),
        axis.title.x = element_text(size=20, face = "bold"), 
        axis.text.x=element_text(size=18, color="black", hjust = 0.5),
        axis.title.y = element_text(size= 20, face = "bold"),
        axis.text.y=element_text(size=8, color="black"),
        legend.title = element_text(size = 16, face = "bold"),
        legend.text = element_text(size = 14, face = "bold"),
        legend.position = "bottom", 
        legend.key.width=unit(1,"cm"))
sturgeonAbacus
ggsave(paste0(owd,"/","SturgeonAbacus.png"), width = 19, height = 10)  

#----- gam smooths for sturgeon data -----#
test <- mergedDetectionsTags %>%
  filter(common_name == "Atlantic sturgeon") %>%
  #mutate(ordinalDay = yday(dateEST)) %>% #this wasnt used here but still valuable to hold onto
  mutate(monthDay = format(dateEST, '%m-%d'), 
         fakeYear = "2001", #non leap year and will likely never be included in the actual dataset
         fakeDate = paste(fakeYear, monthDay, sep = "-"), 
         fakeDate = as.POSIXct(fakeDate, "%Y-%m-%d")) %>% 
  group_by(tag_id, fakeDate) %>%
  summarise(count = n()) %>%
  ggplot(aes(x = fakeDate, y = count)) +
  geom_point() +
  geom_smooth(method = "gam", formula = y ~ s(x, bs = "cs")) +
  labs(title= "", x ="Date", y ="Detection count") + #making the label the same for shape and color merged the legend
  theme_bw() +
  scale_x_datetime(date_breaks = "2 month", date_labels ="%b") +
  scale_shape_manual(values = c(15, 16)) +
  #scale_colour_viridis() +
  theme(plot.title = element_text(size = 20, face = "bold", hjust = 0.5,),
        axis.title.x = element_text(size=20, face = "bold"), 
        axis.text.x=element_text(size=18, color="black", hjust = 0.5),
        axis.title.y = element_text(size= 20, face = "bold"),
        axis.text.y=element_text(size=16, color="black"),
        legend.title = element_text(size = 16, face = "bold"),
        legend.text = element_text(size = 14, face = "bold"),
        legend.position = "bottom", 
        legend.key.width=unit(1,"cm"))
test

#----- sturgeon COA calculations -----#
sturgCOA <- mergedDetectionsTags %>% 
  mutate(dti=lubridate::round_date(EST, "15 mins")) %>% 
  group_by(dti, transmitterID) %>% 
  dplyr::summarise(lonCOA=mean(Longitude), latCOA=mean(Latitude)) %>% 
  as.data.frame()
levels(as.factor(sturgCOA$lonCOA))
str(sturgCOA)

sf_sturgCOA <- sf::st_as_sf(sturgCOA, coords = c("lonCOA", "latCOA"), 
                                  crs = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs") %>% #"+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs" "EPSG:4979"
  sf::st_transform(32618)

sf_receivers <- sf::st_as_sf(receivers, coords = c("Longitude", "Latitude"), 
                            crs = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs") %>% #"+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs" "EPSG:4979"
  sf::st_transform(32618)

sturgeonCOA <- ggplot() +
  geom_sf(data = sf_sturgCOA, aes(color = "red"), alpha = 0.4, shape = 16) +
  geom_sf(data = sf_receivers, aes(color = "black"), size = 3.5) +#this should be geom_sf but having CRS issues for some reason
  annotation_scale(location = "bl", width_hint = 0.13) +
  annotation_north_arrow(location = "br", which_north = "true", 
                         pad_x = unit(0.45, "in"), pad_y = unit(0.01, "in"),
                         style = north_arrow_fancy_orienteering) +
  coord_sf(xlim = c( st_bbox(sf_sturgCOA)[["xmin"]]-0.001,  st_bbox(sf_sturgCOA)[["xmax"]]+0.001), 
           ylim = c( st_bbox(sf_sturgCOA)[["ymin"]]-0.001,  st_bbox(sf_sturgCOA)[["ymax"]]+0.001))  +
  theme_minimal() +
  scale_fill_manual(values = c(alpha("orange", 0.5), alpha("yellow", 0.2)), labels = c("50%", "95%")) + #these effectively make the legend
  scale_color_manual(values = c("black", "red"), labels = c("Receivers", "Animal positions")) + #these effectively make the legend
  #scale_y_continuous(labels = scales::number_format(accuracy = 0.01)) +
  theme(panel.background = element_rect(fill = "white",
                                        color = "white"),
        plot.background = element_rect(fill = "white",
                                       color = "white")) +
  labs(title = expression(paste("Calculated centers of activity of sturgeon ", italic("Acipenser"), " spp.")),
       x =NULL, y = NULL, fill = "", color = "") + #, 
  theme(plot.title = element_text(size = 16, face = "bold", hjust = 0.5), 
        axis.title.x = element_text(size= 14), 
        axis.text.x=element_text(size=12, color="black", hjust = 0.5),
        axis.title.y = element_text(size= 14),
        axis.text.y=element_text(size=12, color="black"), 
        legend.title = element_text(size = 12, face = "bold"),
        legend.text = element_text(size = 10, face = "bold")) +
  theme(legend.position= c(0.07, 0.95),
        legend.justification= c("left", "top"),
        legend.spacing.y = unit(-0.1, 'cm'),
        legend.margin=margin(0,0,0,0),
        legend.box.margin=margin(0,0,0,0))
sturgeonCOA
ggsave(paste0(owd,"/","sturgeonCOA.png"))

# testing out network analysis things -------------------------------------

#have to do a fake network analysis becayse the receivers are within 1 km of each other, doesn't work properly
  #plus sp is deprecated so need to find an sf workaround for other projects

sturg_summary <- mergedDetectionsTags %>%
  filter(common_name == "Atlantic sturgeon") %>%   
  arrange(EST) %>%
  group_by(tag_id) %>%
  dplyr::mutate(llon=lag(Longitude), llat=lag(Latitude)) %>%  # lag longitude, lag latitude (previous position)
  filter(Longitude!=lag(Longitude)) %>%  # If you didn't change positions, drop this row.
  filter(!is.na(llon)) %>%  
  group_by(Latitude, Longitude, llon, llat) %>% 
  summarise(count = n()) %>% 
  dplyr::mutate(fakeBins = case_when(
    count < 100 ~ "Low Use",
    count >= 100 & count < 800 ~ "Moderate Use",
    count >= 800 ~ "High Use")) %>% 
  dplyr::mutate(fakeBins = factor(fakeBins, levels = c("Low Use", "Moderate Use", "High Use")))
# Also drop any NA lag longitudes (i.e. the first detection of each)
#mutate(bearing=argosfilter::bearing(llat, Latitude, llon, Longitude)) %>% # use mutate and argosfilter to add bearings!
#mutate(dist=argosfilter::distance(llat, Latitude, llon, Longitude)) # use mutate and argosfilter to add distances!

ggplot() +  # xend and yend define your segments
  geom_segment(data = sturg_summary, aes(x=llon, xend=Longitude, y=llat, yend=Latitude, color = fakeBins, size = fakeBins)) + 
  #geom_curve(data = sturg_summary, aes(x=llon, xend=Longitude, y=llat, yend=Latitude, color = fakeBins, size = fakeBins)) +   
  scale_size_manual(values=c(1, 2, 3)) + # and geom_segment() and geom_curve() will connect them
geom_point(data = receivers, aes(Longitude, Latitude), color = "black", size = 3.5)    
  
# omitted stuff -----------------------------------------------------------

#----- zoomed in version of abacus plot, highlighting specific timeframes -----#
sturgeonAbacusZoom <- mergedTags %>%
  mutate(borrowSide = case_when(
    grepl("BurE", Station.Name) ~ "Borrow East",
    grepl("BurW", Station.Name) ~ "Borrow West",
    grepl("RB1", Station.Name) ~ "Borrow West")) %>%
  filter(common_name == "Atlantic Sturgeon") %>%
  #sample_n(., 1000) %>%
  mutate(longitude = as.numeric(as.character(longitude))) %>% 
  group_by(tagID) %>%
  mutate(horizDirection = longitude - lag(longitude, default = first(longitude))) %>%
  ungroup() %>%
  ggplot() + 
  geom_point(aes(x=EST, y=as.factor(tagID), col = horizDirection, shape = borrowSide), size = 8) + #data = subset(mergedTags, mergedTags$common_name == "Atlantic Sturgeon"
  #guides(col = guide_legend(reverse=T)) +
  labs(title= "", x ="Date", y ="Atlantic Sturgeon Transmitter ID", col = "", shape = "Detected Location") + 
  scale_colour_viridis() +
  theme_bw() +
  scale_x_datetime(date_breaks = "1 week", date_labels ="%b %d\n%Y", 
                   limits = as.POSIXct(c('2022-10-01 00:00:00', '2022-12-13 11:59:59'), format = "%Y-%m-%d %H:%M:%S")) +
  scale_shape_manual(values = c(15, 16)) +
  #annotate("rect",
  #xmin = as.POSIXct('2022-11-15 00:00:00', format = "%Y-%m-%d %H:%M:%S"), 
  #xmax = as.POSIXct('2022-11-21 00:00:00', format = "%Y-%m-%d %H:%M:%S"), 
  #ymin = -Inf, ymax = -10,  fill = "blue", alpha=.3) +
  #scale_colour_gradient2_tableau(
  #palette = "Orange-Blue Diverging") +
  theme(plot.title = element_text(size = 20, face = "bold", hjust = 0.5,),
        axis.title.x = element_text(size=20, face = "bold"), 
        axis.text.x=element_text(size=18, color="black", hjust = 0.5),
        axis.title.y = element_text(size= 20, face = "bold"),
        axis.text.y=element_text(size=18, color="black"),
        legend.title = element_text(size = 16, face = "bold"),
        legend.text = element_text(size = 14, face = "bold"),
        legend.position = "bottom", 
        legend.key.width=unit(1,"cm"))
sturgeonAbacusZoom
ggsave(paste0(owd,"/","SturgeonAbacusZoom.png"), width = 19, height = 10)  

#----- directionality plot of tagged sturgeon -----#
test <- mergedTags %>%
  mutate(burrowSide = case_when(
    grepl("BurE", Station.Name) ~ "Burrow East",
    grepl("BurW", Station.Name) ~ "Burrow West",
    grepl("RB1", Station.Name) ~ "Burrow West")) %>%
  filter(common_name == "Atlantic Sturgeon") %>%
  #sample_n(., 1000) %>%
  mutate(longitude = as.numeric(as.character(longitude))) %>% 
  group_by(tagID) %>%
  mutate(horizDirection = longitude - lag(longitude, default = first(longitude))) %>%
  ungroup() %>%
  ggplot() + 
  geom_point(aes(x=EST, y=as.factor(tagID), col = horizDirection, shape = burrowSide), size = 6) + #data = subset(mergedTags, mergedTags$common_name == "Atlantic Sturgeon"
  #guides(col = guide_legend(reverse=T)) +
  labs(title= "", x ="Date", y ="Atlantic Sturgeon Transmitter ID", col = "", shape = "Detected Location") + 
  scale_colour_viridis() +
  theme_bw() +
  scale_x_datetime(date_breaks = "4 month", date_labels ="%b-%Y") +
  scale_shape_manual(values = c(15, 16)) +
  #scale_colour_gradient2_tableau(
  #palette = "Orange-Blue Diverging") +
  theme(plot.title = element_text(size = 20, face = "bold", hjust = 0.5,),
        axis.title.x = element_text(size=20, face = "bold"), 
        axis.text.x=element_text(size=18, color="black", hjust = 0.5),
        axis.title.y = element_text(size= 20, face = "bold"),
        axis.text.y=element_text(size=18, color="black"),
        legend.title = element_text(size = 16, face = "bold"),
        legend.text = element_text(size = 14, face = "bold"),
        legend.position = "bottom", 
        legend.key.width=unit(1,"cm"))
test

#----- bubble plot showing number of detections at each receiver -----#
test <- detection_bubble_plot(walleye_detections_filtered)

#----- spaghetti chart with detections for each species, number of detections -----#
test <- mergedTags %>%
  dplyr::mutate(dummy = "Unknown") %>%
  mutate(commonName2 = coalesce(common_name, dummy)) %>% #
  dplyr::select(-dummy) %>%
  left_join(mergedTags %>% 
              group_by(common_name) %>% 
              summarise(N = length(unique(tagID)))) %>%
  mutate(Label = factor(paste0(commonName2,' (n = ',N,')'))) %>%
  group_by(common_name, commonName2, Label, dateEST) %>%
  summarise(count = n()) %>%
  filter(Label != "Unknown (n = 1)")
test<- as.data.frame(test)
test$common_name[which(is.na(test$common_name))] <- "Unknown"

ggplot(data=test, aes(x=dateEST, y=count)) +
  geom_point(data = test %>% dplyr::select(-Label), aes(group=common_name), color="grey", size=0.5, alpha=0.5) + 
  #geom_line(aes(group=Label), color="black", size=0.5, alpha=0.5) + 
  geom_point(aes(color=common_name), color="red", size=1.2 ) + #color="#69b3a2"
  #scale_color_viridis(discrete = TRUE) +
  scale_x_date(date_breaks = "4 month", date_minor_breaks = "1 month", date_labels ="%b %Y") +
  labs(title= "", x ="Month", y ="Detection Count") + 
  facet_wrap(~Label) +
  theme_bw() +
  theme(plot.title = element_text(size = 20, face = "bold", hjust = 0.5,),
        axis.title.x = element_text(size= 16, face = "bold"), 
        axis.text.x=element_text(size=12, color="black", angle = 0, hjust = 0.5),
        axis.title.y = element_text(size= 16, face = "bold"),
        axis.text.y=element_text(size=12, color="black"),
        legend.title = element_text(size = 12, face = "bold"),
        legend.text = element_text(size = 12, face = "bold"),
        legend.position = "none", 
        legend.key.size = unit(2, "cm"),
        strip.text.x = element_text(size = 14, face = "bold",), 
        panel.grid = element_blank())
ggsave(paste0(owd,"/","SpeciesSpaghettiPlot.png"), width = 19, height = 10)  

#----- JUST STURGEON spaghetti chart with detections for each species, number of detections -----#
test <- mergedTags %>%
  dplyr::mutate(dummy = "Unknown") %>%
  mutate(commonName2 = coalesce(common_name, dummy)) %>% #
  dplyr::select(-dummy) %>%
  left_join(mergedTags %>% 
              group_by(common_name) %>% 
              summarise(N = length(unique(tagID)))) %>%
  mutate(Label = factor(paste0(commonName2,' (n = ',N,')'))) %>%
  group_by(common_name, commonName2, Label, dateEST) %>%
  summarise(count = n()) %>%
  filter(Label != "Unknown (n = 1)") %>%
  filter(common_name == "Atlantic Sturgeon")
test<- as.data.frame(test)
test$common_name[which(is.na(test$common_name))] <- "Unknown"

ggplot(data=test, aes(x=dateEST, y=count)) +
  #geom_point(data = test %>% dplyr::select(-Label), aes(group=common_name), color="grey", size=4, alpha=0.5) + 
  #geom_line(aes(group=Label), color="black", size=0.5, alpha=0.5) + 
  #geom_point(aes(color=common_name), color="red", size=4 ) + #color="#69b3a2"
  geom_bar(aes(x=dateEST, y=count, color=common_name), color="red") +
  #scale_color_viridis(discrete = TRUE) +
  scale_x_date(date_breaks = "4 month", date_minor_breaks = "1 month", date_labels ="%b %Y") +
  labs(title= "", x ="Month", y ="Detection Count") + 
  facet_wrap(~Label) +
  theme_bw() +
  theme(plot.title = element_text(size = 20, face = "bold", hjust = 0.5,),
        axis.title.x = element_text(size= 16, face = "bold"), 
        axis.text.x=element_text(size=12, color="black", angle = 0, hjust = 0.5),
        axis.title.y = element_text(size= 16, face = "bold"),
        axis.text.y=element_text(size=12, color="black"),
        legend.title = element_text(size = 12, face = "bold"),
        legend.text = element_text(size = 12, face = "bold"),
        legend.position = "none", 
        legend.key.size = unit(2, "cm"),
        strip.text.x = element_text(size = 14, face = "bold",), 
        panel.grid = element_blank())
ggsave(paste0(owd,"/","SpeciesSpaghettiPlot.png"), width = 19, height = 10)  


#----- spaghetti chart with detections for each species, number of individuals -----#
specDetectPlot <- mergedTags %>%
  dplyr::mutate(dummy = "Unknown") %>%
  mutate(commonName2 = coalesce(common_name, dummy)) %>% #
  dplyr::select(-dummy) %>%
  group_by(common_name, commonName2, dateEST) %>%
  summarise(indiv = length(unique(transmitterID)))
specDetectPlot <- as.data.frame(specDetectPlot)  
specDetectPlot$common_name[which(is.na(specDetectPlot$common_name))] <- "Unknown"

specDetectPlot <- as.data.frame(specDetectPlot)
specDetectPlot$common_name[which(is.na(specDetectPlot$common_name))] <- "Unknown"
specDetectPlot$commonName2[which(is.na(specDetectPlot$commonName2))] <- "Unknown"

ggplot(data=specDetectPlot, aes(x=dateEST, y=indiv)) +
  geom_point( data=specDetectPlot %>% dplyr::select(-common_name), aes(group=commonName2), color="grey", size=0.5, alpha=0.5) +
  geom_point( aes(color=common_name), color="red", size=1.2 )+
  scale_color_viridis(discrete = TRUE) +
  #scale_y_continuous(limits = c(0, 7)) +
  scale_x_date(date_breaks = "3 month", date_minor_breaks = "1 week", date_labels ="%b\n%Y") +
  labs(title= "", x ="Month", y ="Unique Individuals") + 
  facet_wrap(~common_name)+
  theme_bw() +
  theme(plot.title = element_text(size = 20, face = "bold", hjust = 0.5,),
        axis.title.x = element_text(size= 16, face = "bold"), 
        axis.text.x=element_text(size=12, color="black", angle = 0, hjust = 0.5),
        axis.title.y = element_text(size= 16, face = "bold"),
        axis.text.y=element_text(size=12, color="black"),
        legend.title = element_text(size = 12, face = "bold"),
        legend.text = element_text(size = 12, face = "bold"),
        legend.position = "none", 
        legend.key.size = unit(2, "cm"),
        strip.text.x = element_text(size = 14, face = "bold",), 
        panel.grid = element_blank())
ggsave(paste0(owd,"/","SpeciesIndiv_SpaghettiPlot.png"), width = 19, height = 10)  

#----- monthly detections by species since RB1 deployment, heat map -----#
summaryAllTags <- mergedTags %>%
  group_by(common_name, MonthYR) %>%
  summarise(count = n(), 
            individuals = length(unique(tagID))) 
summaryAllTags <- as.data.frame(summaryAllTags)
summaryAllTags$common_name[which(is.na(summaryAllTags$common_name))] <- "Unknown"

ggplot(data = summaryAllTags, aes(x= MonthYR, y=reorder(common_name, desc(common_name)), fill= count)) + #subset(summaryAllTags, !is.na(summaryAllTags$common_name)) ##if I want this plot without the NAs 
  geom_tile() +
  theme_bw() +
  scale_fill_viridis(direction = 1) +
  labs(title= "Monthly Detections at RB1", x ="Month", y ="Species", fill = "Detection Counts") + 
  theme(plot.title = element_text(size = 20, face = "bold", hjust = 0.5,),
        axis.title.x = element_text(size= 16, face = "bold"), 
        axis.text.x=element_text(size=12, color="black", angle = 0, hjust = 0.5),
        axis.title.y = element_text(size= 16, face = "bold"),
        axis.text.y=element_text(size=12, color="black"),
        legend.title = element_text(size = 12, face = "bold"),
        legend.text = element_text(size = 12, face = "bold"),
        legend.position = "bottom", 
        legend.key.size = unit(2, "cm"),
        strip.text.x = element_text(size = 14, face = "bold"))
ggsave(paste0(owd,"/","RB1_SpeciesHeatMap.png"), width = 19, height = 10)  

#make list of latitude and longitude for each site
Station.Name <- c("BurE_1", "BurE_3", "BurE_4", "BurE_5", 
                  "BurW_1", "BurW_2", "BurW_3", "BurW_4", "BurW_6", "RB1")
latitude <- c(40.550517, 40.556925, 40.549808, 40.545718, 
              40.550396, 40.545406, 40.541391, 40.540821, 40.546933, 40.546283)
longitude <- c(-73.784746, -73.793109, -73.798329, -73.791463, 
               -73.822206, -73.829873, -73.824162, -73.815183, -73.812142, -73.8186)
stationInfo <- cbind(Station.Name, latitude, longitude)
stationInfo <- as.data.frame(stationInfo)
stationInfo$Station.Name <- as.factor(stationInfo$Station.Name)
stationInfo$latitude <- as.factor(stationInfo$latitude)
stationInfo$longitude <- as.factor(stationInfo$longitude)

sturgeonTags <- merge(sturgeonTags, unique(stationInfo)[, c("Station.Name", "latitude", "longitude")], by="Station.Name", all.x=TRUE)

