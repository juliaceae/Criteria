#This script: 
#1. reads the toxics criteria table originally created by Kara Goodwin and modified by Peter Bryant (see "Criteria.csv" and "CriteriaTableReadMe.txt" in this github repository)
#2. re-formats the table
#3. determines the applicable standard.
#Script originally created by Peter Bryant
#Modified for github by Julia Crown

#install.packages("plyr")
#install.packages("Hmisc")

require(plyr)
library(dplyr)
require(Hmisc)
require(reshape2)
require(stringr)
options('scipen' = 100) #suppress scientific notation
new.folder <- dir.create(paste("\\\\Deqhq1\\PSP\\Rscripts\\Criteria\\",Sys.Date(), sep="")) 
outpath.criteria <- paste("\\\\Deqhq1\\PSP\\Rscripts\\Criteria\\",Sys.Date(), "\\", sep="") 

#The file "Criteria.csv" is found in this github repository
#The file "Criteria.csv" NO LONGER contains the replacements due to DEQ's 4/18/14 update of Table 30 (checked 8/21/14 by JCrown).  
criteria <- read.csv('//deqhq1/PSP/Rscripts/Criteria/Criteria.csv', stringsAsFactors = FALSE)
OHA.HH.criteria <- read.csv('//deqhq1/PSP/Rscripts/Criteria/OHA.HH.DF_PSP Benchmarks List 8 20 2014.csv', stringsAsFactors = FALSE)

#trims white space around the pollutant name
criteria$Pollutant <- str_trim(criteria$Pollutant)
OHA.HH.criteria$active.ingredient <- str_trim(OHA.HH.criteria$active.ingredient)

#create a copy of the criteria table and remove unneeded columns
criteria.values <- within(criteria, rm('Notes','No.', 'Sort', 'min.AL.DEQ.WQS', 'flag.if.WQS.is.greater.than.EPA', 'min.AL.EPA.benchmark','minimum.criteria.benchmark.value','source','alias','CAS.No.',
                                       'Carcinogen','Fish.SV.175.g.d..ppb..','Fish.SV.17.5.g.d..ppb.','Fish.SV.32.g.d..ppb.',
                                       'X', 'X.1', 'X.2', 'X.3', 'X.4', 'X.5', 'X.6'))
OHA.HH.criteria <- within(OHA.HH.criteria, rm('UPDATED.HIERARCHY.RULES.OR.WQPMT.HH.Benchmarks','PREVIOUS.HIERARCHY.RULES.OR.WQPMT.Human.Health.Benchmark..µg.L..1', 'Notes.or.Refences', 'Listed.as.an.EPA.or.Oregon.POI.'))

#remove duplicated pollutant rows
criteria.values <- criteria.values[!duplicated(criteria.values$Pollutant),]
OHA.HH.criteria <- OHA.HH.criteria[!duplicated(OHA.HH.criteria$active.ingredient),]

#rename the fields
criteria.values <- plyr::rename(criteria.values , replace = c('Freshwater.Acute.Criteria..CMC..ug.L' = 'Table 30 Toxic Substances - Freshwater Acute'
                                                        ,
                                                        'Freshwater.Chronic.Criteria..CCC..ug.L' = 'Table 30 Toxic Substances - Freshwater Chronic',
                                                        "Freshwater.Fish.Acute.1" = 'OPP.Aquatic.Life.Benchmarks.Acute.Fish',
                                                        "Freshwater.Fish.Chronic.2" = 'OPP Aquatic Life Benchmarks - Chronic Fish',
                                                        "Freshwater.Invertebrates.Acute.3" = 'OPP Aquatic Life Benchmarks - Acute Invertebrates',
                                                        'Freshwater.Invertebrates.Chronic.4' = 'OPP Aquatic Life Benchmarks - Chronic Invertebrates',
                                                        "Freshwater.Nonvascular.Plants.Acute.5" = 'OPP Aquatic Life Benchmarks - Acute Nonvascular Plants',
                                                        "Freshwater.Vascular.Plants.Acute.6" = 'OPP Aquatic Life Benchmarks - Acute Vascular Plants',
                                                        "Office.of.Water.Aquatic.Life.Criteria.Maximum.Concentration..CMC." = 'Office of Water Aquatic Life Criteria - Maximum Concentration (CMC)',
                                                        "Office.of.Water.Aquatic.Life.Criteria.Continuous.Concentration..CCC." = 'Office of Water Aquatic Life Criteria - Continuous Concentration (CCC)',
                                                        "Marine.Acute.Criteria..CMC..ug.L" = 'Table 30 Toxic Substances - Saltwater Acute',
                                                        "Marine.Chronic.Criteria..CCC..ug.L" = 'Table 30 Toxic Substances - Saltwater Chronic',
                                                        'Human.Health.Criteria...................Water...Organism..ug.L.'= 'Table 40 Human Health Criteria for Toxic Pollutants - Water + Organism',
                                                        'Human.Health.Criteria..Organism.Only..ug.L.' = 'Table 40 Human Health Criteria for Toxic Pollutants - Organism Only',
                                                        "OR.MCLs" = 'OHA Maximum Contaminant Levels',
                                                        "Acute.One.Day.HHBP..ppb." = 'EPA Human Health Benchmarks - Acute', 
                                                        "Chronic..Lifetime.HHBP..ppb." = 'EPA Human Health Benchmarks - Chronic'))
OHA.HH.criteria <- plyr::rename(OHA.HH.criteria , replace = c('active.ingredient'= 'Pollutant',
                                                        'I..Maximum.Contaminant.Level..MCL...µg.L.'= 'OHA.MCL',
                                                        'II..DEQ.HH.Bencharks..Table.40...µg.L.' = 'DEQ.HH.WQS',
                                                        'III..EPA.Human.Health.Benchmark.for.Pesticides..HHBP...µg.L.' = 'EPA.HH',
                                                        'IV..Lifetime.Health.Advisory..LTHA...µg.L.' = 'EPA.LTHA',
                                                        "V.Health.Based.Screening.Level..HBSL..Low..µg.L." = 'USGS.HBSL.Low',
                                                        "VI..Health.Based.Screening.Level..HBSL..High..µg.L." = 'USGS.HBSL.High',
                                                        "VII.Regional.Screening.Levels..RSL...µg.L." = 'EPA.RSL'))

#rename the rownames to the pollutant
#rownames(criteria.values) <- criteria.values[,1]
#rownames(OHA.HH.criteria) <- OHA.HH.criteria[,1]

#The Table 30 numerical updates (adopted by DEQ 4/18/14) (Table 40 did not have any numerical updates) are made here:
criteria.values[criteria.values$Pollutant == "Heptachlor Epoxide", "Table 30 Toxic Substances - Freshwater Acute"] <- "0.52"
criteria.values[criteria.values$Pollutant == "Heptachlor Epoxide", "Table 30 Toxic Substances - Freshwater Chronic"] <- "0.0038"
criteria.values[criteria.values$Pollutant == "Heptachlor Epoxide", "Table 30 Toxic Substances - Saltwater Acute"] <- "0.053"
criteria.values[criteria.values$Pollutant == "Heptachlor Epoxide", "Table 30 Toxic Substances - Saltwater Chronic"] <- "0.0036"

criteria.values[criteria.values$Pollutant == "Endosulfan I", "Table 30 Toxic Substances - Freshwater Acute"] <- "0.22"
criteria.values[criteria.values$Pollutant == "Endosulfan II", "Table 30 Toxic Substances - Freshwater Acute"] <- "0.22"
criteria.values[criteria.values$Pollutant == "Endosulfan I", "Table 30 Toxic Substances - Freshwater Chronic"] <- "0.056"
criteria.values[criteria.values$Pollutant == "Endosulfan II", "Table 30 Toxic Substances - Freshwater Chronic"] <- "0.056"
criteria.values[criteria.values$Pollutant == "Endosulfan I", "Table 30 Toxic Substances - Saltwater Acute"] <- "0.034"
criteria.values[criteria.values$Pollutant == "Endosulfan II", "Table 30 Toxic Substances - Saltwater Acute"] <- "0.034"
criteria.values[criteria.values$Pollutant == "Endosulfan I", "Table 30 Toxic Substances - Saltwater Chronic"] <- "0.0087"
criteria.values[criteria.values$Pollutant == "Endosulfan II", "Table 30 Toxic Substances - Saltwater Chronic"] <- "0.0087"

#The Table 30 DDT also applies to (the sum of) DDE and DDD:
criteria.values[criteria.values$Pollutant == "4,4`-DDD", "Table 30 Toxic Substances - Freshwater Chronic"] <- "0.001"
criteria.values[criteria.values$Pollutant == "4,4`-DDE", "Table 30 Toxic Substances - Freshwater Chronic"] <- "0.001"

#Pentachlorophenol Aquatic Life Water Quality Standard Criteria
#Freshwater are pH dependent. 
#PSP doesn't not usually field sample pH, so use the "default" value of pH 7.8 (EPA OW display values)
criteria.values[criteria.values$Pollutant == "Pentachlorophenol", "Table 30 Toxic Substances - Freshwater Acute"] <- "19.5" #at "default" value of pH 7.8 (EPA OW display values)
criteria.values[criteria.values$Pollutant == "Pentachlorophenol", "Table 30 Toxic Substances - Freshwater Chronic"] <- "15" #at "default" value of pH 7.8 (EPA OW display values)
criteria.values[criteria.values$Pollutant == "Pentachlorophenol", "Table 30 Toxic Substances - Saltwater Acute"] <- "13"
criteria.values[criteria.values$Pollutant == "Pentachlorophenol", "Table 30 Toxic Substances - Saltwater Chronic"] <- "7.9"


#EPA's OPP benchmarks update as of 5/14/13####
#Not using new atrazine/simazine/desethylatrazine/deisopropylatrazine until we discuss at WQPMT on 12/16/14
#min.criteria[min.criteria$criteria.Pollutant == "Atrazine", "min.EPA.criteria"] <- "0.001"
#min.criteria[min.criteria$criteria.Pollutant == "Atrazine", "criteria.minimum.criteria.benchmark.value"] <- "0.001"

#Post WQPMT discussion on 12/16/14.  Not implementing these changes yet, except as "proposed" dashed line on the graphs.
#min.criteria[min.criteria$criteria.Pollutant == "Simazine", "min.EPA.criteria"] <- "2.24"
#min.criteria[min.criteria$criteria.Pollutant == "Simazine", "criteria.minimum.criteria.benchmark.value"] <- "2.24"

#criteria.values[criteria.values$Pollutant == "Desethylatrazine", "Pollutant"] <- "Triazine DEA degradate"
#criteria.values[criteria.values$Pollutant == "Deisopropylatrazine", "Pollutant"] <- "Triazine DIA degradate"
#criteria.values[criteria.values$Pollutant == "DACT Atrazine degradate", "Pollutant"] <- "Triazine DACT degradate"
#criteria.values[criteria.values$Pollutant == "HA Atrazine degradate", "Pollutant"] <- "Triazine HA degradate"

#Acifluorfen Sodium update made after deleting Sodium acifluorfen (EPA 11/24/14-ish -see email chain). 
criteria.values <- criteria.values[criteria.values$Pollutant != "Sodium acifluorfen", ]

#This update being made because the > sign isn't recognized in Excel. The >< signs are dropped here:
criteria.values[,2:18] <- as.data.frame(apply(criteria.values[, 2:18], 2, function(x) gsub("<|>", "", x)),stringsAsFactors=FALSE)
#colwise(function(x) {gsub("<|>","",x)}) (criteria.values)

##############
#THIS IS EXPERIMENTAL: trying to establish the Human Health criteria (for when we start sampling GW)----
#Dave Farrer's fipronil degredates take on parent compound value
#"*Human health benchmark for parent compound used as surrogate in absence of other source of benchmark for environmental degredate or formulation" -- DFarrer PSP Benchmarks List 5 8 2014.xlsx
#min.criteria[min.criteria$Pollutant == "MB46136 Fipronil degradate", "criteria.minimum.criteria.benchmark.value"] <- "0.1"

#This file may not reflect the most current EPA OPP or OW benchmarks, and we do know there were changes to the benchmarks since the last known download date.  
##############

OHA.HH.criteria$Pollutant <- capitalize(OHA.HH.criteria$Pollutant)
#criteria.values[criteria.values$Pollutant == "Chlorpyrifos (Dursban)", "Pollutant"] <- "Chlorpyrifos"

# Commenting out because we only needed to do once intially. 
# #merge OHA.HH.criteria into criteria.values
# criteria.values.merged <- merge(x= criteria.values, y= OHA.HH.criteria, by= "Pollutant", all=TRUE)
# 
# #the list of unmatched pollutants
# OHA.HH.unmatched <- criteria.values.merged[is.na(criteria.values.merged$OPP.Aquatic.Life.Benchmarks.Acute.Fish == TRUE),]
# #write.csv(OHA.HH.unmatched, paste0(outpath.criteria,"OHA.HH.unmatched_savedon", Sys.Date(),".csv")) 

#reconcile the names from OHA Dave Farrer's table to Kara's table names
rename.vector <- c(#"1,3-dichloropropene"="Dichlorobenzene(m) 1,3", #Dave says not a match
                   "2,4-D (Acids & Salts)"="2,4-D acids and salts",
                   "Azinphos-methyl"="Azinphos methyl",
                   "Copper sulfate+"="Copper",
                   "cypermethrIn"="Cypermethrin",
                   "dacthal (DCPA)"="Dacthal (DCPA)",
                   "DBCP"="Dibromochloropropane",
                   "Dicolfol"="Dicofol",
                   "Emamectin benzoate"="Emamectin Benzoate",
                   "Endosulfan sulfate"="Endosulfan Sulfate",
                   "Ethyl parathion"="Ethyl Parathion",
                   "Fenbutatin oxide"="Fenbutatin-oxide",
                   "fipronil degradate MB45950"="MB45950 Fipronil degradate",
                   "fipronil degradate MB46136"="MB46136 Fipronil degradate",
                   "Fipronil**"="Fipronil",
                   "Hexythiozox"="Hexythiazox",
                   "Methamidaphos"="Methamidophos",
                   #"Metsulfuron methyl"="Metsulfuron",#Dave says not a match
                   "Mevinphos (phosdrin)"="Mevinphos",
                   "MSMA"="MSMA-calcium salt",
                   #"Paraquat dichloride"="Paraquat (dication)",#Dave says not a match
                   "PCP"="Pentachlorophenol",
                   "Pendimethlalin"="Pendimethalin",
                   "Quintozene"="PCNB, Pentachloronitrobenzene (Quintozene)",
                   "S-metolachlor"="S-Metolachlor",
                   "Sulfometuron methyl"="Sulfometuron-methyl",
                   "Thiophanate-methyl"="Thiophanate methyl"
                   )
OHA.HH.criteria$Pollutant <- mapvalues(OHA.HH.criteria$Pollutant , from = names(rename.vector), to = rename.vector)
OHA.HH.criteria <- within(OHA.HH.criteria, rm(OHA.MCL, DEQ.HH.WQS, EPA.HH))
#rename table names to meaningful names
OHA.HH.criteria <- plyr::rename(OHA.HH.criteria, c('EPA.LTHA' = 'Office of Water Lifetime Health Advisory', 
                                             'USGS.HBSL.Low' = 'USGS Health Based Screening Level Low', 
                                             'USGS.HBSL.High' = 'USGS Health Based Screening Level High', 
                                             'EPA.RSL' = 'EPA Region 3 Superfund Clean-up Program Regional Screening Level'))

#merge OHA.HH.criteria into criteria.values
criteria.values.merged <- merge(x= criteria.values, y= OHA.HH.criteria, by= "Pollutant", all=TRUE)

# #DOUBLE CHECK
# #the list of unmatched pollutants
# OHA.HH.unmatched <- criteria.values.merged[is.na(criteria.values.merged$OPP.Aquatic.Life.Benchmarks.Acute.Fish == TRUE),]
# #Yes, the list of unmatched names only contains the thirteen parameters that I (and Dave corrected) couldn't find in the Criteria.csv.
# 
# #write the table with the new OHA columns included
# write.csv(criteria.values.merged, paste0(outpath.criteria,"Criteria_savedon", Sys.Date(),".csv")) 

#melt all the benchmarks and criteria
criteria.values.melted <- melt(criteria.values.merged, id.vars = 'Pollutant')
#Add a variable to distinguish saltwater criteria vs. freshwater criteria.
criteria.values.melted$Matrix <- ifelse(criteria.values.melted$variable %in% c('Table 30 Toxic Substances - Saltwater Acute','Table 30 Toxic Substances - Saltwater Chronic'),
                                        'SW','FW')
#Add more criteria used in the Integrated Report(in case need them in the future)
t40oo <- criteria.values.melted[criteria.values.melted$variable == 'Table 40 Human Health Criteria for Toxic Pollutants - Organism Only',]
t40oo$Matrix <- 'SW'
criteria.values.melted <- rbind(criteria.values.melted, t40oo)
criteria.values.melted$ID <- paste(criteria.values.melted$Pollutant, criteria.values.melted$Matrix)
criteria.values.melted[criteria.values.melted$ID == 'Arsenic, Total inorganic SW' & criteria.values.melted$variable == 'Table 40 Human Health Criteria for Toxic Pollutants - Organism Only','value'] <- 1
criteria.values.melted[criteria.values.melted$ID == 'Arsenic, Total recoverable SW' & criteria.values.melted$variable == 'Table 40 Human Health Criteria for Toxic Pollutants - Organism Only','value'] <- 1
criteria.values.melted[criteria.values.melted$ID == 'Manganese, Total recoverable SW' & criteria.values.melted$variable == 'Table 40 Human Health Criteria for Toxic Pollutants - Organism Only','value'] <- 100
hardness.pollutants <- criteria.values.melted[criteria.values.melted$value == 'hardness',]
criteria.values.melted.nonnum <- criteria.values.melted
criteria.values.melted$value <- suppressWarnings(as.numeric(criteria.values.melted$value))
#filter only benchmarks and criteria that have values
criteria.values.melted.applicable <- criteria.values.melted[!is.na(criteria.values.melted$value),]
criteria.values.melted.applicable <- criteria.values.melted.applicable[criteria.values.melted.applicable$value != 0,]

# #Checking if OHA.MCL is equivalent to Kara's MCL's
# oha <- criteria.values.melted.applicable[criteria.values.melted.applicable$variable %in% c('OHA Maximum Contaminant Levels','OHA.MCL'),]
# oha.casted <- dcast(oha)
# oha.casted <- dcast(oha, Pollutant ~ variable, value.var = "value")

#Get minimums. #Get the applicable benchmark or criteria. 
#Option 1 is preferring the minimum of the State WQS over the minimum of the rest of the benchmarks 
#PSP only needs Freshwater benchmarks/criteria at this time (J Crown 7/12/16)
criteria.values.melted.applicable <- criteria.values.melted.applicable[criteria.values.melted.applicable$Matrix == 'FW',]
#determine state criteria vs. federal benchmarks and Aquatic Life vs. Human Health
state.AQL <-  c('Table 30 Toxic Substances - Freshwater Acute',                           
                'Table 30 Toxic Substances - Freshwater Chronic')
benchmarks.AQL <- c('OPP.Aquatic.Life.Benchmarks.Acute.Fish',                                
                    'OPP Aquatic Life Benchmarks - Chronic Fish',                            
                    'OPP Aquatic Life Benchmarks - Acute Invertebrates',                     
                    'OPP Aquatic Life Benchmarks - Chronic Invertebrates',                   
                    'OPP Aquatic Life Benchmarks - Acute Nonvascular Plants',                
                    'OPP Aquatic Life Benchmarks - Acute Vascular Plants',                   
                    'Office of Water Aquatic Life Criteria - Maximum Concentration (CMC)',   
                    'Office of Water Aquatic Life Criteria - Continuous Concentration (CCC)')
state.HH <- c('Table 40 Human Health Criteria for Toxic Pollutants - Water + Organism', 
              'Table 40 Human Health Criteria for Toxic Pollutants - Organism Only', 
              'OHA Maximum Contaminant Levels')
benchmarks.HH <- c('EPA Human Health Benchmarks - Acute',                                   
                   'EPA Human Health Benchmarks - Chronic',                                 
                   'Office of Water Lifetime Health Advisory',                              
                   'USGS Health Based Screening Level Low',                                 
                   'USGS Health Based Screening Level High',                                
                   'EPA Region 3 Superfund Clean-up Program Regional Screening Level')
#subset the data by state vs. federal and aquatic life vs. human health. 
criteria.values.melted.applicable.state.AQL <- criteria.values.melted.applicable[criteria.values.melted.applicable$variable %in% state.AQL,]
criteria.values.melted.applicable.benchmarks.AQL <- criteria.values.melted.applicable[criteria.values.melted.applicable$variable %in% benchmarks.AQL,]
criteria.values.melted.applicable.state.HH <- criteria.values.melted.applicable[criteria.values.melted.applicable$variable %in% state.HH,]
criteria.values.melted.applicable.benchmarks.HH <- criteria.values.melted.applicable[criteria.values.melted.applicable$variable %in% benchmarks.HH,]

#Aquatic Life: find the applicable minimum benchmark(s)/criterion(a)
#take minimum(s) of state AQL criteria
mins.state.AQL <- criteria.values.melted.applicable.state.AQL %>% 
  group_by(Pollutant) %>% 
  slice(which(value %in% sort(value)[1])) %>% 
  as.data.frame
#take minimum(s) of federal AQL benchmarks 
mins.benchmarks.AQL <- criteria.values.melted.applicable.benchmarks.AQL %>% 
  group_by(Pollutant) %>% 
  slice(which(value %in% sort(value)[1])) %>% 
  as.data.frame
#create table (min.AQL) that takes the minimum(s) of the state AQL criteria if it exists, otherwise take the minimum of the federal AQL benchmarks. 
ppp <- unique(c(mins.state.AQL$Pollutant, mins.benchmarks.AQL$Pollutant))
for (i in 1:length(ppp)) {
  if (ppp[i] %in% mins.state.AQL$Pollutant) {
    to_add <- mins.state.AQL[mins.state.AQL$Pollutant %in% ppp[i],]
  } else {
    to_add <- mins.benchmarks.AQL[mins.benchmarks.AQL$Pollutant %in% ppp[i],]
  }
  if (i == 1) {
    min.AQL <- to_add    
  } else {
    min.AQL <- rbind(min.AQL, to_add)
  }
}
#create the final AQL table with one applicable benchmark/criterion per pollutant. 
#concatenate the minimums to preserve the names of the minimum benchmark/criteria values. 
concat.AQL <- min.AQL %>% group_by(Pollutant) %>% summarise(min.AQL.value = unique(value), min.AQL.criteria = paste(variable, collapse = ", "))

#Human Health: find the applicable minimum benchmark(s)/criterion(a)
#take minimum(s) of state HH criteria
mins.state.HH <- criteria.values.melted.applicable.state.HH %>% 
  group_by(Pollutant) %>%
  slice(which(value %in% sort(value)[1])) %>% 
  as.data.frame
#take minimum(s) of federal HH benchmarks
mins.benchmarks.HH  <- criteria.values.melted.applicable.benchmarks.HH %>% 
  group_by(Pollutant)%>% 
  slice(which(value %in% sort(value)[1])) %>% 
  as.data.frame
#create table (min.HH) that takes the minimum(s) of the state HH criteria if it exists, otherwise take the minimum of the federal HH benchmarks. 
ppp <- unique(c(mins.state.HH$Pollutant, mins.benchmarks.HH$Pollutant))
for (i in 1:length(ppp)) {
  if (ppp[i] %in% mins.state.HH$Pollutant) {
    to_add <- mins.state.HH[mins.state.HH$Pollutant %in% ppp[i],]
  } else {
    to_add <- mins.benchmarks.HH[mins.benchmarks.HH$Pollutant %in% ppp[i],]
  }
  if (i == 1) {
    min.HH <- to_add    
  } else {
    min.HH <- rbind(min.HH, to_add)
  }
}
#create the final HH table with one applicable benchmark/criterion value per pollutant. 
#concatenate the minimums to preserve the names of the minimum benchmark/criteria value(s). 
concat.HH <- min.HH %>% group_by(Pollutant) %>% summarise(min.HH.value = unique(value), min.HH.criteria = paste(variable, collapse = ", "))



  ###  megatable <- merge(megatable, concat, by = 'Pollutant', all.x = TRUE)



min.state.AQL <- apply(criteria.values.merged.2[ ,4:5], 1, min, na.rm=TRUE)#get min of state WQS

# ############TEST##########
# DF <- data.frame("V 1"=c(2,8,1),V2=c(7,3,5),V3=c(9,6,4), V4=c(0,0,1))
# colnames(DF)[apply(DF,1,which.max)]
# colnames(DF)[apply(DF,1,which.min)]
# DF <- data.frame(V1=c(2,8,1),V2=c(7,3,5),V3=c(7,6,4))
# #start here
# apply(DF,1,function(x) which(x==min(x)))
# DF$name <- apply(DF,1,function(x) which(x==min(x)))
# 
# criteria.values.merged.2$min.state.AQL.name <- apply(criteria.values.merged.2[ ,4:5],1,function(x) which(x==min(x)))
# 
# 
# DF <- criteria.values.merged.2[ ,4:5]
# DF$min <- apply(criteria.values.merged.2[ ,4:5], 1, min, na.rm=TRUE)#get min of state WQS
# DF$name <- apply(DF,1,function(x) which(x==min(x)))
# View(DF)
# 
# df1<-data.frame(a=sample(1:50,10),b=sample(1:50,10),c=sample(1:50,10))
# colnames(df1)[1]
# colnames(df1)[apply(df1,1,which.min)]
# colnames(df1)[apply(df1,1,which.min)]
# 
# colnames(df1[apply(df1,1,which.max)])
# 
# 
# colnames(df1[1])
# colnames(criteria.values.merged.2[(criteria.values.merged.2[ ,4:5], 1, min, na.rm=TRUE)])
# 
# 
# library(reshape)
# mmm <- melt(criteria.values.merged.2)
# 
# ##########################


min.other.AQL <- apply(criteria.values.merged.2[ ,6:13], 1, min, na.rm=TRUE) #get min of EPA benchmarks
min.AQL.0 <- ifelse(is.infinite(min.state.AQL)==TRUE, min.other.AQL, min.state.AQL)#if there is no WQS, use the EPA, otherwise, use WQS 
min.AQL.1 <- data.frame("Pollutant"=criteria.values$Pollutant, min.state.AQL, min.other.AQL, min.AQL.0, stringsAsFactors=FALSE)#add the Pollutant names back in
#gsub "Inf" for "NA":
#min.AQL.1[,2:4] <- as.data.frame(apply(min.AQL.1[, 2:4], 2, function(x) gsub(Inf, NA, x)),stringsAsFactors=FALSE)
min.AQL.1 <- colwise(function(x) {gsub(Inf,NA,x)}) (min.AQL.1) #change Inf to NA (both lines of code unfortunately make the columns characters)
min.AQL.1[,2:4] <- colwise(as.numeric) (min.AQL.1[,2:4]) #change numeric columns back to numeric
#min.AQL.1$min.AQL.1 <- as.numeric(min.AQL.1$min.AQL.1)
str(min.AQL.1)

min.AQL.1[min.AQL.1$Pollutant == '2,4-D', 2:4] <- min.AQL.1[min.AQL.1$Pollutant == "2,4-D acids and salts", 2:4] #replace the 2,4-D benchmark with the benchmark for 2,4-D acids and salts

# min.state.HH <- apply(criteria.values.merged.2[ ,4:5], 1, min, na.rm=TRUE)
# min.other.HH <- apply(criteria.values.merged.2[ ,6:13], 1, min, na.rm=TRUE)
# min.HH.1 <- ifelse(is.infinite(min.state.HH)==TRUE, min.other.HH, min.state.HH)
# min.HH.1 <- as.data.frame(cbind(criteria.values.merged$Pollutant, min.state.HH, min.other.HH, min.HH.1), stringsAsFactors=FALSE)
# min.HH.1$min.HH.1 <- as.numeric(min.HH.1$min.HH.1)
# str(min.HH.1)

write.csv(min.AQL.1, paste0(outpath.criteria,"min.Aquatic.Life.criteria.values_savedon", Sys.Date(),".csv")) 
# write.csv(min.HH.1, paste0(outpath.criteria,"min.Human.Health.criteria.values_savedon", Sys.Date(),".csv")) 

save(min.AQL.1, file=paste0(outpath.criteria,"min.Aquatic.Life.criteria.values_savedon", Sys.Date(),".Rdata"))

# #make a new table in "long" format----
# criteria.values.melted <- melt(criteria.values, id.vars = 'Pollutant')
# #OHA.HH.criteria.melted <- melt(OHA.HH.criteria, id.vars = 'active.ingredient')
# #add saltwater or freshwater designations
# #criteria.values.melted$Matrix <- ifelse(criteria.values.melted$variable %in% c('Table 30 Toxic Substances - Saltwater Acute','Table 30 Toxic Substances - Saltwater Chronic'),
# #                                      'SW','FW')
# #add more saltwater/freshwater designations
# #t40oo <- criteria.values.melted[criteria.values.melted$variable == 'Table 40 Human Health Criteria for Toxic Pollutants - Organism Only',]
# #t40oo$Matrix <- 'SW'
# #criteria.values.melted <- rbind(criteria.values.melted, t40oo)
# #add column with the pollutant name and SW/FW designation
# #criteria.values.melted$ID <- paste(criteria.values.melted$Pollutant, criteria.values.melted$Matrix)
# #criteria.values.melted[criteria.values.melted$ID == 'Arsenic, Total inorganic SW' & criteria.values.melted$variable == 'Table 40 Human Health Criteria for Toxic Pollutants - Organism Only','value'] <- 1
# #criteria.values.melted[criteria.values.melted$ID == 'Arsenic, Total recoverable SW' & criteria.values.melted$variable == 'Table 40 Human Health Criteria for Toxic Pollutants - Organism Only','value'] <- 1
# #criteria.values.melted[criteria.values.melted$ID == 'Manganese, Total recoverable SW' & criteria.values.melted$variable == 'Table 40 Human Health Criteria for Toxic Pollutants - Organism Only','value'] <- 100
# #hardness.pollutants <- criteria.values.melted[criteria.values.melted$value == 'hardness',] #subset pollutants that use a hardness criteria
# criteria.values.melted$value <- suppressWarnings(as.numeric(criteria.values.melted$value)) #convert from character to numeric (e.g. make blanks to NAs)
# 
# #remove criteria with NA or zero values
# criteria.values.melted.applicable <- criteria.values.melted[!is.na(criteria.values.melted$value),]
# criteria.values.melted.applicable <- criteria.values.melted.applicable[criteria.values.melted.applicable$value != 0,]
# 
# #### TESTING AQUATIC LIFE Minimum criteria value (Option 1) ####
# #Following WQPMT rules to determine min AQL value for PSP program
# #Option 1 is preferring the minimum of the State WQS over the minimum of the rest of the benchmarks
# min.AQL.criteria.values <- ddply(criteria.values.melted, .(Pollutant), function(m) {
#   m <- m[m$value != 0,] #criteria value does not equal zero (about 23 records)
#   if (all(is.na(m[m$variable %in% c('Table 30 Toxic Substances - Freshwater Acute', 
#                                     'Table 30 Toxic Substances - Freshwater Chronic'),
#                   'value']))) {
#     m <- m[m$variable %in% c('OPP Aquatic Life Benchmarks - Acute Fish',
#                         'OPP Aquatic Life Benchmarks - Chronic Fish',
#                         'OPP Aquatic Life Benchmarks - Acute Invertebrates',
#                         'OPP Aquatic Life Benchmarks - Chronic Invertebrates',
#                         'OPP Aquatic Life Benchmarks - Acute Nonvascular Plants',
#                         'OPP Aquatic Life Benchmarks - Acute Vascular Plants',
#                         'Office of Water Aquatic Life Criteria - Maximum Concentration (CMC)',
#                         'Office of Water Aquatic Life Criteria - Continuous Concentration (CCC)'),]
#     i = which(m$value == min(m$value,na.rm = TRUE)) #then index the minimum value row
#   } else {
#     m <- m[m$variable %in% c('Table 30 Toxic Substances - Freshwater Acute', 
#                              'Table 30 Toxic Substances - Freshwater Chronic'),]
#     i = which(m$value == min(m$value,na.rm = TRUE))
#   }
#   return (m[i,])
# })
# 
# 
# #### TESTING HUMAN HEALTH Minimum criteria value (Option 1) ####
# #Following WQPMT rules to determine min HH value for PSP program
# #Option 1 is preferring the minimum of the State WQS or OHA MCLs over the minimum of the rest of the benchmarks
# min.HH.criteria.values <- ddply(criteria.values.melted, .(Pollutant), function(m) {
#   m <- m[m$value != 0,] #criteria value does not equal zero (about 23 records)
#   if (all(is.na(m[m$variable %in% c('Table 40 Human Health Criteria for Toxic Pollutants - Water + Organism', #if these criteria are all NA
#                                     'Table 40 Human Health Criteria for Toxic Pollutants - Organism Only', 
#                                     'OHA Maximum Contaminant Levels'),
#                   'value']))) {
#     m <- m[m$variable %in% c('EPA Human Health Benchmarks - Acute', 
#                              'EPA Human Health Benchmarks - Chronic'),]
#     i = which(m$value == min(m$value,na.rm = TRUE)) #then index the minimum value row
#   } else {
#     m <- m[m$variable %in% c('Table 40 Human Health Criteria for Toxic Pollutants - Water + Organism', #if these criteria are all NA
#                              'Table 40 Human Health Criteria for Toxic Pollutants - Organism Only', 
#                              'OHA Maximum Contaminant Levels'),]
#     i = which(m$value == min(m$value,na.rm = TRUE))
#   }
#   return (m[i,])
# })
# #min.HH.criteria.values <- min.HH.criteria.values[min.HH.criteria.values$Matrix != "SW",]
# 
# 
# write.csv(min.HH.criteria.values, paste0(outpath.criteria,"min.Human.Health.criteria.values_savedon", Sys.Date(),".csv")) 
# write.csv(min.AQL.criteria.values, paste0(outpath.criteria,"min.Aquatic.Life.criteria.values_savedon", Sys.Date(),".csv")) 

