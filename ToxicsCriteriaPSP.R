#This script: 
#1. reads the toxics criteria table originally created by Kara Goodwin and modified by Peter Bryant (see "Criteria.csv" and "CriteriaTableReadMe.txt" in this github repository)
#2. re-formats the table
#3. determines the applicable standard.
#Script originally created by Peter Bryant
#Modified for github by Julia Crown

require(plyr)
require(Hmisc)
require(reshape2)
require(stringr)
options('scipen' = 100) #suppress scientific notation
new.folder <- dir.create(paste("\\\\Deqhq1\\PSP\\Rscripts\\Criteria\\",Sys.Date(), sep="")) 
outpath <- paste("\\\\Deqhq1\\PSP\\Rscripts\\Criteria\\",Sys.Date(), "\\", sep="") 

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
criteria.values <- rename(criteria.values , replace = c('Freshwater.Acute.Criteria..CMC..ug.L' = 'Table 30 Toxic Substances - Freshwater Acute',
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
OHA.HH.criteria <- rename(OHA.HH.criteria , replace = c('active.ingredient'= 'Pollutant',
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


#THIS IS A PLACEHOLDER...
#Dave Farrer's fipronil degredates take on parent compound value
#"*Human health benchmark for parent compound used as surrogate in absence of other source of benchmark for environmental degredate or formulation" -- DFarrer PSP Benchmarks List 5 8 2014.xlsx
#min.criteria[min.criteria$Pollutant == "MB46136 Fipronil degradate", "criteria.minimum.criteria.benchmark.value"] <- "0.1"

#This file may not reflect the most current EPA OPP or OW benchmarks, and we do know there were changes to the benchmarks since the last known download date.  

OHA.HH.criteria$Pollutant <- capitalize(OHA.HH.criteria$Pollutant)
#criteria.values[criteria.values$Pollutant == "Chlorpyrifos (Dursban)", "Pollutant"] <- "Chlorpyrifos"

#merge OHA.HH.criteria into criteria.values
criteria.values.merged <- merge(x= criteria.values, y= OHA.HH.criteria, by= "Pollutant", all=TRUE)

#the list of unmatched pollutants
OHA.HH.unmatched <- criteria.values.merged[is.na(criteria.values.merged$OPP.Aquatic.Life.Benchmarks.Acute.Fish == TRUE),]
write.csv(OHA.HH.unmatched, paste0(outpath,"OHA.HH.unmatched_savedon", Sys.Date(),".csv")) 

#reconcile the names from OHA Dave Farrer's table to Kara's table names
rename.vector <- c("1,3-dichloropropene"="Dichlorobenzene(m) 1,3",
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
                   "Metsulfuron methyl"="Metsulfuron",
                   "Mevinphos (phosdrin)"="Mevinphos",
                   "MSMA"="MSMA-calcium salt",
                   "Paraquat dichloride"="Paraquat (dication)",
                   "PCP"="Pentachlorophenol",
                   "Pendimethlalin"="Pendimethalin",
                   "Quintozene"="PCNB, Pentachloronitrobenzene (Quintozene)",
                   "S-metolachlor"="S-Metolachlor",
                   "Sulfometuron methyl"="Sulfometuron-methyl",
                   "Thiophanate-methyl"="Thiophanate methyl"
                   )

OHA.HH.criteria$Pollutant <- mapvalues(OHA.HH.criteria$Pollutant , from = names(rename.vector), to = rename.vector)

#merge OHA.HH.criteria into criteria.values
criteria.values.merged <- merge(x= criteria.values, y= OHA.HH.criteria, by= "Pollutant", all=TRUE)

#DOUBLE CHECK
#the list of unmatched pollutants
OHA.HH.unmatched <- criteria.values.merged[is.na(criteria.values.merged$OPP.Aquatic.Life.Benchmarks.Acute.Fish == TRUE),]
#Yes, the list of unmatched names only contains the ten parameters that I couldn't find in the Criteria.csv.

#write the table with the new OHA columns included
write.csv(criteria.values.merged, paste0(outpath,"Criteria_savedon", Sys.Date(),".csv")) 

#TESTING AQL criteria
#Option 1 is preferring the minimum of the State WQS over the minimum of the rest of the benchmarks

criteria.values.merged2 <- colwise(as.numeric) (criteria.values.merged[,2:25])
criteria.values.merged2 <- cbind(criteria.values.merged$Pollutant, criteria.values.merged2)



min.state.AQL <- apply(criteria.values.merged2[ ,4:5], 1, min)
min.other.AQL <- apply(criteria.values.merged2[ ,c(6:11, 12:13)], 1, min)
min.AQL.1 <- ifelse(is.na(min.state.AQL==TRUE), min.other.AQL, min.state.AQL)
min.AQL.1 <- as.data.frame(cbind(criteria.values.merged$Pollutant, min.state.AQL, min.other.AQL, min.AQL.1))

min.state.HH <- apply(criteria.values.merged2[ ,c(2:3, 19)], 1, min)
min.other.HH <- apply(criteria.values.merged2[ ,c(17:18, 22:25)], 1, min)
min.HH.1 <- ifelse(is.na(min.state.HH==TRUE), min.other.HH, min.state.HH)
min.HH.1 <- as.data.frame(cbind(criteria.values.merged$Pollutant, min.state.HH, min.other.HH, min.HH.1))







#make a new table in "long" format
criteria.values.melted <- melt(criteria.values, id.vars = 'Pollutant')
#OHA.HH.criteria.melted <- melt(OHA.HH.criteria, id.vars = 'active.ingredient')
#add saltwater or freshwater designations
#criteria.values.melted$Matrix <- ifelse(criteria.values.melted$variable %in% c('Table 30 Toxic Substances - Saltwater Acute','Table 30 Toxic Substances - Saltwater Chronic'),
#                                      'SW','FW')
#add more saltwater/freshwater designations
#t40oo <- criteria.values.melted[criteria.values.melted$variable == 'Table 40 Human Health Criteria for Toxic Pollutants - Organism Only',]
#t40oo$Matrix <- 'SW'
#criteria.values.melted <- rbind(criteria.values.melted, t40oo)
#add column with the pollutant name and SW/FW designation
#criteria.values.melted$ID <- paste(criteria.values.melted$Pollutant, criteria.values.melted$Matrix)
#criteria.values.melted[criteria.values.melted$ID == 'Arsenic, Total inorganic SW' & criteria.values.melted$variable == 'Table 40 Human Health Criteria for Toxic Pollutants - Organism Only','value'] <- 1
#criteria.values.melted[criteria.values.melted$ID == 'Arsenic, Total recoverable SW' & criteria.values.melted$variable == 'Table 40 Human Health Criteria for Toxic Pollutants - Organism Only','value'] <- 1
#criteria.values.melted[criteria.values.melted$ID == 'Manganese, Total recoverable SW' & criteria.values.melted$variable == 'Table 40 Human Health Criteria for Toxic Pollutants - Organism Only','value'] <- 100
#hardness.pollutants <- criteria.values.melted[criteria.values.melted$value == 'hardness',] #subset pollutants that use a hardness criteria
criteria.values.melted$value <- suppressWarnings(as.numeric(criteria.values.melted$value)) #convert from character to numeric (e.g. make blanks to NAs)

#remove criteria with NA or zero values
criteria.values.melted.applicable <- criteria.values.melted[!is.na(criteria.values.melted$value),]
criteria.values.melted.applicable <- criteria.values.melted.applicable[criteria.values.melted.applicable$value != 0,]

#### TESTING AQUATIC LIFE Minimum criteria value (Option 1) ####
#Following WQPMT rules to determine min AQL value for PSP program
#Option 1 is preferring the minimum of the State WQS over the minimum of the rest of the benchmarks
min.AQL.criteria.values <- ddply(criteria.values.melted, .(Pollutant), function(m) {
  m <- m[m$value != 0,] #criteria value does not equal zero (about 23 records)
  if (all(is.na(m[m$variable %in% c('Table 30 Toxic Substances - Freshwater Acute', 
                                    'Table 30 Toxic Substances - Freshwater Chronic'),
                  'value']))) {
    m <- m[m$variable %in% c('OPP Aquatic Life Benchmarks - Acute Fish',
                        'OPP Aquatic Life Benchmarks - Chronic Fish',
                        'OPP Aquatic Life Benchmarks - Acute Invertebrates',
                        'OPP Aquatic Life Benchmarks - Chronic Invertebrates',
                        'OPP Aquatic Life Benchmarks - Acute Nonvascular Plants',
                        'OPP Aquatic Life Benchmarks - Acute Vascular Plants',
                        'Office of Water Aquatic Life Criteria - Maximum Concentration (CMC)',
                        'Office of Water Aquatic Life Criteria - Continuous Concentration (CCC)'),]
    i = which(m$value == min(m$value,na.rm = TRUE)) #then index the minimum value row
  } else {
    m <- m[m$variable %in% c('Table 30 Toxic Substances - Freshwater Acute', 
                             'Table 30 Toxic Substances - Freshwater Chronic'),]
    i = which(m$value == min(m$value,na.rm = TRUE))
  }
  return (m[i,])
})


#### TESTING HUMAN HEALTH Minimum criteria value (Option 1) ####
#Following WQPMT rules to determine min HH value for PSP program
#Option 1 is preferring the minimum of the State WQS or OHA MCLs over the minimum of the rest of the benchmarks
min.HH.criteria.values <- ddply(criteria.values.melted, .(Pollutant), function(m) {
  m <- m[m$value != 0,] #criteria value does not equal zero (about 23 records)
  if (all(is.na(m[m$variable %in% c('Table 40 Human Health Criteria for Toxic Pollutants - Water + Organism', #if these criteria are all NA
                                    'Table 40 Human Health Criteria for Toxic Pollutants - Organism Only', 
                                    'OHA Maximum Contaminant Levels'),
                  'value']))) {
    m <- m[m$variable %in% c('EPA Human Health Benchmarks - Acute', 
                             'EPA Human Health Benchmarks - Chronic'),]
    i = which(m$value == min(m$value,na.rm = TRUE)) #then index the minimum value row
  } else {
    m <- m[m$variable %in% c('Table 40 Human Health Criteria for Toxic Pollutants - Water + Organism', #if these criteria are all NA
                             'Table 40 Human Health Criteria for Toxic Pollutants - Organism Only', 
                             'OHA Maximum Contaminant Levels'),]
    i = which(m$value == min(m$value,na.rm = TRUE))
  }
  return (m[i,])
})
#min.HH.criteria.values <- min.HH.criteria.values[min.HH.criteria.values$Matrix != "SW",]


write.csv(min.HH.criteria.values, paste0(outpath,"min.Human.Health.criteria.values_savedon", Sys.Date(),".csv")) 
write.csv(min.AQL.criteria.values, paste0(outpath,"min.Aquatic.Life.criteria.values_savedon", Sys.Date(),".csv")) 

