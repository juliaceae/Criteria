#This script: 
#1. reads the toxics criteria table originally created by Kara Goodwin and modified by Peter Bryant (see "Criteria.csv" and "CriteriaTableReadMe.txt" in this github repository)
#2. re-formats the table
#3. determines the applicable standard.
#Script originally created by Peter Bryant
#Modified for github by Julia Crown

require(plyr)
require(reshape2)
require(stringr)
options('scipen' = 100) #suppress scientific notation

#This file "Criteria.csv" is found in this github repository
#This file contains the replacements due to DEQ's 4/18/14 update of Table 30 (checked 8/21/14 by JCrown)
#This file may not reflect the most current EPA OPP or OW benchmarks, and we do know there were changes to the benchmarks since the last known download date.  
criteria <- read.csv('//deqhq1/PSP/Rscripts/Criteria/Criteria.csv', stringsAsFactors = FALSE)

#trims white space around the pollutant name
criteria$Pollutant <- str_trim(criteria$Pollutant)

#create a copy of the criteria table and remove unneeded columns
criteria.values <- within(criteria, rm('Notes','No.', 'index', 'Sort', 'minimum.criteria.benchmark.value','source','alias','CAS.No.',
                                       'Carcinogen','Fish.SV.175.g.d..ppb..','Fish.SV.17.5.g.d..ppb.','Fish.SV.32.g.d..ppb.',
                                       'X'))
#remove duplicated pollutant rows
criteria.values <- criteria.values[!duplicated(criteria.values$Pollutant),]
#rename the fields
criteria.values <- rename(criteria.values , replace = c('Human.Health.Criteria...................Water...Organism..ug.L.'= 'Table 40 Human Health Criteria for Toxic Pollutants - Water + Organism',
                                                        'Human.Health.Criteria..Organism.Only..ug.L.' = 'Table 40 Human Health Criteria for Toxic Pollutants - Organism Only',
                                                        'Freshwater.Acute.Criteria..CMC..ug.L' = 'Table 30 Toxic Substances - Freshwater Acute',
                                                        'Freshwater.Chronic.Criteria..CCC..ug.L' = 'Table 30 Toxic Substances - Freshwater Chronic',
                                                        "Freshwater.Fish.Acute.1" = 'OPP Aquatic Life Benchmarks - Acute Fish',
                                                        "Freshwater.Fish.Chronic.2" = 'OPP Aquatic Life Benchmarks - Chronic Fish',
                                                        "Freshwater.Invertebrates.Acute.3" = 'OPP Aquatic Life Benchmarks - Acute Invertebrates',
                                                        'Freshwater.Invertebrates.Chronic.4' = 'OPP Aquatic Life Benchmarks - Chronic Invertebrates',
                                                        "Freshwater.Nonvascular.Plants.Acute.5" = 'OPP Aquatic Life Benchmarks - Acute Nonvascular Plants',
                                                        "Freshwater.Vascular.Plants.Acute.6" = 'OPP Aquatic Life Benchmarks - Acute Vascular Plants',
                                                        "Office.of.Water.Aquatic.Life.Criteria.Maximum.Concentration..CMC." = 'Office of Water Aquatic Life Criteria - Maximum Concentration (CMC)',
                                                        "Office.of.Water.Aquatic.Life.Criteria.Continuous.Concentration..CCC." = 'Office of Water Aquatic Life Criteria - Continuous Concentration (CCC)',
                                                        "Marine.Acute.Criteria..CMC..ug.L" = 'Table 30 Toxic Substances - Saltwater Acute',
                                                        "Marine.Chronic.Criteria..CCC..ug.L" = 'Table 30 Toxic Substances - Saltwater Chronic',
                                                        "OR.MCLs" = 'OHA Maximum Contaminant Levels',
                                                        "Acute.One.Day.HHBP..ppb." = 'EPA Human Health Benchmarks - Acute', 
                                                        "Chronic..Lifetime.HHBP..ppb." = 'EPA Human Health Benchmarks - Chronic'))
#rename the rownames to the pollutant
rownames(criteria.values) <- criteria.values[,1]
#make a new table in "long" format
criteria.values.melted <- melt(criteria.values, id.vars = 'Pollutant')
#add saltwater or freshwater designations
criteria.values.melted$Matrix <- ifelse(criteria.values.melted$variable %in% c('Table 30 Toxic Substances - Saltwater Acute','Table 30 Toxic Substances - Saltwater Chronic'),
                                      'SW','FW')
#
t40oo <- criteria.values.melted[criteria.values.melted$variable == 'Table 40 Human Health Criteria for Toxic Pollutants - Organism Only',]
t40oo$Matrix <- 'SW'
criteria.values.melted <- rbind(criteria.values.melted, t40oo)
criteria.values.melted$ID <- paste(criteria.values.melted$Pollutant, criteria.values.melted$Matrix)
criteria.values.melted[criteria.values.melted$ID == 'Arsenic, Total inorganic SW' & criteria.values.melted$variable == 'Table 40 Human Health Criteria for Toxic Pollutants - Organism Only','value'] <- 1
criteria.values.melted[criteria.values.melted$ID == 'Arsenic, Total recoverable SW' & criteria.values.melted$variable == 'Table 40 Human Health Criteria for Toxic Pollutants - Organism Only','value'] <- 1
criteria.values.melted[criteria.values.melted$ID == 'Manganese, Total recoverable SW' & criteria.values.melted$variable == 'Table 40 Human Health Criteria for Toxic Pollutants - Organism Only','value'] <- 100
hardness.pollutants <- criteria.values.melted[criteria.values.melted$value == 'hardness',] #subset pollutants that use a hardness criteria
criteria.values.melted$value <- suppressWarnings(as.numeric(criteria.values.melted$value)) #convert from character to numeric (e.g. make blanks to NAs)

#remove criteria with NA or zero values
criteria.values.melted.applicable <- criteria.values.melted[!is.na(criteria.values.melted$value),]
criteria.values.melted.applicable <- criteria.values.melted.applicable[criteria.values.melted.applicable$value != 0,]

#### TESTING AQUATIC LIFE Minimum criteria value ####
#Following WQPMT rules to determine min AQL value for PSP program
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


#### TESTING HUMAN HEALTH Minimum criteria value ####
#Following WQPMT rules to determine min HH value for PSP program
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
min.HH.criteria.values <- min.HH.criteria.values[min.HH.criteria.values$Matrix != "SW",]


new.folder <- dir.create(paste("\\\\Deqhq1\\PSP\\Rscripts\\2014\\",Sys.Date(), sep="")) 
outpath.plot.points <- paste("\\\\Deqhq1\\PSP\\Rscripts\\2014\\",Sys.Date(), "\\", sep="") 
write.csv(min.HH.criteria.values, paste0(outpath.plot.points,"min.Human.Health.criteria.values_savedon", Sys.Date(),".csv")) 
write.csv(min.AQL.criteria.values, paste0(outpath.plot.points,"min.Aquatic.Life.criteria.values_savedon", Sys.Date(),".csv")) 

