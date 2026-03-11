# Package ID: edi.2058.1 Cataloging System:https://pasta.edirepository.org.
# Data set title: Long-term record of lake and stream biogeochemistry from the Loch Vale Watershed, Rocky Mountain National Park, Colorado, USA: 1981-2024.
# Data set creator:  Jill Baron -  
# Data set creator:  Isabella Oleksy -  
# Data set creator:  Mary Farruggia -  
# Data set creator:  Adeline Kelly -  
# Data set creator:  Steve Zary -  
# Data set creator:  Owen Bricker -  
# Data set creator:  Douglas Hansen -  
# Data set creator:  Rob Edwards -  
# Data set creator:  Brian Olver -  
# Data set creator:  Sarah Spaulding -  
# Data set creator:  Mark Brenner -  
# Data set creator:  Keith Schoepflin -  
# Data set creator:  Scott Denning -  
# Data set creator:  Ben Riebau -  
# Data set creator:  Eric Allstott -  
# Data set creator:  Jorin Botte -  
# Data set creator:  Austin Krcmarik -  
# Data set creator:  Lisa Foster -  
# Data set creator:  Tom Riley -  
# Data set creator:  Jill Oropeza -  
# Data set creator:  Eric Richer -  
# Data set creator:  Jared Heath -  
# Data set creator:  Timothy Fegel -  
# Data set creator:  Daniel Bowker -  
# Data set creator:  Timothy Weinmann -  
# Contact:  Isabella Oleksy -    - Isabella.Oleksy@colorado.edu
# Contact:  Jill Baron -    - Jill.baron@colostate.edu
# Contact:  Mary Farruggia -    - mj.farruggia@colorado.edu
# Stylesheet v2.16 for metadata conversion into program: John H. Porter, Univ. Virginia, jporter@virginia.edu      
# Uncomment the following lines to have R clear previous work, or set a working directory
# rm(list=ls())      

# setwd("C:/users/my_name/my_dir")       



options(HTTPUserAgent="EDI_CodeGen")
	      

inUrl1  <- "https://pasta.lternet.edu/package/data/eml/edi/2058/1/b2223f654a2dbdc62f988cfe6259ba95" 
infile1 <- tempfile()
try(download.file(inUrl1,infile1,method="curl",extra=paste0(' -A "',getOption("HTTPUserAgent"),'"')))
if (is.na(file.size(infile1))) download.file(inUrl1,infile1,method="auto")

                   
 Loch_chem <-read.csv(infile1,header=F 
          ,skip=1
            ,sep=","  
                ,quot='"' 
        , col.names=c(
                    "site",     
                    "siteId",     
                    "sampleType",     
                    "sampleReplicate",     
                    "sampleLocation",     
                    "waterbodyType",     
                    "year",     
                    "month",     
                    "day",     
                    "date",     
                    "timeDenver",     
                    "datetimeDenver",     
                    "parameter",     
                    "value",     
                    "flag",     
                    "tempFlag",     
                    "labLocation",     
                    "lab",     
                    "longitudeDD",     
                    "latitudeDD",     
                    "elevationM",     
                    "elevationFt",     
                    "usgsSiteNo",     
                    "datetimeUTC",     
                    "waterbodyMaxDepth",     
                    "samplingDepth",     
                    "lagosLakeId"    ), check.names=TRUE)
               
unlink(infile1)
		    
# Fix any interval or ratio columns mistakenly read in as nominal and nominal columns read as numeric or dates read as strings
                
if (class(Loch_chem$site)!="factor") Loch_chem$site<- as.factor(Loch_chem$site)
if (class(Loch_chem$siteId)!="factor") Loch_chem$siteId<- as.factor(Loch_chem$siteId)
if (class(Loch_chem$sampleType)!="factor") Loch_chem$sampleType<- as.factor(Loch_chem$sampleType)
if (class(Loch_chem$sampleReplicate)=="factor") Loch_chem$sampleReplicate <-as.numeric(levels(Loch_chem$sampleReplicate))[as.integer(Loch_chem$sampleReplicate) ]               
if (class(Loch_chem$sampleReplicate)=="character") Loch_chem$sampleReplicate <-as.numeric(Loch_chem$sampleReplicate)
if (class(Loch_chem$sampleLocation)!="factor") Loch_chem$sampleLocation<- as.factor(Loch_chem$sampleLocation)
if (class(Loch_chem$waterbodyType)!="factor") Loch_chem$waterbodyType<- as.factor(Loch_chem$waterbodyType)
if (class(Loch_chem$year)=="factor") Loch_chem$year <-as.numeric(levels(Loch_chem$year))[as.integer(Loch_chem$year) ]               
if (class(Loch_chem$year)=="character") Loch_chem$year <-as.numeric(Loch_chem$year)
if (class(Loch_chem$month)=="factor") Loch_chem$month <-as.numeric(levels(Loch_chem$month))[as.integer(Loch_chem$month) ]               
if (class(Loch_chem$month)=="character") Loch_chem$month <-as.numeric(Loch_chem$month)
if (class(Loch_chem$day)=="factor") Loch_chem$day <-as.numeric(levels(Loch_chem$day))[as.integer(Loch_chem$day) ]               
if (class(Loch_chem$day)=="character") Loch_chem$day <-as.numeric(Loch_chem$day)                                   
# attempting to convert Loch_chem$date dateTime string to R date structure (date or POSIXct)                                
tmpDateFormat<-"%Y-%m-%d"
tmp1date<-as.Date(Loch_chem$date,format=tmpDateFormat)
# Keep the new dates only if they all converted correctly
if(nrow(Loch_chem[Loch_chem$date != "",]) == length(tmp1date[!is.na(tmp1date)])){Loch_chem$date <- tmp1date } else {print("Date conversion failed for Loch_chem$date. Please inspect the data and do the date conversion yourself.")}                                                                    
                                
if (class(Loch_chem$timeDenver)!="factor") Loch_chem$timeDenver<- as.factor(Loch_chem$timeDenver)                                   
# attempting to convert Loch_chem$datetimeDenver dateTime string to R date structure (date or POSIXct)                                
tmpDateFormat<-"%Y-%m-%d %H:%M:%S" 
tmp1datetimeDenver<-as.POSIXct(Loch_chem$datetimeDenver,format=tmpDateFormat)
# Keep the new dates only if they all converted correctly
if(nrow(Loch_chem[Loch_chem$datetimeDenver != "",]) == length(tmp1datetimeDenver[!is.na(tmp1datetimeDenver)])){Loch_chem$datetimeDenver <- tmp1datetimeDenver } else {print("Date conversion failed for Loch_chem$datetimeDenver. Please inspect the data and do the date conversion yourself.")}                                                                    
                                
if (class(Loch_chem$parameter)!="factor") Loch_chem$parameter<- as.factor(Loch_chem$parameter)
if (class(Loch_chem$value)=="factor") Loch_chem$value <-as.numeric(levels(Loch_chem$value))[as.integer(Loch_chem$value) ]               
if (class(Loch_chem$value)=="character") Loch_chem$value <-as.numeric(Loch_chem$value)
if (class(Loch_chem$flag)!="factor") Loch_chem$flag<- as.factor(Loch_chem$flag)
if (class(Loch_chem$tempFlag)!="factor") Loch_chem$tempFlag<- as.factor(Loch_chem$tempFlag)
if (class(Loch_chem$labLocation)!="factor") Loch_chem$labLocation<- as.factor(Loch_chem$labLocation)
if (class(Loch_chem$lab)!="factor") Loch_chem$lab<- as.factor(Loch_chem$lab)
if (class(Loch_chem$longitudeDD)=="factor") Loch_chem$longitudeDD <-as.numeric(levels(Loch_chem$longitudeDD))[as.integer(Loch_chem$longitudeDD) ]               
if (class(Loch_chem$longitudeDD)=="character") Loch_chem$longitudeDD <-as.numeric(Loch_chem$longitudeDD)
if (class(Loch_chem$latitudeDD)=="factor") Loch_chem$latitudeDD <-as.numeric(levels(Loch_chem$latitudeDD))[as.integer(Loch_chem$latitudeDD) ]               
if (class(Loch_chem$latitudeDD)=="character") Loch_chem$latitudeDD <-as.numeric(Loch_chem$latitudeDD)
if (class(Loch_chem$elevationM)=="factor") Loch_chem$elevationM <-as.numeric(levels(Loch_chem$elevationM))[as.integer(Loch_chem$elevationM) ]               
if (class(Loch_chem$elevationM)=="character") Loch_chem$elevationM <-as.numeric(Loch_chem$elevationM)
if (class(Loch_chem$elevationFt)=="factor") Loch_chem$elevationFt <-as.numeric(levels(Loch_chem$elevationFt))[as.integer(Loch_chem$elevationFt) ]               
if (class(Loch_chem$elevationFt)=="character") Loch_chem$elevationFt <-as.numeric(Loch_chem$elevationFt)
if (class(Loch_chem$usgsSiteNo)=="factor") Loch_chem$usgsSiteNo <-as.numeric(levels(Loch_chem$usgsSiteNo))[as.integer(Loch_chem$usgsSiteNo) ]               
if (class(Loch_chem$usgsSiteNo)=="character") Loch_chem$usgsSiteNo <-as.numeric(Loch_chem$usgsSiteNo)                                   
# attempting to convert Loch_chem$datetimeUTC dateTime string to R date structure (date or POSIXct)                                
tmpDateFormat<-"%Y-%m-%d %H:%M:%S" 
tmp1datetimeUTC<-as.POSIXct(Loch_chem$datetimeUTC,format=tmpDateFormat)
# Keep the new dates only if they all converted correctly
if(nrow(Loch_chem[Loch_chem$datetimeUTC != "",]) == length(tmp1datetimeUTC[!is.na(tmp1datetimeUTC)])){Loch_chem$datetimeUTC <- tmp1datetimeUTC } else {print("Date conversion failed for Loch_chem$datetimeUTC. Please inspect the data and do the date conversion yourself.")}                                                                    
                                
if (class(Loch_chem$waterbodyMaxDepth)=="factor") Loch_chem$waterbodyMaxDepth <-as.numeric(levels(Loch_chem$waterbodyMaxDepth))[as.integer(Loch_chem$waterbodyMaxDepth) ]               
if (class(Loch_chem$waterbodyMaxDepth)=="character") Loch_chem$waterbodyMaxDepth <-as.numeric(Loch_chem$waterbodyMaxDepth)
if (class(Loch_chem$samplingDepth)=="factor") Loch_chem$samplingDepth <-as.numeric(levels(Loch_chem$samplingDepth))[as.integer(Loch_chem$samplingDepth) ]               
if (class(Loch_chem$samplingDepth)=="character") Loch_chem$samplingDepth <-as.numeric(Loch_chem$samplingDepth)
if (class(Loch_chem$lagosLakeId)=="factor") Loch_chem$lagosLakeId <-as.numeric(levels(Loch_chem$lagosLakeId))[as.integer(Loch_chem$lagosLakeId) ]               
if (class(Loch_chem$lagosLakeId)=="character") Loch_chem$lagosLakeId <-as.numeric(Loch_chem$lagosLakeId)
                
# Convert Missing Values to NA for non-dates
                
Loch_chem$timeDenver <- as.factor(ifelse((trimws(as.character(Loch_chem$timeDenver))==trimws("NA")),NA,as.character(Loch_chem$timeDenver)))
Loch_chem$labLocation <- as.factor(ifelse((trimws(as.character(Loch_chem$labLocation))==trimws("NA")),NA,as.character(Loch_chem$labLocation)))
Loch_chem$lab <- as.factor(ifelse((trimws(as.character(Loch_chem$lab))==trimws("NA")),NA,as.character(Loch_chem$lab)))
Loch_chem$usgsSiteNo <- ifelse((trimws(as.character(Loch_chem$usgsSiteNo))==trimws("NA")),NA,Loch_chem$usgsSiteNo)               
suppressWarnings(Loch_chem$usgsSiteNo <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(Loch_chem$usgsSiteNo))==as.character(as.numeric("NA"))),NA,Loch_chem$usgsSiteNo))
Loch_chem$waterbodyMaxDepth <- ifelse((trimws(as.character(Loch_chem$waterbodyMaxDepth))==trimws("NA")),NA,Loch_chem$waterbodyMaxDepth)               
suppressWarnings(Loch_chem$waterbodyMaxDepth <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(Loch_chem$waterbodyMaxDepth))==as.character(as.numeric("NA"))),NA,Loch_chem$waterbodyMaxDepth))
Loch_chem$samplingDepth <- ifelse((trimws(as.character(Loch_chem$samplingDepth))==trimws("NA")),NA,Loch_chem$samplingDepth)               
suppressWarnings(Loch_chem$samplingDepth <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(Loch_chem$samplingDepth))==as.character(as.numeric("NA"))),NA,Loch_chem$samplingDepth))
Loch_chem$lagosLakeId <- ifelse((trimws(as.character(Loch_chem$lagosLakeId))==trimws("NA")),NA,Loch_chem$lagosLakeId)               
suppressWarnings(Loch_chem$lagosLakeId <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(Loch_chem$lagosLakeId))==as.character(as.numeric("NA"))),NA,Loch_chem$lagosLakeId))


# Here is the structure of the input data frame:
print("Loch_chem) Structure")		    
str(Loch_chem)                            
attach(Loch_chem)                            
# The analyses below are basic descriptions of the variables. After testing, they should be replaced.                 

print(" ")
print("Summary of site")
print(summary(site))
print(" ")
print("Summary of siteId")
print(summary(siteId))
print(" ")
print("Summary of sampleType")
print(summary(sampleType))
print(" ")
print("Summary of sampleReplicate")
print(summary(sampleReplicate))
print(" ")
print("Summary of sampleLocation")
print(summary(sampleLocation))
print(" ")
print("Summary of waterbodyType")
print(summary(waterbodyType))
print(" ")
print("Summary of year")
print(summary(year))
print(" ")
print("Summary of month")
print(summary(month))
print(" ")
print("Summary of day")
print(summary(day))
print(" ")
print("Summary of date")
print(summary(date))
print(" ")
print("Summary of timeDenver")
print(summary(timeDenver))
print(" ")
print("Summary of datetimeDenver")
print(summary(datetimeDenver))
print(" ")
print("Summary of parameter")
print(summary(parameter))
print(" ")
print("Summary of value")
print(summary(value))
print(" ")
print("Summary of flag")
print(summary(flag))
print(" ")
print("Summary of tempFlag")
print(summary(tempFlag))
print(" ")
print("Summary of labLocation")
print(summary(labLocation))
print(" ")
print("Summary of lab")
print(summary(lab))
print(" ")
print("Summary of longitudeDD")
print(summary(longitudeDD))
print(" ")
print("Summary of latitudeDD")
print(summary(latitudeDD))
print(" ")
print("Summary of elevationM")
print(summary(elevationM))
print(" ")
print("Summary of elevationFt")
print(summary(elevationFt))
print(" ")
print("Summary of usgsSiteNo")
print(summary(usgsSiteNo))
print(" ")
print("Summary of datetimeUTC")
print(summary(datetimeUTC))
print(" ")
print("Summary of waterbodyMaxDepth")
print(summary(waterbodyMaxDepth))
print(" ")
print("Summary of samplingDepth")
print(summary(samplingDepth))
print(" ")
print("Summary of lagosLakeId")
print(summary(lagosLakeId)) 
# Get more details on character variables
                 

print(" ")
print("Summary of site")
print(summary(as.factor(Loch_chem$site))) 

print(" ")
print("Summary of siteId")
print(summary(as.factor(Loch_chem$siteId))) 

print(" ")
print("Summary of sampleType")
print(summary(as.factor(Loch_chem$sampleType))) 

print(" ")
print("Summary of sampleLocation")
print(summary(as.factor(Loch_chem$sampleLocation))) 

print(" ")
print("Summary of waterbodyType")
print(summary(as.factor(Loch_chem$waterbodyType))) 

print(" ")
print("Summary of timeDenver")
print(summary(as.factor(Loch_chem$timeDenver))) 

print(" ")
print("Summary of parameter")
print(summary(as.factor(Loch_chem$parameter))) 

print(" ")
print("Summary of flag")
print(summary(as.factor(Loch_chem$flag))) 

print(" ")
print("Summary of tempFlag")
print(summary(as.factor(Loch_chem$tempFlag))) 

print(" ")
print("Summary of labLocation")
print(summary(as.factor(Loch_chem$labLocation))) 

print(" ")
print("Summary of lab")
print(summary(as.factor(Loch_chem$lab)))
detach(Loch_chem)               
        



