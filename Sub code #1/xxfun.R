


## Adding additional features
makedat <- function(mydata) {
  
  # default dataset
  mdat <- mydata
  
  # 00: DB.area
  DB.area <- read.csv(
    file = "Dat_extra/00_area.csv", 
    header = T, 
    fileEncoding = "EUC-KR"
  )
  mdat <- merge(
    x = mdat, 
    y = DB.area[,!colnames(DB.area)%in%"sgg"],  
    by = c("sgg_code"),
    all.x = T
  )  
  
  # 01: DB.grdp
  DB.grdp <- read.csv(
    file = "Dat_extra/01_grdp.csv", 
    header = T, 
    fileEncoding = "EUC-KR"
  )
  DB.grdp$year <- DB.grdp$year + 1
  mdat <- merge(
    x = mdat, 
    y = DB.grdp[,!colnames(DB.grdp)%in%"sgg"],  
    by = c("sgg_code", "year"), 
    all.x = T
  )  
  
  # 02: DB.money
  DB.money <- read.csv(
    file = "Dat_extra/02_money.csv", 
    header = T, 
    fileEncoding = "EUC-KR"
  )
  DB.money$year <- DB.money$year + 1
  mdat <- merge(
    x = mdat, 
    y = DB.money[,!colnames(DB.money)%in%"sgg"],         
    by = c("sgg_code", "year"), 
    all.x = T
  )  
  
  # 03: DB.property_tax
  DB.property_tax <- read.csv(
    file = "Dat_extra/03_property_tax.csv", 
    header = T, 
    fileEncoding = "EUC-KR"
  )
  DB.property_tax$year <- DB.property_tax$year + 1
  mdat <- merge(
    x = mdat, 
    y = DB.property_tax[,!colnames(DB.property_tax)%in%"sgg"],  
    by = c("sgg_code", "year"), 
    all.x = T
  )  
  
  # 04: DB.building
  DB.building <- read.csv(
    file = "Dat_extra/04_building.csv", 
    header = T, 
    fileEncoding = "EUC-KR"
  )
  DB.building$year <- DB.building$year + 1
  mdat <- merge(
    x = mdat, 
    y = DB.building[,!colnames(DB.building)%in%"sgg"],      
    by = c("sgg_code", "year"), 
    all.x = T
  )  
  
  # 05: DB.city_rate
  DB.city_rate <- read.csv(
    file = "Dat_extra/05_city_rate.csv", 
    header = T, 
    fileEncoding = "EUC-KR"
  )
  DB.city_rate$year <- DB.city_rate$year + 1
  mdat <- merge(
    x = mdat, 
    y = DB.city_rate[,!colnames(DB.city_rate)%in%"sgg"],     
    by = c("sgg_code", "year"), 
    all.x = T
  )  
  
  # 06: DB.people
  DB.people <- read.csv(
    file = "Dat_extra/06_people.csv", 
    header = T, 
    fileEncoding = "EUC-KR"
  )
  DB.people$year <- DB.people$year + 1
  mdat <- merge(
    x = mdat, 
    y = DB.people[,!colnames(DB.people)%in%"sgg"],        
    by = c("sgg_code", "year"), 
    all.x = T
  )  
  
  # 07: DB.vulnerable
  DB.vulnerable <- read.csv(
    file = "Dat_extra/07_vulnerable.csv", 
    header = T, 
    fileEncoding = "EUC-KR"
  )
  DB.vulnerable$year <- DB.vulnerable$year + 1
  mdat <- merge(
    x = mdat, 
    y = DB.vulnerable[,!colnames(DB.vulnerable)%in%"sgg"],    
    by = c("sgg_code", "year"), 
    all.x = T
  )  
  
  # 08: DB.recovery_cost
  DB.recovery_cost <- read.csv(
    file = "Dat_extra/08_recovery_cost.csv", 
    header = T, 
    fileEncoding = "EUC-KR"
  )
  DB.recovery_cost$year <- DB.recovery_cost$year + 1
  mdat <- merge(
    x = mdat, 
    y = DB.recovery_cost[,!colnames(DB.recovery_cost)%in%"sgg"],
    by = c("sgg_code", "year"), 
    all.x = T
  ) 
  
  # 09: DB.odat
  DB.odat <- read.csv(
    file = "Dat_extra/09_odat.csv", 
    header = T, 
    fileEncoding = "EUC-KR"
  )
  DB.odat$year <- DB.odat$year + 1  
  mdat <- merge(
    x = mdat, 
    y = DB.odat[,!colnames(DB.odat)%in%"sido"],   
    by = c("sido_code", "year"), 
    all.x = T
  )  
  
  # 10: DB.ddat
  DB.ddat <- read.csv(
    file = "Dat_extra/10_ddat.csv", 
    header = T, 
    fileEncoding = "EUC-KR"
  )
  DB.ddat$year <- DB.ddat$year + 1 
  mdat <- merge(
    x = mdat, 
    y = DB.ddat[,!colnames(DB.ddat)%in%"sgg"],          
    by = c("sgg_code", "year"), 
    all.x = T
  )  
  
  # 11: DB.pdat
  DB.pdat <- read.csv(
    file = "Dat_extra/11_pdat.csv", 
    header = T, 
    fileEncoding = "EUC-KR"
  )
  DB.pdat$year <- DB.pdat$year + 1   
  mdat <- merge(
    x = mdat, 
    y = DB.pdat[,!colnames(DB.pdat)%in%"sgg"],          
    by = c("sgg_code", "year"),
    all.x = T
  )  
  
  # 12: DB.cDRZ
  DB.cDRZ <- read.csv(
    file = "Dat_extra/12_cDRZ.csv", 
    header = T, 
    fileEncoding = "EUC-KR"
  )
  DB.cDRZ$year <- DB.cDRZ$year + 1 
  mdat <- merge(
    x = mdat, 
    y = DB.cDRZ[,!colnames(DB.cDRZ)%in%"sgg"],   
    by = c("sgg_code", "year"), 
    all.x = T
  )  
  
  # 13: DB.nDRZ
  DB.nDRZ <- read.csv(
    file = "Dat_extra/13_nDRZ.csv", 
    header = T, 
    fileEncoding = "EUC-KR"
  )
  DB.nDRZ$year <- DB.nDRZ$year + 1 
  mdat <- merge(
    x = mdat, 
    y = DB.nDRZ[,!colnames(DB.nDRZ)%in%"sgg"],          
    by = c("sgg_code", "year"), 
    all.x = T
  )  
  
  # missing imputation
  mdat[is.na(mdat)] <- 0
  
  # sorting
  o <- order(mdat$sgg_code, mdat$beg_date)
  mdat <- mdat[o,]
  
  # return
  return(mdat)
}

# End..

