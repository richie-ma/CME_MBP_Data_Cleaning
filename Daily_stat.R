#########################################################################
## This code is used to extract daily statistics at each trading day
## Richie R. Ma, ruchuan2@illinois.edu, UIUC, ACE DEPT
####################################################################

rm(list=ls())


library(stringr)
library(data.table)

name <- list.files(pattern = "MBO")
date <-  as.Date(substr(name,1,8), "%Y%m%d")

for (i in 440:444){
  print(name[i])
  MDP <- name[i]
  input <- MDP
  data <- fread(input, header=F)[[1L]]
  
  if(date[i] < "2015-11-20"){

  Index <- str_replace_all(data, "\001",",") 
rm(data)

Index <- str_replace_all(Index, "1128=([^,]*),9=([^,]*),","")
Index <- str_replace_all(Index, "49=([^,]*),34=([^,]*)","")
Index <- str_replace_all(Index,",5799=([^,]*),",",")
Index <- str_replace_all(Index,",268=([^,]*),",",")
Index <- str_replace_all(Index,",279=([^,]*),",",")
Index <- str_replace_all(Index,",22=([^,]*),",",")
Index <- str_replace_all(Index,",48=([^,]*),",",")
Index <- str_replace_all(Index,",273=([^,]*),",",")
Index <- str_replace_all(Index,",274=([^,]*),",",")
Index <- str_replace_all(Index,",451=([^,]*),",",")
Index <- str_replace_all(Index,",1003=([^,]*),",",")
Index <- str_replace_all(Index,",1020=([^,]*),",",")
Index <- str_replace_all(Index,",277=([^,]*),",",")

OPEN <- str_subset(Index, "269=4")
SETTLE <- str_subset(Index, "269=6")
HIGH_PX <- str_subset(Index, "269=7")
LOW_PX <- str_subset(Index, "269=8")
HIGH_BID <- str_subset(Index, "269=N")
LOW_OFFER <- str_subset(Index, "269=O")
VOLUME <- str_subset(Index, "269=B")
OPEN_INT <- str_subset(Index, "269=C")
#ELEC_VOLUME <- str_subset(Index, "269=e")
LIMIT <- str_subset(Index, "35=f") ##g =Threshold Limits and Price Band Variation

rm(Index)

########################## OPEN ####################################################
if(length(OPEN)!=0){

open <- str_match_all(OPEN, "83=([^,]*),107=([^,]*),269=4,270=([^,]*),286=([^,]*),")

n_row <- sapply(open, nrow)
open.info <- unlist(str_extract_all(OPEN,"52=([^,]*),75=([^,]*),"))
open.info <- str_dup(open.info, n_row)
open <- as.data.table(do.call(rbind, open))[,-1]
names(open)[c(1:4)] <- c("Seq","Code", "OPEN_PX","Flag")

open[Flag==5, Flag:="IndicativeOpen"][Flag==0, Flag:="DailyOpen"]

open.info <- str_match_all(open.info, "52=([^,]*),75=([^,]*),")
open.info <- as.data.table(do.call(rbind,open.info))[,-1]
names(open.info)[c(1:2)] <- c("Time", "Date")

open$Seq <- as.numeric(open$Seq)
open$PX <- as.numeric(open$PX)

open <- cbind(open.info, open)
rm(OPEN, open.info, n_row)

setkey(open, Code, Seq)

contract <- unique(open$Code)

# save the results

for (k in 1:length(contract)) {
  
  
  open.sf <- open[Code==contract[k]]
  
  if(dim(open.sf )[1]!=0){
    ## delete the file with empty size and create new folders to save data to each folders
    ## create multiple folders to save the data
    new_fol <- file.path(paste0("C:/Users/ruchuan2/Box/soybean/daily stat/open/",contract[k]))
    if(!dir.exists(new_fol)){
      dir.create(new_fol)
    }
    fname <- file.path(new_fol,
                       paste0(name[i], "_",contract[k],".rda"))
    save(open.sf, file = fname) ## save all contracts
    
  }
}

rm(open)
}
######################## SETTLE ###########################################
if(length(SETTLE)!=0){

settle <- str_match_all(SETTLE, "83=([^,]*),107=([^,]*),269=6,270=([^,]*),")

n_row <- sapply(settle, nrow)
settle.info <- unlist(str_extract_all(SETTLE,"52=([^,]*),75=([^,]*),"))
settle.info <- str_dup(settle.info, n_row)
settle <- as.data.table(do.call(rbind, settle))[,-1]
names(settle)[c(1:3)] <- c("Seq","Code","SETTLE_PX")


settle.info <- str_match_all(settle.info, "52=([^,]*),75=([^,]*),")
settle.info <- as.data.table(do.call(rbind,settle.info))[,-1]
names(settle.info)[c(1:2)] <- c("Time", "Date")

settle$Seq <- as.numeric(settle$Seq)
settle$SETTLE_PX <- as.numeric(settle$SETTLE_PX)

settle <- cbind(settle.info, settle)
rm(SETTLE, settle.info, n_row)

setkey(settle, Code, Seq)

contract <- unique(settle$Code)

# save the results

for (k in 1:length(contract)) {
  
  
  settle.sf <- settle[Code==contract[k]]
  
  if(dim(settle.sf )[1]!=0){
    ## delete the file with empty size and create new folders to save data to each folders
    ## create multiple folders to save the data
    new_fol <- file.path(paste0("saving path",contract[k]))
    if(!dir.exists(new_fol)){
      dir.create(new_fol)
    }
    fname <- file.path(new_fol,
                       paste0(name[i], "_",contract[k],".rda"))
    save(settle.sf, file = fname) ## save all contracts
    
  }
}


rm(settle)
}
#################### SESSION HIGH PX #######################################
if(length(HIGH_PX)!=0){

high_px <- str_match_all(HIGH_PX, "83=([^,]*),107=([^,]*),269=7,270=([^,]*),")

n_row <- sapply(high_px, nrow)
high_px.info <- unlist(str_extract_all(HIGH_PX,"52=([^,]*),75=([^,]*),"))
high_px.info <- str_dup(high_px.info, n_row)
high_px <- as.data.table(do.call(rbind, high_px))[,-1]
names(high_px)[c(1:3)] <- c("Seq","Code","HIGH")


high_px.info <- str_match_all(high_px.info, "52=([^,]*),75=([^,]*),")
high_px.info <- as.data.table(do.call(rbind,high_px.info))[,-1]
names(high_px.info)[c(1:2)] <- c("Time", "Date")

high_px$Seq <- as.numeric(high_px$Seq)
high_px$HIGH <- as.numeric(high_px$HIGH)

high_px <- cbind(high_px.info, high_px)
rm(HIGH_PX, high_px.info, n_row)

setkey(high_px, Code, Seq)

contract <- unique(high_px$Code)

# save the results

for (k in 1:length(contract)) {
  
  
  high_px.sf <- high_px[Code==contract[k]]
  
  if(dim(high_px.sf )[1]!=0){
    ## delete the file with empty size and create new folders to save data to each folders
    ## create multiple folders to save the data
    new_fol <- file.path(paste0("saving path",contract[k]))
    if(!dir.exists(new_fol)){
      dir.create(new_fol)
    }
    fname <- file.path(new_fol,
                       paste0(name[i], "_",contract[k],".rda"))
    save(high_px.sf, file = fname) ## save all contracts
    
  }
}
rm(high_px)
}
############### SESSION LOW PRICE ###################################
if(length(LOW_PX)!=0){


low_px <- str_match_all(LOW_PX, "83=([^,]*),107=([^,]*),269=8,270=([^,]*),")

n_row <- sapply(low_px, nrow)
low_px.info <- unlist(str_extract_all(LOW_PX,"52=([^,]*),75=([^,]*),"))
low_px.info <- str_dup(low_px.info, n_row)
low_px <- as.data.table(do.call(rbind, low_px))[,-1]
names(low_px)[c(1:3)] <- c("Seq","Code","LOW")


low_px.info <- str_match_all(low_px.info, "52=([^,]*),75=([^,]*),")
low_px.info <- as.data.table(do.call(rbind,low_px.info))[,-1]
names(low_px.info)[c(1:2)] <- c("Time" , "Date")

low_px$Seq <- as.numeric(low_px$Seq)
low_px$LOW <- as.numeric(low_px$LOW)

low_px <- cbind(low_px.info, low_px)
rm(LOW_PX, low_px.info, n_row)

setkey(low_px, Code, Seq)

contract <- unique(low_px$Code)

# save the results

for (k in 1:length(contract)) {
  
  
  low_px.sf <- low_px[Code==contract[k]]
  
  if(dim(low_px.sf )[1]!=0){
    ## delete the file with empty size and create new folders to save data to each folders
    ## create multiple folders to save the data
    new_fol <- file.path(paste0("saving path",contract[k]))
    if(!dir.exists(new_fol)){
      dir.create(new_fol)
    }
    fname <- file.path(new_fol,
                       paste0(name[i], "_",contract[k],".rda"))
    save(low_px.sf, file = fname) ## save all contracts
    
  }
}
rm(low_px)
}
############### SESSION HIGH BID ###################################
if(length(HIGH_BID)!=0){

high_bid <- str_match_all(HIGH_BID,  "83=([^,]*),107=([^,]*),269=N,270=([^,]*),")

n_row <- sapply(high_bid, nrow)
high_bid.info <- unlist(str_extract_all(HIGH_BID,"52=([^,]*),75=([^,]*),"))
high_bid.info <- str_dup(high_bid.info, n_row)
high_bid <- as.data.table(do.call(rbind, high_bid))[,-1]
names(high_bid)[c(1:3)] <- c("Seq","Code","HIGH_BID")


high_bid.info <- str_match_all(high_bid.info, "52=([^,]*),75=([^,]*),")
high_bid.info <- as.data.table(do.call(rbind,high_bid.info))[,-1]
names(high_bid.info)[c(1:2)] <- c("Time", "Date")

high_bid$Seq <- as.numeric(high_bid$Seq)
high_bid$HIGH_BID <- as.numeric(high_bid$HIGH_BID)

high_bid <- cbind(high_bid.info, high_bid)
rm(HIGH_BID, high_bid.info, n_row)

setkey(high_bid, Code, Seq)

contract <- unique(high_bid$Code)

# save the results

for (k in 1:length(contract)) {
  
  
  high_bid.sf <- high_bid[Code==contract[k]]
  
  if(dim(high_bid.sf )[1]!=0){
    ## delete the file with empty size and create new folders to save data to each folders
    ## create multiple folders to save the data
    new_fol <- file.path(paste0("saving path",contract[k]))
    if(!dir.exists(new_fol)){
      dir.create(new_fol)
    }
    fname <- file.path(new_fol,
                       paste0(name[i], "_",contract[k],".rda"))
    save(high_bid.sf, file = fname) ## save all contracts
    
  }
}
rm(high_bid)
}
############# SESSION LOW OFFER ###############################
if(length(LOW_OFFER)!=0){



low_offer <- str_match_all(LOW_OFFER, "83=([^,]*),107=([^,]*),269=O,270=([^,]*),")

n_row <- sapply(low_offer, nrow)
low_offer.info <- unlist(str_extract_all(LOW_OFFER,"52=([^,]*),75=([^,]*),"))
low_offer.info <- str_dup(low_offer.info, n_row)
low_offer <- as.data.table(do.call(rbind, low_offer))[,-1]
names(low_offer)[c(1:3)] <- c("Seq","Code","LOW_OFFER")


low_offer.info <- str_match_all(low_offer.info, "52=([^,]*),75=([^,]*),")
low_offer.info <- as.data.table(do.call(rbind,low_offer.info))[,-1]
names(low_offer.info)[c(1:2)] <- c("Time", "Date")

low_offer$Seq <- as.numeric(low_offer$Seq)
low_offer$LOW_OFFER <- as.numeric(low_offer$LOW_OFFER)

low_offer <- cbind(low_offer.info, low_offer)
rm(LOW_OFFER, low_offer.info, n_row)

setkey(low_offer, Code, Seq)

contract <- unique(low_offer$Code)

# save the results

for (k in 1:length(contract)) {
  
  
  low_offer.sf <- low_offer[Code==contract[k]]
  
  if(dim(low_offer.sf )[1]!=0){
    ## delete the file with empty size and create new folders to save data to each folders
    ## create multiple folders to save the data
    new_fol <- file.path(paste0("saving path",contract[k]))
    if(!dir.exists(new_fol)){
      dir.create(new_fol)
    }
    fname <- file.path(new_fol,
                       paste0(name[i], "_",contract[k],".rda"))
    save(low_offer.sf, file = fname) ## save all contracts
    
  }
}
rm(low_offer)
}
############# VOLUME  ###############################
if(length(VOLUME)!=0){
volume <- str_match_all(VOLUME, "83=([^,]*),107=([^,]*),269=B,270=([^,]*),")

n_row <- sapply(volume, nrow)
volume.info <- unlist(str_extract_all(VOLUME,"52=([^,]*),75=([^,]*),"))
volume.info <- str_dup(volume.info, n_row)
volume <- as.data.table(do.call(rbind, volume))[,-1]
names(volume)[c(1:3)] <- c("Seq","Code","VOLUME")


volume.info <- str_match_all(volume.info, "52=([^,]*),75=([^,]*),")
volume.info <- as.data.table(do.call(rbind,volume.info))[,-1]
names(volume.info)[c(1:2)] <- c("Time","Date")

volume$Seq <- as.numeric(volume$Seq)
volume$VOLUME <- as.numeric(volume$VOLUME)

volume <- cbind(volume.info, volume)
rm(VOLUME, volume.info, n_row)

setkey(volume, Code, Seq)

contract <- unique(volume$Code)

# save the results

for (k in 1:length(contract)) {
  
  
  volume.sf <- volume[Code==contract[k]]
  
  if(dim(volume.sf )[1]!=0){
    ## delete the file with empty size and create new folders to save data to each folders
    ## create multiple folders to save the data
    new_fol <- file.path(paste0("saving path",contract[k]))
    if(!dir.exists(new_fol)){
      dir.create(new_fol)
    }
    fname <- file.path(new_fol,
                       paste0(name[i], "_",contract[k],".rda"))
    save(volume.sf, file = fname) ## save all contracts
    
  }
}
rm(volume)
}
############# OPEN INTEREST  ###############################
if(length(OPEN_INT)!=0){
open_int <- str_match_all(OPEN_INT, "83=([^,]*),107=([^,]*),269=C,270=([^,]*),")

n_row <- sapply(open_int, nrow)
open_int.info <- unlist(str_extract_all(OPEN_INT,"52=([^,]*),75=([^,]*),"))
open_int.info <- str_dup(open_int.info, n_row)
open_int <- as.data.table(do.call(rbind, open_int))[,-1]
names(open_int)[c(1:3)] <- c("Seq","Code", "OPEN_INT")


open_int.info <- str_match_all(open_int.info, "52=([^,]*),75=([^,]*),")
open_int.info <- as.data.table(do.call(rbind,open_int.info))[,-1]
names(open_int.info)[c(1:2)] <- c("Time","Date")

open_int$Seq <- as.numeric(open_int$Seq)
open_int$OPEN_INT <- as.numeric(open_int$OPEN_INT)

open_int <- cbind(open_int.info, open_int)
rm(OPEN_INT, open_int.info, n_row)

setkey(open_int, Code, Seq)

contract <- unique(open_int$Code)

# save the results

for (k in 1:length(contract)) {
  
  
  open_int.sf <- open_int[Code==contract[k]]
  
  if(dim(open_int.sf )[1]!=0){
    ## delete the file with empty size and create new folders to save data to each folders
    ## create multiple folders to save the data
    new_fol <- file.path(paste0("saving path",contract[k]))
    if(!dir.exists(new_fol)){
      dir.create(new_fol)
    }
    fname <- file.path(new_fol,
                       paste0(name[i], "_",contract[k],".rda"))
    save(open_int.sf, file = fname) ## save all contracts
    
  }
}
rm(open_int)
}
############# ELECTRONIC VOLUME  ###############################

#elec_volume <- str_match_all(ELEC_VOLUME, "83=([^,]*),107=([^,]*),269=e,270=([^,]*),")

#n_row <- sapply(elec_volume, nrow)
#elec_volume.info <- unlist(str_extract_all(ELEC_VOLUME,"52=([^,]*),75=([^,]*),"))
#elec_volume.info <- str_dup(elec_volume.info, n_row)
#elec_volume <- as.data.table(do.call(rbind, elec_volume))[,-1]
#names(elec_volume)[c(1:3)] <- c("Seq","Code","ELEC_VOLUME")


#elec_volume.info <- str_match_all(elec_volume.info, "52=([^,]*),75=([^,]*),")
#elec_volume.info <- as.data.table(do.call(rbind,elec_volume.info))[,-1]
#names(elec_volume.info)[c(1:2)] <- c("Time", "Date")

#elec_volume$Seq <- as.numeric(elec_volume$Seq)
#elec_volume$ELEC_VOLUME <- as.numeric(elec_volume$ELEC_VOLUME)

#elec_volume <- cbind(elec_volume.info, elec_volume)
#rm(ELEC_VOLUME, elec_volume.info, n_row)

#contract <- unique(elec_volume$Code)

# save the results

#for (k in 1:length(contract)) {
  
  
 # elec_volume.sf <- elec_volume[Code==contract[k]]
  
  #if(dim(elec_volume.sf )[1]!=0){
    ## delete the file with empty size and create new folders to save data to each folders
    ## create multiple folders to save the data
   # new_fol <- file.path(paste0("saving path","/",contract[k]))
    #if(!dir.exists(new_fol)){
    #  dir.create(new_fol)
  #  }
  #  fname <- file.path(new_fol,
   #                    paste0(name[i], "_",contract[k],".rda"))
  #  save(elec_volume.sf, file = fname) ## save all contracts
    
  #}
#}
#rm(elec_volume)

############# LIMIT  ###############################

## 1148 
## Allowable low limit price for the trading day.  orders submitted with px below this will be rejected 
##  1149
## Allowable high limit price for the trading day.  orders submitted with px above this will be rejected
## 1143 	MaxPriceVariation for price banding
if(length(LIMIT)!=0){

LIMIT <- gsub("1128=([^,]*),9=([^,]*),35=([^,]*),49=([^,]*),34=([^,]*),","",LIMIT )
LIMIT <- gsub("52=([^,]*),22=([^,]*),",",",LIMIT )
LIMIT <- gsub(",48=([^,]*),",",",LIMIT )
LIMIT <- gsub(",10=([^,]*),",",",LIMIT ) 

  
  ## generate the trade data (without order flow)
  limit <- str_match_all(LIMIT , "75=([^,]*),107=([^,]*),332=([^,]*),333=([^,]*),")
  limit <- as.data.table(do.call(rbind,limit))[,-1]
  names(limit)[c(1:4)] <- c("Date","Code","high_limit","low_limit")
  limit[, `:=`(high_limit=as.numeric(high_limit), low_limit=as.numeric(low_limit))]
  

rm(LIMIT)

contract <- unique(limit$Code)

# save the results

for (k in 1:length(contract)) {
  
  
  limit.sf <- limit[Code==contract[k]]
  
  if(dim(limit.sf )[1]!=0){
    ## delete the file with empty size and create new folders to save data to each folders
    ## create multiple folders to save the data
    new_fol <- file.path(paste0("saving path",contract[k]))
    if(!dir.exists(new_fol)){
      dir.create(new_fol)
    }
    fname <- file.path(new_fol,
                       paste0(name[i], "_",contract[k],".rda"))
    save(limit.sf, file = fname) ## save all contracts
    
  }
}
rm(limit)
}

} else {

  Index <- gsub("\001",",",data) # delete all "\001" for each tag and replace it with "," (comma)
  rm(data)



######################################################################################################

Index <- gsub("1128=([^,]*),9=([^,]*),35=([^,]*),49=([^,]*),","",Index)
Index <- gsub(",34=([^,]*),",",",Index)
Index <- gsub(",52=([^,]*),",",",Index) # delete the record time (not the transact time)
Index <- gsub(",5799=([^,]*),",",",Index)
Index <- gsub(",268=([^,]*),",",", Index)
Index <- gsub(",279=([^,]*),",",",Index)
Index <- gsub(",48=([^,]*),",",",Index)
Index <- gsub(",37705=([^,]*),",",",Index)
Index <- gsub(",37=([^,]*),",",",Index)
Index <- gsub(",32=([^,]*),",",",Index)
Index <- gsub(",10=([^,]*),",",",Index)

OPEN <- str_subset(Index, "269=4")
SETTLE <- str_subset(Index, "269=6")
HIGH_PX <- str_subset(Index, "269=7")
LOW_PX <- str_subset(Index, "269=8")
HIGH_BID <- str_subset(Index, "269=N")
LOW_OFFER <- str_subset(Index, "269=O")
VOLUME <- str_subset(Index, "269=B")
OPEN_INT <- str_subset(Index, "269=C")
ELEC_VOLUME <- str_subset(Index, "269=e")
LIMIT <- str_subset(Index, "269=g") ##g =Threshold Limits and Price Band Variation

rm(Index)

########################## OPEN ####################################################
if(length(OPEN)!=0){

open <- str_match_all(OPEN, "269=4,55=([^,]*),83=([^,]*),270=([^,]*),286=([^,]*),")

n_row <- sapply(open, nrow)
open.info <- unlist(str_extract_all(OPEN,"75=([^,]*),60=([^,]*),"))
open.info <- str_dup(open.info, n_row)
open <- as.data.table(do.call(rbind, open))[,-1]
names(open)[c(1:4)] <- c("Code","Seq","OPEN_PX","Flag")

open[Flag==5, Flag:="IndicativeOpen"][Flag==0, Flag:="DailyOpen"]

open.info <- str_match_all(open.info, "75=([^,]*),60=([^,]*),")
open.info <- as.data.table(do.call(rbind,open.info))[,-1]
names(open.info)[c(1:2)] <- c("Date","Time")

open$Seq <- as.numeric(open$Seq)
open$PX <- as.numeric(open$PX)

open <- cbind(open.info, open)
rm(OPEN, open.info, n_row)

setkey(open, Code, Seq)

contract <- unique(open$Code)

# save the results

for (k in 1:length(contract)) {
  
  
  open.sf <- open[Code==contract[k]]
  
  if(dim(open.sf )[1]!=0){
    ## delete the file with empty size and create new folders to save data to each folders
    ## create multiple folders to save the data
    new_fol <- file.path(paste0("saving path",contract[k]))
    if(!dir.exists(new_fol)){
      dir.create(new_fol)
    }
    fname <- file.path(new_fol,
                       paste0(name[i], "_",contract[k],".rda"))
    save(open.sf, file = fname) ## save all contracts
    
  }
}

rm(open)
}
######################## SETTLE ###########################################
if(length(SETTLE)!=0){

settle <- str_match_all(SETTLE, "269=6,55=([^,]*),83=([^,]*),270=([^,]*),")

n_row <- sapply(settle, nrow)
settle.info <- unlist(str_extract_all(SETTLE,"75=([^,]*),60=([^,]*),"))
settle.info <- str_dup(settle.info, n_row)
settle <- as.data.table(do.call(rbind, settle))[,-1]
names(settle)[c(1:3)] <- c("Code","Seq","SETTLE_PX")


settle.info <- str_match_all(settle.info, "75=([^,]*),60=([^,]*),")
settle.info <- as.data.table(do.call(rbind,settle.info))[,-1]
names(settle.info)[c(1:2)] <- c("Date","Time")

settle$Seq <- as.numeric(settle$Seq)
settle$SETTLE_PX <- as.numeric(settle$SETTLE_PX)

settle <- cbind(settle.info, settle)
rm(SETTLE, settle.info, n_row)

setkey(settle, Code, Seq)

contract <- unique(settle$Code)

# save the results

for (k in 1:length(contract)) {
  
  
  settle.sf <- settle[Code==contract[k]]
  
  if(dim(settle.sf )[1]!=0){
    ## delete the file with empty size and create new folders to save data to each folders
    ## create multiple folders to save the data
    new_fol <- file.path(paste0("saving path",contract[k]))
    if(!dir.exists(new_fol)){
      dir.create(new_fol)
    }
    fname <- file.path(new_fol,
                       paste0(name[i], "_",contract[k],".rda"))
    save(settle.sf, file = fname) ## save all contracts
    
  }
}


rm(settle)
}
#################### SESSION HIGH PX #######################################
if(length(HIGH_PX)!=0){
  
high_px <- str_match_all(HIGH_PX, "269=7,55=([^,]*),83=([^,]*),270=([^,]*),")

n_row <- sapply(high_px, nrow)
high_px.info <- unlist(str_extract_all(HIGH_PX,"75=([^,]*),60=([^,]*),"))
high_px.info <- str_dup(high_px.info, n_row)
high_px <- as.data.table(do.call(rbind, high_px))[,-1]
names(high_px)[c(1:3)] <- c("Code","Seq","HIGH")


high_px.info <- str_match_all(high_px.info, "75=([^,]*),60=([^,]*),")
high_px.info <- as.data.table(do.call(rbind,high_px.info))[,-1]
names(high_px.info)[c(1:2)] <- c("Date","Time")

high_px$Seq <- as.numeric(high_px$Seq)
high_px$HIGH <- as.numeric(high_px$HIGH)

high_px <- cbind(high_px.info, high_px)
rm(HIGH_PX, high_px.info, n_row)

setkey(high_px, Code, Seq)

contract <- unique(high_px$Code)



# save the results

for (k in 1:length(contract)) {
  
  
  high_px.sf <- high_px[Code==contract[k]]
  
  if(dim(high_px.sf )[1]!=0){
    ## delete the file with empty size and create new folders to save data to each folders
    ## create multiple folders to save the data
    new_fol <- file.path(paste0("saving path",contract[k]))
    if(!dir.exists(new_fol)){
      dir.create(new_fol)
    }
    fname <- file.path(new_fol,
                       paste0(name[i], "_",contract[k],".rda"))
    save(high_px.sf, file = fname) ## save all contracts
    
  }
}
 rm(high_px)
}
############### SESSION LOW PRICE ###################################
if(length(LOW_PX)!=0){
  
low_px <- str_match_all(LOW_PX, "269=8,55=([^,]*),83=([^,]*),270=([^,]*),")

n_row <- sapply(low_px, nrow)
low_px.info <- unlist(str_extract_all(LOW_PX,"75=([^,]*),60=([^,]*),"))
low_px.info <- str_dup(low_px.info, n_row)
low_px <- as.data.table(do.call(rbind, low_px))[,-1]
names(low_px)[c(1:3)] <- c("Code","Seq","LOW")


low_px.info <- str_match_all(low_px.info, "75=([^,]*),60=([^,]*),")
low_px.info <- as.data.table(do.call(rbind,low_px.info))[,-1]
names(low_px.info)[c(1:2)] <- c("Date","Time")

low_px$Seq <- as.numeric(low_px$Seq)
low_px$LOW <- as.numeric(low_px$LOW)

low_px <- cbind(low_px.info, low_px)
rm(LOW_PX, low_px.info, n_row)

setkey(low_px, Code, Seq)

contract <- unique(low_px$Code)

# save the results

for (k in 1:length(contract)) {
  
  
  low_px.sf <- low_px[Code==contract[k]]
  
  if(dim(low_px.sf )[1]!=0){
    ## delete the file with empty size and create new folders to save data to each folders
    ## create multiple folders to save the data
    new_fol <- file.path(paste0("saving path",contract[k]))
    if(!dir.exists(new_fol)){
      dir.create(new_fol)
    }
    fname <- file.path(new_fol,
                       paste0(name[i], "_",contract[k],".rda"))
    save(low_px.sf, file = fname) ## save all contracts
    
  }
}
rm(low_px)
}
############### SESSION HIGH BID ###################################
if(length(HIGH_BID)!=0){
  
high_bid <- str_match_all(HIGH_BID, "269=N,55=([^,]*),83=([^,]*),270=([^,]*),")

n_row <- sapply(high_bid, nrow)
high_bid.info <- unlist(str_extract_all(HIGH_BID,"75=([^,]*),60=([^,]*),"))
high_bid.info <- str_dup(high_bid.info, n_row)
high_bid <- as.data.table(do.call(rbind, high_bid))[,-1]
names(high_bid)[c(1:3)] <- c("Code","Seq","HIGH_BID")


high_bid.info <- str_match_all(high_bid.info, "75=([^,]*),60=([^,]*),")
high_bid.info <- as.data.table(do.call(rbind,high_bid.info))[,-1]
names(high_bid.info)[c(1:2)] <- c("Date","Time")

high_bid$Seq <- as.numeric(high_bid$Seq)
high_bid$HIGH_BID <- as.numeric(high_bid$HIGH_BID)

high_bid <- cbind(high_bid.info, high_bid)
rm(HIGH_BID, high_bid.info, n_row)

setkey(high_bid, Code, Seq)

contract <- unique(high_bid$Code)

# save the results

for (k in 1:length(contract)) {
  
  
  high_bid.sf <- high_bid[Code==contract[k]]
  
  if(dim(high_bid.sf )[1]!=0){
    ## delete the file with empty size and create new folders to save data to each folders
    ## create multiple folders to save the data
    new_fol <- file.path(paste0("saving path",contract[k]))
    if(!dir.exists(new_fol)){
      dir.create(new_fol)
    }
    fname <- file.path(new_fol,
                       paste0(name[i], "_",contract[k],".rda"))
    save(high_bid.sf, file = fname) ## save all contracts
    
  }
}
rm(high_bid)
}
############# SESSION LOW OFFER ###############################
if(length(LOW_OFFER)!=0){
low_offer <- str_match_all(LOW_OFFER, "269=O,55=([^,]*),83=([^,]*),270=([^,]*),")

n_row <- sapply(low_offer, nrow)
low_offer.info <- unlist(str_extract_all(LOW_OFFER,"75=([^,]*),60=([^,]*),"))
low_offer.info <- str_dup(low_offer.info, n_row)
low_offer <- as.data.table(do.call(rbind, low_offer))[,-1]
names(low_offer)[c(1:3)] <- c("Code","Seq","LOW_OFFER")


low_offer.info <- str_match_all(low_offer.info, "75=([^,]*),60=([^,]*),")
low_offer.info <- as.data.table(do.call(rbind,low_offer.info))[,-1]
names(low_offer.info)[c(1:2)] <- c("Date","Time")

low_offer$Seq <- as.numeric(low_offer$Seq)
low_offer$LOW_OFFER <- as.numeric(low_offer$LOW_OFFER)

low_offer <- cbind(low_offer.info, low_offer)
rm(LOW_OFFER, low_offer.info, n_row)

setkey(low_offer, Code, Seq)

contract <- unique(low_offer$Code)

# save the results

for (k in 1:length(contract)) {
  
  
  low_offer.sf <- low_offer[Code==contract[k]]
  
  if(dim(low_offer.sf )[1]!=0){
    ## delete the file with empty size and create new folders to save data to each folders
    ## create multiple folders to save the data
    new_fol <- file.path(paste0("saving path",contract[k]))
    if(!dir.exists(new_fol)){
      dir.create(new_fol)
    }
    fname <- file.path(new_fol,
                       paste0(name[i], "_",contract[k],".rda"))
    save(low_offer.sf, file = fname) ## save all contracts
    
  }
}
rm(low_offer)
}
############# VOLUME  ###############################
if(length(VOLUME)!=0){
volume <- str_match_all(VOLUME, "269=B,55=([^,]*),83=([^,]*),271=([^,]*),")

n_row <- sapply(volume, nrow)
volume.info <- unlist(str_extract_all(VOLUME,"75=([^,]*),60=([^,]*),"))
volume.info <- str_dup(volume.info, n_row)
volume <- as.data.table(do.call(rbind, volume))[,-1]
names(volume)[c(1:3)] <- c("Code","Seq","VOLUME")


volume.info <- str_match_all(volume.info, "75=([^,]*),60=([^,]*),")
volume.info <- as.data.table(do.call(rbind,volume.info))[,-1]
names(volume.info)[c(1:2)] <- c("Date","Time")

volume$Seq <- as.numeric(volume$Seq)
volume$VOLUME <- as.numeric(volume$VOLUME)

volume <- cbind(volume.info, volume)
rm(VOLUME, volume.info, n_row)

setkey(volume, Code, Seq)

contract <- unique(volume$Code)

# save the results

for (k in 1:length(contract)) {
  
  
  volume.sf <- volume[Code==contract[k]]
  
  if(dim(volume.sf )[1]!=0){
    ## delete the file with empty size and create new folders to save data to each folders
    ## create multiple folders to save the data
    new_fol <- file.path(paste0("saving path",contract[k]))
    if(!dir.exists(new_fol)){
      dir.create(new_fol)
    }
    fname <- file.path(new_fol,
                       paste0(name[i], "_",contract[k],".rda"))
    save(volume.sf, file = fname) ## save all contracts
    
  }
}
rm(volume)
}
############# OPEN INTEREST  ###############################
if(length(OPEN_INT)!=0){
open_int <- str_match_all(OPEN_INT, "269=C,55=([^,]*),83=([^,]*),271=([^,]*),")

n_row <- sapply(open_int, nrow)
open_int.info <- unlist(str_extract_all(OPEN_INT,"75=([^,]*),60=([^,]*),"))
open_int.info <- str_dup(open_int.info, n_row)
open_int <- as.data.table(do.call(rbind, open_int))[,-1]
names(open_int)[c(1:3)] <- c("Code","Seq","OPEN_INT")


open_int.info <- str_match_all(open_int.info, "75=([^,]*),60=([^,]*),")
open_int.info <- as.data.table(do.call(rbind,open_int.info))[,-1]
names(open_int.info)[c(1:2)] <- c("Date","Time")

open_int$Seq <- as.numeric(open_int$Seq)
open_int$OPEN_INT <- as.numeric(open_int$OPEN_INT)

open_int <- cbind(open_int.info, open_int)
rm(OPEN_INT, open_int.info, n_row)

setkey(open_int, Code, Seq)

contract <- unique(open_int$Code)

# save the results

for (k in 1:length(contract)) {
  
  
  open_int.sf <- open_int[Code==contract[k]]
  
  if(dim(open_int.sf )[1]!=0){
    ## delete the file with empty size and create new folders to save data to each folders
    ## create multiple folders to save the data
    new_fol <- file.path(paste0("saving path",contract[k]))
    if(!dir.exists(new_fol)){
      dir.create(new_fol)
    }
    fname <- file.path(new_fol,
                       paste0(name[i], "_",contract[k],".rda"))
    save(open_int.sf, file = fname) ## save all contracts
    
  }
}
rm(open_int)
}
############# ELECTRONIC VOLUME  ###############################
if(length(ELEC_VOLUME)!=0){
elec_volume <- str_match_all(ELEC_VOLUME, "269=e,55=([^,]*),83=([^,]*),271=([^,]*),")

n_row <- sapply(elec_volume, nrow)
elec_volume.info <- unlist(str_extract_all(ELEC_VOLUME,"75=([^,]*),60=([^,]*),"))
elec_volume.info <- str_dup(elec_volume.info, n_row)
elec_volume <- as.data.table(do.call(rbind, elec_volume))[,-1]
names(elec_volume)[c(1:3)] <- c("Code","Seq","ELEC_VOLUME")


elec_volume.info <- str_match_all(elec_volume.info, "75=([^,]*),60=([^,]*),")
elec_volume.info <- as.data.table(do.call(rbind,elec_volume.info))[,-1]
names(elec_volume.info)[c(1:2)] <- c("Date","Time")

elec_volume$Seq <- as.numeric(elec_volume$Seq)
elec_volume$ELEC_VOLUME <- as.numeric(elec_volume$ELEC_VOLUME)

elec_volume <- cbind(elec_volume.info, elec_volume)
rm(ELEC_VOLUME, elec_volume.info, n_row)

setkey(elec_volume, Code, Seq)

contract <- unique(elec_volume$Code)

# save the results

for (k in 1:length(contract)) {
  
  
  elec_volume.sf <- elec_volume[Code==contract[k]]
  
  if(dim(elec_volume.sf )[1]!=0){
    ## delete the file with empty size and create new folders to save data to each folders
    ## create multiple folders to save the data
    new_fol <- file.path(paste0("saving path",contract[k]))
    if(!dir.exists(new_fol)){
      dir.create(new_fol)
    }
    fname <- file.path(new_fol,
                       paste0(name[i], "_",contract[k],".rda"))
    save(elec_volume.sf, file = fname) ## save all contracts
    
  }
}
rm(elec_volume)
}
############# LIMIT  ###############################
if(length(LIMIT)!=0){
## 1148 
## Allowable low limit price for the trading day.  orders submitted with px below this will be rejected 
##  1149
## Allowable high limit price for the trading day.  orders submitted with px above this will be rejected
## 1143 	MaxPriceVariation for price banding


limit <- str_match_all(LIMIT, "269=g,55=([^,]*),83=([^,]*),1149=([^,]*),1148=([^,]*),1143=([^,]*),")

n_row <- sapply(limit, nrow)
limit.info <- unlist(str_extract_all(LIMIT,"75=([^,]*),60=([^,]*),"))
limit.info <- str_dup(limit.info, n_row)
limit <- as.data.table(do.call(rbind, limit))[,-1]
names(limit)[c(1:5)] <- c("Code","Seq","high_limit", "low_limit", "variation")


limit.info <- str_match_all(limit.info, "75=([^,]*),60=([^,]*),")
limit.info <- as.data.table(do.call(rbind,limit.info))[,-1]
names(limit.info)[c(1:2)] <- c("Date","Time")

limit$Seq <- as.numeric(limit$Seq)
limit$high_limit <- as.numeric(limit$high_limit)
limit$low_limit <- as.numeric(limit$low_limit)
limit$variation <- as.numeric(limit$variation)

limit <- cbind(limit.info, limit)
rm(LIMIT, limit.info, n_row)

setkey(limit, Code, Seq)

contract <- unique(limit$Code)

# save the results

for (k in 1:length(contract)) {
  
  
  limit.sf <- limit[Code==contract[k]]
  
  if(dim(limit.sf )[1]!=0){
    ## delete the file with empty size and create new folders to save data to each folders
    ## create multiple folders to save the data
    new_fol <- file.path(paste0("saving path",contract[k]))
    if(!dir.exists(new_fol)){
      dir.create(new_fol)
    }
    fname <- file.path(new_fol,
                       paste0(name[i], "_",contract[k],".rda"))
    save(limit.sf, file = fname) ## save all contracts
    
  }
}
rm(limit)
}
}
}





    
    

    
