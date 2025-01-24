## this file is used to clean the limit order book

setwd("C:/Users/ruchuan2/Box/Corn")

rm(list=ls())

library(data.table)
library(lubridate)
library(dplyr)
library(stringr)


## batching
## there are two versions of MDP data, one is FIX and another is MBP
## MBP starts on 11/20/2015

name <- list.files(pattern = "MD")
date <- as.Date(str_sub(name,1,8),"%Y%m%d")

Date <- list()
week_seq <- c()
for (i in 339:339){
  print(name[i])
  file <- name[i]
  #date <- as.Date(str_sub(name[i],1,8),"%Y%m%d")
  date <- as.Date(str_sub(name[i],1,8),"%Y%m%d")
  Date[[i]] <- as.Date(str_sub(name[i],1,8),"%Y%m%d")
  
  input <- file
  data <- readLines(input)
  
  week_seq[i] <- sample(10000:99999,1,replace = F)
  ## loop starts
  ## test data
  
  #test <- readLines("C:/Users/ruchuan2/Box/live cattle/xcme_md_le_fut_20160329_r_00441")
  #Index <- gsub("\001",",",test) # delete all "\001" for each tag and replace it with "," (comma)
  #rm(test)
  
  Index <- gsub("\001",",",data) # delete all "\001" for each tag and replace it with "," (comma)
  rm(data)
  
  ## let R know whether it is FIX or MDP data
  ## For the FIX data  
  
  if(date < as.Date("2015-11-20", "%Y-%m-%d")){
    Index <- gsub(".*(.*)(35=[^X])(.*)*.","",Index) ## delete 35 is not equal to X
    Index <- gsub("1128=([^,]*),9=([^,]*),35=([^,]*),49=([^,]*),","",Index)
    Index <- gsub(",268=([^,]*),",",", Index)
    Index <- gsub(",22=([^,]*),",",", Index)
    Index <- gsub(",48=([^,]*),",",",Index)
    Index <- gsub(",273=([^,]*),",",",Index)
    Index <- gsub(",336=([^,]*),",",",Index)
    Index <- gsub(",10=([^,]*),",",",Index)
    Index <- gsub(",5797=([^,]*),",",",Index)
    
    ##----------------------------------------LOB cleaning-------------------------------------
    ###################################### outright orders only since they have the info of number of orders#################################
    ## we should find the following items
    ## in terms of add tag52 tag75 tag279=0(add) or 1(change) or 2(delete), tag83(sequence), tag107 (Code) tag269=0(bid) or 1(ask), tag270(PX),271(Qty),346(# of orders),1023(PX_depth)
    if(length(str_subset(Index, "269=[01]"))!=0){
      
      
      part1 <- str_subset(Index, "279=[012],83=([^,]*),107=([^,]*),269=([01]*),") ## find both tag269=0 and tag269=1
      part1_dt <- str_extract_all(part1,"34=([^,]*),52=([^,]*),75=([^,]*),")
      part1_info <- str_match_all(part1,"279=([^,]*),83=([^,]*),107=([^,]*),269=([^,]*),270=([^,]*),271=([^,]*),346=([^,]*),1023=([^,]*),")
      n_row_part1_info <- sapply(part1_info, nrow)
      part1_dt <- str_dup(part1_dt,n_row_part1_info)
      part1_dt <- str_match_all(part1_dt, "34=([^,]*),52=([^,]*),75=([^,]*),")
      part1_dt <- as.data.table(do.call(rbind, part1_dt))[,-1]
      part1_info <- as.data.table(do.call(rbind,part1_info))[,-1]
      message <- cbind(part1_dt, part1_info)
      names(message)[c(1:11)] <- c("MsgSeq","Time","Date","Update","Seq","Code","bidask","PX","Qty","Ord","PX_depth")
      message[, RK := "R"]
      message <- relocate(message, MsgSeq, .after=RK)
      
    }
    ####################################### Implied orders only #################################################
    #We should consider the implied order actually since it affects the limit order book
    ## we should find the following items
    ## in terms of add 279=0(add) or 1(change) or 2(delete), 269=E(implied bid) or 1(implied ask), 83(sequence),270(PX),271(Qty),346(# of orders),1023(PX_depth)
    if(length(str_subset(Index, "276=K"))!=0){
      
      part2 <- str_subset(Index, "276=K") ## find both tag269=0 and tag269=1
      part2_dt <- str_extract_all(part2,"34=([^,]*),52=([^,]*),75=([^,]*),")
      part2_info <- str_match_all(part2,"279=([^,]*),83=([^,]*),107=([^,]*),269=([^,]*),270=([^,]*),271=([^,]*),276=([^,]*),1023=([^,]*),")
      n_row_part2_info <- sapply(part2_info, nrow)
      part2_dt <- str_dup(part2_dt,n_row_part2_info)
      part2_dt <- str_match_all(part2_dt, "34=([^,]*),52=([^,]*),75=([^,]*),")
      part2_dt <- as.data.table(do.call(rbind, part2_dt))[,-1]
      part2_info <- as.data.table(do.call(rbind,part2_info))[,-1]
      message_2 <- cbind(part2_dt, part2_info)
      names(message_2)[c(1:11)] <- c("MsgSeq","Time","Date","Update","Seq","Code","bidask","PX","Qty","RK","PX_depth")
      message_2 <- relocate(message_2, MsgSeq, .after=RK)
      message_2[, Ord:= 0]
      
      message_all <- rbind(message, message_2, fill=TRUE)
      
      message_all$MsgSeq <- as.numeric(message_all$MsgSeq)
      message_all$Update <- as.numeric(message_all$Update)
      message_all$Seq <- as.numeric(message_all$Seq)
      message_all$PX <- as.numeric(message_all$PX)
      message_all$Qty <- as.numeric(message_all$Qty)
      message_all$Ord <- as.numeric(message_all$Ord)
      message_all$PX_depth <- as.numeric(message_all$PX_depth)
      setkey(message_all, Code, Seq)
      
    } else{
      if(class(message)[1]=="data.table"){
        
        message_all <- message
        
      }
    }
    rm(message,message_2,part1_dt,part1_info,part2_dt,part2_info,Index,n_row_part1_info,n_row_part2_info,part1,part2)
  }else{
    ## remove the unnecessary tags
    
    Index <- gsub(".*(.*)(35=[^X])(.*)*.","",Index) ## delete 35 is not equal to X
    Index <- gsub("1128=([^,]*),9=([^,]*),35=([^,]*),49=([^,]*),","",Index)
    Index <- gsub(",52=([^,]*),",",",Index) # delete the record time (not the transact time)
    Index <- gsub(",5799=([^,]*),",",",Index)
    Index <- gsub(",268=([^,]*),",",", Index)
    Index <- gsub(",48=([^,]*),",",",Index)
    Index <- gsub(",10=([^,]*),",",",Index)
    ##----------------------------------------LOB cleaning-------------------------------------
    ###################################### outright orders only since they have the info of number of orders#################################
    ## we should find the following items
    ## in terms of add 279=0(add) or 1(change) or 2(delete), 269=0(bid) or 1(ask), 83(sequence),270(PX),271(Qty),346(# of orders),1023(PX_depth)
    if(length(str_subset(Index, "269=[01]"))!=0){
      
      part1 <- str_subset(Index, "279=[012],269=([01]*),") ## find both tag269=0 and tag269=1
      part1_dt <- str_extract_all(part1,"75=([^,]*),34=([^,]*),60=([^,]*),")
      part1_info <- str_match_all(part1,"279=([^,]*),269=([^,]*),55=([^,]*),83=([^,]*),270=([^,]*),271=([^,]*),346=([^,]*),1023=([^,]*),")
      n_row_part1_info <- sapply(part1_info, nrow)
      part1_dt <- str_dup(part1_dt,n_row_part1_info)
      part1_dt <- str_match_all(part1_dt, "75=([^,]*),34=([^,]*),60=([^,]*),")
      part1_dt <- as.data.table(do.call(rbind, part1_dt))[,-1]
      part1_info <- as.data.table(do.call(rbind,part1_info))[,-1]
      message <- cbind(part1_dt, part1_info)
      names(message)[c(1:11)] <- c("Date","MsgSeq","Time","Update","bidask","Code","Seq","PX","Qty","Ord","PX_depth")
      message[, RK:="R"]
      message <- relocate(message, MsgSeq, .after=RK)
      message <- relocate(message, Time, .before=Date)
      
    }
    ####################################### Implied orders only #################################################
    #We should conisder the implied order actually since it affects the limit order book
    ## we should find the following items
    ## in terms of add 279=0(add) or 1(change) or 2(delete), 269=E(implied bid) or 1(implied ask), 83(sequence),270(PX),271(Qty),346(# of orders),1023(PX_depth)
    if(length(str_subset(Index, "279=[012],269=([EF]*),"))!=0){
      
      part2 <- str_subset(Index, "279=[012],269=([EF]*),") ## find both tag269=0 and tag269=1
      part2_dt <- str_extract_all(part2,"75=([^,]*),34=([^,]*),60=([^,]*),")
      part2_info <- str_match_all(part2,"279=([^,]*),269=([^,]*),55=([^,]*),83=([^,]*),270=([^,]*),271=([^,]*),1023=([^,]*),")
      n_row_part2_info <- sapply(part2_info, nrow)
      part2_dt <- str_dup(part2_dt,n_row_part2_info)
      part2_dt <- str_match_all(part2_dt, "75=([^,]*),34=([^,]*),60=([^,]*),")
      part2_dt <- as.data.table(do.call(rbind, part2_dt))[,-1]
      part2_info <- as.data.table(do.call(rbind,part2_info))[,-1]
      message_2 <- cbind(part2_dt, part2_info)
      names(message_2)[c(1:10)] <- c("Date","MsgSeq","Time","Update","bidask","Code","Seq","PX","Qty","PX_depth") 
      message_2[, RK:="K"]
      message_2[, Ord:=0]
      message_2 <- relocate(message_2, Ord, .before=RK)
      message_2 <- relocate(message_2, MsgSeq, .after=RK)
      message_2 <- relocate(message_2, Time, .before=Date)
      
      message_all <- rbind(message, message_2, fill=TRUE)
      
      
      message_all$MsgSeq <- as.numeric(message_all$MsgSeq)
      message_all$Update <- as.numeric(message_all$Update)
      message_all$Seq <- as.numeric(message_all$Seq)
      message_all$PX <- as.numeric(message_all$PX)
      message_all$Qty <- as.numeric(message_all$Qty)
      message_all$PX_depth <- as.numeric(message_all$PX_depth)
      
      setkey(message_all, Code, Seq)
    } else{
      if(class(message)[1]=="data.table"){
        
        message_all <- message
        
      }
      
      
    }
    rm(message,message_2,part1_dt,part1_info,part2_dt,part2_info,Index,n_row_part1_info,n_row_part2_info,part1,part2)
  }
  
  
  ######################### extract message of each futures contract and save the results
  if(exists("message_all")==TRUE){
    
    contract <- unique(message_all$Code)
    
    
    
    for (j in 1:length(contract)) {
      print(contract[j])
      message_sf <- subset(message_all, message_all$Code==contract[j])
      
      #save the message data of each single contract
      
      ## delete the file with empty size and create new folders to save data to each folders
      ## create multiple folders to save the data
      if(dim(message_sf)[1]!=0){
        new_fol <- file.path(paste0("C:/Users/ruchuan2/Box/Corn message","/",contract[j]))
        
        if(!dir.exists(new_fol)){
          dir.create(new_fol)
        }
        
        
        if(i==339){
        fname <- file.path(new_fol,
                           paste0( "xcbt_md_zc_fut_", substr(name[i], 1, 8), "_r_", "48879", "_",contract[j],".rda"))
        } else{
          
          if(epiweek(date)==epiweek(Date[[i-1]])){
            week_seq1 <- week_seq[i-1]
            week_seq[i] <- week_seq[i-1]
            fname <- file.path(new_fol,
                               paste0("xcbt_md_zc_fut_", substr(name[i], 1, 8), "_r_", week_seq1, "_",contract[j],".rda"))
            
          } else{
            
            fname <- file.path(new_fol,
                               paste0("xcbt_md_zc_fut_", substr(name[i], 1, 8), "_r_", week_seq[i], "_",contract[j],".rda"))
            
          }
          
          
        }
        
        save(message_sf, file = fname) ## save all contracts
      }
    }
  }
}



