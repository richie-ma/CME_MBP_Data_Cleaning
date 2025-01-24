## trade summary


setwd("R:/_RawData/MD/MD_Corn_Fut")

rm(list=ls())

library(data.table)
library(stringr)

## batching
name <- list.files(pattern = "MD_xcbt_zc_fut_eth")
date <-  as.Date(substr(name,1,8), "%Y%m%d")

for (i in 1:length(name)){
  print(name[i])
  MDP <- name[i]
  input <- MDP
  data <-  fread(input, header = F)[[1L]]
  
  if(date[i] < "2015-11-20"){
    ## loop starts##  
    #test <- readLines("C:/Users/ruchuan2/Box/Corn/xcbt_md_zc_fut_20180402-r-00074")
    #Index <- gsub("\001",",",test) # delete all "\001" for each tag and replace it with "," (comma)
    #rm(test)
    
    Index <- gsub("\001",",",data) # delete all "\001" for each tag and replace it with "," (comma)
    rm(data)
    
    ## delete the tags that are not necessary
    Index <- gsub("1128=([^,]*),9=([^,]*),35=([^,]*),49=([^,]*),34=([^,]*),","",Index)
    Index <- gsub(",5799=([^,]*),",",",Index)
    Index <- gsub(",268=([^,]*),",",", Index)
    Index <- gsub(",279=([^,]*),",",",Index)
    Index <- gsub(",22=([^,]*),",",",Index)
    Index <- gsub(",48=([^,]*),",",",Index)
    Index <- gsub(",273=([^,]*),",",",Index)
    Index <- gsub(",274=([^,]*),",",",Index)
    Index <- gsub(",451=([^,]*),",",",Index)
    Index <- gsub(",1003=([^,]*),",",",Index)
    Index <- gsub(",1020=([^,]*),",",",Index)
    Index <- gsub(",277=([^,]*),",",",Index)
    
    
    ## searching for the following tags, 
    ## tag52-time stamp
    ## tag75-date
    #  tag83-seq#
    #  tag107-contract
    #  tag269-type: 2-trade
    #  tag270-px
    #  tag271-qty
    
    ## generate the trade data (with order flow)
    ## first find the tag269=2
    ## Trade summary pattern
    ## 75 60 269=2 55 270 271 346 5797 37711-trade id 37705 37 32
    
    Trade <- str_subset(Index, "269=2")
    Trade <- gsub(",269=2,",",",Trade)
    rm(Index)
    
    if(length(Trade)!=0){
      
      ## generate the trade data (without order flow)
      trade <- str_match_all(Trade, "83=([^,]*),107=([^,]*),270=([^,]*),271=([^,]*),")
      n_row <- sapply(trade, nrow)
      trade.info <- unlist(str_extract_all(Trade,"52=([^,]*),75=([^,]*),"))
      trade.info <- str_dup(trade.info, n_row)
      trade <- as.data.table(do.call(rbind, trade))[,-1]
      names(trade)[c(1:4)] <- c("Seq","Code","PX","Size")
      
      trade.info <- str_match_all(trade.info, "52=([^,]*),75=([^,]*),")
      trade.info <- as.data.table(do.call(rbind,trade.info))[,-1]
      names(trade.info)[c(1:2)] <- c("Time","Date")
      
      trade.info$Date <- as.Date(trade.info$Date, "%Y%m%d")
      #Time <- paste(substr(trade.info$Time,1,8),substr(trade.info$Time,9,10),substr(trade.info$Time,11,12),
                    #substr(trade.info$Time,13,14),substr(trade.info$Time,15,23),sep=".")
      #trade.info$Time <- as.POSIXct(Time, tz="UTC", "%Y%m%d.%H.%M.%OS") ## original time is UTC
      #attr(trade.info$Time, "tzone") <- "America/Chicago" ## US central time
      #trade$MsgSeq <- as.numeric(trade$MsgSeq)
      trade$Seq <- as.numeric(trade$Seq)
      trade$PX <- as.numeric(trade$PX)
      trade$Size <- as.numeric(trade$Size)
      
      trade <- cbind(trade.info, trade)
      rm(trade.info)
      
      ## how many contracts are transacted today
      contract <- unique(trade$Code)
      
      ## save the transaction data of each single contract
      for (j in 1:length(contract)) {
        
        print(contract[j])
        
        Trade.sf <- subset(trade, trade$Code==contract[j])
        
        if(dim(Trade.sf)[1]!=0){
          
          ## delete the file with empty size and create new folders to save data to each folders
          ## create multiple folders to save the data
          new_fol <- file.path(paste0("C:/Users/ruchuan2/Box/Corn/transaction","/",contract[j]))
          if(!dir.exists(new_fol)){
            dir.create(new_fol)
          }
          fname <- file.path(new_fol,
                             paste0(name[i], "_",contract[j],".rda"))
          save(Trade.sf, file = fname) ## save all contracts
          
        }
      }
    }
  } else{
    
    #test <- readLines("C:/Users/user/Box/Corn/xcbt_md_zc_fut_20151209_r_00097")
    #Index <- gsub("\001",",",test) # delete all "\001" for each tag and replace it with "," (comma)
    #rm(test)
    
    Index <- str_subset(data, "\001269=2")
    Index <- str_replace_all(Index, "\001",",") 
    Trade <- str_replace_all(Index,",269=2,",",")
    rm(Index)
    
    # delete all "\001" for each tag and replace it with "," (comma)
    rm(data)
    
    ## delete the tags that are not necessary
    
    Trade <- str_replace_all(Trade, "1128=([^,]*),9=([^,]*),35=([^,]*),49=([^,]*),","")
    Trade <- str_replace_all(Trade, ",52=([^,]*),",",") # delete the record time (not the transact time)
    Trade <- str_replace_all(Trade, ",5799=([^,]*),",",")
    Trade <- str_replace_all(Trade, ",268=([^,]*),",",")
    Trade <- str_replace_all(Trade, ",279=([^,]*),",",")
    Trade <- str_replace_all(Trade, ",48=([^,]*),",",")
    Trade <- str_replace_all(Trade, ",37705=([^,]*),",",")
    Trade <- str_replace_all(Trade, "37=([^,]*),","")
    Trade <- str_replace_all(Trade, "32=([^,]*),","")
    
    ## searching for the following tags, 
    ## tag52-time stamp
    ## tag75-date
    #  tag83-seq#
    #  tag55-contract
    #  tag269-type: 2-trade
    #  tag270-px
    #  tag271-qty
    #  tag346-# of orders in a given px level
    #  tag5797-buyer/seller initiated
    #  tag33705-order entries
    
    ## generate the trade data (with order flow)
    ## consider the tradings with both specific defined aggressors or non-defined aggressors (implied order or sides are not defined)
    ## tag5797=0 or tag5797=1 or tag5797=2
    ## first find the tag269=2
    ## Trade summary pattern
    ## 75 60 269=2 55 270 271 346 5797 37711-trade id 37705 37 32

    
    if(length(Trade)!=0){
      
      ## generate the trade data (without order flow)
      trade <- str_match_all(Trade, "55=([^,]*),83=([^,]*),270=([^,]*),271=([^,]*),346=([^,]*),5797=([^,]*)")
      n_row <- sapply(trade, nrow)
      trade.info <- unlist(str_extract_all(Trade,"75=([^,]*),34=([^,]*),60=([^,]*),"))
      trade.info <- str_dup(trade.info, n_row)
      trade <- as.data.table(do.call(rbind, trade))[,-1]
      names(trade)[c(1:6)] <- c("Code","Seq","PX","Size","Ord","agg")
      
      trade.info <- str_match_all(trade.info, "75=([^,]*),34=([^,]*),60=([^,]*),")
      trade.info <- as.data.table(do.call(rbind,trade.info))[,-1]
      names(trade.info)[c(1:3)] <- c("Date","MsgSeq", "Time")
      
      trade.info$Date <- as.Date(trade.info$Date, "%Y%m%d")
      #Time <- paste(substr(trade.info$Time,1,8),substr(trade.info$Time,9,10),substr(trade.info$Time,11,12),
       #             substr(trade.info$Time,13,14),substr(trade.info$Time,15,23),sep=".")
      #trade.info$Time <- as.POSIXct(Time, tz="UTC", "%Y%m%d.%H.%M.%OS") ## original time is UTC
      #attr(trade.info$Time, "tzone") <- "America/Chicago" ## US central time
      #trade$MsgSeq <- as.numeric(trade$MsgSeq)
      trade$Seq <- as.numeric(trade$Seq)

      trade$PX <- as.numeric(trade$PX)
      trade$Size <- as.numeric(trade$Size)
      trade$Ord <- as.numeric(trade$Ord)
      trade$agg <- as.numeric(trade$agg)
      
      trade <- cbind(trade.info, trade)
      trade$MsgSeq <- as.numeric(trade$MsgSeq)
      rm(Time, trade.info)
      
      ## how many contracts are transacted today
      contract <- unique(trade$Code)
      
      setkey(trade, Code, Seq)
      
      # save the results
      
      for (k in 1:length(contract)) {
        
        #print(contract[k])
        
        Trade.sf <- subset(trade, trade$Code==contract[k])
        
        if(dim(Trade.sf)[1]!=0){
          ## delete the file with empty size and create new folders to save data to each folders
          ## create multiple folders to save the data
          new_fol <- file.path(paste0("C:/Users/ruchuan2/Box/Corn/transaction","/",contract[k]))
          if(!dir.exists(new_fol)){
            dir.create(new_fol)
          }
          fname <- file.path(new_fol,
                             paste0("xcbt_md_zc_fut_",substr(name[i], 1,8),"_", "r","_",sample(100000,1, replace = F), "_",contract[k],".rda"))
          save(Trade.sf, file = fname) ## save all contracts
          
        }
      }
    }  
  }
}
