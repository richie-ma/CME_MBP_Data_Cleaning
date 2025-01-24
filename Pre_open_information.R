##########################################################################
## This file is to know the matched quantities through the pre-open auction
## Opening period 35=f, 326=15
## open period 35=f, 326=17
## Richie R. Ma, ruchuan2@illinois.edu
##############################################################################


rm(list=ls())

library(data.table)
library(stringr)
library(lubridate)

name <- list.files(pattern = "xcbt_md_zc_")
date <-  as.Date(substr(name,16,23), "%Y%m%d")


for (i in 1:length(name)){
  print(name[i])
  MDP <- name[i]
  input <- MDP
  data <-  fread(input, header = F)[[1L]]
  
  # delete all "\001" for each tag and replace it with "," (comma)
  
  
  
  
  if(date[i] < "2015-11-20"){
    
    ### One need to find the continuous trading session and the opening match sessions by different securities 
    
    
    Index <- str_subset(data, "\001336=([^,]*)")
    
    Index <- str_replace_all(Index,"\001",",")
    
    
    Index <- str_replace_all(Index, "1128=([^,]*),9=([^,]*),35=([^,]*),49=([^,]*),","")
    Index <- str_replace_all(Index, "34=([^,]*),","")
    
    
    Index <- str_replace_all(Index, ",268=([^,]*),279=([^,]*),22=([^,]*),48=([^,]*),",",")
    Index <- str_replace_all(Index, ",10=([^,]*),","")
    Index <- str_replace_all(Index, ",269=([^,]*),270=([^,]*),271=([^,]*),273=([^,]*)","")
    Index <- str_replace_all(Index, ",276=([^,]*)","")
    
    if(length(Index)!=0){
      
      Index1 <- str_match_all(Index, "83=([^,]*),107=([^,]*),336=([^,]*),")
      n_row <- sapply(Index1, nrow)

      
      Index1 <- as.data.table(do.call(rbind, Index1))[,-1]
      
      names(Index1)[c(1:3)] <- c("Seq","Code","SessionID")
      
      Index1$Seq <- as.numeric(Index1$Seq)
      Index1$SessionID <- as.numeric(Index1$SessionID)
      
      
      Index1.info <- unlist(str_extract_all(Index,"52=([^,]*),75=([^,]*),"))
      Index1.info <- str_dup(Index1.info, n_row)
      
      Index1.info <- str_match_all(Index1.info, "52=([^,]*),75=([^,]*),")
      Index1.info <- as.data.table(do.call(rbind,Index1.info))[,-1]
      names(Index1.info)[c(1:2)] <- c("Time","Date")
      
      Index1 <- cbind(Index1.info, Index1)
      rm(Index1.info)
      
      Index1[, SessionID:=fifelse(SessionID==0, "preopen", fifelse(SessionID==1, "opening", "continuous"))]
      # Index1[,SessionID:=fifelse(SessionID=="preopen" & shift(SessionID, 1, "lag", fill=NA)=="continuous", "preopen2", SessionID), by=.(Code)]
      
      Index1[, Time:=as.POSIXct(Time, "GMT", "%Y%m%d%H%M%S%OS")]
      attr(Index1$Time, "tzone") <- "America/Chicago"
      Index1[, date:=date(Time)]
      Index1[, Date:=as.Date(Date, "%Y%m%d")]
      
      Index1[,hour:=hour(Time)]
      
      Index1 <- Index1[!is.na(Time)]
      
      Index1 <- Index1[!(hour>=13 & Date==date & SessionID=="preopen")]
      
      Index1 <- Index1[Date==date & SessionID=="preopen", SessionID:="preopen2"]
      
      
      Index1[, end_seq:=Seq[.N], by=.(date, Code,SessionID)]
      
      Index1 <- Index1[, .SD[1], by=.(Code,date,  SessionID)]
      
      Session_info <- Index1
      
      setkey(Session_info, Code)
      
      Session_info[hour==8 & SessionID=="opening", SessionID:="opening2"]
      
    }
    
    save(Session_info, file=paste0("saving path",date[i], ".rda"))
    
    ### simulated buy and sell
    
    Buy <- str_subset(data, "\001269=F")
    Sell <- str_subset(data, "\001269=E")
    
    if(length(Buy)!=0){
    
    Buy <- str_replace_all(Buy,"\001",",")
    
    Buy <- str_replace_all(Buy, "1128=([^,]*),9=([^,]*),35=([^,]*),49=([^,]*),","")
    Buy <- str_replace_all(Buy, "34=([^,]*),","")
    Buy <- str_replace_all(Buy, ",268=([^,]*),",",")
    Buy <- str_replace_all(Buy, ",22=([^,]*),",",")
    Buy <- str_replace_all(Buy, ",48=([^,]*),",",")
    Buy <- str_replace_all(Buy, ",273=([^,]*),",",")
    
    Buy <- str_replace_all(Buy, ",286=([^,]*),",",")
    Buy <- str_replace_all(Buy, ",10=([^,]*),",",")
    
    buy <- str_match_all(Buy, "279=([^,]*),83=([^,]*),107=([^,]*),269=F,270=([^,]*),")
    n_row <- sapply(buy, nrow)
    Buy.info <- unlist(str_extract_all(Buy,"52=([^,]*),75=([^,]*),"))
    Buy.info <- str_dup(Buy.info, n_row)
    buy <- as.data.table(do.call(rbind, buy))[,-1]
    names(buy)[c(1:4)] <- c("Update","Seq","Code","PX")
    
    Buy.info <- str_match_all(Buy.info, "52=([^,]*),75=([^,]*),")
    Buy.info <- as.data.table(do.call(rbind,Buy.info))[,-1]
    names(Buy.info)[c(1:2)] <- c("Time","Date")
    
    Buy.info$Date <- as.Date(Buy.info$Date, "%Y%m%d")
    
    Buy1 <- cbind( Buy.info, buy)
    rm(Buy.info, buy)
    
    
    
    buy2 <- str_match_all(Buy, "279=([^,]*),83=([^,]*),107=([^,]*),269=F,270=([^,]*),271=([^,]*),")
    n_row <- sapply(buy2, nrow)
    Buy.info2 <- unlist(str_extract_all(Buy,"52=([^,]*),75=([^,]*),"))
    Buy.info2 <- str_dup(Buy.info2, n_row)
    buy2 <- as.data.table(do.call(rbind, buy2))[,-1]
    names(buy2)[c(1:5)] <- c("Update","Seq","Code","PX", "Qty")
    
    Buy.info2 <- str_match_all(Buy.info2, "52=([^,]*),75=([^,]*),")
    Buy.info2 <- as.data.table(do.call(rbind,Buy.info2))[,-1]
    names(Buy.info2)[c(1:2)] <- c("Time","Date")
    
    Buy.info2$Date <- as.Date(Buy.info2$Date, "%Y%m%d")
    
    Buy2 <- cbind( Buy.info2, buy2)
    rm(Buy.info2, buy2)
    
    setkey(Buy2, Code, Seq)
    setkey(Buy1, Code, Seq)
    
    Buy2.2 <- Buy2[, -c("Qty")]
    
    Buy <- rbind(Buy1, Buy2.2)
    
    Buy <- unique(Buy)
    
    Buy <- merge(Buy, Buy2[, .(Seq, Code, Qty)], by=c("Seq", "Code"), all=TRUE)
    setkey(Buy, Code, Seq)
    
    Buy[, sim:="Buy"]
    
    rm(Buy1, Buy2, Buy2.2)
    
    }
    
    #################### simulated sell #######################
    
    if(length(Sell)!=0){
    
    Sell <- str_replace_all(Sell,"\001",",")
    
    Sell <- str_replace_all(Sell, "1128=([^,]*),9=([^,]*),35=([^,]*),49=([^,]*),","")
    Sell <- str_replace_all(Sell, "34=([^,]*),","")
    Sell <- str_replace_all(Sell, ",268=([^,]*),",",")
    Sell <- str_replace_all(Sell, ",22=([^,]*),",",")
    Sell <- str_replace_all(Sell, ",48=([^,]*),",",")
    Sell <- str_replace_all(Sell, ",273=([^,]*),",",")
    
    Sell <- str_replace_all(Sell, ",286=([^,]*),",",")
    Sell <- str_replace_all(Sell, ",10=([^,]*),",",")
    
    sell <- str_match_all(Sell, "279=([^,]*),83=([^,]*),107=([^,]*),269=E,270=([^,]*),")
    n_row <- sapply(sell, nrow)
    Sell.info <- unlist(str_extract_all(Sell,"52=([^,]*),75=([^,]*),"))
    Sell.info <- str_dup(Sell.info, n_row)
    sell <- as.data.table(do.call(rbind, sell))[,-1]
    names(sell)[c(1:4)] <- c("Update","Seq","Code","PX")
    
    Sell.info <- str_match_all(Sell.info, "52=([^,]*),75=([^,]*),")
    Sell.info <- as.data.table(do.call(rbind,Sell.info))[,-1]
    names(Sell.info)[c(1:2)] <- c("Time","Date")
    
    Sell.info$Date <- as.Date(Sell.info$Date, "%Y%m%d")
    
    Sell1 <- cbind( Sell.info, sell)
    rm(Sell.info, sell)
    
    
    
    sell2 <- str_match_all(Sell, "279=([^,]*),83=([^,]*),107=([^,]*),269=E,270=([^,]*),271=([^,]*),")
    n_row <- sapply(sell2, nrow)
    Sell.info2 <- unlist(str_extract_all(Sell,"52=([^,]*),75=([^,]*),"))
    Sell.info2 <- str_dup(Sell.info2, n_row)
    sell2 <- as.data.table(do.call(rbind, sell2))[,-1]
    names(sell2)[c(1:5)] <- c("Update","Seq","Code","PX", "Qty")
    
    Sell.info2 <- str_match_all(Sell.info2, "52=([^,]*),75=([^,]*),")
    Sell.info2 <- as.data.table(do.call(rbind,Sell.info2))[,-1]
    names(Sell.info2)[c(1:2)] <- c("Time","Date")
    
    Sell.info2$Date <- as.Date(Sell.info2$Date, "%Y%m%d")
    
    Sell2 <- cbind( Sell.info2, sell2)
    rm(Sell.info2, sell2)
    
    setkey(Sell2, Code, Seq)
    setkey(Sell1, Code, Seq)
    
    Sell2.2 <- Sell2[, -c("Qty")]
    
    Sell <- rbind(Sell1, Sell2.2)
    
    Sell <- unique(Sell)
    
    Sell <- merge(Sell, Sell2[, .(Seq, Code, Qty)], by=c("Seq", "Code"), all=TRUE)
    setkey(Sell, Code, Seq)
    
    Sell[, sim:="Sell"]
    
    rm(Sell1, Sell2, Sell2.2)
    
    
    
    Simulated <- rbind(Buy, Sell)
    
    setkey(Simulated, Code, Seq)
    
    save(Simulated, file=paste0("saving path",date[i], ".rda"))
    
    }
  }else{
    
    Session_info <- list()
    
    Index <- str_subset(data, "\00135=f")
    rm(data)
    Index <- str_replace_all(Index,"\001",",")
    
    Index <- str_replace_all(Index, ",52=([^,]*),",",")
    Index <- str_replace_all(Index, ",5799=([^,]*),",",")
    Index <- str_replace_all(Index, "1128=([^,]*),9=([^,]*),35=([^,]*),49=([^,]*),","")
    Index <- str_replace_all(Index, "1151=([^,]*),","")
    Index <- str_replace_all(Index, "6937=([^,]*),","")
    Index <- str_replace_all(Index, "10=([^,]*),","")
    Index <- str_replace_all(Index, "327=([^,]*),","")
    
    Index1 <- str_subset(Index, "326=21")
    
    if(length(Index1)!=0){
      
      Index1 <- str_match_all(Index1, "34=([^,]*),60=([^,]*),75=([^,]*),326=([^,]*),1174=([^,]*),")
      
      
      
      Index1 <- as.data.table(do.call(rbind, Index1))[,-1]
      
      names(Index1)[c(1:5)] <- c("MsgSeq","Time","Date","TradingStatus", "TradingEvent")
      Session_info[[1]] <- Index1
    }
    
    
    
    Index2 <- str_subset(Index, "326=15")
    
    if(length(Index2)!=0){
      
      Index2 <- str_match_all(Index2, "34=([^,]*),60=([^,]*),75=([^,]*),326=([^,]*),1174=([^,]*),")
      Index2 <- as.data.table(do.call(rbind, Index2))[,-1]
      names(Index2)[c(1:5)] <- c("MsgSeq","Time","Date","TradingStatus", "TradingEvent")
      Session_info[[2]] <- Index2
    }
    
    Index3 <- str_subset(Index, "326=17")
    
    if(length(Index3)!=0){
      
      Index3 <- str_match_all(Index3, "34=([^,]*),60=([^,]*),75=([^,]*),326=([^,]*),1174=([^,]*),")
      Index3 <- as.data.table(do.call(rbind, Index3))[,-1]
      names(Index3)[c(1:5)] <- c("MsgSeq","Time","Date","TradingStatus", "TradingEvent")
      Session_info[[3]] <- Index3
    }
    
    
    Session_info <- rbindlist(Session_info)
    
    if(dim(Session_info)[1]!=0){
      
      Session_info[, Time:=as.POSIXct(Time, "GMT", "%Y%m%d%H%M%S%OS")]
      attr(Session_info$Time, "tzone") <- "America/Chicago"
      setkey(Session_info, MsgSeq)
      
      Session_info[, session:=fifelse(TradingStatus==21 & (TradingEvent==0|TradingEvent==4), "preopen", fifelse(TradingStatus==21 & TradingEvent==1, "preopen_nocancel",
                                                                                                                fifelse(TradingStatus==15, "opening", fifelse(TradingStatus==17, "open", "none"))))]
      
      save(Session_info, file=paste0("saving path",date[i], ".rda"))
    }
    
  }
}
