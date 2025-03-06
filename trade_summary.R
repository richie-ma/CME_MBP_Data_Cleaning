## dependents: data.table, stringr

trade_summary <- function(raw_data_path, date, price_displayformat=NULL, sunday_raw_data_path=NULL, ...){
  
  date <- as.Date(date)
  
  if (class(date)!="Date"){
   
     stop("date should be in the format as YYYY-MM-DD.")
    
  }
  
  data <-  fread(raw_data_path, header = F, sep="\\", fill=TRUE)[[1L]]
  
  ### checking the price display factor
  
  
  if(date < "2015-11-20"){
    
    Index <- str_subset(data, "\001269=2")
    Index <- str_replace_all(Index, "\001",",") 
    Trade <- str_replace_all(Index,",269=2,",",")
    
    rm(Index)
    rm(data)
    
    ## delete the tags that are not necessary
    
    Trade <-  str_replace_all(Trade, "1128=([^,]*),9=([^,]*),35=([^,]*),49=([^,]*),", "")
    Trade <-  str_replace_all(Trade, ",5799=([^,]*),", ",")
    Trade <-  str_replace_all(Trade, ",268=([^,]*),", ",")
    Trade <-  str_replace_all(Trade, ",279=([^,]*),", ",")
    Trade <-  str_replace_all(Trade, ",22=([^,]*),", ",")
    Trade <-  str_replace_all(Trade, ",48=([^,]*),", ",")
    Trade <-  str_replace_all(Trade, ",273=([^,]*),", ",")
    Trade <-  str_replace_all(Trade, ",274=([^,]*),", ",")
    Trade <-  str_replace_all(Trade, ",451=([^,]*),", ",")
    Trade <-  str_replace_all(Trade, ",1020=([^,]*),", ",")
    
    
    
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
    
    if(length(Trade)!=0){
      
      ## generate the trade data (without order flow)
      trade <- str_match_all(Trade, "83=([^,]*),107=([^,]*),270=([^,]*),271=([^,]*),1003=([^,]*),5797=([^,]*),")
      n_row <- sapply(trade, nrow)
      trade.info <- unlist(str_extract_all(Trade,"34=([^,]*),52=([^,]*),75=([^,]*),"))
      trade.info <- str_dup(trade.info, n_row)
      trade <- as.data.table(do.call(rbind, trade))[,-1]
      names(trade)[c(1:6)] <- c("Seq","Code","PX", "Size","TrdID","agg")
      
      trade.info <- str_match_all(trade.info, "34=([^,]*),52=([^,]*),75=([^,]*),")
      trade.info <- as.data.table(do.call(rbind,trade.info))[,-1]
      names(trade.info)[c(1:3)] <- c("MsgSeq","SendingTime","Date")
      
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
      
      ###
      
      trade1 <- str_match_all(Trade, "83=([^,]*),107=([^,]*),270=([^,]*),271=([^,]*),1003=([^,]*),")
      n_row <- sapply(trade1, nrow)
      trade.info1 <- unlist(str_extract_all(Trade,"34=([^,]*),52=([^,]*),75=([^,]*),"))
      trade.info1 <- str_dup(trade.info1, n_row)
      trade1 <- as.data.table(do.call(rbind, trade1))[,-1]
      names(trade1)[c(1:5)] <- c("Seq","Code","PX", "Size","TrdID")
      
      trade.info1 <- str_match_all(trade.info1, "34=([^,]*),52=([^,]*),75=([^,]*),")
      trade.info1 <- as.data.table(do.call(rbind,trade.info1))[,-1]
      names(trade.info1)[c(1:3)] <- c("MsgSeq","SendingTime","Date")
      
      trade.info1$Date <- as.Date(trade.info1$Date, "%Y%m%d")
      #Time <- paste(substr(trade.info$Time,1,8),substr(trade.info$Time,9,10),substr(trade.info$Time,11,12),
      #substr(trade.info$Time,13,14),substr(trade.info$Time,15,23),sep=".")
      #trade.info$Time <- as.POSIXct(Time, tz="UTC", "%Y%m%d.%H.%M.%OS") ## original time is UTC
      #attr(trade.info$Time, "tzone") <- "America/Chicago" ## US central time
      #trade$MsgSeq <- as.numeric(trade$MsgSeq)
      trade1$Seq <- as.numeric(trade1$Seq)
      trade1$PX <- as.numeric(trade1$PX)
      trade1$Size <- as.numeric(trade1$Size)
      
      trade1 <- cbind(trade.info1, trade1)
      rm(trade.info1)
      
      trade1.1 <- trade[, -"agg"]
      
      trade1 <- rbind(trade1.1, trade1)
      
      trade1 <- unique(trade1)
      
      trade <- merge(trade1, trade[, .(Seq, Code, agg)], by=c("Seq", "Code"), all=TRUE)
      setkey(trade, Code, Seq)
      
      rm(trade1.1, trade1)
      
      
      trade2 <- str_match_all(Trade, "83=([^,]*),107=([^,]*),270=([^,]*),271=([^,]*),277=([^,]*),1003=([^,]*),5797=([^,]*),")
      n_row <- sapply(trade2, nrow)
      trade.info2 <- unlist(str_extract_all(Trade,"34=([^,]*),52=([^,]*),75=([^,]*),"))
      trade.info2 <- str_dup(trade.info2, n_row)
      trade2 <- as.data.table(do.call(rbind, trade2))[,-1]
      names(trade2)[c(1:7)] <- c("Seq","Code","PX","Size","TrdCon","TrdID","agg")
      
      trade.info2 <- str_match_all(trade.info2, "34=([^,]*),52=([^,]*),75=([^,]*),")
      trade.info2 <- as.data.table(do.call(rbind,trade.info2))[,-1]
      names(trade.info2)[c(1:3)] <- c("MsgSeq","SendingTime","Date")
      
      trade.info2$Date <- as.Date(trade.info2$Date, "%Y%m%d")
      #Time <- paste(substr(trade.info$Time,1,8),substr(trade.info$Time,9,10),substr(trade.info$Time,11,12),
      #substr(trade.info$Time,13,14),substr(trade.info$Time,15,23),sep=".")
      #trade.info$Time <- as.POSIXct(Time, tz="UTC", "%Y%m%d.%H.%M.%OS") ## original time is UTC
      #attr(trade.info$Time, "tzone") <- "America/Chicago" ## US central time
      #trade$MsgSeq <- as.numeric(trade$MsgSeq)
      trade2$Seq <- as.numeric(trade2$Seq)
      trade2$PX <- as.numeric(trade2$PX)
      trade2$Size <- as.numeric(trade2$Size)
      
      trade2 <- cbind(trade.info2, trade2)
      rm( trade.info2)
      
      ## special trades
      
      trade3 <- str_match_all(Trade, "83=([^,]*),107=([^,]*),270=([^,]*),271=([^,]*),277=([^,]*),1003=([^,]*),")
      n_row <- sapply(trade3, nrow)
      trade.info3 <- unlist(str_extract_all(Trade,"34=([^,]*),52=([^,]*),75=([^,]*),"))
      trade.info3 <- str_dup(trade.info3, n_row)
      trade3 <- as.data.table(do.call(rbind, trade3))[,-1]
      names(trade3)[c(1:6)] <- c("Seq","Code","PX","Size","TrdCon","TrdID")
      
      trade.info3 <- str_match_all(trade.info3, "34=([^,]*),52=([^,]*),75=([^,]*),")
      trade.info3 <- as.data.table(do.call(rbind,trade.info3))[,-1]
      names(trade.info3)[c(1:3)] <- c("MsgSeq","SendingTime","Date")
      
      trade.info3$Date <- as.Date(trade.info3$Date, "%Y%m%d")
      #Time <- paste(substr(trade.info$Time,1,8),substr(trade.info$Time,9,10),substr(trade.info$Time,11,12),
      #substr(trade.info$Time,13,14),substr(trade.info$Time,15,23),sep=".")
      #trade.info$Time <- as.POSIXct(Time, tz="UTC", "%Y%m%d.%H.%M.%OS") ## original time is UTC
      #attr(trade.info$Time, "tzone") <- "America/Chicago" ## US central time
      #trade$MsgSeq <- as.numeric(trade$MsgSeq)
      trade3$Seq <- as.numeric(trade3$Seq)
      trade3$PX <- as.numeric(trade3$PX)
      trade3$Size <- as.numeric(trade3$Size)
      
      trade3 <- cbind(trade.info3, trade3)
      rm(trade.info3)
      
      trade2.2 <- trade2[, -c("agg")]
      
      trade2_3_all <- rbind(trade2.2, trade3)
      
      trade2_3_all <- unique(trade2_3_all)
      
      trade2_3_all <- merge(trade2[, .(Code, Seq, agg)], trade2_3_all, by=c( "Code","Seq"), all=TRUE)
      
      setkey(trade2_3_all, Code, Seq)
      
      
      setkey(trade, Code, Seq)
      
      trade <- rbind(trade, trade2_3_all, fill=TRUE)
      
      setkey(trade, Code, Seq)
      setcolorder(trade, c("Date", "MsgSeq", "SendingTime", "Code", "Seq", "PX", "Size", "agg"))
      
     
    }
  }  else{
  
    
    Index <- str_subset(data, "\001269=2")
    Index <- str_replace_all(Index, "\001",",") 
    Trade <- str_replace_all(Index,",269=2,",",")
    rm(Index)
    
    rm(data)
    
    ## delete the tags that are not necessary
    
    Trade <- str_replace_all(Trade, "1128=([^,]*),9=([^,]*),35=([^,]*),49=([^,]*),","")
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
      trade.info <- unlist(str_extract_all(Trade,"75=([^,]*),34=([^,]*),52=([^,]*),60=([^,]*),"))
      trade.info <- str_dup(trade.info, n_row)
      trade <- as.data.table(do.call(rbind, trade))[,-1]
      names(trade)[c(1:6)] <- c("Code","Seq","PX","Size","Ord","agg")
      
      trade.info <- str_match_all(trade.info, "75=([^,]*),34=([^,]*),52=([^,]*),60=([^,]*),")
      trade.info <- as.data.table(do.call(rbind,trade.info))[,-1]
      names(trade.info)[c(1:4)] <- c("Date","MsgSeq","SendingTime", "TransactTime")
      
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
      setkey(trade, Code, Seq)
      setcolorder(trade, c("Date", "MsgSeq", "SendingTime", "TransactTime","Code", "Seq", "PX", "Size","Ord", "agg"))
    }    
   
    }  
  
  results <- split(trade, by="Code")
  
  if(is.null(price_displayformat)){
    
    source("meta_data.R")
    
    if(is.null(sunday_raw_data_path)){
      
      stop("Sunday's security definition at the same week must be provided to get the price display format")
    }
    
    definition <- meta_data(sunday_raw_data_path, date=date)
    
    setnames(definition, "Symbol", "Code")
    
    definition <- definition[Code %in% names(results)]
    
    results <- lapply(results, function(x) {
      x[, PX := PX * definition[Code == unique(x$Code), as.numeric(DisplayFactor) ]]
      return(x)
    })
    

    
  }else{
    
   results <- lapply(results, function(x) x[, grep("PX", colnames(x)):=lapply(.SD, function(x) as.numeric(x)*as.numeric(price_displayformat)), .SDcols = patterns("PX")])
    
  }
  
  cat("CME MDP 3.0 Trade Summary", "\n", 
      "contracts:", names(results))
  return(results)
}

