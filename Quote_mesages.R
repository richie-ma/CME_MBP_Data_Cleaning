## dependents: data.table stringr

quote_message <- function(raw_data_path, date, price_displayformat=NULL, sunday_raw_data_path=NULL, ...){
 
  date <- as.Date(date)
  
  if (class(date)!="Date"){
    
    stop("date should be in the format as YYYY-MM-DD.")
    
  }
  
  
  
  data <- fread(raw_data_path, header = F, sep="\\", fill=TRUE)[[1L]]
  
  

  
  ## let R know whether it is FIX or MDP data
  ## For the FIX data  
  
  if(date < as.Date("2015-11-20", "%Y-%m-%d")){
    
     
    Index <- str_subset(data, "\001269=[01]|\001276=RK")
    rm(data)
    
    Index <- str_subset(Index, "\00135=X")
    Index <- str_replace_all(Index, "\001",",") 
    

    Index <-  str_replace_all(Index, "1128=([^,]*),9=([^,]*),35=([^,]*),49=([^,]*),", "")
    Index <-  str_replace_all(Index, ",268=([^,]*),",  ",")
    Index <-  str_replace_all(Index, ",22=([^,]*),",  ",")
    Index <-  str_replace_all(Index, ",48=([^,]*),",  ",")
    Index <-  str_replace_all(Index, ",273=([^,]*),",  ",")
    Index <-  str_replace_all(Index, ",336=([^,]*),",  ",")
    Index <-  str_replace_all(Index, ",10=([^,]*),",  ",")
    Index <-  str_replace_all(Index, ",5797=([^,]*),",  ",")
    

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
      names(message)[c(1:11)] <- c("MsgSeq","SendingTime","Date","Update","Seq","Code","Side","PX","Qty","Ord","PX_depth")
      message[, Implied := "N"]

      
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
      names(message_2)[c(1:11)] <- c("MsgSeq","SendingTime","Date","Update","Seq","Code","Side","PX","Qty","Implied","PX_depth")
      message_2[, Implied := "Y"]
      
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
    
    setcolorder(message_all, c("Date", "MsgSeq", "SendingTime", "Code","Seq","Update", "Side", "PX", "Qty","Ord", "Implied", "PX_depth"))
    
    #rm(message,message_2,part1_dt,part1_info,part2_dt,part2_info,Index,n_row_part1_info,n_row_part2_info,part1,part2)
  }
  
  else{
    ## remove the unnecessary tags
    Index <- str_subset(data, "\001269=[01EF]")
    rm(data)
    
    Index <- str_subset(Index, "\00135=X")
    Index <- str_replace_all(Index, "\001",",") 
    
    Index <-  str_replace_all(Index, "1128=([^,]*),9=([^,]*),35=([^,]*),49=([^,]*),", "")
    Index <-  str_replace_all(Index, ",5799=([^,]*),",  ",")
    Index <-  str_replace_all(Index, ",268=([^,]*),",  ",")
    Index <-  str_replace_all(Index, ",48=([^,]*),",  ",")
    Index <-  str_replace_all(Index, ",10=([^,]*),",  ",")
    

    ##----------------------------------------LOB cleaning-------------------------------------
    ###################################### outright orders only since they have the info of number of orders#################################
    ## we should find the following items
    ## in terms of add 279=0(add) or 1(change) or 2(delete), 269=0(bid) or 1(ask), 83(sequence),270(PX),271(Qty),346(# of orders),1023(PX_depth)
    if(length(str_subset(Index, "269=[01]"))!=0){
      
      part1 <- str_subset(Index, "279=[012],269=([01]*),") ## find both tag269=0 and tag269=1
      part1_dt <- str_extract_all(part1,"75=([^,]*),34=([^,]*),52=([^,]*),60=([^,]*),")
      part1_info <- str_match_all(part1,"279=([^,]*),269=([^,]*),55=([^,]*),83=([^,]*),270=([^,]*),271=([^,]*),346=([^,]*),1023=([^,]*),")
      n_row_part1_info <- sapply(part1_info, nrow)
      part1_dt <- str_dup(part1_dt,n_row_part1_info)
      part1_dt <- str_match_all(part1_dt, "75=([^,]*),34=([^,]*),52=([^,]*),60=([^,]*),")
      part1_dt <- as.data.table(do.call(rbind, part1_dt))[,-1]
      part1_info <- as.data.table(do.call(rbind,part1_info))[,-1]
      message <- cbind(part1_dt, part1_info)
      names(message)[c(1:12)] <- c("Date","MsgSeq","SendingTime", "TransactTime","Update","Side","Code","Seq","PX","Qty","Ord","PX_depth")
      message[, Implied:="N"]

      
    }
    ####################################### Implied orders only #################################################
    #We should conisder the implied order actually since it affects the limit order book
    ## we should find the following items
    ## in terms of add 279=0(add) or 1(change) or 2(delete), 269=E(implied bid) or 1(implied ask), 83(sequence),270(PX),271(Qty),346(# of orders),1023(PX_depth)
    if(length(str_subset(Index, "279=[012],269=([EF]*),"))!=0){
      
      part2 <- str_subset(Index, "279=[012],269=([EF]*),") ## find both tag269=0 and tag269=1
      part2_dt <- str_extract_all(part2,"75=([^,]*),34=([^,]*),52=([^,]*),60=([^,]*),")
      part2_info <- str_match_all(part2,"279=([^,]*),269=([^,]*),55=([^,]*),83=([^,]*),270=([^,]*),271=([^,]*),1023=([^,]*),")
      n_row_part2_info <- sapply(part2_info, nrow)
      part2_dt <- str_dup(part2_dt,n_row_part2_info)
      part2_dt <- str_match_all(part2_dt, "75=([^,]*),34=([^,]*),52=([^,]*),60=([^,]*),")
      part2_dt <- as.data.table(do.call(rbind, part2_dt))[,-1]
      part2_info <- as.data.table(do.call(rbind,part2_info))[,-1]
      message_2 <- cbind(part2_dt, part2_info)
      names(message_2)[c(1:11)] <- c("Date","MsgSeq","SendingTime","TransactTime", "Update","Side","Code","Seq","PX","Qty","PX_depth") 
      message_2[, Implied:="Y"]


      
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
    setcolorder(message_all, c("Date", "MsgSeq", "SendingTime", "TransactTime", "Code","Seq","Update", "Side", "PX", "Qty","Ord", "Implied", "PX_depth"))
    
   # rm(message,message_2,part1_dt,part1_info,part2_dt,part2_info,Index,n_row_part1_info,n_row_part2_info,part1,part2)
  }
  
  results <- split(message_all, by="Code")
  
  if(is.null(price_displayformat)){
    
    source("C:/Users/ruchuan2/Box/cme.mdp/R/meta_data.R")
    
    if(is.null(sunday_raw_data_path)){
      
      stop("Sunday's security definition at the same week must be provided to get the price display format")
    }
    
   definition <- meta_data(sunday_raw_data_path, date=date)
    
    setnames(definition, "Symbol", "Code")
    
    definition <- definition[Code %in% names(results)]
    
    results <- lapply(results, function(x) {
      x[, PX:= as.numeric(PX) * definition[Code == unique(x$Code), as.numeric(DisplayFactor) ]]
      return(x)
    })
    
    
    
  }else{
    
    results <- lapply(results, function(x) x[, grep("PX", colnames(x)):=lapply(.SD, function(x) as.numeric(x)*as.numeric(price_displayformat)), .SDcols = patterns("PX")])
    
  }
    
   
    cat("CME MDP 3.0 Quote Messages", "\n", 
        "contracts:", names(results))
    return(results)

    
  
}






