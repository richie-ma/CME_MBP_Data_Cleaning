

mbo_quote_queue <- function(raw_data_path,implied_quotes=FALSE,
                          price_displayformat=NULL, sunday_raw_data_path=NULL, ...){
    
  
  
  
  data <- fread(raw_data_path, header = F, sep="\\", fill=TRUE)[[1L]]
  
### MBO is only available since 2017 so only one template is available.
  
  Index <- str_subset(data, "\001269=[01]|\001276=RK")
  rm(data)
  
  Index <- str_subset(Index, "\00135=X")
  Index <- str_replace_all(Index, "\001",",") 
  
  ## select the new order submissions in the file
  ## there are two parts
  ## one is from the tag279=0 with tag269=0 or 1 (0: bid; 1: ask), tag37706 (order quantity), tag37707 (order priority, lower value higher priority)
  ## and tag37 (order id)
  
  Index <- str_replace_all(Index, "1128=([^,]*),9=([^,]*),35=([^,]*),49=([^,]*),", "")
  Index <- str_replace_all(Index, ",5799=([^,]*),", ",")
  Index <- str_replace_all(Index, ",268=([^,]*),",",")
  Index <- str_replace_all(Index, ",48=([^,]*),",",")
  Index <- str_replace_all(Index, ",10=([^,]*),", ",")
  
  mbo <- list()
  ##----------------------------------------Order submission/modification/cancellation-------------------------------------
  
  ############################### Order submission/modification/cancellation part 1 #####################################
  ## MBO does not have a sequence number
  ## MBO and MDP combined format have sequence number
  
  ## bid/ask order (tag269=0 or 1)
  part1 <- str_subset(Index, "279=[012],269=([01]*),") ## find both tag269=0 and tag269=1 
  
  if(length(part1)!=0){
  
  ## extract MBP info
  ## we need to include the implied orders here.
  part1_order <- str_subset(part1, "279=[012],269=([01]*),55=([^,]*),270=([^,]*),37706=([^,]*),37707=([^,]*),37=([^,]*),") 
  info_part1 <- str_extract_all(part1_order, "75=([^,]*),34=([^,]*),52=([^,]*),60=([^,]*),")
  order_part1 <- str_match_all(part1_order, "279=([012]*),269=([01]*),55=([^,]*),270=([^,]*),37706=([^,]*),37707=([^,]*),37=([^,]*),")
  n_row_1 <- sapply(order_part1, nrow)
  info_part1 <- str_dup(info_part1, n_row_1)
  info_part1 <- str_match_all(info_part1, "75=([^,]*),34=([^,]*),52=([^,]*),60=([^,]*),")
  order_part1 <- as.data.table(do.call(rbind,order_part1))[,-1]
  info_part1 <- as.data.table(do.call(rbind, info_part1))[,-1]
  part1_order <- cbind(info_part1,order_part1)
  names(part1_order)[c(1:11)] <- c("Date","MsgSeq","SendingTime","TransactTime",
                                   "Update","Side","Code","PX","Qty","Order_priority","Order_id")
  
  part1_order$MsgSeq <- as.numeric(part1_order$MsgSeq)
  part1_order$Update <- as.numeric(part1_order$Update) # 1 represents submission, 2 represents modification, 3 represents cancellation
  part1_order$PX <- as.numeric(part1_order$PX)
  part1_order$Qty <- as.numeric(part1_order$Qty)
  part1_order$Order_priority <- as.numeric(part1_order$Order_priority)
  part1_order$Order_id <- as.numeric(part1_order$Order_id)
  mbo[[1]] <- part1_order
  rm(info_part1, order_part1, part1, part1_order)
  
  
  
  }
  ###################################### Complete submission/modification/cancellation part 1 ###############################################
  # 
  # ### implied orders
  # 
  
  if(isTRUE(implied_quotes)){
  
  part1.1 <- str_subset(Index, "279=[012],269=([EF]*),") ## find both tag269=0 and tag269=1

  if(length(part1.1)!=0){

    ## extract MBP info
    ## we need to include the implied orders here.
    part1.1_order <- str_subset(part1.1, "279=[012],269=([EF]*),55=([^,]*),83=([^,]*),270=([^,]*),271=([^,]*),")
    info_part1.1 <- str_extract_all(part1.1_order, "75=([^,]*),34=([^,]*),52=([^,]*),60=([^,]*),")
    order_part1.1 <- str_match_all(part1.1_order, "279=([012]*),269=([EF]*),55=([^,]*),83=([^,]*),270=([^,]*),271=([^,]*),")
    n_row_1.1 <- sapply(order_part1.1, nrow)
    info_part1.1 <- str_dup(info_part1.1, n_row_1.1)
    info_part1.1 <- str_match_all(info_part1.1, "75=([^,]*),34=([^,]*),52=([^,]*),60=([^,]*),")
    order_part1.1 <- as.data.table(do.call(rbind,order_part1.1))[,-1]
    info_part1.1 <- as.data.table(do.call(rbind, info_part1.1))[,-1]
    part1.1_order <- cbind(info_part1.1,order_part1.1)
    names(part1.1_order)[c(1:10)] <- c("Date","MsgSeq","SendingTime","TransactTime",
                                       "Update","Side","Code","Seq","PX","Qty")

    part1.1_order$MsgSeq <- as.numeric(part1.1_order$MsgSeq)
    part1.1_order$Update <- as.numeric(part1.1_order$Update) # 1 represents submission, 2 represents modification, 3 represents cancellation
    part1.1_order$PX <- as.numeric(part1.1_order$PX)
    part1.1_order$Qty <- as.numeric(part1.1_order$Qty)
    part1.1_order$Seq <- as.numeric(part1.1_order$Seq)

    mbo[[2]] <- part1.1_order
    rm(info_part1.1, order_part1.1, part1.1, part1.1_order)
    
  }
  
  }
  
  ############################ Order submission/modification/cancellation part 2 ############################################################
  ## multiple book update records
  ## pattern
  ## tag75, tag60, tag268, tag279, tag269, tag55, tag270, tag271, tag346, tag1023, tag37, tag37707, tag37706, tag9633, tag37708=0
  
  part2 <- str_subset(Index, "279=[012],269=([01]*),")
  part2 <- str_subset(part2, "9633=([^,]*),")
  
  if(length(part2)!=0){
  
  order_part2 <- str_subset(part2, "37=([^,]*),37707=([^,]*),37706=([^,]*),9633=([^,]*),37708=([^,]*),")
  
  order_part2.order <- str_match_all(order_part2, "37=([^,]*),37707=([^,]*),37706=([^,]*),9633=([^,]*),37708=([^,]*),")
  n_row_2 <- sapply(order_part2.order, nrow)
  
  rm(part2)
  
  for(z in 1:length(order_part2.order)){
    
    order_part2.order[[z]] <- as.data.table(order_part2.order[[z]])[,-1]
  }
  
  order_part2.order <- rbindlist(order_part2.order, idcol = TRUE)
  
  setnames(order_part2.order, c("V2", "V3", "V4", "V5", "V6"), c("Order_id", "Order_priority", "Qty", "Ref_ID", "Update"))
  order_part2.details <- order_part2.order
  
  info_part2 <- str_match_all(order_part2, "269=([^,]*),55=([^,]*),83=([^,]*),270=([^,]*),271=([^,]*),")
  info_part2.time <- str_extract_all(order_part2, "75=([^,]*),34=([^,]*),52=([^,]*),60=([^,]*),")
  n_row_3 <- sapply(info_part2, nrow)
  
  rm(order_part2)
  
  info_part2.time <- str_dup(info_part2.time, n_row_3)
  
  info_part2.time <- str_match_all(info_part2.time, "75=([^,]*),34=([^,]*),52=([^,]*),60=([^,]*),")
  
  for(z in 1:length(info_part2.time)){
    
    info_part2.time[[z]] <- as.data.table(info_part2.time[[z]])[,-1]
  }
  
  info_part2.time <- rbindlist(info_part2.time, idcol = TRUE)
  
  setnames(info_part2.time, c("V2", "V3", "V4", "V5"), c("Date", "MsgSeq","SendingTime", "TransactTime"))
  
  
  for(z in 1:length(info_part2)){
    
    info_part2[[z]] <- as.data.table(info_part2[[z]])[,-1]
  }
  
  info_part2 <- rbindlist(info_part2, idcol = FALSE)
  
  setnames(info_part2, c("V2", "V3", "V4", "V5", "V6"), c("Side", "Code", "Seq", "PX", "Qty"))
  
  order_part2.info <- cbind(info_part2.time, info_part2)
  #order_part2.info <- order_part2.info[, -6]
  rm(info_part2, info_part2.time, order_part2.order)
  
  gc()
  
  order_part2.details$MsgSeq <- as.numeric(order_part2.details$MsgSeq)
  order_part2.details$Ref_ID <- as.numeric(order_part2.details$Ref_ID)
  order_part2.details$Order_priority <- as.numeric(order_part2.details$Order_priority)
  order_part2.details$Qty <- as.numeric(order_part2.details$Qty)
  order_part2.details$Update <- as.numeric(order_part2.details$Update)
  order_part2.details$.id <- as.numeric(order_part2.details$.id)
  
  order_part2.info$Side <- as.numeric(order_part2.info$Side)
  order_part2.info$Seq <- as.numeric(order_part2.info$Seq)
  order_part2.info$PX <- as.numeric(order_part2.info$PX)
  order_part2.info$Qty <- as.numeric(order_part2.info$Qty)
  order_part2.info$.id <- as.numeric(order_part2.info$.id)
  
  order_part2.info[, Ref_ID:=1:.N, by=.(.id)] ## here note that it should be sort by sequence number while not MsgSeq. Seq # is unique in
                                                        ## MBO-MBP template.

  
  
  part2_order <- order_part2.info[, -c("Qty")][order_part2.details[, .(Order_id, Order_priority, Qty,Ref_ID,Update, .id)], on=.(.id, Ref_ID), nomatch=NULL][, -c("Ref_ID", ".id")]
  mbo[[3]] <- part2_order
  rm(order_part2.details, order_part2.info, part2_order)
  
  }
  
  
  MBO <- rbindlist(mbo,fill = TRUE)
  
  MBO <- split(MBO, by="Code")
  
  if(is.null(price_displayformat)){
    
    source("meta_data.R")
    
    if(is.null(sunday_raw_data_path)){
      
      stop("Sunday's security definition at the same week must be provided to get the price display format")
    }
    
    definition <- meta_data(sunday_raw_data_path, date=date)
    
    setnames(definition, "Symbol", "Code")
    
    definition <- definition[Code %in% names(MBO)]
    
    MBO <- lapply(MBO, function(x) {
      x[, PX:= as.numeric(PX) * definition[Code == unique(x$Code), as.numeric(DisplayFactor) ]]
      return(x)
    })
    
    
    
  }else{
    
    MBO <- lapply(MBO, function(x) x[, grep("PX", colnames(x)):=lapply(.SD, function(x) as.numeric(x)*as.numeric(price_displayformat)), .SDcols = patterns("PX")])
    
  }
  
  
  cat("CME MDP 3.0 Quote Messages with Queue Information", "\n", 
      "contracts:", names(MBO))
  return(MBO)
  

}


