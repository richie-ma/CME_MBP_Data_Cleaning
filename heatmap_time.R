###################################

# limit order book with heatmaps

####################################


heatmap_time <- function(mbp_order_book, level=NULL, start_timestamp, end_timestamp, animation=FALSE, ...){
  
  if("DT" %in% colnames(mbp_order_book) == T){
    
    setnames(mbp_order_book, "DT", "Time") 
    
  }
  
  if(isTRUE(animation)){
    
    stop("Animation is not currently supported for tick BBO chart")
  }
  
  if(class(mbp_order_book$Time)[1]=="character"){
    
    mbp_order_book[, Time:= paste0(substr(Time, 1, 4), "-", substr(Time, 5, 6), "-",
                                   substr(Time, 7, 8),
                                   " ", substr(Time, 9, 10),
                                   ":", substr(Time, 11, 12), ":", substr(Time, 13, 14),
                                   ".", substr(Time, 15, 23))] ## dafult as.posixct time format
    
    mbp_order_book <- mbp_order_book[, `:=`(Time=as.POSIXct(Time, tz="GMT", "%Y-%m-%d %H:%M:%OS"))]
    
  }
  
  ## change timezone to UTC
  start_timestamp <- as.POSIXct(start_timestamp, trading.tz) 
  end_timestamp <- as.POSIXct(end_timestamp, trading.tz) 
  attr(start_timestamp, "tzone") <- "GMT"
  attr(end_timestamp, "tzone") <- "GMT"
  
  mbp_order_book <- mbp_order_book[Time %between% c(start_timestamp, end_timestamp)][, -c("Date", "Seq", "MsgSeq")]
  attr(mbp_order_book$Time, "tzone") <- trading.tz
  
  if(is.null(level)==F){
    
    columns <- c(unlist(mapply(function(x) c(paste0("Bid_PX_", x), paste0("Bid_Qty_", x), paste0("Bid_Ord_", x)), level:1)),
                 unlist(mapply(function(x) c(paste0("Ask_PX_", x), paste0("Ask_Qty_", x), paste0("Ask_Ord_", x)), 1:level)))
    
    columns_delete <- colnames(mbp_order_book)[5: (length(colnames(mbp_order_book))-1)][which(colnames(mbp_order_book)[5: (length(colnames(mbp_order_book))-1)] %in% columns ==F)]
    
    mbp_order_book <- mbp_order_book[, !..columns_delete]
  } 
  
  mbp_order_book <- melt(mbp_order_book, c("Time", "Code"), measures=patterns("PX"), 
               variable.name= "fields",
               value.name = c("values"))[, `:=`(fields=substr(fields, 5, 5), side=substr(fields, 1, 3), 
                                                depth=as.numeric(fifelse(str_sub(fields, -1)=="0", "10", str_sub(fields, -1))))][, px:=values[fields=="P"], by=.(Time, side, depth)][!fields=="P"]
  
  mbp_order_book <- dcast(mbp_order_book, Time + Code + px + side + depth ~ fields, value.var = c("values"))
  
  setkey(mbp_order_book, Time, side, depth)
  
  
  heatmap_book <- ggplot(mbp_order_book)+
    geom_tile(data = mbp_order_book[side=="Ask"], aes(x = Time, y = px, fill = Q), 
             stat="identity", alpha=0.5) +
    geom_line(data = mbp_order_book[side=="Ask" & depth==1], aes(x = Time, y = px), color="blue") +
    labs(fill = "Liquidity: Ask")+
  
    scale_fill_gradientn(
      colors = c("green1", "yellow"),  ### Ask
      limits = c(mbp_order_book[side == "Ask", min(Q)], mbp_order_book[side == "Ask", max(Q)]),
      guide = guide_legend(order = 1, reverse = TRUE)
    )+
    
    new_scale_fill() +
    geom_tile(data = mbp_order_book[side=="Bid"], aes(x = Time, y = px, fill = Q), 
             stat="identity", alpha=0.5) +
    geom_line(data = mbp_order_book[side=="Bid" & depth==1], aes(x = Time, y = px), color="black")+
    labs(fill = "Liquidity: Bid")+
    
    scale_fill_gradientn(
      colors = c("red1", "yellow"),  ###Bid
      limits = c(mbp_order_book[side == "Bid", min(Q)], mbp_order_book[side == "Bid", max(Q)]),
      guide = guide_legend(order = 2, reverse = TRUE)
    )+
    labs(title = paste0(mbp_order_book[, .SD[1,Time]],"--", mbp_order_book[, .SD[.N,Time]], "   ", mbp_order_book[, Code]), ## bbo info
         x = "Price", 
         y = "Quantity") +
    theme_minimal()
  
  return(heatmap_book)
  
}
