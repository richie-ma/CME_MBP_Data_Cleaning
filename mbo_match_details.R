


mbo_match_details <- function(raw_data_path, implied_quotes=FALSE,
                            price_displayformat=NULL, sunday_raw_data_path=NULL, ...){
  
  

  data <- fread(raw_data_path, header = F, sep="\\", fill=TRUE)[[1L]]


## loop starts##  
#test <- readLines("C:/Users/ruchuan2/Box/MBO/xcbt_md_zc_fut_20180410-r-00008")
#Index <- gsub("\001",",",test) # delete all "\001" for each tag and replace it with "," (comma)
#rm(test)

Index <- str_subset(data, "\001269=2")
Index <- str_replace_all(Index, "\001",",") 
Trade <- str_replace_all(Index,",269=2,",",")
  
rm(Index)
rm(data)

## delete the tags that are not necessary

Trade <- str_replace_all(Trade, "1128=([^,]*),9=([^,]*),35=([^,]*),49=([^,]*)," , "")
Trade <- str_replace_all(Trade, ",5799=([^,]*),",",")
Trade <- str_replace_all(Trade, ",268=([^,]*),",",")
Trade <- str_replace_all(Trade, ",279=([^,]*),",",")
Trade <- str_replace_all(Trade, ",48=([^,]*),",",")
Trade <- str_replace_all(Trade, ",37705=([^,]*),",",")
Trade <- str_replace_all(Trade, ",269=2,",",")


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


####################################### Part 1 #################################################
##-----------(no order details and order details together, one line, not the whole line)----------
part1<- str_subset(Trade, "55=([^,]*),83=([^,]*),270=([^,]*),271=([^,]*),346=([^,]*),5797=([^,]*),37711=([^,]*),")
info1 <- unlist(str_extract_all(part1, "75=([^,]*),34=([^,]*),52=([^,]*),60=([^,]*),"))

##-----find all 37-32 combinations
trade_order1 <- str_match_all(part1,"37=([^,]*),32=([^,]*),")
n_row_1 <- sapply(trade_order1, nrow) # calculate how many rows in a list element
trade_order1 <- as.data.table(do.call(rbind,trade_order1))[,-1]
names(trade_order1)[c(1:2)] <- c("order_id","matched_qty")

## generate info 1
info1.1 <- str_dup(info1, n_row_1)
info1.1 <- str_match_all(info1.1, "75=([^,]*),34=([^,]*),52=([^,]*),60=([^,]*)")
info1.1 <- as.data.table(do.call(rbind,info1.1))[,-1]
names(info1.1)[c(1:4)] <- c("Date","MsgSeq", "SendingTime", "TransactTime")

info_trade_bind <- cbind(info1.1, trade_order1)

info_trade_bind$MsgSeq <- as.numeric(info_trade_bind$MsgSeq)
info_trade_bind$matched_qty <- as.numeric(info_trade_bind$matched_qty)

setkey(info_trade_bind, MsgSeq)

##generate info 2
info2 <- str_match_all(part1, "55=([^,]*),83=([^,]*),270=([^,]*),271=([^,]*),346=([^,]*),5797=([^,]*),37711=([^,]*),")
n_row_2 <- sapply(info2, nrow) # calculate how many rows in a list element
info2 <- as.data.table(do.call(rbind,info2))[,-1]
names(info2)[c(1:7)] <- c("Code", "Seq", "PX", "Qty", "Ord", "agg", "trade_id")

info1.2 <- str_dup(info1, n_row_2)
info1.2 <- str_match_all(info1.2, "75=([^,]*),34=([^,]*),52=([^,]*),60=([^,]*)")
info1.2 <- as.data.table(do.call(rbind,info1.2))[,-1]
names(info1.2)[c(1:4)] <- c("Date","MsgSeq", "SendingTime", "TransactTime")

info2 <- cbind(info1.2, info2)

info2$MsgSeq <- as.numeric(info2$MsgSeq)
info2$Seq <- as.numeric(info2$Seq)
info2$PX <- as.numeric(info2$PX)
info2$Qty <- as.numeric(info2$Qty)
info2$agg <- as.numeric(info2$agg)
info2$trade_id <- as.numeric(info2$trade_id)
info2$Ord <- as.numeric(info2$Ord)


setkey(info2, MsgSeq, trade_id)

rm(n_row_1, n_row_2, info1, Trade, part1, info1.1, info1.2)

### merge info2 and info_trade_bind

##order_details ## defined by CME

###-------------------------------------------------------------------
## we assign the order details to trade summary based on message sequence number
## note that some trade summary may not have the complete order details
## we process it from each message sequence

Message.sequence <- unique(info2$MsgSeq)


trade.summary.all <- list()

for(i in 1:length(Message.sequence)){
  
 ## print(i)

trade.summary <- info2[MsgSeq==Message.sequence[i]]  

trade.summary[, end.id:=cumsum(Ord)]
trade.summary[, start.id:=end.id-Ord+1]

order_details <- info_trade_bind[MsgSeq==Message.sequence[i]]

trade.summary.list <- list()


  
  trade.ID <- unique(trade.summary$trade_id)
  
  for(j in 1:length(trade.ID)){
  
  trade.summary.single <- trade.summary[trade_id==trade.ID[j]]
  order_details.single <- na.omit(order_details[trade.summary.single$start.id:trade.summary.single$end.id])
  
  if(dim(order_details.single)[1]!=0){
  
  trade.summary.single <- trade.summary.single[order_details.single[,.(MsgSeq, order_id, matched_qty)], on=.(MsgSeq)][, -c("start.id", "end.id")]

  
  
  } else {
    
    trade.summary.single <- trade.summary.single[, -c("start.id", "end.id")]
    
  }
  trade.summary.list[[j]] <- trade.summary.single
  
  } 
  
  trade.summary.list <- rbindlist(trade.summary.list, fill=TRUE)
  
  
  trade.summary.all[[i]] <- trade.summary.list
  
}

trade.summary.all <- rbindlist(trade.summary.all, fill = TRUE)

MBO.Trade <- as.data.table(trade.summary.all)

setkey(MBO.Trade, MsgSeq)

rm(trade.summary.all, trade.summary.list, info2, info_trade_bind)

MBO.Trade <- split(MBO.Trade, by="Code")

if(is.null(price_displayformat)){
  
  source("C:/Users/ruchuan2/Box/cme.mdp/R/meta_data.R")
  
  if(is.null(sunday_raw_data_path)){
    
    stop("Sunday's security definition at the same week must be provided to get the price display format")
  }
  
  definition <- meta_data(sunday_raw_data_path, date=date)
  
  setnames(definition, "Symbol", "Code")
  
  definition <- definition[Code %in% names(MBO.Trade)]
  
  MBO.Trade <- lapply(MBO.Trade, function(x) {
    x[, PX := PX * definition[Code == unique(x$Code), as.numeric(DisplayFactor) ]]
    return(x)
  })
  
  
  
}else{
  
  MBO.Trade <- lapply(MBO.Trade, function(x) x[, grep("PX", colnames(x)):=lapply(.SD, function(x) as.numeric(x)*as.numeric(price_displayformat)), .SDcols = patterns("PX")])
  
}

cat("CME MDP 3.0 Trade Summary with Matching Details", "\n", 
    "contracts:", names(MBO.Trade))
return(MBO.Trade)

}


}











  


