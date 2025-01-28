### Before you proceed this part, you should make sure that the messages have been saved properly 
###################################### cleaning LOB only 10 depths/ reconstruct the LOB ###########################
## load all of files
## you should start from each contract
## outright orders/implied orders


## set working dictionary
setwd("C:/Users/ruchuan2/OneDrive - University of Illinois - Urbana/Corn_message") 

rm(list=ls())

library(data.table)


contract_LOB_folder <- list.files(path = "C:/Users/ruchuan2/OneDrive - University of Illinois - Urbana/Corn_message", pattern = "ZC") ## all the underlying files
i <- which(contract_LOB_folder=="ZCZ9")

for (i in i:i){
  contract_all <- list.files(path =paste0("C:/Users/ruchuan2/OneDrive - University of Illinois - Urbana/Corn_message/",contract_LOB_folder[i]), pattern = "xcbt") 
  contract_all_date <- as.Date(substr(contract_all, 16, 23), "%Y%m%d")
  
  
  
  week_seq <- unique(substr(contract_all,27,31))
  j <- which(week_seq=="74427")
  
  for(j in j:j){
  
    LOB_week <- paste0(subset(contract_all, substr(contract_all,27,31)==week_seq[j]))
    #print(LOB_week)
    message_all <- list()
    
    for (l in 1:length(LOB_week)) {
      
      message_all[[l]] <- load(paste0("C:/Users/ruchuan2/OneDrive - University of Illinois - Urbana/Corn_message/",contract_LOB_folder[i],"/" ,LOB_week[l]))
      message_all[[l]] <- unique(message_sf)
    }
    
  
    message_all <- rbindlist(message_all,use.names = TRUE)
 
    message_all <- unique(message_all)
    
    #setnames(message_all, c("n_order"), c("Ord")) ## corn futures only
    
    
    ## identify the implied orders as K 
    
    if(as.Date(message_all$Date[1], "%Y%m%d") >= "2015-11-20"){
      
      message_all[message_all$bidask=="E"|message_all$bidask=="F", "RK"] <- "K"
      
      message_all[message_all$bidask==0|message_all$bidask==1, "RK"] <- "R"
    }
    
    date <- unique(message_all$Date)
    rm(message_sf)    
    
    print(LOB_week[l])
    
    
    ## outright orders
    
    message_all$Update <- as.numeric(message_all$Update)
    message_all$Seq <- as.numeric(message_all$Seq)
    message_all$PX <- as.numeric(message_all$PX)
    message_all$Qty <- as.numeric(message_all$Qty)
    message_all$Ord <- as.numeric(message_all$Ord)
    message_all$PX_depth <- as.numeric(message_all$PX_depth)
    message_all$MsgSeq <- as.numeric(message_all$MsgSeq)
    message_all_outright <- message_all[message_all$RK=="R",]
    
    ## implied orders
    
    message_all_implied <- message_all[message_all$RK=="K",]
    
    
    ## outright order book  
    
    LOB_outright <- matrix(as.numeric(0),nrow=dim(message_all_outright)[1], ncol=65)
    colnames(LOB_outright)[c(1:65)] <- c("Date", "Time", "Seq", "Code",
                                         "Bid_PX_10", "Bid_Qty_10", "Bid_Ord_10", "Bid_PX_9", "Bid_Qty_9", "Bid_Ord_9", "Bid_PX_8", "Bid_Qty_8", "Bid_Ord_8",
                                         "Bid_PX_7", "Bid_Qty_7", "Bid_Ord_7", "Bid_PX_6", "Bid_Qty_6", "Bid_Ord_6", "Bid_PX_5", "Bid_Qty_5", "Bid_Ord_5",
                                         "Bid_PX_4", "Bid_Qty_4", "Bid_Ord_4", "Bid_PX_3", "Bid_Qty_3", "Bid_Ord_3", "Bid_PX_2", "Bid_Qty_2", "Bid_Ord_2",
                                         "Bid_PX_1", "Bid_Qty_1", "Bid_Ord_1",
                                         "Ask_PX_1", "Ask_Qty_1", "Ask_Ord_1", "Ask_PX_2", "Ask_Qty_2", "Ask_Ord_2", "Ask_PX_3", "Ask_Qty_3", "Ask_Ord_3",
                                         "Ask_PX_4", "Ask_Qty_4", "Ask_Ord_4", "Ask_PX_5", "Ask_Qty_5", "Ask_Ord_5", "Ask_PX_6", "Ask_Qty_6", "Ask_Ord_6",
                                         "Ask_PX_7", "Ask_Qty_7", "Ask_Ord_7", "Ask_PX_8", "Ask_Qty_8", "Ask_Ord_8", "Ask_PX_9", "Ask_Qty_9", "Ask_Ord_9",
                                         "Ask_PX_10", "Ask_Qty_10", "Ask_Ord_10", "MsgSeq"
    )
    
    
    LOB_outright[,3] <- message_all_outright$Seq
    LOB_outright[,65] <- message_all_outright$MsgSeq
    ## implied order book
    
    LOB_implied <- matrix(as.numeric(0),nrow=dim(message_all_implied)[1], ncol=13)
    colnames(LOB_implied)[c(1:13)] <- c("Date", "Time", "Seq", "Code",
                                        "Bid_PX_2", "Bid_Qty_2",
                                        "Bid_PX_1", "Bid_Qty_1",
                                        "Ask_PX_1", "Ask_Qty_1", "Ask_PX_2", "Ask_Qty_2", "MsgSeq"
    )
    
    
    LOB_implied[,3] <- message_all_implied$Seq
    LOB_implied[,13] <- message_all_implied$MsgSeq
    ## insert messages (outright orders)
    
    ######################################## outright orders (max 10 depths) #####################################
    if(dim(message_all_outright)[1]!=0){
      
      for (k in 1:dim(message_all_outright)[1]){
        
       #   print(k)
        
        
        ########################################## Order submission ########################################
        
        if(message_all_outright$Update[k]==0){
          
          ## bid order
          if(message_all_outright$bidask[k]==0){
            
            column_name1 <- paste0("Bid_PX_",as.character(message_all_outright$PX_depth[k]))
            column_index1 <- which(colnames(LOB_outright)==column_name1)
            column_name2 <- paste0("Bid_Qty_",as.character(message_all_outright$PX_depth[k]))
            column_index2 <- which(colnames(LOB_outright)==column_name2)
            column_name3 <- paste0("Bid_Ord_",as.character(message_all_outright$PX_depth[k]))
            column_index3 <- which(colnames(LOB_outright)==column_name3)
            
            ## not the last order
            if(column_index1>5){
              
              if (k==1){
                
                LOB_outright[k,column_index1] <- message_all_outright$PX[k]
                
                LOB_outright[k,column_index2] <- message_all_outright$Qty[k]
                
                LOB_outright[k,column_index3] <- message_all_outright$Ord[k]
                
              } 
              else{
                
                LOB_outright[k,c(5:64)] <- LOB_outright[k-1,c(5:64)]  
                
                LOB_outright[k,column_index1] <- message_all_outright$PX[k]
                
                LOB_outright[k,column_index2] <- message_all_outright$Qty[k]
                
                LOB_outright[k,column_index3] <- message_all_outright$Ord[k]
                
                LOB_outright[k,c(5:(column_index3-3))] <- LOB_outright[k-1,c(8:column_index3)]
                
              } 
            }
            
            
            else {
              LOB_outright[k,c(5:64)] <- LOB_outright[k-1,c(5:64)]
              ## assign PX
              column_name1 <- paste0("Bid_PX_",as.character(message_all_outright$PX_depth[k]))
              column_index1 <- which(colnames(LOB_outright)==column_name1)
              LOB_outright[k,column_index1] <- message_all_outright$PX[k]
              
              ## assign Qty  
              column_name2 <- paste0("Bid_Qty_",as.character(message_all_outright$PX_depth[k]))
              column_index2 <- which(colnames(LOB_outright)==column_name2)
              LOB_outright[k,column_index2] <- message_all_outright$Qty[k] 
              
              ## assign n_order 
              column_name3 <- paste0("Bid_Ord_",as.character(message_all_outright$PX_depth[k]))
              column_index3 <- which(colnames(LOB_outright)==column_name3)
              LOB_outright[k,column_index3] <- message_all_outright$Ord[k]
            }
            
          }
          
          
          
          ## ask order
          if(message_all_outright$bidask[k]==1){
            
            column_name1 <- paste0("Ask_PX_",as.character(message_all_outright$PX_depth[k]))
            column_name2 <- paste0("Ask_Qty_",as.character(message_all_outright$PX_depth[k]))
            column_name3 <- paste0("Ask_Ord_",as.character(message_all_outright$PX_depth[k]))
            column_index1 <- which(colnames(LOB_outright)==column_name1)
            column_index2 <- which(colnames(LOB_outright)==column_name2)
            column_index3 <- which(colnames(LOB_outright)==column_name3)
            
            if(column_index1<62){
              
              if(k==1){
                
                LOB_outright[k,column_index1] <- message_all_outright$PX[k] 
                
                LOB_outright[k,column_index2] <- message_all_outright$Qty[k] 
                
                LOB_outright[k,column_index3] <- message_all_outright$Ord[k]
              } 
              else{
                
                ## process the previous information    
                
                LOB_outright[k,c(5:64)] <- LOB_outright[k-1,c(5:64)]  
                
                LOB_outright[k,column_index1] <- message_all_outright$PX[k] 
                
                LOB_outright[k,column_index2] <- message_all_outright$Qty[k] 
                
                LOB_outright[k,column_index3] <- message_all_outright$Ord[k]
                
                LOB_outright[k,c((column_index1+3):64)] <- LOB_outright[k-1, c(column_index1:61)]
                
              } 
              
            }
            else {
              ## assign PX
              LOB_outright[k,c(5:64)] <- LOB_outright[k-1,c(5:64)]
              column_name1 <- paste0("Ask_PX_",as.character(message_all_outright$PX_depth[k]))
              column_index1 <- which(colnames(LOB_outright)==column_name1)
              LOB_outright[k,column_index1] <- message_all_outright$PX[k]
              
              ## assign Qty  
              column_name2 <- paste0("Ask_Qty_",as.character(message_all_outright$PX_depth[k]))
              column_index2 <- which(colnames(LOB_outright)==column_name2)
              LOB_outright[k,column_index2] <- message_all_outright$Qty[k] 
              
              ## assign n_order 
              column_name3 <- paste0("Ask_Ord_",as.character(message_all_outright$PX_depth[k]))
              column_index3 <- which(colnames(LOB_outright)==column_name3)
              LOB_outright[k,column_index3] <- message_all_outright$Ord[k]
              
            }
          }
        }
        
        
        
        ################################### order modification  ##########################################
        
        
        if(message_all_outright$Update[k]==1){
          
          ## bid order
          
          if(message_all_outright$bidask[k]==0){
            
            if(k!=1){
              LOB_outright[k,c(5:64)] <- LOB_outright[k-1,c(5:64)]
            }
            
            ## assign PX  
            column_name1 <- paste0("Bid_PX_",as.character(message_all_outright$PX_depth[k]))
            column_index1 <- which(colnames(LOB_outright)==column_name1)
            LOB_outright[k,column_index1] <- message_all_outright$PX[k] 
            
            ## assign Qty  
            column_name2 <- paste0("Bid_Qty_",as.character(message_all_outright$PX_depth[k]))
            column_index2 <- which(colnames(LOB_outright)==column_name2)
            LOB_outright[k,column_index2] <- message_all_outright$Qty[k] 
            
            ## assign n_order 
            column_name3 <- paste0("Bid_Ord_",as.character(message_all_outright$PX_depth[k]))
            column_index3 <- which(colnames(LOB_outright)==column_name3)
            LOB_outright[k,column_index3] <- message_all_outright$Ord[k]
          }
          
          ## ask order
          else {
            
            if(k!=1){
              
              LOB_outright[k,c(5:64)] <- LOB_outright[k-1,c(5:64)]
              
            }
            ## assign PX  
            column_name1 <- paste0("Ask_PX_",as.character(message_all_outright$PX_depth[k]))
            column_index1 <- which(colnames(LOB_outright)==column_name1)
            LOB_outright[k,column_index1] <- message_all_outright$PX[k] 
            
            ## assign Qty  
            column_name2 <- paste0("Ask_Qty_",as.character(message_all_outright$PX_depth[k]))
            column_index2 <- which(colnames(LOB_outright)==column_name2)
            LOB_outright[k,column_index2] <- message_all_outright$Qty[k] 
            
            ## assign n_order 
            column_name3 <- paste0("Ask_Ord_",as.character(message_all_outright$PX_depth[k]))
            column_index3 <- which(colnames(LOB_outright)==column_name3)
            LOB_outright[k,column_index3] <- message_all_outright$Ord[k]
            
          } 
        }
        
        
        ################################### order cancellation #####################################################
        if(message_all_outright$Update[k]==2){
          
          ## move forward
          ## bid order
          if(message_all_outright$bidask[k]==0){
            
            column_name1 <- paste0("Bid_PX_",as.character(message_all_outright$PX_depth[k]))
            column_index1 <- which(colnames(LOB_outright)==column_name1)
            column_name2 <- paste0("Bid_Qty_",as.character(message_all_outright$PX_depth[k]))
            column_index2 <- which(colnames(LOB_outright)==column_name2)
            column_name3 <- paste0("Bid_Ord_",as.character(message_all_outright$PX_depth[k]))
            column_index3 <- which(colnames(LOB_outright)==column_name3)
            
            ## not the last order
            if(k!=1){
              if(column_index3>8){
                
                LOB_outright[k,c(5:64)] <- LOB_outright[k-1,c(5:64)]
                LOB_outright[k,c(8:column_index3)] <- LOB_outright[k-1, c(5:(column_index3-3))]
                LOB_outright[k, c(5:7)] <- 0
                
              }
              
              else {
                ## assign PX
                
                LOB_outright[k,c(5:64)] <- LOB_outright[k-1,c(5:64)]
                LOB_outright[k, c(5:7)] <- 0
                
              }
              
            } else{
              LOB_outright[k,column_index1] <- 0
              LOB_outright[k,column_index2] <- 0
              LOB_outright[k,column_index3] <- 0
            }
          }
          
          ## ask order
          if(message_all_outright$bidask[k]==1){
            
            column_name1 <- paste0("Ask_PX_",as.character(message_all_outright$PX_depth[k]))
            column_index1 <- which(colnames(LOB_outright)==column_name1)
            column_name2 <- paste0("Ask_Qty_",as.character(message_all_outright$PX_depth[k]))
            column_index2 <- which(colnames(LOB_outright)==column_name2)
            column_name3 <- paste0("Ask_Ord_",as.character(message_all_outright$PX_depth[k]))
            column_index3 <- which(colnames(LOB_outright)==column_name3)
            
            ## not the last order
            if(k!=1){
              if(column_index1<62){
                
                LOB_outright[k,c(5:64)] <- LOB_outright[k-1,c(5:64)]
                LOB_outright[k,c(column_index1:61)] <- LOB_outright[k-1, c((column_index1+3):64)]
                LOB_outright[k, c(62:64)] <- 0
                
              } 
              
              else {
                ## assign PX
                
                LOB_outright[k,c(5:64)] <- LOB_outright[k-1,c(5:64)]
                LOB_outright[k, c(62:64)] <- 0
                
              }
              
            } else{
              
              LOB_outright[k,column_index1] <- 0
              LOB_outright[k,column_index2] <- 0
              LOB_outright[k,column_index3] <- 0
              
            }
          }
        }
      }
      rm(column_index3,column_name3)
    }
    
   
    
    ############################################ implied orders ##############################################
    if(dim(message_all_implied)[1]!=0){
      
      
      for (n in 1:dim(message_all_implied)[1]) {
        
        #   print(n)
        
        
        
        ########################################## Order submission ########################################
        
        if(message_all_implied$Update[n]==0){
          
          ## bid order
          if(message_all_implied$bidask[n]==0 | message_all_implied$bidask[n]=="E"){
            
            column_name1 <- paste0("Bid_PX_",as.character(message_all_implied$PX_depth[n]))
            column_index1 <- which(colnames(LOB_implied)==column_name1)
            column_name2 <- paste0("Bid_Qty_",as.character(message_all_implied$PX_depth[n]))
            column_index2 <- which(colnames(LOB_implied)==column_name2)
            
            ## not the last order
            if(column_index1>5){
              
              if (n==1){
                
                LOB_implied[n,column_index1] <- message_all_implied$PX[n]
                
                LOB_implied[n,column_index2] <- message_all_implied$Qty[n]
                
              } 
              else{
                
                LOB_implied[n,c(5:12)] <- LOB_implied[n-1,c(5:12)]  
                
                LOB_implied[n,column_index1] <- message_all_implied$PX[n]
                
                LOB_implied[n,column_index2] <- message_all_implied$Qty[n]
                
                LOB_implied[n,c(5:(column_index2-2))] <- LOB_implied[n-1,c(7:column_index2)]
                
              } 
            }
            
            
            else {
              LOB_implied[n,c(5:12)] <- LOB_implied[n-1,c(5:12)]
              ## assign PX
              column_name1 <- paste0("Bid_PX_",as.character(message_all_implied$PX_depth[n]))
              column_index1 <- which(colnames(LOB_implied)==column_name1)
              LOB_implied[n,column_index1] <- message_all_implied$PX[n]
              
              ## assign Qty  
              column_name2 <- paste0("Bid_Qty_",as.character(message_all_implied$PX_depth[n]))
              column_index2 <- which(colnames(LOB_implied)==column_name2)
              LOB_implied[n,column_index2] <- message_all_implied$Qty[n] 
              
              
            }
            
          }
          
          
          
          ## ask order
          if(message_all_implied$bidask[n]==1 | message_all_implied$bidask[n]=="F"){
            
            column_name1 <- paste0("Ask_PX_",as.character(message_all_implied$PX_depth[n]))
            column_name2 <- paste0("Ask_Qty_",as.character(message_all_implied$PX_depth[n]))
            column_index1 <- which(colnames(LOB_implied)==column_name1)
            column_index2 <- which(colnames(LOB_implied)==column_name2)
            
            if(column_index1<11){
              
              if(n==1){
                
                LOB_implied[n,column_index1] <- message_all_implied$PX[n] 
                
                LOB_implied[n,column_index2] <- message_all_implied$Qty[n] 
                
              } 
              else{
                
                ## process the previous information    
                
                LOB_implied[n,c(5:12)] <- LOB_implied[n-1,c(5:12)]  
                
                LOB_implied[n,column_index1] <- message_all_implied$PX[n] 
                
                LOB_implied[n,column_index2] <- message_all_implied$Qty[n] 
                
                LOB_implied[n,c((column_index1+2):12)] <- LOB_implied[n-1, c(column_index1:10)]
                
              } 
              
            }
            else {
              ## assign PX
              LOB_implied[n,c(5:12)] <- LOB_implied[n-1,c(5:12)]
              column_name1 <- paste0("Ask_PX_",as.character(message_all_implied$PX_depth[n]))
              column_index1 <- which(colnames(LOB_implied)==column_name1)
              LOB_implied[n,column_index1] <- message_all_implied$PX[n]
              
              ## assign Qty  
              column_name2 <- paste0("Ask_Qty_",as.character(message_all_implied$PX_depth[n]))
              column_index2 <- which(colnames(LOB_implied)==column_name2)
              LOB_implied[n,column_index2] <- message_all_implied$Qty[n] 
              
            }
          }
        }
        
        
        
        ################################### order modification  ##########################################
        
        
        if(message_all_implied$Update[n]==1){
          
          ## bid order
          
          if(message_all_implied$bidask[n]==0 | message_all_implied$bidask[n]=="E"){
            
            if(n!=1){
              LOB_implied[n,c(5:12)] <- LOB_implied[n-1,c(5:12)]
              
            }
            
            ## assign PX  
            column_name1 <- paste0("Bid_PX_",as.character(message_all_implied$PX_depth[n]))
            column_index1 <- which(colnames(LOB_implied)==column_name1)
            LOB_implied[n,column_index1] <- message_all_implied$PX[n] 
            
            ## assign Qty  
            column_name2 <- paste0("Bid_Qty_",as.character(message_all_implied$PX_depth[n]))
            column_index2 <- which(colnames(LOB_implied)==column_name2)
            LOB_implied[n,column_index2] <- message_all_implied$Qty[n] 
            
          }
          
          ## asn order
          else if(message_all_implied$bidask[n]==1 | message_all_implied$bidask[n]=="F"){
            
            if(n!=1){
              LOB_implied[n,c(5:12)] <- LOB_implied[n-1,c(5:12)]
            }
            
            ## assign PX  
            column_name1 <- paste0("Ask_PX_",as.character(message_all_implied$PX_depth[n]))
            column_index1 <- which(colnames(LOB_implied)==column_name1)
            LOB_implied[n,column_index1] <- message_all_implied$PX[n] 
            
            ## assign Qty  
            column_name2 <- paste0("Ask_Qty_",as.character(message_all_implied$PX_depth[n]))
            column_index2 <- which(colnames(LOB_implied)==column_name2)
            LOB_implied[n,column_index2] <- message_all_implied$Qty[n] 
            
            
          } 
        }
        
        
        ################################### order cancellation #####################################################
        
        if(message_all_implied$Update[n]==2){
          
          ## move forward
          ## bid order
          if(message_all_implied$bidask[n]==0 | message_all_implied$bidask[n]=="E"){
            
            column_name1 <- paste0("Bid_PX_",as.character(message_all_implied$PX_depth[n]))
            column_index1 <- which(colnames(LOB_implied)==column_name1)
            column_name2 <- paste0("Bid_Qty_",as.character(message_all_implied$PX_depth[n]))
            column_index2 <- which(colnames(LOB_implied)==column_name2)
            
            ## not the last order
            if(n!=1){
              if(column_index2>6){
                
                LOB_implied[n,c(5:12)] <- LOB_implied[n-1,c(5:12)]
                LOB_implied[n,c(7:column_index2)] <- LOB_implied[n-1, c(5:(column_index2-2))]
                LOB_implied[n, c(5:6)] <- 0
                
              }
              
              else{
                ## assign PX
                
                LOB_implied[n,c(5:12)] <- LOB_implied[n-1,c(5:12)]
                LOB_implied[n, c(5:6)] <- 0
                
              }
            } else{
              LOB_implied[n,column_index1] <- 0
              LOB_implied[n,column_index2] <- 0
            }
          }
          
          
          ## ask order
          if(message_all_implied$bidask[n]==1 | message_all_implied$bidask[n]=="F"){
            
            column_name1 <- paste0("Ask_PX_",as.character(message_all_implied$PX_depth[n]))
            column_index1 <- which(colnames(LOB_implied)==column_name1)
            column_name2 <- paste0("Ask_Qty_",as.character(message_all_implied$PX_depth[n]))
            column_index2 <- which(colnames(LOB_implied)==column_name2)
            
            
            ## not the last order
            if(n!=1){
              if(column_index1<11){
                
                LOB_implied[n,c(5:12)] <- LOB_implied[n-1,c(5:12)]
                LOB_implied[n,c(column_index1:10)] <- LOB_implied[n-1, c((column_index1+2):12)]
                LOB_implied[n, c(11:12)] <- 0
                
              } 
              
              else {
                ## assign PX
                
                LOB_implied[n,c(5:12)] <- LOB_implied[n-1,c(5:12)]
                LOB_implied[n, c(11:12)] <- 0
                
              }
            } else{
              
              LOB_implied[n,column_index1] <- 0
              LOB_implied[n,column_index2] <- 0
              
            }
          }
        }
        
      }
      rm(column_index1,column_index2,column_name1,column_name2)
    }
    ## delete the unnecessary variables
    
    
    ###################################### Consolidated book ###########################################
    ## This part is used to consolidate the implied orders and outright orders, to get a general limit order book of the futures markets
    ## merge the two tables
    if(dim(message_all_implied)[1]!=0 & dim(message_all_outright)[1]!=0){
      
      LOB_implied_new <- matrix(NA,nrow=dim(message_all_outright)[1],ncol=13) 
      
      colnames(LOB_implied_new)[c(1:13)] <- c("Date", "Time", "Seq", "Code",
                                              "Bid_PX_2", "Bid_Qty_2", 
                                              "Bid_PX_1", "Bid_Qty_1",
                                              "Ask_PX_1", "Ask_Qty_1",
                                              "Ask_PX_2", "Ask_Qty_2", "MsgSeq"
                                              
      )
      
      LOB_implied_new[,3] <- message_all_outright$Seq
      LOB_implied_new[,13] <- as.numeric(message_all_outright$MsgSeq)
      LOB_implied_new <- rbind(LOB_implied_new,LOB_implied)
      LOB_implied_new <- as.data.table(LOB_implied_new)
      setkey(LOB_implied_new,MsgSeq)
      LOB_implied_new[,c(5:13)] <- nafill(LOB_implied_new[,c(5:13)],"locf")
      LOB_implied_new[which(is.na(LOB_implied_new$Bid_PX_1==TRUE)),c(5:13)] <-0 
      
      ###------------------------------------------------------------------------------------------------------------
      LOB_outright_new <- matrix(NA, nrow=dim(message_all_implied)[1], ncol=65)
      
      colnames(LOB_outright_new)[c(1:65)] <- c("Date", "Time", "Seq", "Code",
                                                                                  "Bid_PX_10", "Bid_Qty_10", "Bid_Ord_10", "Bid_PX_9", "Bid_Qty_9", "Bid_Ord_9", "Bid_PX_8", "Bid_Qty_8", "Bid_Ord_8",
                                                                                  "Bid_PX_7", "Bid_Qty_7", "Bid_Ord_7", "Bid_PX_6", "Bid_Qty_6", "Bid_Ord_6", "Bid_PX_5", "Bid_Qty_5", "Bid_Ord_5",
                                                                                  "Bid_PX_4", "Bid_Qty_4", "Bid_Ord_4", "Bid_PX_3", "Bid_Qty_3", "Bid_Ord_3", "Bid_PX_2", "Bid_Qty_2", "Bid_Ord_2",
                                                                                  "Bid_PX_1", "Bid_Qty_1", "Bid_Ord_1",
                                                                                  "Ask_PX_1", "Ask_Qty_1", "Ask_Ord_1", "Ask_PX_2", "Ask_Qty_2", "Ask_Ord_2", "Ask_PX_3", "Ask_Qty_3", "Ask_Ord_3",
                                                                                  "Ask_PX_4", "Ask_Qty_4", "Ask_Ord_4", "Ask_PX_5", "Ask_Qty_5", "Ask_Ord_5", "Ask_PX_6", "Ask_Qty_6", "Ask_Ord_6",
                                                                                  "Ask_PX_7", "Ask_Qty_7", "Ask_Ord_7", "Ask_PX_8", "Ask_Qty_8", "Ask_Ord_8", "Ask_PX_9", "Ask_Qty_9", "Ask_Ord_9",
                                                                                  "Ask_PX_10", "Ask_Qty_10", "Ask_Ord_10", "MsgSeq"
      )
      LOB_outright_new[,3] <- message_all_implied$Seq
      LOB_outright_new[,65] <- as.numeric(message_all_implied$MsgSeq)
      LOB_outright_new <- rbind(LOB_outright_new,LOB_outright)
      LOB_outright_new <- as.data.table(LOB_outright_new)
      setkey(LOB_outright_new,MsgSeq)
      LOB_outright_new[,c(5:65)] <- nafill(LOB_outright_new[,c(5:65)],"locf")
      LOB_outright_new <- as.matrix(LOB_outright_new)
      
      rm(LOB_implied,LOB_outright)
      
      ##-----------------------------------------------------------------------------------------------------------------
      
      rm(message_all_implied,message_all_outright)
      
      ## consolidating
      
      
      LOB_conso_list <- list()
      
      for (a in 1:dim(message_all)[1]) {
        
        LOB_conso <- matrix(as.numeric(0),nrow=1,ncol=65) 
        
        colnames(LOB_conso)[c(1:65)] <- c("Date", "Time", "Seq", "Code",
                                          "Bid_PX_10", "Bid_Qty_10", "Bid_Ord_10", "Bid_PX_9", "Bid_Qty_9", "Bid_Ord_9", "Bid_PX_8", "Bid_Qty_8", "Bid_Ord_8",
                                          "Bid_PX_7", "Bid_Qty_7", "Bid_Ord_7", "Bid_PX_6", "Bid_Qty_6", "Bid_Ord_6", "Bid_PX_5", "Bid_Qty_5", "Bid_Ord_5",
                                          "Bid_PX_4", "Bid_Qty_4", "Bid_Ord_4", "Bid_PX_3", "Bid_Qty_3", "Bid_Ord_3", "Bid_PX_2", "Bid_Qty_2", "Bid_Ord_2",
                                          "Bid_PX_1", "Bid_Qty_1", "Bid_Ord_1",
                                          "Ask_PX_1", "Ask_Qty_1", "Ask_Ord_1", 
                                          "Ask_PX_2", "Ask_Qty_2", "Ask_Ord_2", "Ask_PX_3", "Ask_Qty_3", "Ask_Ord_3",
                                          "Ask_PX_4", "Ask_Qty_4", "Ask_Ord_4", "Ask_PX_5", "Ask_Qty_5", "Ask_Ord_5", "Ask_PX_6", "Ask_Qty_6", "Ask_Ord_6",
                                          "Ask_PX_7", "Ask_Qty_7", "Ask_Ord_7", "Ask_PX_8", "Ask_Qty_8", "Ask_Ord_8", "Ask_PX_9", "Ask_Qty_9", "Ask_Ord_9",
                                          "Ask_PX_10", "Ask_Qty_10", "Ask_Ord_10", "MsgSeq"
                                          
        )
        

        
            # print(a)
        
        LOB_conso[, c(5:65)] <- LOB_outright_new[a, c(5:65)]
        
        if (LOB_implied_new$Bid_PX_1[a]!=0){
          
          if(LOB_implied_new$Bid_PX_1[a] %in% LOB_conso[,c(seq(5,32,3))]==TRUE){
            
            bid1_index <- as.numeric(3*which(LOB_conso[,c(seq(5,32,3))]==LOB_implied_new$Bid_PX_1[a], arr.ind = TRUE)+2)
            
            LOB_conso[,bid1_index] <- LOB_implied_new$Bid_PX_1[a]
            LOB_conso[,bid1_index+1] <- LOB_implied_new$Bid_Qty_1[a]+LOB_conso[,bid1_index+1]
            
            
          }
          
          else{
            
            
            px_seq <- as.numeric(LOB_conso[,c(seq(5,32,3))])
            
            conso_px <- which(sort(c(px_seq[px_seq>0],as.numeric(LOB_implied_new$Bid_PX_1[a])),decreasing = TRUE)==as.numeric(LOB_implied_new$Bid_PX_1[a]))
            
            if(conso_px <= 10){
              
              conso_px_lv <- paste0("Bid_PX_",as.character(conso_px))
              conso_px_id <- which(colnames(LOB_conso)==conso_px_lv)
              
              if(conso_px_id >5){
                
                
                LOB_conso[, c(5:(conso_px_id-1))] <- LOB_conso[, c(8:(conso_px_id+2))]
                
                
                LOB_conso[,conso_px_id] <-  LOB_implied_new$Bid_PX_1[a]
                LOB_conso[,conso_px_id+1] <- LOB_implied_new$Bid_Qty_1[a]
                LOB_conso[,conso_px_id+2] <-  0
                
              } else {
                
                LOB_conso[,conso_px_id] <-  LOB_implied_new$Bid_PX_1[a]
                LOB_conso[,conso_px_id+1] <- LOB_implied_new$Bid_Qty_1[a]
                LOB_conso[,conso_px_id+2] <-  0
                
              }
              
              
              
            }
          }
        }
        
        if(LOB_implied_new$Bid_PX_2[a]!=0 ){
          
          
          
          if(LOB_implied_new$Bid_PX_2[a] %in% LOB_conso[,c(seq(5,32,3))]==TRUE){
            
            bid2_index <- as.numeric(3*which(LOB_conso[,c(seq(5,32,3))]==LOB_implied_new$Bid_PX_2[a], arr.ind = TRUE)+2)
            
            
            LOB_conso[,bid2_index] <- LOB_implied_new$Bid_PX_2[a]
            LOB_conso[,bid2_index+1] <- LOB_implied_new$Bid_Qty_2[a]+LOB_conso[,bid2_index+1]
            
          }
          
          else{
            
            
            px_seq <- as.numeric(LOB_conso[,c(seq(5,32,3))])
            
            conso_px <- which(sort(c(px_seq[px_seq>0],as.numeric(LOB_implied_new$Bid_PX_2[a])),decreasing = TRUE)==as.numeric(LOB_implied_new$Bid_PX_2[a]))
            
            if(conso_px <= 10){
              
              conso_px_lv <- paste0("Bid_PX_",as.character(conso_px))
              conso_px_id <- which(colnames(LOB_conso)==conso_px_lv)
              
              if(conso_px_id >5){
                
                
                LOB_conso[, c(5:(conso_px_id-1))] <- LOB_conso[, c(8:(conso_px_id+2))]
                
                LOB_conso[,conso_px_id] <-  LOB_implied_new$Bid_PX_2[a]
                LOB_conso[,conso_px_id+1] <- LOB_implied_new$Bid_Qty_2[a]
                LOB_conso[,conso_px_id+2] <-  0
                
              } else {
                
                LOB_conso[,conso_px_id] <-  LOB_implied_new$Bid_PX_2[a]
                LOB_conso[,conso_px_id+1] <- LOB_implied_new$Bid_Qty_2[a]
                LOB_conso[,conso_px_id+2] <-  0
                
              }
              
            }
          }
          
        }
        ## ask orders  
        if (LOB_implied_new$Ask_PX_1[a]!=0 ){
          
          if(LOB_implied_new$Ask_PX_1[a] %in% LOB_conso[,c(seq(35,64,3))]==TRUE){
            
            ask1_index <- as.numeric(3*which(LOB_conso[,c(seq(35,64,3))]==LOB_implied_new$Ask_PX_1[a], arr.ind = TRUE)+32)
            
            LOB_conso[,ask1_index] <- LOB_implied_new$Ask_PX_1[a]
            LOB_conso[,ask1_index+1] <- LOB_implied_new$Ask_Qty_1[a]+LOB_conso[,ask1_index+1]
            
          }
          
          else{
            
            
            px_seq <- as.numeric(LOB_conso[,c(seq(35,64,3))])
            
            conso_px <- which(sort(c(px_seq[px_seq>0],as.numeric(LOB_implied_new$Ask_PX_1[a])),decreasing = FALSE)==as.numeric(LOB_implied_new$Ask_PX_1[a]))
            
            if(conso_px <= 10){
              
              conso_px_lv <- paste0("Ask_PX_",as.character(conso_px))
              conso_px_id <- which(colnames(LOB_conso)==conso_px_lv)
              
              if(conso_px_id <62){
                
                
                
                LOB_conso[, c((conso_px_id+3):64)] <- LOB_conso[, c(conso_px_id:61)]
                
                
                
                LOB_conso[,conso_px_id] <-  LOB_implied_new$Ask_PX_1[a]
                LOB_conso[,conso_px_id+1] <- LOB_implied_new$Ask_Qty_1[a]
                LOB_conso[,conso_px_id+2] <-  0
                
              } else {
                
                LOB_conso[,conso_px_id] <-  LOB_implied_new$Ask_PX_1[a]
                LOB_conso[,conso_px_id+1] <- LOB_implied_new$Ask_Qty_1[a]
                LOB_conso[,conso_px_id+2] <-  0
                
              }
              
            }
          }
        }
        
        if(LOB_implied_new$Ask_PX_2[a]!=0 ){
          
          if(LOB_implied_new$Ask_PX_2[a] %in% LOB_conso[,c(seq(35,64,3))]==TRUE){
            
            ask2_index <- as.numeric(3*which(LOB_conso[,c(seq(35,64,3))]==LOB_implied_new$Ask_PX_2[a], arr.ind = TRUE)+32)
            
            
            LOB_conso[,ask2_index] <- LOB_implied_new$Ask_PX_2[a]
            LOB_conso[,ask2_index+1] <- LOB_implied_new$Ask_Qty_2[a]+LOB_conso[,ask2_index+1]
            
          }
          
          else{
            
            
            px_seq <- as.numeric(LOB_conso[,c(seq(35,64,3))])
            
            conso_px <- which(sort(c(px_seq[px_seq>0],as.numeric(LOB_implied_new$Ask_PX_2[a])),decreasing = FALSE)==as.numeric(LOB_implied_new$Ask_PX_2[a]))
            
            if(conso_px <= 10){
              
              conso_px_lv <- paste0("Ask_PX_",as.character(conso_px))
              conso_px_id <- which(colnames(LOB_conso)==conso_px_lv)
              
              if(conso_px_id <62){
                
                
                
                LOB_conso[, c((conso_px_id+3):64)] <- LOB_conso[, c(conso_px_id:61)]
                
                
                
                LOB_conso[,conso_px_id] <-  LOB_implied_new$Ask_PX_2[a]
                LOB_conso[,conso_px_id+1] <- LOB_implied_new$Ask_Qty_2[a]
                LOB_conso[,conso_px_id+2] <-  0
                
              } else {
                
                LOB_conso[,conso_px_id] <-  LOB_implied_new$Ask_PX_2[a]
                LOB_conso[,conso_px_id+1] <- LOB_implied_new$Ask_Qty_2[a]
                LOB_conso[,conso_px_id+2] <-  0
                
              }
              
            }
          }
          
          
        }
        
        LOB_conso_list[[a]] <- LOB_conso
      }
      
      
      ## assign the date, time, and code
      
        LOB_conso <- as.data.table(data.table::transpose(LOB_conso_list))
        colnames(LOB_conso)[c(1:65)] <- c("Date", "Time", "Seq", "Code",
                                          "Bid_PX_10", "Bid_Qty_10", "Bid_Ord_10", "Bid_PX_9", "Bid_Qty_9", "Bid_Ord_9", "Bid_PX_8", "Bid_Qty_8", "Bid_Ord_8",
                                          "Bid_PX_7", "Bid_Qty_7", "Bid_Ord_7", "Bid_PX_6", "Bid_Qty_6", "Bid_Ord_6", "Bid_PX_5", "Bid_Qty_5", "Bid_Ord_5",
                                          "Bid_PX_4", "Bid_Qty_4", "Bid_Ord_4", "Bid_PX_3", "Bid_Qty_3", "Bid_Ord_3", "Bid_PX_2", "Bid_Qty_2", "Bid_Ord_2",
                                          "Bid_PX_1", "Bid_Qty_1", "Bid_Ord_1",
                                          "Ask_PX_1", "Ask_Qty_1", "Ask_Ord_1", 
                                          "Ask_PX_2", "Ask_Qty_2", "Ask_Ord_2", "Ask_PX_3", "Ask_Qty_3", "Ask_Ord_3",
                                          "Ask_PX_4", "Ask_Qty_4", "Ask_Ord_4", "Ask_PX_5", "Ask_Qty_5", "Ask_Ord_5", "Ask_PX_6", "Ask_Qty_6", "Ask_Ord_6",
                                          "Ask_PX_7", "Ask_Qty_7", "Ask_Ord_7", "Ask_PX_8", "Ask_Qty_8", "Ask_Ord_8", "Ask_PX_9", "Ask_Qty_9", "Ask_Ord_9",
                                          "Ask_PX_10", "Ask_Qty_10", "Ask_Ord_10", "MsgSeq"
                                          
        )
      
      
      LOB_conso$Seq <- message_all[, "Seq"]
      LOB_conso$MsgSeq <- message_all[, "MsgSeq"]
      LOB_conso$Date <- message_all[, "Date"]
      LOB_conso$Time <- message_all[, "Time"]
      LOB_conso$Code <- message_all[, "Code"]
      #LOB_conso$MsgSeq <- message_all[, "MsgSeq"]
      
      rm(LOB_implied,LOB_implied_new,LOB_outright)
      
     
      
      
    }else {
      
      if(dim(LOB_outright)[1]!=0){
        
        LOB_conso <- as.data.table(LOB_outright)
        #LOB_conso$MsgSeq <- message_all_outright$MsgSeq
      }
      
      else if(dim(LOB_implied)[1]!=0){
        
        LOB_conso <- as.data.table(LOB_implied)
        #LOB_conso$MsgSeq <- message_all_implied$MsgSeq
      }
      
    }
    
    
    for (m in 1:length(date)) {
      
      print(date[m])
      
      LOB_date <- LOB_conso[Date==date[m],]
      
      LOB_date$Seq <- as.numeric(LOB_date$Seq)
      
      LOB_date$Bid_PX_10 <- as.numeric(LOB_date$Bid_PX_10)
      LOB_date$Bid_Qty_10 <- as.numeric(LOB_date$Bid_Qty_10)
      LOB_date$Bid_Ord_10 <- as.numeric(LOB_date$Bid_Ord_10)
      LOB_date$Bid_PX_9 <- as.numeric(LOB_date$Bid_PX_9)
      LOB_date$Bid_Qty_9 <- as.numeric(LOB_date$Bid_Qty_9)
      LOB_date$Bid_Ord_9 <- as.numeric(LOB_date$Bid_Ord_9)
      LOB_date$Bid_PX_8 <- as.numeric(LOB_date$Bid_PX_8)
      LOB_date$Bid_Qty_8 <- as.numeric(LOB_date$Bid_Qty_8)
      LOB_date$Bid_Ord_8 <- as.numeric(LOB_date$Bid_Ord_8)
      LOB_date$Bid_PX_7 <- as.numeric(LOB_date$Bid_PX_7)
      LOB_date$Bid_Qty_7 <- as.numeric(LOB_date$Bid_Qty_7)
      LOB_date$Bid_Ord_7 <- as.numeric(LOB_date$Bid_Ord_7)
      LOB_date$Bid_PX_6 <- as.numeric(LOB_date$Bid_PX_6)
      LOB_date$Bid_Qty_6 <- as.numeric(LOB_date$Bid_Qty_6)
      LOB_date$Bid_Ord_6 <- as.numeric(LOB_date$Bid_Ord_6)
      LOB_date$Bid_PX_5 <- as.numeric(LOB_date$Bid_PX_5)
      LOB_date$Bid_Qty_5 <- as.numeric(LOB_date$Bid_Qty_5)
      LOB_date$Bid_Ord_5 <- as.numeric(LOB_date$Bid_Ord_5)
      LOB_date$Bid_PX_4 <- as.numeric(LOB_date$Bid_PX_4)
      LOB_date$Bid_Qty_4 <- as.numeric(LOB_date$Bid_Qty_4)
      LOB_date$Bid_Ord_4 <- as.numeric(LOB_date$Bid_Ord_4)
      LOB_date$Bid_PX_3 <- as.numeric(LOB_date$Bid_PX_3)
      LOB_date$Bid_Qty_3 <- as.numeric(LOB_date$Bid_Qty_3)
      LOB_date$Bid_Ord_3 <- as.numeric(LOB_date$Bid_Ord_3)
      LOB_date$Bid_PX_2 <- as.numeric(LOB_date$Bid_PX_2)
      LOB_date$Bid_Qty_2 <- as.numeric(LOB_date$Bid_Qty_2)
      LOB_date$Bid_Ord_2 <- as.numeric(LOB_date$Bid_Ord_2)
      LOB_date$Bid_PX_1 <- as.numeric(LOB_date$Bid_PX_1)
      LOB_date$Bid_Qty_1 <- as.numeric(LOB_date$Bid_Qty_1)
      LOB_date$Bid_Ord_1 <- as.numeric(LOB_date$Bid_Ord_1)
      LOB_date$Ask_PX_10 <- as.numeric(LOB_date$Ask_PX_10)
      LOB_date$Ask_Qty_10 <- as.numeric(LOB_date$Ask_Qty_10)
      LOB_date$Ask_Ord_10 <- as.numeric(LOB_date$Ask_Ord_10)
      LOB_date$Ask_PX_9 <- as.numeric(LOB_date$Ask_PX_9)
      LOB_date$Ask_Qty_9 <- as.numeric(LOB_date$Ask_Qty_9)
      LOB_date$Ask_Ord_9 <- as.numeric(LOB_date$Ask_Ord_9)
      LOB_date$Ask_PX_8 <- as.numeric(LOB_date$Ask_PX_8)
      LOB_date$Ask_Qty_8 <- as.numeric(LOB_date$Ask_Qty_8)
      LOB_date$Ask_Ord_8 <- as.numeric(LOB_date$Ask_Ord_8)
      LOB_date$Ask_PX_7 <- as.numeric(LOB_date$Ask_PX_7)
      LOB_date$Ask_Qty_7 <- as.numeric(LOB_date$Ask_Qty_7)
      LOB_date$Ask_Ord_7 <- as.numeric(LOB_date$Ask_Ord_7)
      LOB_date$Ask_PX_6 <- as.numeric(LOB_date$Ask_PX_6)
      LOB_date$Ask_Qty_6 <- as.numeric(LOB_date$Ask_Qty_6)
      LOB_date$Ask_Ord_6 <- as.numeric(LOB_date$Ask_Ord_6)
      LOB_date$Ask_PX_5 <- as.numeric(LOB_date$Ask_PX_5)
      LOB_date$Ask_Qty_5 <- as.numeric(LOB_date$Ask_Qty_5)
      LOB_date$Ask_Ord_5 <- as.numeric(LOB_date$Ask_Ord_5)
      LOB_date$Ask_PX_4 <- as.numeric(LOB_date$Ask_PX_4)
      LOB_date$Ask_Qty_4 <- as.numeric(LOB_date$Ask_Qty_4)
      LOB_date$Ask_Ord_4 <- as.numeric(LOB_date$Ask_Ord_4)
      LOB_date$Ask_PX_3 <- as.numeric(LOB_date$Ask_PX_3)
      LOB_date$Ask_Qty_3 <- as.numeric(LOB_date$Ask_Qty_3)
      LOB_date$Ask_Ord_3 <- as.numeric(LOB_date$Ask_Ord_3)
      LOB_date$Ask_PX_2 <- as.numeric(LOB_date$Ask_PX_2)
      LOB_date$Ask_Qty_2 <- as.numeric(LOB_date$Ask_Qty_2)
      LOB_date$Ask_Ord_2 <- as.numeric(LOB_date$Ask_Ord_2)
      LOB_date$Ask_PX_1 <- as.numeric(LOB_date$Ask_PX_1)
      LOB_date$Ask_Qty_1 <- as.numeric(LOB_date$Ask_Qty_1)
      LOB_date$Ask_Ord_1 <- as.numeric(LOB_date$Ask_Ord_1)
      
      
      if(dim(LOB_date)[1]!=0){
        fold <- file.path(paste0("C:/Users/ruchuan2/OneDrive - University of Illinois - Urbana/Corn_LOB","/",contract_LOB_folder[i]))
        if(!dir.exists(fold)){
          dir.create(fold)
        }
        fname <- file.path(fold,
                           paste0("xcbt_md_zc_fut_",date[m],"_","r","_",week_seq[j],"_",contract_LOB_folder[i],".rda"))
        save(LOB_date, file = fname) ## save
      }
    }
  }
gc()



}

