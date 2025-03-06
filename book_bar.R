######################################################

## Book bar with implied liquidity

 book_bar <- function(mbp_order_book, implied_book=NULL, level=NULL, animation=FALSE, anim_fps=10, ...){
  
  
   
  if("DT" %in% colnames(mbp_order_book) == T){
    
    setnames(mbp_order_book, "DT", "Time") 
    
  } 
  
   if(is.null(level)==F){
     
     columns <- c(unlist(mapply(function(x) c(paste0("Bid_PX_", x), paste0("Bid_Qty_", x), paste0("Bid_Ord_", x)), level:1)),
                  unlist(mapply(function(x) c(paste0("Ask_PX_", x), paste0("Ask_Qty_", x), paste0("Ask_Ord_", x)), 1:level)))
     
     columns_delete <- colnames(mbp_order_book)[5: (length(colnames(mbp_order_book))-1)][which(colnames(mbp_order_book)[5: (length(colnames(mbp_order_book))-1)] %in% columns ==F)]
     
     mbp_order_book <- mbp_order_book[, !..columns_delete]
   } 
  
  dcast_data <- function(data, ...){

  data <- melt(data, c("Time", "Code"), measures=patterns("PX"), 
               variable.name= "fields",
               value.name = c("values"))[, `:=`(fields=substr(fields, 5, 5), side=substr(fields, 1, 3), 
                                                depth=as.numeric(fifelse(str_sub(fields, -1)=="0", "10", str_sub(fields, -1))))][, px:=values[fields=="P"], by=.(side, depth, Time)][!fields=="P"]
  
  data <- dcast(data, Time + Code + px + side + depth ~ fields, value.var = c("values"))
  
  
  setkey(data, side, depth)
  
  data <- data[level %between% c(1, level)]
  return(data)
  }
  
  mbp_order_book <- dcast_data(mbp_order_book)
 
  if(isTRUE(animation) & (mbp_order_book[, length(unique(Time))]==1|implied_book[, length(unique(Time))]==1)){
    
    stop("Animated figure needs dataset at multiple timestamps")
    
  }
  
  if(isTRUE(animation)==FALSE & (mbp_order_book[, length(unique(Time))]!=1|implied_book[, length(unique(Time))]!=1)){
    
    stop("Static figure only needs dataset at a single timestamp")
  }
  
  if(is.null(implied_book)){
  
    
    book_plot <- ggplot()+
      geom_col(data = mbp_order_book[side=="Ask"], aes(x = px, y = Q, fill = O), 
               alpha=0.7) +
      geom_text(data = mbp_order_book[side == "Ask" & depth!=1], aes(x = px, y = Q, label = px),  
                hjust=0, position = position_fill(vjust = 0))+
      geom_text(data = mbp_order_book[side == "Ask" & depth==1], aes(x = px, y = Q, label = px, fontface = "bold"),  
                hjust=0, position = position_fill(vjust = 0))+
      geom_text(data = mbp_order_book[side == "Ask"& depth!=1], aes(x = px, y = Q, label = Q), 
                hjust=1, position = position_fill(vjust = 0))+
      geom_text(data = mbp_order_book[side == "Ask"& depth==1], aes(x = px, y = Q, label = Q, fontface = "bold"), 
                hjust=1, position = position_fill(vjust = 0))+
      scale_fill_gradientn(
        name = "#Ord: Ask",
        colors = c("green1", "yellow"),  ### Ask
        limits = c(mbp_order_book[side == "Ask", min(O)], mbp_order_book[side == "Ask", max(O)]),
        guide = guide_legend(order = 1, reverse = TRUE)
      )+
      
      new_scale_fill() +
      geom_col(data = mbp_order_book[side=="Bid"], aes(x = px, y = Q, fill = O), 
               alpha=0.7) +
      geom_text(data = mbp_order_book[side == "Bid" & depth!=1], aes(x = px, y = Q, label = px),  
                hjust=0, position = position_fill(vjust = 0))+
      geom_text(data = mbp_order_book[side == "Bid" & depth==1], aes(x = px, y = Q, label = px, fontface = "bold"),  
                hjust=0, position = position_fill(vjust = 0))+
      geom_text(data = mbp_order_book[side == "Bid"& depth!=1], aes(x = px, y = Q, label = Q), 
                hjust=1, position = position_fill(vjust = 0))+
      geom_text(data = mbp_order_book[side == "Bid"& depth==1], aes(x = px, y = Q, label = Q, fontface = "bold"), 
                hjust=1, position = position_fill(vjust = 0))+
      geom_hline(yintercept = 0, linetype = 1, linewidth=0.5, color = "blue")+
      scale_fill_gradientn(
        name = "#Ord: Bid",
        colors = c("red1", "yellow"),  ###Bid
        limits = c(mbp_order_book[side == "Bid", min(O)], mbp_order_book[side == "Bid", max(O)]),
        guide = guide_legend(order = 2, reverse = TRUE)
      )+
      labs(title = mbp_order_book[, Time], ## bbo info
           x = "Price", 
           y = "Quantity") +
      coord_flip()+
      theme_minimal()
    
    
  }else{
    
    if(dim(mbp_order_book)!=dim(implied_book)){
      
      stop("Outright book and implied book do not have the same number of observations. Resampling may be needed")
    }
    
    
    
    if("DT" %in% colnames(implied_book) == T){
      
      setnames(implied_book, "DT", "Time") 
      
    } 
    

    
    
    implied_book <- dcast_data(implied_book)
    implied_book[, type:="Implied"]
    mbp_order_book[, type:="Outright"]
    merged <- rbind(mbp_order_book, implied_book, fill=TRUE)
    
    merged[, cum_Q:=sum(Q), by=.(Time, Code, px)]
    
    
    book_plot <- ggplot()+
      geom_col(data = merged[side=="Ask"], aes(x = px, y = Q, fill = type), 
               alpha=0.7) +
      scale_fill_manual(name = "Ord Type", values = c("Implied"="grey"),
                        guide = guide_legend(order = 1, reverse = TRUE))+
      
      new_scale_fill()+
      
      geom_col(data = mbp_order_book[side=="Ask"], aes(x = px, y = Q, fill = O), 
               alpha=0.7)+
      
      geom_text(data = mbp_order_book[side == "Ask" & depth!=1], aes(x = px, y = Q, label = px),  
                hjust=0, position = position_fill(vjust=0))+
      geom_text(data = mbp_order_book[side == "Ask" & depth==1], aes(x = px, y = Q, label = px, fontface = "bold"),  
                hjust=0, position = position_fill(vjust=0))+
      geom_text(data = merged[side == "Ask"& depth!=1 & type=="Outright"], aes(x = px, y = cum_Q, label = cum_Q), 
                hjust=1, position = position_fill(vjust=0))+
      geom_text(data = merged[side == "Ask"& depth==1  & type=="Outright"], aes(x = px, y = cum_Q, label = cum_Q, fontface = "bold"), 
                hjust=1, position = position_fill(vjust=0))+
      scale_fill_gradientn(
        name = "#Ord: Ask",
        colors = c("green1", "yellow"),  ### Ask
        limits = c(mbp_order_book[side == "Ask", min(O)], mbp_order_book[side == "Ask", max(O)]),
        guide = guide_legend(order = 2, reverse = TRUE)
      )+
      
      new_scale_fill() +
      geom_col(data = merged[side=="Bid"], aes(x = px, y = Q, fill = type), 
               alpha=0.7) +
      scale_fill_manual(name = "Ord Type", values = c("Implied"="grey"),
                        guide = guide_legend(order = 1, reverse = TRUE))+
      
      new_scale_fill()+
      
      geom_col(data = mbp_order_book[side=="Bid"], aes(x = px, y = Q, fill = O), 
               alpha=0.7) +
      
      geom_text(data = mbp_order_book[side == "Bid" & depth!=1], aes(x = px, y = Q, label = px),  
                hjust=0, position = position_fill(vjust = 0))+
      geom_text(data = mbp_order_book[side == "Bid" & depth==1], aes(x = px, y = Q, label = px, fontface = "bold"),  
                hjust=0, position = position_fill(vjust = 0))+
      geom_text(data = merged[side == "Bid"& depth==1 & type=="Outright"], aes(x = px, y = cum_Q, label = cum_Q, fontface = "bold"), 
                hjust=1, position = position_fill(vjust = 0))+
      geom_text(data = merged[side == "Bid"& depth!=1 & type=="Outright"], aes(x = px, y = cum_Q, label = cum_Q), 
                hjust=1, position = position_fill(vjust = 0))+
      geom_hline(yintercept = 0, linetype = 1, linewidth=0.5, color = "blue")+
      scale_fill_gradientn(
        name = "#Ord: Bid",
        colors = c("red1", "yellow"),  ###Bid
        limits = c(mbp_order_book[side == "Bid", min(O)], mbp_order_book[side == "Bid", max(O)]),
        guide = guide_legend(order = 3, reverse = TRUE)
      )+
      labs(title = mbp_order_book[, Time], ## bbo info
           x = "Price", 
           y = "Quantity") +
      coord_flip()+
      theme_minimal()
  }
  
 
  
  
  
  if(isTRUE(animation)){
    
 
    
    anim_book_plot <- function(data,...){
      
      book_plot <- book_plot+
        labs(title = paste0(data[,unique(Code)],"  ", 'Time: {frame_time}'), ## bbo info
             x = "Price", 
             y = "Quantity") +
        transition_time(Time) +
        view_follow()+
        ease_aes('linear')
      
      book_plot <- animate(book_plot, nframes=data[, length(unique(Time))], fps=anim_fps, height=800, width = 1200)
      return(book_plot)
    }
    
    if(is.null(implied_book)){
      
      book_plot <- anim_book_plot(mbp_order_book)
      
    }else{
      
      book_plot <- anim_book_plot(merged)
    }
    
    
    
  }
  return(book_plot)
  
 }
 
#  test <- book_bar(outright_book, implied_book, 5, animation = TRUE )
# anim_save("C:/Users/ruchuan2/Box/cme.mdp/book_bar_conso2.gif", test, res = 200) 
