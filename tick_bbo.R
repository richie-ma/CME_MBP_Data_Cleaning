#########################################################

# Tick and bbo plot
## This figures shows the trade price and corresponding BBO
## within a certain period of time, for example, 25ms.
## The data input is tick-level.

#data visualization dependents: 
# ggplot2
# gganimate
# data.table
# stringr
# highfrequency
# ggnewscale


tick_bbo <- function(mbp_order_book, trade_summary, trading.tz="America/Chicago", 
                     start_timestamp, end_timestamp, animation=FALSE){
  
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

  if(class(trade_summary$Time)[1]=="character"){

    trade_summary[, Time:= paste0(substr(Time, 1, 4), "-", substr(Time, 5, 6), "-",
                                   substr(Time, 7, 8),
                                   " ", substr(Time, 9, 10),
                                   ":", substr(Time, 11, 12), ":", substr(Time, 13, 14),
                                   ".", substr(Time, 15, 23))] ## dafult as.posixct time format

    trade_summary <- trade_summary[, `:=`(Time=as.POSIXct(Time, tz="GMT", "%Y-%m-%d %H:%M:%OS"))]

  }
  
  
  ## change timezone to UTC
  start_timestamp <- as.POSIXct(start_timestamp, trading.tz) 
  end_timestamp <- as.POSIXct(end_timestamp, trading.tz) 
  attr(start_timestamp, "tzone") <- "GMT"
  attr(end_timestamp, "tzone") <- "GMT"
  
  trade_summary <- trade_summary[Time %between% c(start_timestamp, end_timestamp)][, .(Time, Code, trd_price=PX, trd_size=Size, trd_agg=agg)]
  mbp_order_book <- mbp_order_book[Time %between% c(start_timestamp, end_timestamp)][, .(Time, Code, Bid_PX_1, Ask_PX_1, Bid_Qty_1, Ask_Qty_1)]
  attr(mbp_order_book$Time, "tzone") <- trading.tz
  attr(trade_summary$Time, "tzone") <- trading.tz
  ## merge the two datasets with full join
  
  plot_data <- merge(trade_summary, mbp_order_book, all=TRUE)
  plot_data[, c("trd_price_fill")] <- nafill(plot_data[, .(trd_price)], "locf")
  
  
  attr(plot_data$Time, "tzone") <- trading.tz
  #plot_data[, c("trd_price", "trd_size", "trd_agg")] <- nafill(plot_data[, .(trd_price, trd_size, trd_agg)], "locf")
  

 
  tickbbo <- ggplot()+
    geom_line(data = plot_data, aes(x = Time, y = Bid_PX_1, color='Bid_PX_1'), alpha=1, linewidth=1) +
    geom_line(data = plot_data, aes(x = Time, y = Ask_PX_1, color='Ask_PX_1'), alpha=1, linewidth=1) +
    geom_step(data = plot_data, aes(x = Time, y = trd_price_fill, color='trd_price_fill'), alpha=1, linewidth=1) +
    scale_color_manual(name = NULL, values = c('Ask_PX_1'="green",'Bid_PX_1'= "red",'trd_price_fill'="blue"),
                       labels = c("Best Bid", "Best Offer", "Last Trade"))+
    geom_point(data = plot_data[trd_agg==1], aes(x = Time, y = trd_price, size = trd_size), 
               shape = 17,color='springgreen4', fill='springgreen4', alpha=1) +
    scale_size_continuous(name="Buy Trade Size")+
    
    new_scale("size")+
    
    geom_point(data = plot_data[trd_agg==2], aes(x = Time, y = trd_price, size = trd_size),
               shape = 25, color='firebrick', fill='firebrick', alpha=1) +
    scale_size_continuous(name="Sell Trade Size")+
    
    new_scale("size")+
    geom_point(data = plot_data[trd_agg==0], aes(x = Time, y = trd_price, size = trd_size),
               shape = 1, color='orange', fill='orange', alpha=1, show.legend = F) +
    geom_ribbon(data = plot_data, aes(x = Time, ymin = Bid_PX_1, ymax = Ask_PX_1), fill='gray', alpha=0.25) +
    
    scale_x_datetime(labels = scales::date_format("%H:%M:%S", tz=trading.tz))+
    
    #mbp_order_book2[.N, Bid_Qty_1], " x ", mbp_order_book2[.N, Ask_Qty_1]), ## bbo info
    labs(title = plot_data[, unique(Code)],
         x = "Time", 
         y = "Price") +
    theme_minimal()
    
    
  
  # plot_data[, Time2:=1:.N]
  # 
  # tickbbo <- ggplot()+
  #   geom_line(data = plot_data, aes(x = Time2, y = Bid_PX_1, color='Bid_PX_1'), alpha=1, linewidth=1) +
  #   geom_line(data = plot_data, aes(x = Time2, y = Ask_PX_1, color='Ask_PX_1'), alpha=1, linewidth=1) +
  #   geom_step(data = plot_data, aes(x = Time2, y = trd_price_fill, color='trd_price_fill'), alpha=1, linewidth=1) +
  #   scale_color_manual(name = NULL, values = c('Ask_PX_1'="green",'Bid_PX_1'= "red",'trd_price_fill'="blue"),
  #                      labels = c("Best Bid", "Best Offer", "Last Trade"))+
  #   geom_point(data = plot_data[trd_agg==1], aes(x = Time2, y = trd_price, size = trd_size), 
  #              shape = 17,color='springgreen4', fill='springgreen4', alpha=1) +
  #   scale_size_continuous(name="Buy Trade Size")+
  #   
  #   new_scale("size")+
  #   
  #   geom_point(data = plot_data[trd_agg==2], aes(x = Time2, y = trd_price, size = trd_size),
  #              shape = 25, color='firebrick', fill='firebrick', alpha=1) +
  #   scale_size_continuous(name="Sell Trade Size")+
  #   
  #   new_scale("size")+
  #   geom_point(data = plot_data[trd_agg==0], aes(x = Time2, y = trd_price, size = trd_size),
  #              shape = 1, color='orange', fill='orange', alpha=1, show.legend = F) +
  #   geom_ribbon(data = plot_data, aes(x = Time2, ymin = Bid_PX_1, ymax = Ask_PX_1), fill='gray', alpha=0.25) +
  #   
  #   scale_x_continuous( breaks = plot_data[seq(1, .N, 5000),Time2], labels = plot_data[seq(1, .N, 5000),substr(Time, 12, 19)])+
  # 
  #   #mbp_order_book2[.N, Bid_Qty_1], " x ", mbp_order_book2[.N, Ask_Qty_1]), ## bbo info
  #   labs(title = plot_data[, unique(Code)],
  #        x = "Price", 
  #        y = "Time") +
  #   theme_minimal()+
  #   view_step_manual(
  #     pause_length = 0,
  #     step_length = 100,
  #     xmin = plot_data[1:(.N-100), Time2],
  #     xmax = plot_data[101:.N, Time2],
  #     ymin = plot_data[, min(Bid_PX_1, na.rm = T)],
  #     ymax = plot_data[, max(Ask_PX_1, na.rm = T)],
  #     wrap = FALSE,
  #     ease = 'linear')
  #  ### It shouldn't be view_zoom_manual, especially when you have a pretty long time series.
  #  
  # 
  # tickbbo <- animate(tickbbo, fps=10, height=400, width = 1000)
  
  figure <- plot_group(plot_data)
      
  
  return(figure)
  
}


