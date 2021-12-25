# ref 
# - [Introduction to Candlesticks](https://stockcharts.com/school/doku.php?id=chart_school:chart_analysis:introduction_to_candlesticks)
# - [ggplot-for-ohlc-chart-using-bars-not-candles](https://stackoverflow.com/questions/28201587/ggplot-for-ohlc-chart-using-bars-not-candles)

# color_up, color_down
# - stockcharts: '#000000', '#CF002F'
# - A share: 'red', 'green'
# - ggplot2: '#18C0C4', '#F6736D'
# rgb()

# candlstick
pp_candle = function(
    dt, from=NULL, to=Sys.Date(), 
    color_up = '#F6736D', color_down = '#18C0C4', title = title, 
    rm_weekend = TRUE, rm_xaxis = FALSE, yaxis_log = FALSE, x = 'open|high|low|close', 
    linear_trend = NULL, multi_series = list(nrow=NULL,ncol=NULL), 
    addti = list(sma = list(n=50), mm = list(n=25)),
    x_scale = 0.6, subtitle_str = NULL, ...
) {
    price_str = close_prev = change = change_pct = high = low = updn_1day = updn_2day = symbol = V1 = NULL
    
    # copy dt
    dt = copy(dt)
    
    # number symbols/ nrow==1 & ncol==1
    num_syb = dt[, length(unique(symbol))]
    multi_series_all1 = list(...)[['multi_series_all1']]
    
    # x and updn
    dt[, updn_1day := ifelse(close > open, 'hollow', paste('filled', updn_2day, sep = '_'))]
    
    # plot
    ## change dt date range to from/to
    dat = dt[date>=from & date <= to]
    if (!multi_series_all1) {
        p = ggplot(dat, aes(color = updn_2day)) +
            geom_segment(aes(x = x, y = low, xend = x, yend = high)) + 
            geom_rect(aes(xmin = x - x_scale / 2, xmax = x + x_scale / 2, ymin = open, ymax = close, fill = updn_1day)) + 
            scale_fill_manual(values = c('hollow' = 'white', 'filled_up' = color_up, 'filled_down' = color_down)) +
            scale_color_manual(values = c('up' = color_up, 'down' = color_down)) + 
            geom_text(x = dat[1, x], y = Inf, aes(label = date), data = dat[,.SD[.N], by=symbol], hjust = 0, vjust = 1, color = 'black', na.rm = TRUE, alpha = 0.6, size = rel(3)) +
            geom_text(x = dat[.N, x], y = Inf, aes(label = price_str), data=subtitle_str, hjust = 1, vjust = 1, color = 'black', parse = TRUE, na.rm = TRUE, alpha = 0.6, size = rel(3)) +
            # annotate('text', label='@http://shichen.name/pedar/', x=dat[, x[.N %/% 2], by=symbol][,V1[1]], y=-Inf, vjust = -1, color = 'gray', alpha = 0.2) + 
            # geom_text(aes(label='@http://shichen.name/pedar', x=dat[, x[.N], by=symbol][,V1[1]], y=Inf), vjust = -0.5, hjust = 1, color = '#F0F0F0') + 
            coord_cartesian(clip = 'off') + 
            guides(fill = FALSE, color = FALSE) + 
            labs(x=NULL, y=NULL) + theme_bw()
        
        # facet 
        if (num_syb>1) {
            p = p + 
                facet_wrap(~ title1 + title2, nrow = multi_series$nrow, ncol = multi_series$ncol, scales = multi_series$scales) + 
                theme(strip.background = element_blank(),
                      strip.text = element_text(hjust = 0, margin = margin(b=0)))
        }
        
        ###### overlay techinal indicators ######
        if (!is.null(addti) & length(addti)>0) {
            p = do.call(pp_add_ti_overlay, args = list(p = p, dt = dt[date>=from & date <= to], from = from, to = to, rm_weekend = rm_weekend, addti = addti))
        }
        
        # add linear trend line
        if (!is.null(linear_trend)) 
            p = do.call(pp_add_linear_trend, args = list(p=p, dt = dat, rm_weekend=rm_weekend, linear_trend=linear_trend))
    }
    
    # remove margin
    p = p + theme(plot.margin = unit(rep(0, 4), 'cm'))
    # set xaxis
    p = pp_set_xaxis(p, dat, rm_weekend = rm_weekend, rm_xaxis = rm_xaxis)
    # set yaxis
    p = pp_set_yaxis(p, yaxis_log = yaxis_log)
    # set title 
    p = pp_set_title(p, dat, title = title)
    
    ###### oscillator techinal indicators ######
    if (!is.null(addti) & length(addti)>0 & num_syb==1) {
        p = do.call(pp_add_ti_oscillator, args = list(p = p, dt = dt[date>=from & date <= to], from = from, to = to, rm_weekend = rm_weekend, addti = addti))
    } 
    
    return(p)
}

pp_candlei = function(
    dt, from=NULL, to=Sys.Date(), 
    color_up = '#F6736D', color_down = '#18C0C4', title = title, 
    rm_weekend = FALSE, rm_xaxis = FALSE, yaxis_log = FALSE, x = 'close|value', 
    linear_trend = NULL, multi_series = list(nrow=NULL,ncol=NULL), 
    addti = list(sma = list(n=50), mm = list(n=25)), subtitle_str = NULL, ...
) {
    close_prev=prev_x=symbol=title1=title2=updn_2day=NULL
    
    # copy dt
    dt = copy(dt)
    
    # number symbols/ nrow==1 & ncol==1
    num_syb = dt[, length(unique(symbol))]
    multi_series_all1 = list(...)[['multi_series_all1']]
    
    # x and updn
    dt[, prev_x := shift(x, 1, type='lag'), by = symbol
    ][updn_2day=='down', `:=`(x1 = prev_x, x2 = x, y1 = close_prev, y2 = close)]
    
    # cutom colors
    i <- list(line = list(color = color_up))
    d <- list(line = list(color = color_down))
    
    # plot
    if (multi_series_all1) {
        
    } else {
        oneplot_candlei = function(dt, from, to, addti, linear_trend, yaxis_log, rm_weekend, color_up, color_down) {
            p = dt[date>=from & date <= to] %>% 
                plot_ly(x = ~ x) %>% 
                add_trace(
                    open = ~ open, close = ~ close,
                    high = ~ high, low = ~ low,
                    increasing = i, decreasing = d, 
                    name = ~ title1, 
                    text = ~ sprintf('change: %s%s\n%s', round(change_pct,2), '%', x),
                    showlegend = FALSE, 
                    type = "candlestick"
                ) %>% 
                layout( 
                    margin = list( pad = 0, b = 0, l = 10, r = 10 ), 
                    xaxis = list(
                        rangeslider = list(visible = F)
                    ),
                    legend = list(x = 0.02, y = 0.98)
                    # showlegend = FALSE
                )
            
            ###### overlay techinal indicators ######
            if (!is.null(addti) & length(addti)>0) {
                p = do.call(pp_add_ti_overlay, args = list(p = p, dt = dt[date>=from & date <= to], from = from, to = to, rm_weekend = rm_weekend, addti = addti, interact = TRUE))
            }
            
            # add linear trend line
            if (!is.null(linear_trend)) 
                p = do.call(pp_add_linear_trend, args = list(p=p, dt = dt, rm_weekend = rm_weekend, linear_trend = linear_trend, interact = TRUE, yaxis_log = yaxis_log))
            
            return(p)
        }
        
        
        # facet 
        if (num_syb>1) {
            p = multiplot(oneplot_candlei, dt, from, to, addti, linear_trend, yaxis_log, rm_weekend, rm_xaxis, color_up, color_down, title, multi_series)
        } else {
            p = oneplot_candlei(dt, from, to, addti, linear_trend, yaxis_log, rm_weekend, color_up, color_down)
        }   
    }
    
    
    ###### oscillator techinal indicators ######
    if (!is.null(addti) & length(addti)>0 & num_syb==1) {
        p = do.call(pp_add_ti_oscillator, args = list(p = p, dt = dt[date>=from & date <= to], from = from, to = to, rm_weekend = rm_weekend, addti = addti, interact = TRUE))
    }
    
    # set title 
    p = pp_set_title(p, dt[date>=from & date <= to], title = title, interact = TRUE)
    # set yaxis
    p = pp_set_yaxis(p, yaxis_log = yaxis_log, interact = TRUE)
    # set xaxis
    p = pp_set_xaxis(p, dt[date>=from & date <= to], rm_weekend = rm_weekend, rm_xaxis = rm_xaxis, interact = TRUE)
    
    return(p)
}

# bar
pp_bar = function(
    dt, from=NULL, to=Sys.Date(), 
    color_up = '#F6736D', color_down = '#18C0C4', title = title, 
    rm_weekend = TRUE, rm_xaxis = FALSE, yaxis_log = FALSE, x = 'open|high|low|close', 
    linear_trend = NULL, multi_series = list(nrow=NULL,ncol=NULL), 
    addti = list(sma = list(n=50), mm = list(n=25)),
    x_scale = 0.6, subtitle_str = NULL, ...
) {
    price_str = close_prev = change = change_pct = high = low = updn_2day = symbol = V1 = NULL
    
    # copy dt
    dt = copy(dt)
    
    # number symbols/ nrow==1 & ncol==1
    num_syb = dt[, length(unique(symbol))]
    multi_series_all1 = list(...)[['multi_series_all1']]
    
    # # add prev close, change and change_pct
    # if (!('close_prev' %in% names(dt))) dt = dt[, close_prev := shift(close, 1, type = 'lag')]
    # if (!('change'     %in% names(dt))) dt = dt[, change := close - close_prev]
    # if (!('change_pct' %in% names(dt))) dt = dt[, change_pct := change/close*100]
    
    # plot
    ## change dt date range to from/to
    dat = dt[date>=from & date <= to]
    
    if (!multi_series_all1) {
        p = ggplot(dat, aes(color = updn_2day)) + 
            geom_linerange(aes(x = x, ymin = low, ymax = high)) +
            geom_segment(aes(x = x, y = open, xend = x - x_scale / 2, yend = open)) + 
            geom_segment(aes(x = x, y = close, xend = x + x_scale / 2, yend = close)) + 
            scale_color_manual(values = c('up' = color_up, 'down' = color_down)) +
            geom_text(x = dat[1, x], y = Inf, aes(label = date), data = dat[,.SD[.N], by=symbol], hjust = 0, vjust = 1, color = 'black', na.rm = TRUE, alpha = 0.6, size = rel(3)) +
            geom_text(x = dat[.N, x], y = Inf, aes(label = price_str), data=subtitle_str, hjust = 1, vjust = 1, color = 'black', parse = TRUE, na.rm = TRUE, alpha = 0.6, size = rel(3)) +
            # annotate('text', label='@http://shichen.name/pedar/', x=dat[, x[.N %/% 2], by=symbol][,V1[1]], y=-Inf, vjust = -1, color = 'gray', alpha = 0.2) + 
            # geom_text(aes(label='@http://shichen.name/pedar', x=dat[, x[.N], by=symbol][,V1[1]], y=Inf), vjust = -0.5, hjust = 1, color = '#F0F0F0') + 
            coord_cartesian(clip = 'off') + 
            guides(color = FALSE) + 
            labs(x=NULL, y=NULL) + theme_bw()
        
        # facet 
        if (num_syb>1) {
            p = p + 
                facet_wrap(~ title1 + title2, nrow = multi_series$nrow, ncol = multi_series$ncol, scales = multi_series$scales) + 
                theme(strip.background = element_blank(),
                      strip.text = element_text(hjust = 0, margin = margin(b=0)))
        }
        
        ##### overlay techinal indicators #####
        if (!is.null(addti) & length(addti)>0) {
            p = do.call(pp_add_ti_overlay, args = list(p = p, dt = dt[date>=from & date <= to], from = from, to = to, rm_weekend = rm_weekend, addti = addti))
        }
        
        # add linear trend line
        if (!is.null(linear_trend)) 
            p = do.call(pp_add_linear_trend, args = list(p=p, dt = dat, rm_weekend = rm_weekend, linear_trend=linear_trend))
    }
    
    # remove margin
    p = p + theme(plot.margin = unit(rep(0, 4), 'cm'))
    # set xaxis
    p = pp_set_xaxis(p, dat, rm_weekend = rm_weekend, rm_xaxis = rm_xaxis)
    # set yaxis
    p = pp_set_yaxis(p, yaxis_log = yaxis_log)
    # set title 
    p = pp_set_title(p, dat, title = title)
    
    ###### oscillator techinal indicators ######
    if (!is.null(addti) & length(addti)>0 & num_syb==1) {
        p = do.call(pp_add_ti_oscillator, args = list(p = p, dt = dt[date>=from & date <= to], from = from, to = to, rm_weekend = rm_weekend, addti = addti))
    }
    
    return(p)
}

pp_bari = function(
    dt, from=NULL, to=Sys.Date(), 
    color_up = '#F6736D', color_down = '#18C0C4', title = title, 
    rm_weekend = FALSE, rm_xaxis = FALSE, yaxis_log = FALSE, x = 'close|value', 
    linear_trend = NULL, multi_series = list(nrow=NULL,ncol=NULL), 
    addti = list(sma = list(n=50), mm = list(n=25)), subtitle_str = NULL, ...
) {
    close_prev=prev_x=symbol=title1=title2=updn_2day=NULL
    # copy dt
    dt = copy(dt)
    
    # number symbols/ nrow==1 & ncol==1
    num_syb = dt[, length(unique(symbol))]
    multi_series_all1 = list(...)[['multi_series_all1']]
    
    # x and updn
    dt[, prev_x := shift(x, 1, type='lag'), by = symbol
    ][updn_2day=='down', `:=`(x1 = prev_x, x2 = x, y1 = close_prev, y2 = close)]
    
    # cutom colors
    i <- list(line = list(color = color_up))
    d <- list(line = list(color = color_down))
    
    # plot
    if (multi_series_all1) {
        
    } else {
        oneplot_bari = function(dt, from, to, addti, linear_trend, yaxis_log, rm_weekend, color_up, color_down) {
            p = dt[date>=from & date <= to] %>% 
                plot_ly(x = ~ x) %>% 
                add_trace(
                    open = ~ open, close = ~ close,
                    high = ~ high, low = ~ low,
                    increasing = i, decreasing = d, 
                    name = ~ title1, 
                    text = ~ sprintf('change: %s%s\n%s', round(change_pct,2), '%', x),
                    showlegend = FALSE, 
                    type = "ohlc"
                ) %>% 
                layout(
                    margin = list( pad = 0, b = 0, l = 10, r = 10 ), 
                    xaxis = list(
                        rangeslider = list(visible = F)
                    ),
                    legend = list(x = 0.02, y = 0.98)
                )
            
            ###### overlay techinal indicators ######
            if (!is.null(addti) & length(addti)>0) {
                p = do.call(pp_add_ti_overlay, args = list(p = p, dt = dt[date>=from & date <= to], from = from, to = to, rm_weekend = rm_weekend, addti = addti, interact = TRUE))
            }
            
            # add linear trend line
            if (!is.null(linear_trend)) 
                p = do.call(pp_add_linear_trend, args = list(p=p, dt = dt, rm_weekend = rm_weekend, linear_trend = linear_trend, interact = TRUE, yaxis_log = yaxis_log))
            
            return(p)
        }
        
        # facet 
        if (num_syb>1) {
            p = multiplot(oneplot_bari, dt, from, to, addti, linear_trend, yaxis_log, rm_weekend, rm_xaxis, color_up, color_down, title, multi_series)
        } else {
            p = oneplot_bari(dt, from, to, addti, linear_trend, yaxis_log, rm_weekend, color_up, color_down)
        }   
    }
    
    
    ###### oscillator techinal indicators ######
    if (!is.null(addti) & length(addti)>0 & num_syb==1) {
        p = do.call(pp_add_ti_oscillator, args = list(p = p, dt = dt[date>=from & date <= to], from = from, to = to, rm_weekend = rm_weekend, addti = addti, interact = TRUE))
    }
    
    # set title 
    p = pp_set_title(p, dt[date>=from & date <= to], title = title, interact = TRUE)
    # set yaxis
    p = pp_set_yaxis(p, yaxis_log = yaxis_log, interact = TRUE)
    # set xaxis
    p = pp_set_xaxis(p, dt[date>=from & date <= to], rm_weekend = rm_weekend, rm_xaxis = rm_xaxis, interact = TRUE)
    
    return(p)
}

# line
pp_line = function(
    dt, from=NULL, to=Sys.Date(), 
    color_up = '#F6736D', color_down = '#18C0C4', title = title, 
    rm_weekend = FALSE, rm_xaxis = FALSE, yaxis_log = FALSE, x = 'close|value', 
    linear_trend = NULL, multi_series = list(nrow=NULL,ncol=NULL), 
    addti = list(sma = list(n=50), mm = list(n=25)), subtitle_str = NULL, ...
) {
    price_str11 = price_str = close_prev = symbol = change = change_pct = high = low = prev_x = updn_2day = x1 = y1 = x2 = y2 = V1 = NULL
    
    # copy dt
    dt = copy(dt)
    
    # number symbols/ nrow==1 & ncol==1
    num_syb = dt[, length(unique(symbol))]
    multi_series_all1 = list(...)[['multi_series_all1']]
    
    # x and updn
    dt[, prev_x := shift(x, 1, type='lag'), by = symbol
       ][updn_2day=='down', `:=`(x1 = prev_x, x2 = x, y1 = close_prev, y2 = close)]

    
    # plot
    ## change dt date range to from/to
    dat = dt[date>=from & date <= to]
    
    if (multi_series_all1) {
        p = ggplot(data = dat) + 
            geom_line(aes(x = x, y = close, color = symbol)) + 
            scale_color_discrete(labels = subtitle_str[, price_str11]) + 
            geom_rug(aes(x=x, y=close), data = subtitle_str, sides='r', color='gray', alpha = 0.8) + 
            # annotate('text', label='@http://shichen.name/pedar/', x=dat[, x[.N %/% 2], by=symbol][,V1[1]], y=-Inf, vjust = -1, color = 'gray', alpha = 0.2) + 
            # geom_text(aes(label='@http://shichen.name/pedar', x=dat[, x[.N], by=symbol][,V1[1]], y=Inf), vjust = -0.5, hjust = 1, color = '#F0F0F0') + 
            coord_cartesian(clip = 'off') + 
            labs(x=NULL, y=NULL, color=NULL) + theme_bw() +
            theme(
                legend.position = c(0,1), 
                legend.justification=c(0,1),
                legend.background = element_blank(), 
                legend.key = element_blank()
            )
    } else {
        p = ggplot(dat) + 
            geom_line(aes(x = x, y = close), color = color_up) +
            geom_segment(aes(x = x1, y = y1, xend = x2, yend = y2), color = color_down, na.rm = TRUE) +
            geom_text(x = dat[1, x], y = Inf, aes(label = date), data = dat[,.SD[.N], by=symbol], hjust = 0, vjust = 1, color = 'black', na.rm = TRUE, alpha = 0.6, size = rel(3)) +
            geom_text(x = dat[.N, x], y = Inf, aes(label = price_str), data=subtitle_str, hjust = 1, vjust = 1, color = 'black', parse = TRUE, na.rm = TRUE, alpha = 0.6, size = rel(3)) +
            geom_rug(aes(x=x, y=close), data = subtitle_str, sides='r', color='gray', alpha = 0.8) + 
            # annotate('text', label='@http://shichen.name/pedar/', x=dat[, x[.N %/% 2], by=symbol][,V1[1]], y=-Inf, vjust = -1, color = 'gray', alpha = 0.2) + 
            # geom_text(aes(label='@http://shichen.name/pedar', x=dat[, x[.N], by=symbol][,V1[1]], y=Inf), vjust = -0.5, hjust = 1, color = '#F0F0F0') + 
            coord_cartesian(clip = 'off') + 
            labs(x=NULL, y=NULL) + theme_bw()
        
        # facet 
        if (num_syb>1) {
            p = p + 
                facet_wrap(~ title1 + title2, nrow = multi_series$nrow, ncol = multi_series$ncol, scales = multi_series$scales) + 
                theme(strip.background = element_blank(),
                      strip.text = element_text(hjust = 0, margin = margin(b=0)))
        }
        
        ###### overlay techinal indicators ######
        if (!is.null(addti) & length(addti)>0) {
            p = do.call(pp_add_ti_overlay, args = list(p = p, dt = dt[date>=from & date <= to], from = from, to = to, rm_weekend = rm_weekend, addti = addti))
        }
        # add linear trend line
        if (!is.null(linear_trend)) 
            p = do.call(pp_add_linear_trend, args = list(p=p, dt = dat, rm_weekend = rm_weekend, linear_trend = linear_trend))
    }
    
    # remove margin
    p = p + theme(plot.margin = unit(rep(0, 4), 'cm'))
    # set xaxis
    p = pp_set_xaxis(p, dat, rm_weekend = rm_weekend, rm_xaxis = rm_xaxis)
    # set yaxis
    p = pp_set_yaxis(p, yaxis_log = yaxis_log)
    # set title 
    p = pp_set_title(p, dat, title = title)
    
    ###### oscillator techinal indicators ######
    if (!is.null(addti) & length(addti)>0 & num_syb==1) {
        p = do.call(pp_add_ti_oscillator, args = list(p = p, dt = dt[date>=from & date <= to], from = from, to = to, rm_weekend = rm_weekend, addti = addti))
    }
    
    return(p)
}

pp_linei = function(
    dt, from=NULL, to=Sys.Date(), 
    color_up = '#F6736D', color_down = '#18C0C4', title = title, 
    rm_weekend = FALSE, rm_xaxis = FALSE, yaxis_log = FALSE, x = 'close|value', 
    linear_trend = NULL, multi_series = list(nrow=NULL,ncol=NULL), 
    addti = list(sma = list(n=50), mm = list(n=25)), subtitle_str = NULL, ...
) {
    close_prev=prev_x=symbol=title1=title2=updn_2day=NULL
    # copy dt
    dt = copy(dt)
    
    # number symbols/ nrow==1 & ncol==1
    num_syb = dt[, length(unique(symbol))]
    multi_series_all1 = list(...)[['multi_series_all1']]
    
    # x and updn
    dt[, prev_x := shift(x, 1, type='lag'), by = symbol
    ][updn_2day=='down', `:=`(x1 = prev_x, x2 = x, y1 = close_prev, y2 = close)]
    
    
    # plot
    if (multi_series_all1) {
        p = dt[date>=from & date <= to] %>% 
            plot_ly(x = ~ x) %>% 
            add_lines(
                y = ~ close, 
                name = ~ title1, 
                color = ~ title1, 
                hoverinfo = 'text+name', 
                text = ~ sprintf('%s\n%s (%s%s)', date, round(close,2), round(change_pct,2), '%')
            ) %>% 
            layout(  
                margin = list( pad = 0, b = 0, l = 10, r = 10 ), 
                legend = list(x = 0.02, y = 0.98)
            )
    } else {
        oneplot_linei = function(dt, from, to, addti, linear_trend, yaxis_log, rm_weekend, color_up, color_down, ...) {
            p = dt[date>=from & date <= to] %>% 
                plot_ly(x = ~ x) %>% 
                add_lines(
                    y = ~ close, 
                    name = ~ title1, 
                    line = list(color = color_up), 
                    showlegend = FALSE, 
                    hoverinfo = 'text+name', 
                    text = ~ sprintf('%s\n%s (%s%s)', date, round(close,2), round(change_pct,2), '%')
                ) %>% 
                add_segments(
                    x = ~ x1, 
                    xend = ~ x2, 
                    y = ~ y1, 
                    yend = ~ y2, 
                    name = ~ title1, 
                    line = list(color = color_down), 
                    showlegend = FALSE, 
                    hoverinfo = 'none' 
                ) %>%
                layout( 
                    margin = list( pad = 0, b = 0, l = 10, r = 10 ), 
                    legend = list(x = 0.02, y = 0.98)
                )
            
            ###### overlay techinal indicators ######
            if (!is.null(addti) & length(addti)>0) {
                p = do.call(pp_add_ti_overlay, args = list(p = p, dt = dt[date>=from & date <= to], from = from, to = to, rm_weekend = rm_weekend, addti = addti, interact = TRUE))
            }
            
            # add linear trend line
            if (!is.null(linear_trend)) 
                p = do.call(pp_add_linear_trend, args = list(p=p, dt = dt, rm_weekend = rm_weekend, linear_trend = linear_trend, interact = TRUE, yaxis_log = yaxis_log))
            
            return(p)
        }
        
        # facet 
        if (num_syb>1) {
            p = multiplot(oneplot_linei, dt, from, to, addti, linear_trend, yaxis_log, rm_weekend, rm_xaxis, color_up, color_down, title, multi_series)
        } else {
            p = oneplot_linei(dt, from, to, addti, linear_trend, yaxis_log, rm_weekend, color_up, color_down)
        }
    }
    
    
    ###### oscillator techinal indicators ######
    if (!is.null(addti) & length(addti)>0 & num_syb==1) {
        p = do.call(pp_add_ti_oscillator, args = list(p = p, dt = dt[date>=from & date <= to], from = from, to = to, rm_weekend = rm_weekend, addti = addti, interact = TRUE))
    }
    
    # set title 
    p = pp_set_title(p, dt[date>=from & date <= to], title = title, interact = TRUE)
    # set yaxis
    p = pp_set_yaxis(p, yaxis_log = yaxis_log, interact = TRUE)
    # set xaxis
    p = pp_set_xaxis(p, dt[date>=from & date <= to], rm_weekend = rm_weekend, rm_xaxis = rm_xaxis, interact = TRUE)
    
    return(p)
}

# step
pp_step = function(
    dt, from=NULL, to=Sys.Date(), 
    color_up = '#F6736D', color_down = '#18C0C4', title = title, 
    rm_weekend = FALSE, rm_xaxis = FALSE, yaxis_log = FALSE, x = 'close|value', 
    linear_trend = NULL, multi_series = list(nrow=NULL,ncol=NULL), 
    addti = list(sma = list(n=50), mm = list(n=25)), subtitle_str = NULL, ...
) {
    price_str11 = price_str = close_prev = symbol = change = change_pct = high = low = prev_x = updn_2day = x1 = y1 = x2 = y2 = V1 = NULL
    
    # copy dt
    dt = copy(dt)
    
    # number symbols/ nrow==1 & ncol==1
    num_syb = dt[, length(unique(symbol))]
    multi_series_all1 = list(...)[['multi_series_all1']]
    
    # x and updn
    dt[, prev_x := shift(x, 1, type='lag'), by = symbol
       ][updn_2day=='down', `:=`(x1 = prev_x, x2 = x, y1 = close_prev, y2 = close)]
    
    
    # last row
    dt_N = dt[,.SD[.N], by=symbol]
    # add new row
    dtN_update = copy(dt_N)[, `:=`(
        date = Sys.Date(), rowid = rowid+1, prev_x = x
    )][, x := date][rm_weekend, x := rowid]
    dt = rbindlist(list(dt, dtN_update), fill = TRUE)
    setkeyv(dt, c('symbol', 'date'))
    
    # plot
    ## change dt date range to from/to
    dat = dt[date>=from & date <= to]
    
    if (multi_series_all1) {
        p = ggplot(data = dat) + 
            geom_step(aes(x = x, y = close, color = symbol)) + 
            scale_color_discrete(labels = subtitle_str[, price_str11]) + 
            geom_rug(aes(x=x, y=close), data = subtitle_str, sides='r', color='gray', alpha = 0.8) + 
            # annotate('text', label='@http://shichen.name/pedar/', x=dat[, x[.N %/% 2], by=symbol][,V1[1]], y=-Inf, vjust = -1, color = 'gray', alpha = 0.2) + 
            # geom_text(aes(label='@http://shichen.name/pedar', x=dat[, x[.N], by=symbol][,V1[1]], y=Inf), vjust = -0.5, hjust = 1, color = '#F0F0F0') + 
            coord_cartesian(clip = 'off') + 
            labs(x=NULL, y=NULL, color=NULL) + theme_bw() +
            theme(
                legend.position = c(0,1), 
                legend.justification=c(0,1),
                legend.background = element_blank(), 
                legend.key = element_blank()
            )
    } else {
        p = ggplot(dat) + 
            geom_step(aes(x = x, y = close), color = color_up) +
            geom_segment(aes(x = x1, y = y1, xend = x2, yend = y1), color = color_down, na.rm = TRUE) +
            geom_segment(aes(x = x2, y = y1, xend = x2, yend = y2), color = color_down, na.rm = TRUE) +
            geom_text(x = dat[1, x], y = Inf, aes(label = date), data=dt_N, hjust = 0, vjust = 1, color = 'black', na.rm = TRUE, alpha = 0.6, size = rel(3)) +
            geom_text(x = dat[.N, x], y = Inf, aes(label = price_str), data=subtitle_str, hjust = 1, vjust = 1, color = 'black', parse = TRUE, na.rm = TRUE, alpha = 0.6, size = rel(3)) +
            geom_rug(aes(x=x, y=close), data = subtitle_str, sides='r', color='gray', alpha = 0.8) + 
            # annotate('text', label='@http://shichen.name/pedar/', x=dat[, x[.N %/% 2], by=symbol][,V1[1]], y=-Inf, vjust = -1, color = 'gray', alpha = 0.2) + 
            # geom_text(aes(label='@http://shichen.name/pedar', x=dat[, x[.N], by=symbol][,V1[1]], y=Inf), vjust = -0.5, hjust = 1, color = '#F0F0F0') + 
            coord_cartesian(clip = 'off') + 
            labs(x=NULL, y=NULL) + theme_bw()
        
        # facet 
        if (num_syb>1) {
            p = p + 
                facet_wrap(~ title1 + title2, nrow = multi_series$nrow, ncol = multi_series$ncol, scales = multi_series$scales) + 
                theme(strip.background = element_blank(),
                      strip.text = element_text(hjust = 0, margin = margin(b=0)))
        }
        
        ###### overlay techinal indicators ######
        if (!is.null(addti) & length(addti)>0) {
            p = do.call(pp_add_ti_overlay, args = list(p = p, dt = dt[date>=from & date <= to], from = from, to = to, rm_weekend = rm_weekend, addti = addti))
        }
        # add linear trend line
        if (!is.null(linear_trend)) 
            p = do.call(pp_add_linear_trend, args = list(p=p, dt = dat, rm_weekend = rm_weekend, linear_trend = linear_trend))
    }
    
    # remove margin
    p = p + theme(plot.margin = unit(rep(0, 4), 'cm'))
    # set xaxis
    p = pp_set_xaxis(p, dat, rm_weekend = rm_weekend, rm_xaxis = rm_xaxis)
    # set yaxis
    p = pp_set_yaxis(p, yaxis_log = yaxis_log)
    # set title 
    p = pp_set_title(p, dat, title = title)
    
    ###### oscillator techinal indicators ######
    if (!is.null(addti) & length(addti)>0 & num_syb==1) {
        p = do.call(pp_add_ti_oscillator, args = list(p = p, dt = dt[date>=from & date <= to], from = from, to = to, rm_weekend = rm_weekend, addti = addti))
    } 
    
    return(p)
}

pp_stepi = function(
    dt, from=NULL, to=Sys.Date(), 
    color_up = '#F6736D', color_down = '#18C0C4', title = title, 
    rm_weekend = FALSE, rm_xaxis = FALSE, yaxis_log = FALSE, x = 'close|value', 
    linear_trend = NULL, multi_series = list(nrow=NULL,ncol=NULL), 
    addti = list(sma = list(n=50), mm = list(n=25)), subtitle_str = NULL, ...
) {
    close_prev=prev_x=symbol=title1=title2=updn_2day=NULL
    # copy dt
    dt = copy(dt)
    
    # number symbols/ nrow==1 & ncol==1
    num_syb = dt[, length(unique(symbol))]
    multi_series_all1 = list(...)[['multi_series_all1']]
    
    # x and updn
    dt[, prev_x := shift(x, 1, type='lag'), by = symbol
    ][updn_2day=='down', `:=`(x1 = prev_x, x2 = x, y1 = close_prev, y2 = close)]
    
    
    # plot
    if (multi_series_all1) {
        p = dt[date>=from & date <= to] %>% 
            plot_ly(x = ~ x) %>% 
            add_lines(
                y = ~ close, 
                name = ~ title1, 
                color = ~ title1, 
                line = list(shape = "hv"), 
                hoverinfo = 'text+name', 
                text = ~ sprintf('%s\n%s (%s%s)', date, round(close,2), round(change_pct,2), '%')
            ) %>% 
            layout( 
                margin = list( pad = 0, b = 0, l = 10, r = 10 ), 
                legend = list(x = 0.02, y = 0.98)
            )
    } else {
        oneplot_stepi = function(dt, from, to, addti, linear_trend, yaxis_log, rm_weekend, color_up, color_down) {
            p = dt[date>=from & date <= to] %>% 
                plot_ly() %>% 
                add_lines(
                    x = ~ x, 
                    y = ~ close, 
                    name = ~ title1, 
                    line = list(shape = "hv", color = color_up), 
                    showlegend = FALSE, 
                    hoverinfo = 'text+name', 
                    text = ~ sprintf('%s\n%s (%s%s)', date, round(close,2), round(change_pct,2), '%')
                ) %>% 
                add_segments(
                    x = ~ x1, xend = ~ x2, 
                    y = ~ y1, yend = ~ y2, 
                    name = ~ title1, 
                    line = list(shape = "hv", color = color_down), 
                    showlegend = FALSE, 
                    hoverinfo = 'none'
                ) %>%
                layout( 
                    margin = list( pad = 0, b = 0, l = 10, r = 10 ), 
                    legend = list(x = 0.02, y = 0.98)
                    # showlegend = FALSE
                )
            
            ###### overlay techinal indicators ######
            if (!is.null(addti) & length(addti)>0) {
                p = do.call(pp_add_ti_overlay, args = list(p = p, dt = dt[date>=from & date <= to], from = from, to = to, rm_weekend = rm_weekend, addti = addti, interact = TRUE))
            }
            
            # add linear trend line
            if (!is.null(linear_trend)) 
                p = do.call(pp_add_linear_trend, args = list(p=p, dt = dt, rm_weekend = rm_weekend, linear_trend = linear_trend, interact = TRUE, yaxis_log = yaxis_log))
            
            return(p)
        }
        
        
        # facet 
        if (num_syb>1) {
            p = multiplot(oneplot_stepi, dt, from, to, addti, linear_trend, yaxis_log, rm_weekend, rm_xaxis, color_up, color_down, title, multi_series)
        } else {
            p = oneplot_stepi(dt, from, to, addti, linear_trend, yaxis_log, rm_weekend, color_up, color_down)
        }
    }
    
    
    ###### oscillator techinal indicators ######
    if (!is.null(addti) & length(addti)>0 & num_syb==1) {
        p = do.call(pp_add_ti_oscillator, args = list(p = p, dt = dt[date>=from & date <= to], from = from, to = to, rm_weekend = rm_weekend, addti = addti, interact = TRUE))
    } 
    
    # set title 
    p = pp_set_title(p, dt[date>=from & date <= to], title = title, interact = TRUE)
    # set yaxis
    p = pp_set_yaxis(p, yaxis_log = yaxis_log, interact = TRUE)
    # set xaxis
    p = pp_set_xaxis(p, dt[date>=from & date <= to], rm_weekend = rm_weekend, rm_xaxis = rm_xaxis, interact = TRUE)
    
    return(p)
}

# # set text size
# pp_set_size = function(geom_text_size = 7) {
#     theme_size = (14/5) * geom_text_size
#     return(list(geom_text_size=geom_text_size, theme_size=theme_size))
# }

# multiple plot
multiplot = function(func, dt, from, to, addti, linear_trend, yaxis_log, rm_weekend, rm_xaxis = FALSE, color_up, color_down, title, multi_series, ...) {
    . = change_pct = name = symbol = NULL
    
    p = dt %>% 
        split(., dt$symbol) %>% 
        lapply(., function(x) {
            do.call(func, list(
                dt=x, from=from, to=to, addti=addti, linear_trend=linear_trend, yaxis_log=yaxis_log, rm_weekend=rm_weekend, color_up=color_up, color_down=color_down
            )) %>% 
                pp_set_yaxis(yaxis_log = yaxis_log, interact = TRUE) %>% 
                pp_set_xaxis(x[date>=from & date <= to], rm_weekend = rm_weekend, rm_xaxis = rm_xaxis, interact = TRUE) %>% 
                pp_set_title(x[date>=from & date <= to], title = title, interact = TRUE) %>% 
                layout(
                    annotations = list(
                        yref = 'paper', xref = 'paper', 
                        x = 0.02, y = 0.95, 
                        align="left", showarrow = FALSE, 
                        text = x[.N, paste0(symbol,' ',name,'\n',date,'\n',round(close,2),' (',round(change_pct,2),'%)')]
                    )
                )
        }) %>% 
        subplot(nrows = multi_series$nrow, shareX = TRUE, margin = .02) %>% 
        layout(title = '', showlegend = FALSE)
    
    return(p)
}

# set xaxis 
pp_set_xaxis = function(p, dt, rm_weekend = TRUE, rm_xaxis = FALSE, interact = FALSE, ...) {
    . = yl = ql = ml = wl = y = m = w = d = brks = freq = NULL
    
    if (interact) {
        p = layout(
            p,
            xaxis = list(
                title = '', #range = c(from, to),
                rangebreaks = list( 
                    bounds=c("sat", "sun")
                )
            ), 
            hovermode = "x unified"
        )
        
        if (!inherits(dt$x, 'Date')) return(p)
        
        p = layout(
            p, 
            xaxis = list(
                type='date',
                tickformatstops = list(
                    list(
                        dtickrange = list(NULL, 1000), 
                        value = "%H:%M:%S.%L ms"
                    ), 
                    list(
                        dtickrange = list(1000, 60000), 
                        value = "%H:%M:%S s"
                    ), 
                    list(
                        dtickrange = list(60000, 3600000), 
                        value = "%H:%M m"
                    ), 
                    list(
                        dtickrange = list(3600000, 86400000), 
                        value = "%H:%M h"
                    ), 
                    list(
                        dtickrange = list(86400000, 604800000), 
                        value = "%y-%m-%d"
                    ), 
                    list(
                        dtickrange = list(604800000, "M1"), 
                        value = "%y-%m-%d"
                    ), 
                    list(
                        dtickrange = list("M1", "M12"), 
                        value = "%Y-%m"
                    ), 
                    list(
                        dtickrange = list("M12", NULL), 
                        value = "%Y Y"
                    )
                )
            )
        )
        return(p)
    } # else {}
    
    
    xfreqs = c('y', 'q', 'm', 'w')
    
    xticks = dt[
        , .(date, rowid)
    ][, `:=`(
        y = year(date), q=quarter(date), m = month(date), w = isoweek(date), d = mday(date)#, rowid = .I
    )][, (paste0(xfreqs,'l')) := lapply(.SD, function(x) {
        xi = x - shift(x, 1, type='lag')
        xi[xi != 0] <- 1
        return(xi)
    }), .SDcols = xfreqs
    ][yl==1 | ql==1 | ml==1 | wl==1, .(date,rowid, y,q,m,w,d, yl,ql,ml,wl)]
    
    # breaks and labels
    xbrk_lab = data.table()
    xbrk_nrow = xbrk_lab[,.N]
    num_xfreqs = 1
    while (xbrk_nrow <= 15 & num_xfreqs <= 3) {
        xfreq = xfreqs[num_xfreqs]
        xlab = paste0(xfreq,'l')
        if (xfreq == 'q') {
            xfreq = 'm'
        } else if (xfreq == 'w') {
            xfreq = 'd'
        }
        # print(xfreq)
        
        xbrk_lab = rbind(
            xbrk_lab, 
            xticks[eval(parse(text = xlab)) == 1, .(date, brks = rowid, labs = eval(parse(text = xfreq)), freq=xfreqs[num_xfreqs])])
        xbrk_lab = unique(xbrk_lab, by = 'brks', fromLast = FALSE) 
        
        xbrk_nrow = xbrk_lab[, .N] + xticks[eval(parse(text=xlab)) == 1, .N]
        num_xfreqs = num_xfreqs+1
    }
    xbrk_lab = xbrk_lab[order(brks)]
    main_brks = xbrk_lab
    
    if (xfreq == 'y') {
        brk_num = c(2, 5, 10, 25, 50, 100)
        i = 1
        while (main_brks[,.N] > 15) {
            minor_brks = main_brks
            main_brks  = main_brks[, rowid := .I][rowid %% brk_num[i] == 0]
            i = i+1
        }
    }
    if (!exists('minor_brks')) {
        # minor_breaks
        xfreq = xfreqs[num_xfreqs]
        xlab = paste0(xfreq,'l')
        if (xfreq == 'q') {
            xfreq = 'm'
        } else if (xfreq == 'w') {
            xfreq = 'd'
        }
        
        minor_brks = xticks[
            eval(parse(text = xlab)) == 1, 
            .(date, brks = rowid, labs = eval(parse(text = xfreq)))]
    }
    
    
    # set x axis
    if (rm_weekend) {
        p = p + scale_x_continuous(
            breaks = main_brks[, brks], labels = main_brks[, labs],
            expand = expand_scale(add=1), 
            minor_breaks = minor_brks[, brks])
    } else {
        p = p + scale_x_date(
            breaks = main_brks[, date], labels = main_brks[, labs],
            expand = expand_scale(add=1), 
            minor_breaks = minor_brks[, date])
    }
    
    p = p + theme(axis.text.x = element_text(face = main_brks[, ifelse(freq=='y', 'bold', ifelse(freq=='q', 'bold.italic', 'plain'))]))
    
    if (rm_xaxis) p = p + theme(axis.ticks.x = element_blank(), axis.text.x = element_blank())
    return(p)
}
# set yaxis
#' @import scales
pp_set_yaxis = function(p, yaxis_log = FALSE, interact = FALSE, title = '') {
    if (interact) {
        if (yaxis_log) {
            p = layout(p, yaxis = list(title = title, side = 'right', hoverformat = '.2f', type = "log"))
        } else {
            p = layout(p, yaxis = list(title = title, side = 'right', hoverformat = '.2f'))
        }
        return(p)
    }
    
    # ggplot2
    if (yaxis_log) {
        p = p + 
            scale_y_continuous(trans='log10', sec.axis = dup_axis()) +
            # scale_y_log10(
            #     breaks = trans_breaks('log10', function(x) 10^x),
            #     labels = trans_format('log10', math_format(10^.x)),
            #     minor_breaks = log10(5) + -2:5
            # ) +
            annotation_logticks(sides = 'rl') 
    } else {
        p = p + scale_y_continuous(sec.axis = dup_axis()) 
    }
    
    p = p + theme(
        axis.text.y.left  = element_text(angle = -90), 
        axis.text.y.right = element_text(angle =  90) )
    
    return(p)
}
# add linear trend line
# sar not fixed
pp_add_linear_trend = function(
    p, dt, 
    rm_weekend = TRUE, 
    linear_trend = c(0, 1, -1), 
    interact = FALSE, 
    yaxis_log = FALSE, ...
) {
    add1_linear_trend = function(p, dt, num_sd, rm_weekend, interact = FALSE, yaxis_log = FALSE) {
        x = symbol = NULL
        
        # add x column
        if (!('x' %in% names(dt))) {
            dt[, x := date]
            if (rm_weekend) dt[, x := seq_len(.N), by=symbol]
        }
        
        # line color
        line_color = 'blue'
        if (num_sd == 0) line_color = 'red'
        
        
        # plot
        if (!interact) {
            p = p + 
                geom_smooth(
                    data = dt, aes_string(x='x', y='close'),
                    method = lm, formula = y+num_sd*sd(y, na.rm = TRUE)~x, 
                    na.rm = TRUE, se = FALSE, color = line_color, size = 0.2)
        } else {
            line_fmt = list(dash="solid", width=0.8, color=line_color)
            ylog = function(close, x, yaxis_log) {
                if (!yaxis_log) {
                    y_sd = dt[, sd(close, na.rm = TRUE)]
                    predict(lm(close ~ x)) + num_sd * y_sd
                } else {
                    y_sd = dt[, sd(log10(close), na.rm = TRUE)]
                    10^(predict(lm(log10(close) ~ x)) + num_sd * y_sd)
                }
            }
            p = p %>% 
                add_lines(
                    x = ~ x, 
                    y = ~ ylog(close, rowid, yaxis_log), 
                    line = line_fmt, 
                    name = sprintf('%0.1fsd', num_sd), 
                    hoverinfo = 'y+name'
                )
        }
    }
    
    for (i in linear_trend) p = add1_linear_trend(p, dt, i, rm_weekend, interact, yaxis_log)
    
    return(p)
}
# set title
pp_set_title = function(p, dt, title = NULL, interact = FALSE) {
    symbol = NULL
    
    if (is.null(title)) title = ''
    # if ('name' %in% names(dt)) title = paste(dt[1, name], title)
    if (dt[, length(unique(symbol))==1]) {
        # title string
        title1 = paste0(title, dt[1,title1]) 
        title2 = dt[1,title2]
        
        # adding title and annotation
        if (!interact) {
            # p = p + ggtitle(title1, subtitle = title2)
            p = p + labs(title = title1, subtitle = title2)
        } else {
            p = p %>% 
                layout(
                    title = list(text = title1, x = 0.01)
                )
            # paste(title1, title2, sep = '\n')
            return(p)
        }
    } else {
        if (!interact) {
            p = p + labs(title = title)
        } else {
            p = p %>% 
                layout(
                    title = list(text = title, x = 0.01)
                )
            return(p)
        }
    }
    
    # showtext_begin();
    p = p + theme(
        plot.title    = element_text(margin = margin(t=1, b=1)),
        plot.subtitle = element_text(margin = margin(t=0, b=1))#,
        # text = element_text( family = 'wqy-microhei')
    )
    # showtext_end() 
    return(p)
}


# add technical indicators
# dt is original dataset without change from/to
# overlay: mm, sma, ema, smma, bb, sar
pp_add_ti_overlay = function(
    p, dt, from=NULL, to=Sys.Date(), 
    addti = list(SMA = list(n=20), SMA = list(n=50)), 
    rm_weekend = NULL, interact = FALSE
) {
    shp = w_position = x = symbol = ti_str = bbands_pctb = NULL
    
    # overlay technical indicators
    names(addti) <- tolower(names(addti))
    ti_not_topbottom = names(addti)[sapply(addti, function(x) !any(x[['position']] %in% c('top','bottom')))]
    ti_overlay = intersect( ti_not_topbottom, tolower(pq_addti_funs()[['overlays']]) )
    addti = addti[names(addti) %in% c(ti_overlay,'w')]
    # return p if no overlay ti
    if (length(addti) == 0 || is.null(addti)) return(p)
    
    # colorblind palette
    cb_palette = c('#E41A1C', '#377EB8', '#4DAF4A', '#984EA3', '#FF7F00', '#FFFF33', '#A65628', '#F781BF', '#999999') # RColorBrewer::brewer.pal(9,'Set1') # display.brewer.all(type = 'qual')
    
    # add x column
    if (!('x' %in% names(dt))) {
        dt[, x := date]
        if (rm_weekend) dt[, x := seq_len(.N), by=symbol]
    }
    
    for (i in seq_along(addti)) {
        # add ti columns to dt
        if (names(addti[i]) == 'w') {
            dtti = copy(dt)
        } else {
            args_list = c(list(dt=dt, col_kp=TRUE, col_formula=TRUE), 
                          lapply(addti[i], function(x) x[setdiff(names(x), c('color', 'position', 'hline'))]) )
            dtti = do.call(pq_addti, args = args_list)
        }
        
        # bind list of dataframes
        if (inherits(dtti,'list')) dtti = rbindlist(dtti, fill = TRUE)
        if (names(addti[i]) == 'bbands') dtti = dtti[,bbands_pctb := NULL]
        dat = dtti[date>=from & date <= to]

        # color 
        color = cb_palette[ifelse(
            i %% length(cb_palette) == 0, 
            length(cb_palette), 
            i %% length(cb_palette)
        )]
        if (!is.null(addti[[i]][['color']])) color = addti[[i]][['color']]
            
        # line type
        geom_type = 'line'
        if (names(addti[i]) %in% c('runmax', 'runmin')) geom_type = 'step'
        if (names(addti[i]) == 'sar') geom_type = 'point'

        ti_names = names(dtti)[grep(sprintf('^%s',names(addti[i])), names(dtti))]

        
        if (names(addti[i]) == 'w' & all(c('w_price', 'w_position', "w_type") %in% names(dat))) {
            p = p + 
                geom_point(
                    aes_string(x='x', y='w_price', shape = 'shp'), 
                    data = dat[,shp:=as.character(sign(w_position))], na.rm = TRUE) +
                scale_shape_manual(values=c(6, 2)) + 
                guides(shape=FALSE)
        } else {
            if (!interact) {
                for (t in ti_names) {
                    p = p + 
                        stat_identity(aes_string(x='x',y=t), data = dat, geom = geom_type, color = color) + 
                        geom_rug(aes_string(x='x', y=t), data=dat[.N], sides='r', color='gray', alpha = 0.8)# linetype = 'longdash', 
                }
                
                # addti formula value
                dat_n = dat[,.SD[.N]
                ][, (ti_names) := lapply(.SD, round, digits = 2), .SDcols = ti_names 
                ][,c('formula_str', ti_names),with=FALSE]
                dat_n[, ti_str := paste0(dat_n, collapse = ' ')]
                # add ti_str
                p = p + 
                    geom_text(x = dat[1, x], y = Inf, aes(label = ti_str), data = dat_n, hjust = 0, vjust = i*1.25+1, color = color, na.rm = TRUE, alpha = 0.6, size = rel(3))
            } else {
                for (t in ti_names) {
                    line_fmt = list(width=0.8, color=color)
                    if (geom_type == 'line') {
                        p = p %>% 
                            add_lines(
                                data = dat,
                                x = ~ x, y = dat[[t]], 
                                name = t, 
                                line = line_fmt,
                                hoverinfo = 'y+name'
                            ) 
                    } else if (geom_type == 'step') {
                        p = p %>% 
                            add_lines(
                                data = dat,
                                x = ~ x, y = dat[[t]], 
                                name = t, 
                                line = line_fmt,
                                interactline = list(shape = "hv"),
                                hoverinfo = 'y+name'
                            ) 
                    } else if (geom_type == 'point') {
                        p = p %>% 
                            add_lines(
                                data = dat,
                                x = ~ x, y = dat[[t]], 
                                name = t, 
                                line = line_fmt,
                                interactline = list(dash = 'dot'),
                                hoverinfo = 'y+name'
                            ) 
                    }
                    
                        
                }
            }
        }
    }
    return(p)
}


plotly_vline <- function(x = 0, color = "red", dash = NULL) {
    list(
        type = "line", 
        y0 = 0, 
        y1 = 1, 
        yref = "paper",
        x0 = x, 
        x1 = x, 
        line = list(color = color, dash = dash)
    )
}

plotly_hline <- function(y = 0, color = "blue", dash = NULL) {
    list(
        type = "line", 
        x0 = 0, 
        x1 = 1, 
        xref = "paper",
        y0 = y, 
        y1 = y, 
        line = list(color = color, dash = dash)
    )
}

# oscillator: macd, ppo, roc, rsi, cci
pp_add_ti_oscillator = function(
    p, dt, 
    from=NULL, to=Sys.Date(),
    addti = list(
        macd = list(n=50, position = 'top'), 
        roc = list(n=25, position = 'bottom'), 
        ppo = list(n=50, position = 'top'), 
        rsi = list(n=25)
    ), 
    rm_weekend = NULL, 
    interact = FALSE
) {
    symbol=x=formula_str=ti_str=NULL
    
    # oscillator technical indicators
    names(addti) <- tolower(names(addti))
    ti_topbottom   = names(addti)[sapply(addti, function(x)  any(x[['position']] %in% c('top','bottom')))]
    # ti_not_overlay = names(addti)[sapply(addti, function(x) !any(x[['position']] %in% c('overlay')))]
    ti_not_topbottom = names(addti)[sapply(addti, function(x) !any(x[['position']] %in% c('top','bottom')))]
    ti_oscillator  = c(
        ti_topbottom,
        # intersect(ti_topbottom, tolower(pq_addti_funs()[['overlays']])), 
        setdiff(ti_not_topbottom, tolower(pq_addti_funs()[['overlays']])),
        # intersect(ti_not_overlay, tolower(pq_addti_funs()[['indicators']])),
        intersect(names(addti), names(which(sapply(dt, is.numeric))))
        )
    if ('bbands' %in% names(addti)) ti_oscillator = c(ti_oscillator, 'bbands')
    addti = addti[names(addti) %in% ti_oscillator]
    # return p if no oscillator ti
    if (length(addti) == 0 || is.null(addti)) return(p)
    
    # colorblind palette
    cb_palette = c('#000000', '#E69F00', '#56B4E9', '#009E73', '#F0E442', '#0072B2', '#D55E00', '#CC79A7') # c('black', 'red', 'blue') # 

    # add x column in dataset
    if (!('rowid' %in% names(dt))) dt[, rowid := seq_len(.N), by=symbol]
    if (!('x' %in% names(dt))) {
        dt[, x := date]
        if (rm_weekend) dt[, x := rowid]
    }
    
    # check height
    addti = lapply(addti, function(x) {
        if (is.null(x[['height']])) x[['height']] = 3
        return(x)
    })
    # check position
    addti = lapply(addti, function(x) {
        if (is.null(x[['position']])) x[['position']] = 'bottom'
        return(x)
    })
    # the plot of last bottom ti will add x-axis
    last_bottom = max(which(sapply(addti, function(x) {
        any(x[['position']] == 'bottom')
    })))

    # plist
    top_plist = bottom_plist = NULL
    for (i in seq_along(addti)) {
        # dataset with technical indicators
        if (names(addti[i]) %in% names(dt)) {
            dtti = copy(dt)
        } else {
            args_list = c(list(dt=dt, col_kp=TRUE, col_formula=TRUE), 
                          lapply(addti[i], function(x) x[setdiff(names(x), c('color', 'position', 'hline', 'height'))]) )
            dtti = do.call(pq_addti, args=args_list)
        }
        
        # # bind list of dataframes
        if (inherits(dtti, 'list')) dtti = rbindlist(dtti, fill = TRUE)
        # # modify indicators
        if (names(addti[i]) == 'bbands') dtti = dtti[,(c('bbands_dn', 'bbands_mavg', 'bbands_up')) := NULL]
        if (names(addti[i]) == 'adx')    dtti = dtti[,(c('adx_dx')) := NULL]
        if (names(addti[i]) == 'atr')    dtti = dtti[,(c('atr_tr', 'atr_truehigh', 'atr_truelow')) := NULL]
        if (names(addti[i]) == 'aroon')  dtti = dtti[,(c('aroon_oscillator')) := NULL]
        dat = dtti[date>=from & date <= to]

        # names of technical indicators
        ti_names = names(dat)[grepl(paste0('^',names(addti[i])), names(dat))]
        
        # plot each column in ti_names
        if (!interact) {
            pi = ggplot(data = dat, aes_string(x = 'x'))
        } else pi = plot_ly(dtti, x = ~ x)
        
        for (t in ti_names) {
            iti_names = which(t == ti_names)
            # color
            color = cb_palette[ifelse(iti_names %% length(cb_palette) == 0, length(cb_palette), iti_names %% length(cb_palette))]
            
            if (!interact) {
                pi = pi + 
                    geom_line(aes_string(y=t), stat = 'identity', color = color) + 
                    geom_rug(aes_string(x='x', y=t), data=dat[.N], sides='r', color='gray', alpha = 0.8)
            } else {
                pi = pi %>% 
                    add_lines(
                        y = dtti[[t]], color = color, 
                        name = t, 
                        line = list(width=0.8, color=color), 
                        showlegend = FALSE, 
                        hoverinfo = 'text+name',
                        text = ~ sprintf('%s\n%s', date, round(dtti[[t]],2)) 
                    ) %>% 
                    layout(
                        title = list(text = NULL), 
                        margin = list(pad=0, b=0, l=10, r=10 ), 
                        legend = list(x = 0.02, y = 0.98)
                    )
            }
            
        }
        
        if (!('formula_str' %in% names(dat))) dat[, formula_str := paste0(ti_names, collapse = ',')]
        # addti formula value
        dat_n = dat[,.SD[.N]
                  ][, (ti_names) := lapply(.SD, round, digits = 2), .SDcols = ti_names
                  ][,c('formula_str', ti_names),with=FALSE]
        dat_n[, ti_str := paste0(dat_n, collapse = ' ')]
        
        # set text/labs format
        # showtext_begin();
        if (!interact) {
            pi = pi + 
                geom_text(x = dat[1, x], y = Inf, aes(label = ti_str), data = dat_n, hjust = 0, vjust = 1, color = 'black', na.rm = TRUE, alpha = 0.6, size = rel(3)) + 
                labs(x=NULL, y=NULL) + theme_bw() + 
                theme(
                    plot.margin = unit(rep(0, 4), 'cm')#,
                    # text = element_text(family='wqy-microhei')
                )
        } else {
            pi = pi %>% 
                layout(annotations = list(
                    yref = 'paper', xref = 'paper', 
                    x = 0.02, y = 0.95, 
                    text = dat_n$ti_str, showarrow = FALSE
                ))
        }
        # showtext_end();
        
        # hlines
        hlines = ti_idicators_hline()
        if (names(addti[i]) %in% tolower(names(hlines)) || 'hline' %in% names(addti[[i]])) {
            if ('hline' %in% names(addti[[i]])) {
                yl_ticks = unlist(addti[[i]][names(addti[[i]]) %in% 'hline'])#[['hline']]
            } else yl_ticks = hlines[[names(addti[i])]]
            
            for (yi in seq_along(yl_ticks)) {
                clr = '#BEAED4'
                if (yi == 1 & (length(yl_ticks) %% 2) == 1) clr = '#7FC97F' 
                # brewer.pal(2,'Accent')
                if (!interact) {
                    pi = pi + geom_hline(yintercept = yl_ticks[yi], color = clr, linetype = 'longdash')
                } else {
                    pi = pi %>% layout(shapes = list(
                        plotly_hline(yl_ticks[yi], color = clr, dash = 'dash')
                    )) 
                }
                
            }
        }
        
        
        # set xaxis
        if (!interact) {
            rm_xaxis = TRUE
            if (i == last_bottom) rm_xaxis = FALSE
            pi = pp_set_xaxis(pi, dat, rm_weekend = rm_weekend, rm_xaxis = rm_xaxis)
        }
        # set yaxis
        pi = pp_set_yaxis(pi, interact = interact)
        
        
        
        # plist
        if (addti[[i]][['position']] == 'top') {
            top_plist[[paste0('p',i)]] = pi
        } else if (addti[[i]][['position']] == 'bottom') {
            bottom_plist[[paste0('p',i)]] = pi
        }
    }
    
    # # arrange plot list
    # if (!is.null(top_plist)) top_plist2 = grid.arrange(grobs = top_plist, ncol = 1)
    # if (!is.null(bottom_plist)) bottom_plist = grid.arrange(grobs = bottom_plist, ncol = 1)

    if (!interact) {
        # heights of subplots
        h = min(unique(sapply(addti, function(x) x[['height']])))
        h = as.character(which.min(abs(h - 1:3)))
        h = list('3' = c(115, 100, 320),
                 '2' = c(111, 100, 222),
                 '1' = c(109, 100, 127))[[h]]
        heights = c(rep(h[2], length(top_plist)), h[3])
        if (length(bottom_plist)>0) heights = c(heights, rep(h[2], length(bottom_plist)-1), h[1])
        
        # return p
        p = grid.arrange(grobs = c(top_plist, list(p0=p), bottom_plist), ncol = 1, heights = heights)
    } else {
        h1 = length(top_plist)
        h3 = length(bottom_plist)
        hs = c(h1*0.2, 0.6, h3*0.2)[which(c(h1*0.2, 0.6, h3*0.2) > 0)]
        if (h1 > 1) top_plist = list(pt = subplot(top_plist, nrows = h1, shareX = TRUE))
        if (h3 > 1) bottom_plist = list(pb = subplot(bottom_plist, nrows = h3, shareX = TRUE))
        p = subplot(c(top_plist, list(p0=p), bottom_plist), nrows = length(hs), margin = 0.02, heights = hs/sum(hs), shareX = TRUE)
        
    }
    
    return(p)
}

check_dt_plot = function(dt, x) {
    . = close_prev = symbol = NULL
    
    dt = check_dt(dt)
    
    xcol <- intersect(names(dt), unlist(strsplit(x,'\\|')))[1]
    if (xcol != 'close') dt[['close']]  = dt[[xcol]]
    
    # dt = split(dt, by = 'symbol')
    dt[, symbol := factor(symbol, levels = dt[,unique(symbol)])]
    
    # rowid
    dt = dt[unique(dt[,.(date)])[order(date)][,rowid := .I][], on='date'][order(symbol, date)]
    # close_prev
    dt = dt[, close_prev := shift(close, 1, type = 'lag'), by = symbol]
    
    setkeyv(dt, c('symbol','date'))
    return(dt)
}

#' creating charts for time series
#' 
#' \code{pq_plot} provides an easy way to create charts for time series dataset based on predefined formats.
#' 
#' @param dt a list/dataframe of time series dataset
#' @param chart_type chart type, including line, step, bar, candle.
#' @param date_range date range. Available value includes '1m'-'11m', 'ytd', 'max' and '1y'-'ny'. Default is max.
#' @param from the start date. Default is NULL. If it is NULL, then calculate using date_range and end date.
#' @param to the end date. Default is the current date.
#' @param x the name of column display on chart.
#' @param freq the data frequency. It supports c('daily', 'weekly', 'monthly', 'quarterly', 'yearly').
#' @param addti list of technical indicators or numerical columns in dt. For technical indicator, it is calculated via \code{pq_addti}, which including overlay and oscillator indicators.
#' @param linear_trend a numeric vector. Default is NULL. If it is not NULL, then display linear trend lines on charts. 
#' @param cumreturns logical, display the cumulative returns. Default is FALSE.
#' @param yaxis_log logical. Default is FALSE.
#' @param color_up the color indicates price going up
#' @param color_down the color indicates price going down
#' @param multi_series a list. It display the number of ncol or nrow, and the yaxis scales in 'free'/'free_y'/'free_x'. Default is NULL.
#' @param rm_weekend whether to remove weekends in xaxis. The default is TRUE for candle and bar chart, and is FALSE for line and step chart.
#' @param title chart title. It will added to the front of chart title if it is specified.
#' @param interact whether to create a interactive graphics, defaults to FALSE.
#' @param ... ignored
#' 
#' @examples 
#' \donttest{
#' # single symbol
#' data(dt_ssec)
#' # dt_ssec = md_stock('^000001', source='163', date_range = 'max')
#' 
#' # chart type
#'   pq_plot(dt_ssec, chart_type = 'line',   date_range = '6m') # line chart (default)
#' # pq_plot(dt_ssec, chart_type = 'step',   date_range = '6m') # step line
#' # pq_plot(dt_ssec, chart_type = 'candle', date_range = '6m') # candlestick
#' # pq_plot(dt_ssec, chart_type = 'bar',    date_range = '6m') # bar chart
#' 
#' # add technical indicators
#' pq_plot(dt_ssec, chart_type = 'line', addti = list(
#'         sma = list(n = 200), 
#'         sma = list(n = 50), 
#'         macd = list()
#' ))
#' # linear trend with yaxis in log
#' pq_plot(dt_ssec, chart_type = 'line', linear_trend = c(-0.8, 0, 0.8), yaxis_log = TRUE)
#' 
#' 
#' # multiple symbols
#' # download datasets
#' # dat = md_stock(c('FB', 'AMZN', 'AAPL', 'NFLX', 'GOOG'), date_range = 'max')
#' # dat = md_stock(c('^000001', '^399001', '^399006', '^000016', '^000300', '^000905'), 
#' #               date_range = 'max', source='163')
#'
#' data(dt_banks)
#' dat = md_stock_adjust(dt_banks, adjust = TRUE)
#' 
#' # linear trend
#' pq_plot(dat, multi_series=list(nrow=2, scales='free_y'), linear_trend=c(-0.8, 0, 0.8))
#' pq_plot(dat, multi_series=list(nrow=2, scales='free_y'), linear_trend=c(-0.8, 0, 0.8), 
#'         yaxis_log=TRUE)
#' 
#' # performance
#' pq_plot(dat, x='close', multi_series = list(nrow=2), cumreturns=TRUE, date_range = 'ytd')
#' pq_plot(dat, x='close', multi_series = list(nrow=1, ncol=1), cumreturns=TRUE, date_range = 'ytd')
#' 
#' }
#' 
#' @import ggplot2 gridExtra 
#' @importFrom stats lm sd predict
#' @importFrom plotly plot_ly add_trace add_lines add_segments layout subplot `%>%`
#' @export
pq_plot = function(
    dt, chart_type = 'line', 
    date_range = 'max', from = NULL, to = Sys.Date(), 
    x = 'close|value', 
    addti = NULL, 
    linear_trend = NULL, cumreturns = FALSE, freq = 'daily',
    yaxis_log = FALSE, 
    color_up = '#CF002F', color_down = '#000000', 
    multi_series = list(nrow=NULL, ncol=NULL), 
    rm_weekend = NULL, title = NULL, interact = FALSE, ...) {
    
    xcol=symbol=.=close_prev=name=title1=updn_2day=change=change_pct=price_str11=NULL
    
    ## bind list of dataframes
    dt = check_dt(dt)
    
    ## from/to
    to = check_to(to, default_to = dt[,max(date)])
    from = check_from(date_range, from, to, default_from = dt[,min(date)], default_date_range = 'max')
    
    # change into performance
    if (cumreturns) {
        dt = pq_return(dt, x=x, freq = freq, cumreturns=cumreturns, date_range=date_range, from=from, to=to)
        x = 'cumreturns'
        title = ifelse(is.null(title), 'performance', paste(title, 'performance')) 
    }
    
    
    # add columns of rowid, close_prev, change, change_pct, x, title1, title2, updn_2day, 
    xcol <- intersect(names(dt), unlist(strsplit(x,'\\|')))[1]
    dt = check_dt_plot(dt, x)
    # close_prev, change, change_pct, x, title1, title2, updn_2day, 
    dt = dt[, `:=`(
         change = close - close_prev,
         change_pct = (1-close_prev/close)*100,
         x = date
     )][, `:=`(
         title1 = paste(symbol, name[.N]),
         title2 = sprintf('[%s/%s]', from, to),
         updn_2day = ifelse(close > close_prev, 'up', 'down')
     ), by = symbol]
    # set title1 as factor
    dt = dt[, `:=`(title1 = factor(title1, levels = dt[,unique(title1)]))]
    # updn_2day
    if (all(c('close', 'open') %in% names(dt))) dt[is.na(updn_2day), updn_2day := ifelse(close > open, 'up', 'down')]
    
    # subtitle / legend string
    # xcol = x
    subtitle_str = dt[, .SD[.N], by = symbol]
    subtitle_str = subtitle_str[, `:=`(
        price_str = sprintf("atop(bold('%s')~'%.2f'~bold('Chg')~'%.2f(%.2f%%)')", xcol,close,change,change_pct),
        price_str11 = sprintf('%s, %s, %.2f', symbol, date, close) )]
    # set price_str11 as factor, 
    # price_str11 is the legend for multiple series when nrow & ncol=1
    subtitle_str = subtitle_str[, price_str11 := factor(price_str11, levels = subtitle_str[,price_str11])]
    
    
    ## multiple series options
    multi_series_allnull = all(sapply(multi_series, is.null))
    multi_series_all1 = all(sapply(list('nrow', 'ncol'), function(x) any(multi_series[[x]] == 1)))
    if (is.null(multi_series[['scales']])) multi_series[['scales']] = 'fixed'
    
    ## chart type
    chart_type = check_arg(chart_type, c('candle', 'bar', 'line', 'step'), default = 'line')
    if (chart_type %in% c('candle', 'bar')) {
        if (multi_series_all1 || !all(c('open', 'high', 'low', 'close') %in% names(dt))) {
            chart_type = 'line'
            warning("The `chart_type` is set to the default 'line'. It should be line or step, if you want to display multiple series on one chart or donot have open/high/low/close columns.")
        }
    }
    ## rm_weekend
    if (!is.logical(rm_weekend)) {
        if (chart_type %in% c('bar', 'candle'))       { rm_weekend = TRUE
        } else if (chart_type %in% c('line', 'step')) { rm_weekend = FALSE}
    }
    if (interact) rm_weekend = FALSE
    # if (!multi_series_all1 & num_syb >1) rm_weekend = FALSE
    if (rm_weekend) dt[, x := rowid]
    
    # chart type
    if (isTRUE(interact)) chart_type = paste0(chart_type, 'i')
    
    # plot list
    plist = list()
    if (!multi_series_allnull) {
        ## multiple series
        if (dt[,length(unique(symbol)) > 1]) ifelse(is.null(title), 'multiple series', title)  
        
        plist[['multi_series']] = suppressWarnings(
            do.call(paste0('pp_',chart_type), args = list(
                dt = dt, 
                date_range = date_range, 
                from = from, to = to, 
                addti = addti,
                x = x, yaxis_log = yaxis_log, 
                color_up = color_up, color_down = color_down, 
                rm_weekend = rm_weekend, title = title, 
                multi_series = multi_series,
                linear_trend = linear_trend, 
                multi_series_all1 = multi_series_all1,
                subtitle_str = subtitle_str
            ))
        )
            
    } else {
        ## single series
        sybs = dt[, unique(symbol)]
        for (s in sybs) {
            dt_s = dt[symbol == s]
            setkeyv(dt_s, 'date')
            
            plist[[s]] = suppressWarnings(
                do.call(paste0('pp_',chart_type), args = list(
                    dt = dt_s, 
                    date_range = date_range, 
                    from = from, to = to, 
                    addti = addti,
                    x = x, yaxis_log = yaxis_log, 
                    color_up = color_up, color_down = color_down, 
                    rm_weekend = rm_weekend, title = title, 
                    multi_series = multi_series,
                    linear_trend = linear_trend,
                    multi_series_all1 = multi_series_all1,
                    subtitle_str = subtitle_str[symbol == s]
                ))
            )
                
        }
    }
    
    return(plist)
}


