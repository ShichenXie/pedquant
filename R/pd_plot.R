# ref 
# - [Introduction to Candlesticks](https://stockcharts.com/school/doku.php?id=chart_school:chart_analysis:introduction_to_candlesticks)
# - [ggplot-for-ohlc-chart-using-bars-not-candles](https://stackoverflow.com/questions/28201587/ggplot-for-ohlc-chart-using-bars-not-candles)

# color_up, color_down
# - stockcharts: "#000000", "#CF002F"
# - A share: "red", "green"
# - ggplot2: "#18C0C4", "#F6736D"
# rgb()

# candlstick
pp_candle = function(
    dt, from=NULL, to=Sys.Date(), 
    color_up = "#F6736D", color_down = "#18C0C4", title = title, 
    rm_weekend = TRUE, rm_xaxis = FALSE, yaxis_log = FALSE, x = "open|high|low|close", 
    linear_trend = NULL, multi_series = list(nrow=NULL,ncol=NULL), 
    addti = list(sma = list(n=50), mm = list(n=25)),
    x_scale = 0.6, ...
) {
    prev_close = change = change_pct = high = low = updn_1day = updn_2day = symbol = V1 = NULL
    
    # copy dt
    dt = copy(dt)
    
    # number symbols/ nrow==1 & ncol==1
    num_syb = dt[, length(unique(symbol))]
    multi_series_all1 = list(...)[['multi_series_all1']]
    
    # x and updn
    dt[, updn_1day := ifelse(close > open, "hollow", paste("filled", updn_2day, sep = "_"))]
    
    # subtitle string
    xcol = x
    subtitle_str = dt[, .SD[.N], by = symbol
                    ][, price_str := sprintf("atop(bold('%s')~'%.2f'~bold('Chg')~'%.2f(%.2f%%)')", xcol, close, change, change_pct), by = symbol]
    
    
    # plot
    ## change dt date range to from/to
    dat = dt[date>=from & date <= to]
    if (!multi_series_all1) {
        p = ggplot(dat, aes(color = updn_2day)) +
            geom_segment(aes(x = x, y = low, xend = x, yend = high)) + 
            geom_rect(aes(xmin = x - x_scale / 2, xmax = x + x_scale / 2, ymin = open, ymax = close, fill = updn_1day)) + 
            scale_fill_manual(values = c("hollow" = "white", "filled_up" = color_up, "filled_down" = color_down)) +
            scale_color_manual(values = c("up" = color_up, "down" = color_down)) + 
            geom_text(x = dat[1, x], y = Inf, aes(label = date), data = dat[,.SD[.N], by=symbol], hjust = 0, vjust = 1, color = "black", na.rm = TRUE, alpha = 0.6, size = rel(3)) +
            geom_text(x = dat[.N, x], y = Inf, aes(label = price_str), data=subtitle_str, hjust = 1, vjust = 1, color = "black", parse = TRUE, na.rm = TRUE, alpha = 0.6, size = rel(3)) +
            # annotate("text", label="@http://shichen.name/pedar/", x=dat[, x[.N %/% 2], by=symbol][,V1[1]], y=-Inf, vjust = -1, color = "gray", alpha = 0.2) + 
            # geom_text(aes(label="@http://shichen.name/pedar", x=dat[, x[.N], by=symbol][,V1[1]], y=Inf), vjust = -0.5, hjust = 1, color = "#F0F0F0") + 
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
            p = do.call(pp_add_ti_overlay, args = list(p = p, dt = dt[date>=from-365 & date <= to], from = from, to = to, rm_weekend = rm_weekend, addti = addti))
        }
        
        # add linear trend line
        if (!is.null(linear_trend)) 
            p = do.call(pp_add_linear_trend, args = list(p=p, dt = dat, rm_weekend=rm_weekend, linear_trend=linear_trend))
    }
    
    # remove margin
    p = p + theme(plot.margin = unit(rep(0, 4), "cm"))
    # set xaxis
    p = pp_set_xaxis(p, dat, rm_weekend = rm_weekend, rm_xaxis = rm_xaxis)
    # set yaxis
    p = pp_set_yaxis(p, yaxis_log = yaxis_log)
    # set title 
    p = pp_set_title(p, dat, title = title)
    
    ###### oscillator techinal indicators ######
    if (!is.null(addti) & length(addti)>0 & num_syb==1) {
        p = do.call(pp_add_ti_oscillator, args = list(p = p, dt = dt[date>=from-365 & date <= to], from = from, to = to, rm_weekend = rm_weekend, addti = addti))
    } 
    
    return(p)
}

# bar
pp_bar = function(
    dt, from=NULL, to=Sys.Date(), 
    color_up = "#F6736D", color_down = "#18C0C4", title = title, 
    rm_weekend = TRUE, rm_xaxis = FALSE, yaxis_log = FALSE, x = "open|high|low|close", 
    linear_trend = NULL, multi_series = list(nrow=NULL,ncol=NULL), 
    addti = list(sma = list(n=50), mm = list(n=25)),
    x_scale = 0.6, ...
) {
    prev_close = change = change_pct = high = low = updn_2day = symbol = V1 = NULL
    
    # copy dt
    dt = copy(dt)
    
    # number symbols/ nrow==1 & ncol==1
    num_syb = dt[, length(unique(symbol))]
    multi_series_all1 = list(...)[['multi_series_all1']]
    
    # # add prev close, change and change_pct
    # if (!("prev_close" %in% names(dt))) dt = dt[, prev_close := shift(close, 1, type = "lag")]
    # if (!("change"     %in% names(dt))) dt = dt[, change := close - prev_close]
    # if (!("change_pct" %in% names(dt))) dt = dt[, change_pct := change/close*100]
    
    # subtitle string
    # subtitle_str = 
    #     dt[, .SD[.N], by = symbol
    #        ][, subtitle_str := sprintf("atop(bold('O')~'%.2f'~bold('H')~'%.2f'~bold('L')~'%.2f'~bold('C')~'%.2f'~bold('Chg')~'%.2f(%.2f%%)')", open, high, low, close, change, change_pct), by = symbol]
    xcol = x
    subtitle_str = dt[, .SD[.N], by = symbol
                    ][, price_str := sprintf("atop(bold('%s')~'%.2f'~bold('Chg')~'%.2f(%.2f%%)')", xcol, close, change, change_pct), by = symbol]
    
    
    # plot
    ## change dt date range to from/to
    dat = dt[date>=from & date <= to]
    
    if (!multi_series_all1) {
        p = ggplot(dat, aes(color = updn_2day)) + 
            geom_linerange(aes(x = x, ymin = low, ymax = high)) +
            geom_segment(aes(x = x, y = open, xend = x - x_scale / 2, yend = open)) + 
            geom_segment(aes(x = x, y = close, xend = x + x_scale / 2, yend = close)) + 
            scale_color_manual(values = c("up" = color_up, "down" = color_down)) +
            geom_text(x = dat[1, x], y = Inf, aes(label = date), data = dat[,.SD[.N], by=symbol], hjust = 0, vjust = 1, color = "black", na.rm = TRUE, alpha = 0.6, size = rel(3)) +
            geom_text(x = dat[.N, x], y = Inf, aes(label = price_str), data=subtitle_str, hjust = 1, vjust = 1, color = "black", parse = TRUE, na.rm = TRUE, alpha = 0.6, size = rel(3)) +
            # annotate("text", label="@http://shichen.name/pedar/", x=dat[, x[.N %/% 2], by=symbol][,V1[1]], y=-Inf, vjust = -1, color = "gray", alpha = 0.2) + 
            # geom_text(aes(label="@http://shichen.name/pedar", x=dat[, x[.N], by=symbol][,V1[1]], y=Inf), vjust = -0.5, hjust = 1, color = "#F0F0F0") + 
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
            p = do.call(pp_add_ti_overlay, args = list(p = p, dt = dt[date>=from-365 & date <= to], from = from, to = to, rm_weekend = rm_weekend, addti = addti))
        }
        
        # add linear trend line
        if (!is.null(linear_trend)) 
            p = do.call(pp_add_linear_trend, args = list(p=p, dt = dat, rm_weekend = rm_weekend, linear_trend=linear_trend))
    }
    
    # remove margin
    p = p + theme(plot.margin = unit(rep(0, 4), "cm"))
    # set xaxis
    p = pp_set_xaxis(p, dat, rm_weekend = rm_weekend, rm_xaxis = rm_xaxis)
    # set yaxis
    p = pp_set_yaxis(p, yaxis_log = yaxis_log)
    # set title 
    p = pp_set_title(p, dat, title = title)
    
    ###### oscillator techinal indicators ######
    if (!is.null(addti) & length(addti)>0 & num_syb==1) {
        p = do.call(pp_add_ti_oscillator, args = list(p = p, dt = dt[date>=from-365 & date <= to], from = from, to = to, rm_weekend = rm_weekend, addti = addti))
    }
    
    return(p)
}

# line
pp_line = function(
    dt, from=NULL, to=Sys.Date(), 
    color_up = "#F6736D", color_down = "#18C0C4", title = title, 
    rm_weekend = FALSE, rm_xaxis = FALSE, yaxis_log = FALSE, x = "close|value", 
    linear_trend = NULL, multi_series = list(nrow=NULL,ncol=NULL), 
    addti = list(sma = list(n=50), mm = list(n=25)), ...
) {
    prev_close = symbol = change = change_pct = high = low = prev_x = updn_2day = x1 = y1 = x2 = y2 = V1 = NULL
    
    # copy dt
    dt = copy(dt)
    
    # number symbols/ nrow==1 & ncol==1
    num_syb = dt[, length(unique(symbol))]
    multi_series_all1 = list(...)[['multi_series_all1']]
    
    # x and updn
    dt[, prev_x := shift(x, 1, type="lag"), by = symbol
       ][updn_2day=="down", `:=`(x1 = prev_x, x2 = x, y1 = prev_close, y2 = close)]
    
    
    # subtitle string
    xcol = x
    subtitle_str = dt[, .SD[.N], by = symbol
                    ][, price_str := sprintf("atop(bold('%s')~'%.2f'~bold('Chg')~'%.2f(%.2f%%)')", xcol, close, change, change_pct), by = symbol]

    
    # plot
    ## change dt date range to from/to
    dat = dt[date>=from & date <= to]
    
    if (multi_series_all1) {
        p = ggplot(data = dat) + 
            geom_line(aes(x = x, y = close, color = symbol)) + 
            scale_color_discrete(labels = subtitle_str[, paste(symbol, date, round(close,2), sep=", ")]) + 
            # annotate("text", label="@http://shichen.name/pedar/", x=dat[, x[.N %/% 2], by=symbol][,V1[1]], y=-Inf, vjust = -1, color = "gray", alpha = 0.2) + 
            # geom_text(aes(label="@http://shichen.name/pedar", x=dat[, x[.N], by=symbol][,V1[1]], y=Inf), vjust = -0.5, hjust = 1, color = "#F0F0F0") + 
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
            geom_text(x = dat[1, x], y = Inf, aes(label = date), data = dat[,.SD[.N], by=symbol], hjust = 0, vjust = 1, color = "black", na.rm = TRUE, alpha = 0.6, size = rel(3)) +
            geom_text(x = dat[.N, x], y = Inf, aes(label = price_str), data=subtitle_str, hjust = 1, vjust = 1, color = "black", parse = TRUE, na.rm = TRUE, alpha = 0.6, size = rel(3)) +
            # annotate("text", label="@http://shichen.name/pedar/", x=dat[, x[.N %/% 2], by=symbol][,V1[1]], y=-Inf, vjust = -1, color = "gray", alpha = 0.2) + 
            # geom_text(aes(label="@http://shichen.name/pedar", x=dat[, x[.N], by=symbol][,V1[1]], y=Inf), vjust = -0.5, hjust = 1, color = "#F0F0F0") + 
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
            p = do.call(pp_add_ti_overlay, args = list(p = p, dt = dt[date>=from-365 & date <= to], from = from, to = to, rm_weekend = rm_weekend, addti = addti))
        }
        # add linear trend line
        if (!is.null(linear_trend)) 
            p = do.call(pp_add_linear_trend, args = list(p=p, dt = dat, rm_weekend = rm_weekend, linear_trend = linear_trend))
    }
    
    # remove margin
    p = p + theme(plot.margin = unit(rep(0, 4), "cm"))
    # set xaxis
    p = pp_set_xaxis(p, dat, rm_weekend = rm_weekend, rm_xaxis = rm_xaxis)
    # set yaxis
    p = pp_set_yaxis(p, yaxis_log = yaxis_log)
    # set title 
    p = pp_set_title(p, dat, title = title)
    
    ###### oscillator techinal indicators ######
    if (!is.null(addti) & length(addti)>0 & num_syb==1) {
        p = do.call(pp_add_ti_oscillator, args = list(p = p, dt = dt[date>=from-365 & date <= to], from = from, to = to, rm_weekend = rm_weekend, addti = addti))
    }
    
    return(p)
}

# step
pp_step = function(
    dt, from=NULL, to=Sys.Date(), 
    color_up = "#F6736D", color_down = "#18C0C4", title = title, 
    rm_weekend = FALSE, rm_xaxis = FALSE, yaxis_log = FALSE, x = "close|value", 
    linear_trend = NULL, multi_series = list(nrow=NULL,ncol=NULL), 
    addti = list(sma = list(n=50), mm = list(n=25)), ...
) {
    prev_close = symbol = change = change_pct = high = low = prev_x = updn_2day = x1 = y1 = x2 = y2 = V1 = NULL
    
    # copy dt
    dt = copy(dt)
    
    # number symbols/ nrow==1 & ncol==1
    num_syb = dt[, length(unique(symbol))]
    multi_series_all1 = list(...)[['multi_series_all1']]
    
    # x and updn
    dt[, prev_x := shift(x, 1, type="lag"), by = symbol
       ][updn_2day=="down", `:=`(x1 = prev_x, x2 = x, y1 = prev_close, y2 = close)]
    
    
    # subtitle string
    xcol = x
    subtitle_str = dt[, .SD[.N], by = symbol
                    ][, price_str := sprintf("atop(bold('%s')~'%.2f'~bold('Chg')~'%.2f(%.2f%%)')", xcol, close, change, change_pct), by = symbol]
    
    
    # last row
    dt_N = dt[,.SD[.N], by=symbol]
    # add new row
    dtN_update = copy(dt_N)[, `:=`(
        date = Sys.Date(), rowid = rowid+1, prev_x = x
    )][, x := sapply(x, function(x) {
        x = ifelse(rm_weekend, rowid, date)
        return(x)
    })]
    dt = rbindlist(list(dt, dtN_update), fill = TRUE)
    setkeyv(dt, c("symbol", "date"))
    
    # plot
    ## change dt date range to from/to
    dat = dt[date>=from & date <= to]
    
    if (multi_series_all1) {
        p = ggplot(data = dat) + 
            geom_step(aes(x = x, y = close, color = symbol)) + 
            scale_color_discrete(labels = subtitle_str[, paste(symbol, date, round(close,2), sep=", ")]) + 
            # annotate("text", label="@http://shichen.name/pedar/", x=dat[, x[.N %/% 2], by=symbol][,V1[1]], y=-Inf, vjust = -1, color = "gray", alpha = 0.2) + 
            # geom_text(aes(label="@http://shichen.name/pedar", x=dat[, x[.N], by=symbol][,V1[1]], y=Inf), vjust = -0.5, hjust = 1, color = "#F0F0F0") + 
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
            geom_text(x = dat[1, x], y = Inf, aes(label = date), data=dt_N, hjust = 0, vjust = 1, color = "black", na.rm = TRUE, alpha = 0.6, size = rel(3)) +
            geom_text(x = dat[.N, x], y = Inf, aes(label = price_str), data=subtitle_str, hjust = 1, vjust = 1, color = "black", parse = TRUE, na.rm = TRUE, alpha = 0.6, size = rel(3)) +
            # annotate("text", label="@http://shichen.name/pedar/", x=dat[, x[.N %/% 2], by=symbol][,V1[1]], y=-Inf, vjust = -1, color = "gray", alpha = 0.2) + 
            # geom_text(aes(label="@http://shichen.name/pedar", x=dat[, x[.N], by=symbol][,V1[1]], y=Inf), vjust = -0.5, hjust = 1, color = "#F0F0F0") + 
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
            p = do.call(pp_add_ti_overlay, args = list(p = p, dt = dt[date>=from-365 & date <= to], from = from, to = to, rm_weekend = rm_weekend, addti = addti))
        }
        # add linear trend line
        if (!is.null(linear_trend)) 
            p = do.call(pp_add_linear_trend, args = list(p=p, dt = dat, rm_weekend = rm_weekend, linear_trend = linear_trend))
    }
    
    # remove margin
    p = p + theme(plot.margin = unit(rep(0, 4), "cm"))
    # set xaxis
    p = pp_set_xaxis(p, dat, rm_weekend = rm_weekend, rm_xaxis = rm_xaxis)
    # set yaxis
    p = pp_set_yaxis(p, yaxis_log = yaxis_log)
    # set title 
    p = pp_set_title(p, dat, title = title)
    
    ###### oscillator techinal indicators ######
    if (!is.null(addti) & length(addti)>0 & num_syb==1) {
        p = do.call(pp_add_ti_oscillator, args = list(p = p, dt = dt[date>=from-365 & date <= to], from = from, to = to, rm_weekend = rm_weekend, addti = addti))
    } 
    
    return(p)
}

# # set text size
# pp_set_size = function(geom_text_size = 7) {
#     theme_size = (14/5) * geom_text_size
#     return(list(geom_text_size=geom_text_size, theme_size=theme_size))
# }

# set xaxis 
pp_set_xaxis = function(p, dt, rm_weekend = TRUE, rm_xaxis = FALSE, ...) {
    . = yl = ql = ml = wl = y = m = w = d = brks = freq = NULL
    
    xfreqs = c("y", "q", "m", "w")
    
    xticks = dt[
        , .(date, rowid)
    ][, `:=`(
        y = year(date), q=quarter(date), m = month(date), w = isoweek(date), d = mday(date)#, rowid = .I
    )][, (paste0(xfreqs,"l")) := lapply(.SD, function(x) {
        xi = x - shift(x, 1, type="lag")
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
        xlab = paste0(xfreq,"l")
        if (xfreq == "q") {
            xfreq = "m"
        } else if (xfreq == "w") {
            xfreq = "d"
        }
        # print(xfreq)
        
        xbrk_lab = rbind(
            xbrk_lab, 
            xticks[eval(parse(text = xlab)) == 1, .(date, brks = rowid, labs = eval(parse(text = xfreq)), freq=xfreqs[num_xfreqs])])
        xbrk_lab = unique(xbrk_lab, by = "brks", fromLast = FALSE) 
        
        xbrk_nrow = xbrk_lab[, .N] + xticks[eval(parse(text=xlab)) == 1, .N]
        num_xfreqs = num_xfreqs+1
    }
    xbrk_lab = xbrk_lab[order(brks)]
    main_brks = xbrk_lab
    
    if (xfreq == "y") {
        brk_num = c(2, 5, 10, 25, 50, 100)
        i = 1
        while (main_brks[,.N] > 15) {
            minor_brks = main_brks
            main_brks  = main_brks[, rowid := .I][rowid %% brk_num[i] == 0]
            i = i+1
        }
    }
    if (!exists("minor_brks")) {
        # minor_breaks
        xfreq = xfreqs[num_xfreqs]
        xlab = paste0(xfreq,"l")
        if (xfreq == "q") {
            xfreq = "m"
        } else if (xfreq == "w") {
            xfreq = "d"
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
    
    p = p + theme(axis.text.x = element_text(face = main_brks[, ifelse(freq=="y", "bold", ifelse(freq=="q", "bold.italic", "plain"))]))
    
    if (rm_xaxis) p = p + theme(axis.ticks.x = element_blank(), axis.text.x = element_blank())
    return(p)
}
# set yaxis
#' @import scales
pp_set_yaxis = function(p, yaxis_log = FALSE) {
    if (yaxis_log) {
        p = p + 
            scale_y_continuous(trans='log10', sec.axis = dup_axis()) +
            # scale_y_log10(
            #     breaks = trans_breaks("log10", function(x) 10^x),
            #     labels = trans_format("log10", math_format(10^.x)),
            #     minor_breaks = log10(5) + -2:5
            # ) +
            annotation_logticks(sides = "rl") 
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
pp_add_linear_trend = function(p, dt, rm_weekend = TRUE, linear_trend = c(0, 1, -1), ...) {
    add1_linear_trend = function(p, dt, num_sd, rm_weekend) {
        x = symbol = NULL
        
        # add x column
        if (!("x" %in% names(dt))) {
            dt[, x := date]
            if (rm_weekend) dt[, x := seq_len(.N), by=symbol]
        }
        
        # line color
        line_color = "blue"
        if (num_sd == 0) line_color = "red"
        
        # plot
        p = p + 
            geom_smooth(
                data = dt, aes_string(x="x", y="close"),
                method = lm, formula = y+num_sd*sd(y, na.rm = TRUE)~x, 
                na.rm = TRUE, se = FALSE, color = line_color, size = 0.2)
        
        return(p)
    }
    
    for (i in linear_trend) p = add1_linear_trend(p, dt, i, rm_weekend)
    
    return(p)
}
# set title
pp_set_title = function(p, dt, title = NULL) {
    symbol = NULL
    
    if (is.null(title)) title = ""
    # if ("name" %in% names(dt)) title = paste(dt[1, name], title)
    if (dt[, length(unique(symbol))==1]) {
        # title string
        title1 = paste0(title, dt[1,title1]) 
        title2 = dt[1,title2]
        # adding title and annotation
        p = p + labs(title = title1, subtitle = title2)
    } else {
        p = p + labs(title = title)
    }
    
    p = p + theme(plot.title    = element_text(margin = margin(b=0)),
                  plot.subtitle = element_text(margin = margin(b=0)),
                  text = element_text(family = switch(Sys.info()[['sysname']],
                                                      Windows= 'SimHei',
                                                      Darwin = 'STHeiti', 
                                                      NA) ))
    return(p)
}



# add technical indicators
# dt is original dataset without change from/to
# overlay: mm, sma, ema, smma, bb, sar
pp_add_ti_overlay = function(
    p, dt, from=NULL, to=Sys.Date(), 
    addti = list(SMA = list(n=20), SMA = list(n=50)), 
    rm_weekend = NULL) {
    # x = symbol = ti_str = NULL
    
    # overlay technical indicators
    names(addti) <- tolower(names(addti))
    ti_not_topbottom = names(addti)[sapply(addti, function(x) !any(x[["position"]] %in% c("top","bottom")))]
    ti_overlay = intersect( ti_not_topbottom, tolower(ti_overlays_indicators()[['overlays']]) )
    addti = addti[names(addti) %in% ti_overlay]
    # return p if no overlay ti
    if (length(addti) == 0 || is.null(addti)) return(p)
    
    # colorblind palette
    cb_palette = c("#E41A1C", "#377EB8", "#4DAF4A", "#984EA3", "#FF7F00", "#FFFF33", "#A65628", "#F781BF", "#999999") # RColorBrewer::brewer.pal(9,"Set1") # display.brewer.all(type = "qual")
    
    # add x column
    if (!("x" %in% names(dt))) {
        dt[, x := date]
        if (rm_weekend) dt[, x := seq_len(.N), by=symbol]
    }
    
    for (i in seq_along(addti)) {
        # add ti columns to dt
        args_list = c(list(dt=dt, col_kp=TRUE, col_formula=TRUE), 
                      lapply(addti[i], function(x) x[setdiff(names(x), c('color', 'position', 'hl'))]) )
        dtti = do.call(pd_addti, args = args_list)
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
        if (!is.null(addti[[i]][["color"]])) color = addti[[i]][["color"]]
            
        # line type
        geom_type = "line"
        if (names(addti[i]) %in% c('runmax', 'runmin')) geom_type = "step"
        
        ti_names = names(dtti)[grep(sprintf("^%s",names(addti[i])), names(dtti))]
        for (t in ti_names) {
            p = p + 
                stat_identity(
                data = dat, 
                aes_string(x="x",y=t), geom = geom_type, color = color) # linetype = "longdash", 
        }
        
        # addti formula value
        dat_n = dat[,.SD[.N]
                  ][, (ti_names) := lapply(.SD, round, digits = 2), .SDcols = ti_names 
                  ][,c("formula_str", ti_names),with=FALSE]
        dat_n[, ti_str := paste0(dat_n, collapse = " ")]
        # add ti_str
        p = p + 
            geom_text(x = dat[1, x], y = Inf, aes(label = ti_str), data = dat_n, hjust = 0, vjust = i*1.25+1, color = color, na.rm = TRUE, alpha = 0.6, size = rel(3))
    }
    return(p)
}


# oscillator: macd, ppo, roc, rsi, cci
pp_add_ti_oscillator = function(
    p, dt, from=NULL, to=Sys.Date(),
    addti = list(macd = list(n=50, position = "top"), roc = list(n=25, position = "bottom"), ppo = list(n=50, position = "top"), rsi = list(n=25)), 
    rm_weekend = NULL) {
    
    # oscillator technical indicators
    # c("macd", "ppo", "roc", "rsi", "cci")
    names(addti) <- tolower(names(addti))
    ti_topbottom = names(addti)[sapply(addti, function(x) any(x[["position"]] %in% c("top","bottom")))]
    ti_not_overlay = names(addti)[sapply(addti, function(x) !any(x[["position"]] %in% c("overlay")))]
    ti_oscillator = c(intersect(ti_topbottom, tolower(ti_overlays_indicators()[['overlays']])),
                      intersect(ti_not_overlay, tolower(ti_overlays_indicators()[['indicators']])) )
    if ('bbands' %in% names(addti)) ti_oscillator = c(ti_oscillator, 'bbands')
    addti = addti[names(addti) %in% ti_oscillator]
    # return p if no oscillator ti
    if (length(addti) == 0 | is.null(addti)) return(p)
    
    # colorblind palette
    cb_palette = c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7") # c("black", "red", "blue") # 

    # add x column
    if (!("rowid" %in% names(dt))) dt[, rowid := seq_len(.N), by=symbol]
    if (!("x" %in% names(dt))) {
        dt[, x := date]
        if (rm_weekend) dt[, x := rowid]
    }
    
    # check position
    addti = lapply(addti, function(x) {
        if (is.null(x[["position"]])) x[["position"]] = "bottom"
        return(x)
    })
    # the plot of last bottom ti will add xaxis
    last_bottom = which.max(sapply(addti, function(x) {
        any(x[["position"]] == "bottom")
    }))

    # plist
    top_plist = bottom_plist = NULL
    for (i in seq_along(addti)) {
        # dataset with technical indicators
        args_list = c(list(dt=dt, col_kp=TRUE, col_formula=TRUE), 
                      lapply(addti[i], function(x) x[setdiff(names(x), c('color', 'position', 'hl'))]) )
        dtti = do.call(pd_addti, args=args_list)
        # # bind list of dataframes
        if (inherits(dtti, 'list')) dtti = rbindlist(dtti, fill = TRUE)
        if (names(addti[i]) == 'bbands') dtti = dtti[,(c('bbands_dn', 'bbands_mavg', 'bbands_up')) := NULL]
        if (names(addti[i]) == 'adx')    dtti = dtti[,(c('adx_dx')) := NULL]
        if (names(addti[i]) == 'atr')    dtti = dtti[,(c('atr_tr', 'atr_truehigh', 'atr_truelow')) := NULL]
        if (names(addti[i]) == 'aroon')  dtti = dtti[,(c('aroon_oscillator')) := NULL]
        dat = dtti[date>=from & date <= to]

        # names of technical indicators
        ti_names = names(dat)[grepl(paste0("^",names(addti[i])), names(dat))]
        pi = ggplot(data = dat, aes_string(x = "x"))
        for (t in ti_names) {
            iti_names = which(t == ti_names)
            # color
            color = cb_palette[ifelse(iti_names %% length(cb_palette) == 0, length(cb_palette), iti_names %% length(cb_palette))]
            
            pi = pi + geom_line(aes_string(y=t), stat = "identity", color = color)
        }
        
        # addti formula value
        dat_n = dat[,.SD[.N]
                    ][, (ti_names) := lapply(.SD, round, digits = 2), .SDcols = ti_names
                    ][,c("formula_str", ti_names),with=FALSE]
        dat_n[, ti_str := paste0(dat_n, collapse = " ")]
        
        # add text, remove labs
        pi = pi + 
            geom_text(x = dat[1, x], y = Inf, aes(label = ti_str), data = dat_n, hjust = 0, vjust = 1, color = "black", na.rm = TRUE, alpha = 0.6, size = rel(3)) + 
            labs(x=NULL, y=NULL)  + 
            theme_bw() + 
            theme(plot.margin = unit(rep(0, 4), "cm"),
                  text = element_text(family = switch(Sys.info()[['sysname']],
                                                      Windows= 'SimHei',
                                                      Darwin = 'STHeiti', 
                                                      NA) ))
        
        # hlines
        hlines = ti_idicators_hline()
        if (names(addti[i]) %in% tolower(names(hlines)) || 'hl' %in% names(addti[[i]])) {
            if ('hl' %in% names(addti[[i]])) {
                yl_ticks = addti[[i]][['hl']]
            } else yl_ticks = hlines[[names(addti[i])]]
            
            for (yi in seq_along(yl_ticks)) {
                clr = "#BEAED4"
                if (yi == 1 & (length(yl_ticks) %% 2) == 1) clr = "#7FC97F" 
                # brewer.pal(2,"Accent")
                
                pi = pi + geom_hline(yintercept = yl_ticks[yi], color = clr, linetype = "longdash")
            }
        }
        
        
        # set xaxis
        rm_xaxis = TRUE
        if (i == last_bottom) rm_xaxis = FALSE
        pi = pp_set_xaxis(pi, dat, rm_weekend = rm_weekend, rm_xaxis = rm_xaxis)
        # set yaxis
        pi = pp_set_yaxis(pi)
        
        
        
        # plist
        if (addti[[i]][["position"]] == "top") {
            top_plist[[paste0("p",i)]] = pi
        } else if (addti[[i]][["position"]] == "bottom") {
            bottom_plist[[paste0("p",i)]] = pi
        }
    }
    
    # # arrange plot list
    # if (!is.null(top_plist)) top_plist2 = grid.arrange(grobs = top_plist, ncol = 1)
    # if (!is.null(bottom_plist)) bottom_plist = grid.arrange(grobs = bottom_plist, ncol = 1)

    # return p
    heights = c(rep(100, length(top_plist)), 320)
    if (length(bottom_plist)>0) heights = c(heights, rep(100, length(bottom_plist)-1), 115)
    
    p = grid.arrange(grobs = c(top_plist, list(p0=p), bottom_plist), ncol = 1, heights = heights)
    return(p)
}


#' create a chart for timeseries data
#' 
#' `pd_plot` creates a charts for a time series dataset. 
#' 
#' @param dt a time series dataset
#' @param chart_type chart type, including line, step, candle.
#' @param freq the frequency that the input data will converted to. It supports weekly, monthly, quarterly and yearly.
#' @param date_range date range. Available value including '1m'-'11m', 'ytd', 'max' and '1y'-. Default is max.
#' @param from the start date. Default is max date in input data.
#' @param to the end date. Default is min date in data.
#' @param addti list of technical indicators, overlay indicators include mm, sma, ema, smma, bb, sar, and oscillators indicators such as macd, roc, ppo, rsi, cci.
#' @param x the variable display on chart
#' @param yaxis_log logical, default is FALSE.
#' @param color_up the color indicates price going up
#' @param color_down the color indicates price going down
#' @param linear_trend a vector of numeric. Default is NULL, which donot show linear trend line. If it is not 0, show linear_trend*sd from linear trend line. 
#' @param perf logical, whether to show the performance of input data. Default is FALSE. 
#' @param multi_series logical, whether to show multiple series. Default is FALSE. 
#' @param rm_weekend weather to remove weekend in xaxis. The default is TRUE for candle and bar chart, and FALSE for line and step chart.
#' @param title chart title. If it is not specified, the symbol of dataset will be used as chart title.
#' @param ... ignored
#' 
#' @examples 
#' \dontrun{
#' # single symbol
#' ssec = md_stock("^000001", source="163")
#' 
#' # chart type
#' pd_plot(ssec) # line chart (default)
#' # pd_plot(ssec, chart_type = "candle") # candlestick
#' # pd_plot(ssec, chart_type = "bar") # bar chart
#' # pd_plot(ssec, chart_type = "step") # step line
#' 
#' # add technical indicators
#' pd_plot(ssec, chart_type = "candle", 
#'   addti = list(sma = list(n = 50), macd = list()))
#' 
#' 
#' # multiple symbols
#' dat1 = md_stock(c('^000001', '^399001'), date_range = 'max', source='163')
#' pd_plot(dat1, linear_trend = c(-0.8, 0, 0.8), multi_series = list(nrow=1, scales = 'free_y'))
#' 
#' 
#' dat2 = md_stock(c('^000016', '^000300'), date_range = 'max', source='163')
#' pd_plot(dat2, linear_trend = c(-0.8, 0, 0.8), multi_series = list(nrow=1, scales = 'free_y'))
#' 
#' }
#' 
#' @import ggplot2 gridExtra
#' @importFrom stats lm
#' @export
pd_plot = function(
    dt, chart_type = "line", freq = NULL, 
    date_range="max", from = NULL, to = Sys.Date(), 
    addti = list(), linear_trend = NULL, 
    perf = FALSE, 
    x = "close|value", yaxis_log = FALSE, 
    color_up = "#F6736D", color_down = "#18C0C4", 
    multi_series = list(nrow=NULL, ncol=NULL), 
    rm_weekend = NULL, ...) {
    
    
    ## title
    title = list(...)[['title']]
    
    ## change freq of input data
    if (!is.null(freq) || match.arg(freq, "daily")!="daily") {
        dt = pd_to_freq(dt, freq)
    }
    
    # change into performance
    if (perf) {
        dt = pd_perf(dt, x=x, date_range=date_range, from=from, to=to)
        title = paste(title, "performance")
    }
    
    ## bind list of dataframes
    if (inherits(dt, 'list')) dt = rbindlist(dt, fill = TRUE)
    setkeyv(dt, c('symbol','date'))
    
    ## date range # from
    date_range = check_date_range(date_range, default = "max")
    from = get_from_daterange(date_range, from, to, min_date = dt[,min(date)])
    
    ## x columns
    x = intersect(names(dt), unlist(strsplit(x,'\\|')))
    if (x != "close") dt[["close"]] = dt[[x]]
    
    # add columns of prev_close, change, change_pct, x, title1, title2, updn_2day, 
    dt = dt[unique(dt[,.(date)])[order(date)][,rowid := .I][], on='date']
    setkeyv(dt, c('symbol', 'date'))
    dt = dt[, prev_close := shift(close, 1, type = "lag"), by = symbol
     ][, `:=`(
         change = close - prev_close,
         change_pct = (1-prev_close/close)*100,
         x = date
     )][, `:=`(
         title1 = paste(symbol, name[.N]),
         title2 = sprintf("[%s/%s]", date[1], date[.N]),
         updn_2day = ifelse(close > prev_close, "up", "down")
     ), by = symbol]
    if (all(c("close", "open") %in% names(dt))) dt[is.na(updn_2day), updn_2day := ifelse(close > open, "up", "down")]
    
    ## multiple series options
    multi_series_allnull = all(sapply(multi_series, is.null))
    multi_series_all1 = all(sapply(list("nrow", "ncol"), function(x) any(multi_series[[x]] == 1)))
    if (is.null(multi_series[["scales"]])) multi_series[["scales"]] = "fixed"
    
    ## chart type
    chart_type = check_arg(chart_type, c("candle", "bar", "line", "step"), default = "line")
    if (chart_type %in% c("candle", "bar")) {
        if (multi_series_all1 || !all(c("open", "high", "low", "close") %in% names(dt))) {
            chart_type = "line"
            warning("The `chart_type` is set to the default 'line'. It should be line or step, if you want to display multiple series on one chart or donot have open/high/low/close columns.")
        }
    }
    ## rm_weekend
    if (!is.logical(rm_weekend)) {
        if (chart_type %in% c('bar', 'candle'))       { rm_weekend = TRUE
        } else if (chart_type %in% c('line', 'step')) { rm_weekend = FALSE}
    }
    # if (!multi_series_all1 & num_syb >1) rm_weekend = FALSE
    if (rm_weekend) dt[, x := rowid]
    
    # plot list
    plist = list()
    if (!multi_series_allnull) {
        ## multiple series
        if (dt[,length(unique(symbol)) > 1]) title = paste0("multiple series", title) 
        plist[["multi_series"]] = 
            do.call(paste0("pp_",chart_type), args = list(
                dt = dt, 
                date_range = date_range, 
                from = from, to = to, 
                addti = addti,
                x = x, yaxis_log = yaxis_log, 
                color_up = color_up, color_down = color_down, 
                rm_weekend = rm_weekend, title = title, 
                multi_series = multi_series,
                linear_trend = linear_trend, 
                multi_series_all1=multi_series_all1
            ))
    } else {
        ## single series
        sybs = dt[, unique(symbol)]
        for (s in sybs) {
            dt_s = dt[symbol == s]
            setkeyv(dt_s, "date")
            
            plist[[s]] = 
                do.call(paste0("pp_",chart_type), args = list(
                    dt = dt_s, 
                    date_range = date_range, 
                    from = from, to = to, 
                    addti = addti,
                    x = x, yaxis_log = yaxis_log, 
                    color_up = color_up, color_down = color_down, 
                    rm_weekend = rm_weekend, title = title, 
                    multi_series = multi_series,
                    linear_trend = linear_trend,
                    multi_series_all1=multi_series_all1
                ))
        }
    }
    
    return(plist)
}


