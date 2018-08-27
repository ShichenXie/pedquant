#' @importFrom scales trans_breaks trans_format math_format
ped1_plot = function(dt, y="close|value", date_range="max", from=NULL, to=Sys.Date(), title=NULL, type="line",  yaxis_log=FALSE, linear_trend=0) {
    .x = symbol = ggplot = geom_line = aes = scale_y_continuous = scale_x_date = labs = theme_bw = NULL
    
    # set dt as datatable
    setDT(dt, key="date")
    
    # y
    y = names(dt)[grepl(y, names(dt))][1]
    
    # from to 
    if (is.null(to)) to = dt[, max(date)]
    to = check_fromto(to, type = tolower(dt[, class(date)]), shift=1) 
    from = get_from_daterange(dt, date_range, to)
    # set range for data
    dat = dt[date>=from & date<=to]
    
    # remove 0 rows in variable
    dt = dt[eval(parse(text=y)) != 0]
    if (type == "step") {
        dt = rbindlist(list(dt, data.table(date=Sys.Date())), fill = TRUE)
        dt[[y]] = fillna(dt[[y]])
    }
    
    # plot title 
    tit = y
    if ("symbol" %in% names(dt)) tit = dt[1, symbol]
    title_str = sprintf("[%s/%s] %s", dt[,date[1]], dt[,date[.N]], paste(tit, title) )
    
    # final price
    final_y = dt[.N][[y]]
    
    
    # plot
    p = ggplot(data = dat) + 
        stat_identity(aes(x = date, y = eval(parse(text=y))), geom = type) + 
        geom_hline(yintercept=final_y, color="gray", size=0.1) + 
        annotate("text", x=dt[.N, date], y=final_y, label=final_y, color="gray", hjust=0, size=3) + 
        # scale_x_date(date_breaks="2 year", date_minor_breaks = "1 year", date_labels = "%y")+
            #breaks = seq(as.Date("1990-01-01"), as.Date("2020-01-01"), by="5 year"), labels = seq(1990,2020,5)) +
        labs(title=title_str, x=NULL, y=NULL) + 
        theme_bw()
    
    if (yaxis_log) {
        p = p + scale_y_continuous(trans='log10', position = "right") +
        #     scale_y_log10(
        #     breaks = trans_breaks("log10", function(x) 10^x),
        #     labels = trans_format("log10", math_format(10^.x)),
        #     position = "right"
        # ) + 
            annotation_logticks(sides = "r") 
            
    } else {
        p = p + scale_y_continuous(position = "right")
    }
    
    if (linear_trend > 0) {
        p = p + 
            geom_smooth(data=dt, aes(x=date, y=eval(parse(text=y))), method=lm, formula=y~x, na.rm=TRUE, se=FALSE, color="red", size=0.2) + 
            geom_smooth(data=dt, aes(x=date, y=eval(parse(text=y))), method=lm, formula=y-linear_trend*sd(y)~x, na.rm=TRUE, se=FALSE, color="blue", size=0.2) + 
            geom_smooth(data=dt, aes(x=date, y=eval(parse(text=y))), method=lm, formula=y+linear_trend*sd(y)~x, na.rm=TRUE, se=FALSE, color="blue", size=0.2)
    }
        
    # p <- plot_ly(data=dt, x = ~date, y = ~eval(parse(text=y)), mode = 'lines')
    
    return(p)
}

pedn_plot = function(dt, y="close|value", date_range="max", from=NULL, to=Sys.Date(), title=NULL, type="line",  yaxis_log=FALSE) {
    . = symbol = .x = NULL
    
    # bind list of dataframe
    if (is.list(dt) & !is.data.frame(dt)) {
        dat = rbindlist(dt, fill = TRUE, idcol = "id")
    }
    setkeyv(dat, c("id","date"))
    
    # y
    y = names(dat)[grepl(y, names(dat))][1]
    
    # from to 
    if (is.null(to)) to = dt[, max(date)]
    to = check_fromto(to, type = tolower(dt[, class(date)]), shift=1) 
    from = get_from_daterange(dat, date_range, to)
    # set range for data
    dat = dat[date>=from & date<=to]
    
    # plot title 
    if (is.null(title)) title = ""
    title_str = sprintf("[%s/%s] %s", dat[,date[1]], dat[,date[.N]], title )
    
    # final x y vlaues
    x_final = dat[, .(date = date[.N]), by=symbol]
    y_final = dat[x_final, on=c("symbol", "date")]

    p = ggplot(data = dat) + 
        stat_identity(aes(x = date, y = eval(parse(text=y)), color=symbol), geom = type) + 
        geom_hline(yintercept=y_final[[y]], color="gray", size=0.1) +
        annotate("text", x=x_final[["date"]], y=y_final[[y]], label=y_final[[y]], color="gray", hjust=0, size=3) +
        labs(title=title_str, x=NULL, y=NULL) + 
        theme_bw() +
        theme(legend.position=c(0.1,0.9), legend.background=element_blank(), legend.key=element_blank()) 
    
    
    if (yaxis_log) {
        p = p + scale_y_log10(
            breaks = trans_breaks("log10", function(x) 10^x),
            labels = trans_format("log10", math_format(10^.x)),
            position = "right"
        ) + annotation_logticks(sides = "l") 
    } else {
        p = p + scale_y_continuous(position = "right")
    }
    
    return(p)
}
#' create a chart for timeseries data
#' 
#' `ped_plot` creates a charts for a time series dataset. 
#' 
#' @param dt a time series dataset
#' @param y the variable display on chart
#' @param date_range date range. Available value including YTD, Max and integers from 1 to 1000. Default is Max.
#' @param from the start date. Default is max date in input data.
#' @param to the end date. Default is min date in data.
#' @param title chart title. If it is not specified, the symbol of dataset will be used as chart title.
#' @param type chart type, including line, step, candle.
#' @param yaxis_log logical, default is FALSE.
#' @param perf logical, whether to show the performance of input data. Default is FALSE. 
#' @param multi_series logical, whether to show multiple series. Default is FALSE. 
#' @param linear_trend non-negative numeric. Default is 0, which donot show linear trend line. If it larger than 0, show linear_trend*sd from linear trend line. 
#' 
#' @examples 
#' \dontrun{
#' ssec = getmd("^000001", source="163")
#' 
#' p = ped_plot(ssec, title="SSEC")
#' print(p)
#' }
#' 
#' @import ggplot2
#' @export
ped_plot = function(dt, y="close|value", date_range="max", from=NULL, to=Sys.Date(), title=NULL, type="line", yaxis_log=FALSE, perf=FALSE, multi_series=FALSE, linear_trend=FALSE) {
    symbol = NULL
    
    # check arguments
    ## date range
    date_range = check_arg(tolower(date_range), c(paste0(1:11,"m"), "ytd", "max", paste0(1:500,"y")), default = "max")
    ## chart type
    type = check_arg(type, c("line", "step")) # candle
    
    # change data to performance
    if (perf) {
        dt = ped_perf(dt, y=y, date_range=date_range, from=from, to=to)
        title = paste(title,"perf")
    }
    
    
    # plot graphic
    ## multiple series
    if (multi_series) return(do.call(pedn_plot, args = list(dt=dt, y=y, date_range=date_range, from=from, to=to, title=title, type=type, yaxis_log=yaxis_log)))
    
    ## single series
    plist = NULL
    dt_names = names(dt)
    if (is.list(dt) & !is.data.frame(dt)) {
        dt = lapply(dt, setDT)
        for (i in dt_names) {
            if (is.null(title)) title = i
            plist[[i]] = do.call(ped1_plot, args = list(dt=dt[[i]], y=y, date_range=date_range, from=from, to=to, title=title, type=type, yaxis_log=yaxis_log, linear_trend=linear_trend))
        }
        
    } else if (is.data.frame(dt)) {
        setDT(dt)
        i = 1
        if ("symbol" %in% dt_names) i = dt[1, symbol]
        
        plist[[i]] = do.call(ped1_plot, args = list(dt=dt, y=y, date_range=date_range, from=from, to=to, title=title, type=type, yaxis_log=yaxis_log, linear_trend=linear_trend))
        
    }
    
    return(plist)
}


#' @importFrom stats lm predict
ped_plot_rp = function(dt, type = "line", title=NULL) {
    log_close = ggplot = geom_line = aes = lm0 = lm_p2 = lm_p1 = lm_m1 = scale_y_continuous = scale_x_date = labs = theme_bw = NULL
    
    
    dt = dt[, log_close := log10(close)
            ][, `:=`(
                lm0 = predict(lm(log_close~date)),
                lm_p2 = predict(lm((log_close+sd(log_close)*2)~date)),
                lm_p1 = predict(lm((log_close+sd(log_close))~date)),
                lm_m1 = predict(lm((log_close-sd(log_close))~date))
                # log_close_p2sd = log_close + sd(log_close)*2,
                # log_close_p1sd = log_close + sd(log_close),
                # log_close_m1sd = log_close - sd(log_close)
            )]
    
    title_str = sprintf("[%s/%s]", dt[,date[1]], dt[,date[.N]])
    if (!is.null(title)) title_str = paste(title_str,title)
    ggplot(data = dt) + 
        geom_line(aes(x=date, y=log_close)) +
        geom_line(aes(date, lm0), color="red") +
        geom_line(aes(date, lm_p2), color="grey") +
        geom_line(aes(date, lm_p1), color="grey") +
        geom_line(aes(date, lm_m1), color="grey") + 
        scale_y_continuous(position = "right") +
        scale_x_date(breaks = seq(as.Date("1990-01-01"), as.Date("2020-01-01"), by="5 year"), labels = seq(1990,2020,5)) +
        labs(title=title_str, x=NULL) +
        theme_bw()
}
# ssec_real = ped_rp1(getmd("^000001", source="163", from="1900-01-01"))
# ped_plot_rp(ssec_real, title="ssec")
# szsec_real = ped_rp1(getmd("^399001", source="163", from="1900-01-01"))
# ped_plot_rp(szsec_real, title="szsec")
# ped_plot(szsec_real, title="szsec")

