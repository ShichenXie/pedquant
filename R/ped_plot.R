
ped1_plot = function(dt, y="close|value", type="line", title=NULL) {
    ggplot = geom_line = aes = scale_y_continuous = scale_x_date = labs = theme_bw = NULL
    
    # y
    y = names(dt)[grepl(y, names(dt))][1]
    # remove 0 rows in variable
    dt = dt[eval(parse(text=y)) != 0]
    if (type == "step") {
        dt = rbindlist(list(dt, data.table(date=Sys.Date())), fill = TRUE)
        dt[[y]] = fillna(dt[[y]])
    }
    # date range
    if (is.null(title)) {
        if ("symbol" %in% names(dt)) {
            title = dt[1,symbol]
        } else {
            title = y
        }
    }
    title_str = sprintf("[%s/%s] %s", dt[,date[1]], dt[,date[.N]], title )
    
    
    # plot
    p = ggplot(data = dt) + 
        stat_identity(aes(x = date, y = eval(parse(text=y))), geom = type) +
        scale_y_continuous(position = "right") +
        # scale_x_date(date_breaks="2 year", date_minor_breaks = "1 year", date_labels = "%y")+
            #breaks = seq(as.Date("1990-01-01"), as.Date("2020-01-01"), by="5 year"), labels = seq(1990,2020,5)) +
        labs(title=title_str, x=NULL, y=NULL) +
        theme_bw()
    
    # p <- plot_ly(data=dt, x = ~date, y = ~eval(parse(text=y)), mode = 'lines')
    
    return(p)
}

#' create a chart for timeseries data
#' 
#' `ped_plot` creates a charts for a time series dataset. 
#' 
#' @param dt a time series dataset
#' @param y the variable display on chart
#' @param type chart style, including line, candle, bar
#' @param title chart title
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
ped_plot = function(dt, y="close|value", type="line", title=NULL) {
    type = check_arg(type, c("line", "step"))
    
    if (is.list(dt) & !is.data.frame(dt)) {
        dt_names = names(dt)
        
        plist = NULL
        for (i in dt_names) {
            if (is.null(title)) title = i
            plist[[i]] = ped1_plot(dt[[i]], y, type, title)
        }
    } else if (is.data.frame(dt)) {
        plist = ped1_plot(dt, y, type, title)
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

