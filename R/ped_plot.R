
ped1_plot = function(df, y="close", type="line", title=NULL) {
    ggplot = geom_line = aes = scale_y_continuous = scale_x_date = labs = theme_bw = NULL
    
    min_date = df[,date[1]]
    max_date = df[,date[.N]] 
    title_str = sprintf("[%s/%s]", min_date, max_date)
    
    # remove 0 rows in variable
    df = df[eval(parse(text=y)) != 0]
    # title string
    if (!is.null(title)) title_str = paste(title_str,title)
    
    # plot
    p = ggplot(data = df) + 
        geom_line(aes(x=date, y=eval(parse(text=y)))) +
        scale_y_continuous(position = "right") +
        scale_x_date(date_breaks="2 year", date_minor_breaks = "1 year", date_labels = "%y")+
            #breaks = seq(as.Date("1990-01-01"), as.Date("2020-01-01"), by="5 year"), labels = seq(1990,2020,5)) +
        labs(title=title_str, x=NULL, y=NULL) +
        theme_bw()
    
    # p <- plot_ly(data=df, x = ~date, y = ~eval(parse(text=y)), mode = 'lines')
    
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
#' ssec = getmd_stock("^000001")
#' 
#' p = ped_plot(ssec, title="SSEC")
#' print(p)
#' }
#' 
#' @import ggplot2
#' @export
ped_plot = function(dt, y="close", type="line", title=NULL) {
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
# ssec_real = ped_rp1(getmd_stock("^000001", print_step = 0))
# ped_plot(ssec_real, title="ssec")
# szsec_real = ped_rp1(getmd_stock("^399001", print_step = 0))
# ped_plot(szsec_real, title="szsec")

# create a new Techinal Analysis Indicator for a ped
ped_addTA = function(dt, ta) {
    
}



