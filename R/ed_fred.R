# # fred
# # http://fred.stlouisfed.org
# # API: https://research.stlouisfed.org/docs/api/fred/
# 
# tags:
#     freq
#     gen
#     geo
#     geot
#     rls
#     src
#     seas
# 


#' @import data.table
#' @importFrom jsonlite fromJSON
ed_fred1 = function(symbol1, from="1776-07-04", to="9999-12-31", na_rm=FALSE) {
    group_id = name = . = title = value = unit = NULL
    
    key = api_key("fred")
    base_url = "https://api.stlouisfed.org/fred/%sapi_key=%s&file_type=json"

    # series of symbol1
    query_series = function(symbol1) {
        setDT(fromJSON(sprintf(base_url, sprintf("series?series_id=%s&", symbol1), key))[["seriess"]])
    }
    series = query_series(symbol1)
    # tags of symbol1
    query_series_tags = function(symbol1) {
        setDT(fromJSON(sprintf(base_url, sprintf("series/tags?series_id=%s&", unlist(symbol1)), key))[["tags"]])[]
    }
    for (i in 1:2) {
        tags = try(query_series_tags(symbol1), silent = TRUE)
        # print(i)
        if (!inherits(tags, 'try-error')) {
            # print(tags[group_id=='geo', paste0(name, collapse = ',')])
            break
        }
    }
    # geo
    if (inherits(tags, 'try-error')) {
        geo = NA
    } else {
        geo_df = tags[group_id=='geo', ]
        if (geo_df[, .N==2]) geo_df = geo_df[!(name %in% c('usa','world')), ]
        geo = geo_df[, paste0(name, collapse = ',')]
    }
    
    # observation data of symbol1
    if (nrow(series) == 0) series[, title := NA][, units := NA]
    
    observ = setDT(
        fromJSON(sprintf(base_url, sprintf("series/observations?series_id=%s&observation_start=%s&observation_end=%s&", symbol1, from, to), key))[["observations"]]
    )[,`:=`(name=series[1,title], geo=geo, unit=series[1,units]
    )][,.(symbol=symbol1, name, date=as.Date(date), value=suppressWarnings(as.numeric(value)), geo, unit)]
    
    if (na_rm) observ = observ[!is.na(value)]
    setkeyv(observ, "date")
    return(observ)
}
# dt = ed_fred1("GNPCA")

#' query FRED economic data
#' 
#' ed_fred provides an interface to access the economic data provided by FRED (\url{https://fred.stlouisfed.org})
#' 
#' @param symbol symbols of FRED economic indicators. It is available via function \code{ed_fred_symbol} or its website. Default is NULL, which calls \code{ed_fred_symbol} in the back. 
#' @param date_range date range. Available value includes '1m'-'11m', 'ytd', 'max' and '1y'-'ny'. Default is '10y'.
#' @param from the start date. Default is NULL. If it is NULL, then calculate using date_range and end date.
#' @param to the end date. Default is the current date.
#' @param na_rm logical, whether to remove missing values. Default is FALSE 
#' @param print_step a non-negative integer, which will print symbol name by each print_step iteration. Default is 1L. 
#' 
#' @return a list of dataframes with columns of symbol, name, date, value, geo, unit. The geo column might be NA according to local internet connection.
#' 
#' @examples 
#' \donttest{
#' dat = ed_fred(c("A191RL1A225NBEA", "GDPCA"))
#' }
#' 
#' @export
ed_fred = function(symbol=NULL, date_range='10y', from=NULL, to=Sys.Date(), na_rm=FALSE, print_step=1L) {
    check_internet('www.stlouisfed.org')
    # 
    if (is.null(symbol)) symbol = ed_fred_symbol()[,symbol]
    # from/to # "1776-07-04"/"9999-12-31"
    to = check_to(to)
    from = check_from(date_range, from, to, default_from = "1776-07-04", default_date_range = '10y')
    
    # data list
    dat_list = load_dat_loop(symbol, ed_fred1, args = list(from = from, to = to, na_rm = na_rm), print_step = print_step)
    return(dat_list)
}




#' @importFrom jsonlite fromJSON
#' @import data.table
ed_fred_symbol_keywords = function(keywords, ...) {
    group_id = . = name = popularity = series_count = frequency = id = title = observation_start = observation_end = seasonal_adjustment = last_updated = NULL
    
    key = api_key("fred")
    base_url = "https://api.stlouisfed.org/fred/%sapi_key=%s&file_type=json"
    keywords = gsub("[^a-zA-Z0-9]+", "%20", keywords)
    

    # filter via frequency
    query_search_tags = function(keywords, tag_group_id) {
        setDT(fromJSON(sprintf(base_url, sprintf(
            "series/search/tags?series_search_text=%s&tag_group_id=%s&", keywords, tag_group_id), key))[["tags"]])
    }
    
    # frequency
    freq_search = query_search_tags(keywords, tag_group_id='freq')
    freq = list(...)[['freq']]
    freq_list = freq_search[group_id == 'freq', name]
    if (length(freq_list)>1) {
        freq = check_arg(freq, choices = freq_list, arg_name = 'freq')
    } else if (length(freq_list)==1) {
        freq = freq_list
    } else {
        return(NULL)
    }

    # symbols
    sybs_search = setDT(fromJSON(sprintf(base_url, sprintf("series/search?search_text=%s&tag_names=%s&", keywords, freq), key))[["seriess"]])[
        , .(symbol=id, name=title, from=observation_start, to=observation_end, frequency, units, seasonal_adjustment, last_updated, popularity)][order(-popularity)]
    
    return(sybs_search)
}

ed_fred_symbol_category = function(category=NULL, ...) {
    symbol = group_id = . = name = popularity = series_count = frequency = id = title = observation_start = observation_end = seasonal_adjustment = last_updated = parent_id = sybs_search = NULL
    
    key = api_key("fred")
    base_url = "https://api.stlouisfed.org/fred/%sapi_key=%s&file_type=json"
    
    # functions
    query_category          = function(id) {
        setDT(fromJSON(sprintf(base_url, sprintf("category?category_id=%s&", unlist(id)), key))[["categories"]])[,.(id, name, parent_id)]
    }
    query_category_children = function(id) {
        dt = setDT(fromJSON(sprintf(base_url, sprintf("category/children?category_id=%s&", unlist(id)), key))[["categories"]])
        if (dt[,.N] > 0) dt = dt[,.(id, name, parent_id)]
        
        return(dt)
    }
    query_category_series   = function(id) {
        . = title = observation_start = observation_end = frequency = seasonal_adjustment = last_updated = popularity = NULL
        
        setDT(fromJSON(sprintf(base_url, sprintf("category/series?category_id=%s&", unlist(id)), key))[["seriess"]])[][, .(symbol=id, name=title, from=observation_start, to=observation_end, frequency, units, seasonal_adjustment, last_updated, popularity)][order(-popularity,symbol)]
    }
    query_category_tags     = function(id) {
        
        setDT(fromJSON(sprintf(base_url, sprintf("category/tags?category_id=%s&", unlist(id)), key))[["tags"]])#[, .(symbol=id, name=title, from=observation_start, to=observation_end, frequency, units, seasonal_adjustment, last_updated, popularity)]
    }
    
    
    # selecting symbols via category id
    ## initializing
    init_cate = setDT(list(
        id = c(32991, 10, 32992, 1, 32455, 32263, 3008, 33060),
        name = c("Money, Banking, & Finance", "Population, Employment, & Labor Markets",  "National Accounts", "Production & Business Activity",  "Prices", "International Data",  "U.S. Regional Data", "Academic Data"),
        parent_id = rep_len(0,8) ))
    
    cate_query = NULL
    ser_query = NULL
    category_id = category
    if (!is.null(category_id)) {
        cate_query = try(query_category_children(category_id), silent = TRUE)
        ser_query = try(query_category_series(category_id), silent = TRUE)
    }
    if (inherits(cate_query, 'try-error') || is.null(category_id) || is.null(cate_query)) cate_query = init_cate
    
    ## selecting
    while (any(!inherits(cate_query, "try-error") & nrow(cate_query)>0 & ("id" %in% names(cate_query)))) {
        if (!(inherits(ser_query,'try-error') || is.null(ser_query))) {
            cate_query = rbind(
                data.table(id=category_id, name='Parent Series', parent_id=category_id), 
                cate_query, fill=TRUE)
        }
        selected_category_df = select_rows_df(cate_query, column='id', onerow=TRUE)
        
        category_id = selected_category_df[,id]
        ser_query = try(query_category_series(category_id), silent = TRUE)
        
        if (selected_category_df[, name == 'Parent Series']) break
        cate_query = try(query_category_children(category_id), silent = TRUE)
    }
    
    
    # frequency
    tags = try(query_category_tags(category_id), silent = TRUE)
    freq = list(...)[['freq']]
    freq_list = tags[group_id == 'freq', name]
    if (length(freq_list)>1) {
        freq = check_arg(freq, choices = freq_list, arg_name = 'freq')
    } else if (length(freq_list)==1) {
        freq = freq_list
    }
    if (!is.null(freq)) ser_query = ser_query[tolower(frequency) == tolower(freq)]
    return(ser_query)
}

#' symbol of FRED economic data
#' 
#' ed_fred_symbol provides an interface to search symbols of economic data from FRED by category or keywords.
#' 
#' @param category the category id. If it is NULL, then search symbols from the top categories step by step.
#' @param keywords the query text. If it is NULL, the function will search symbols by category.
#' @param ... ignored parameters
#' 
#' @examples 
#' \dontrun{
#' # search symbols by category
#' # from top categories
#' symbol_dt1 = ed_fred_symbol()
#' # specify the initial categories
#' symbol_dt2 = ed_fred_symbol(category = 1)
#' 
#' # search symbol by keywords
#' symbol_dt3 = ed_fred_symbol(keywords = "gdp china")
#' 
#' }
#' 
#' @export
ed_fred_symbol = function(category=NULL, keywords = NULL, ...) {
    frequency = . = symbol = name = last_updated = NULL 
    check_internet('www.stlouisfed.org')
    
    if (is.null(keywords)) {
        series = ed_fred_symbol_category(category = category, ...)
    } else {
        series = ed_fred_symbol_keywords(keywords = keywords, ...)
    }
    symbol_rows = select_rows_df(series[,.(symbol,name, frequency, units)], column='symbol', onerow=FALSE)
    return(symbol_rows)
}


