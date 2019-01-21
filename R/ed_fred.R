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
# # oanda (forex & currency)
# # https://www.oanda.com


#' @import data.table
#' @importFrom jsonlite fromJSON
ed1_fred = function(symbol1, from="1776-07-04", to="9999-12-31") {
    . = title = value = NULL
    
    key = api_key("fred")
    base_url = "https://api.stlouisfed.org/fred/%sapi_key=%s&file_type=json"

    # series of symbol1
    series = setDT(fromJSON(sprintf(base_url, sprintf("series?series_id=%s&", symbol1), key))[["seriess"]])

    # observation data of symbol1
    observ = setDT(fromJSON(sprintf(base_url, sprintf("series/observations?series_id=%s&observation_start=%s&observation_end=%s&", symbol1, from, to), key))[["observations"]])[,.(date=as.Date(date), symbol=symbol1, name=series[1,title], value=as.numeric(value), unit=series[1,units])]
    setkeyv(observ, "date")

    return(observ)
}
# dt = ed1_fred("GNPCA")

#' get economic data from FRED
#' 
#' ed_fred provides an interface to access the economic data provided by FRED (\url{https://fred.stlouisfed.org})
#' 
#' @param symbol symbol of indicators in FRED, which is available via function ed_FRED_symbol or its website. 
#' @param from the start date. Default is '1776-07-04'.
#' @param to the end date. Default is current system date.
#' @param print_step A non-negative integer, which will print symbol name by each print_step iteration. Default is 1. 
#' 
#' @examples 
#' \dontrun{
#' dat = ed_fred(c("A191RL1A225NBEA", "GDPCA"))
#' 
#' }
#' 
#' @export
ed_fred = function(symbol, from="1776-07-04", to="9999-12-31", print_step=1L) {
    # fromt to 
    if (from < as.Date("1776-07-04") || !isdatetime(from)) from = as.Date("1776-07-04")
    to=as.Date(to)
    
    # data list
    dat_list = load_dat_loop(symbol, "ed1_fred", args = list(from = from, to = to), print_step=print_step)
    
    return(dat_list)
}


#' @importFrom jsonlite fromJSON
#' @import data.table
ed_symbol_fred_keywords = function(keywords) {
    . = name = popularity = series_count = frequency = id = title = observation_start = observation_end = seasonal_adjustment = last_updated = NULL
    
    key = api_key("fred")
    base_url = "https://api.stlouisfed.org/fred/%sapi_key=%s&file_type=json"
    keywords = gsub("[^a-zA-Z]+", "%20", keywords)
    

    # filter via frequency
    freq_search = setDT(fromJSON(sprintf(base_url, sprintf(
        "series/search/tags?series_search_text=%s&tag_group_id=%s&", keywords, "freq"), key))[["tags"]])
    
    if (freq_search[,.N] == 0) {
        cat("not series was found")
        return(NULL)
    } else {
        freq_search = freq_search[,.(frequency=name, popularity, series_count)]
    }
        
    sel_rowid = NULL
    while (!any(sel_rowid %in% freq_search[,.I])) {
        print(setDF(copy(freq_search)))
        
        sel_rowid = readline("Select one rowid of frequency: ")
    }
    
    filter_variable = "frequency"
    filter_value = freq_search[as.integer(sel_rowid), frequency]
    filter_value = paste0(toupper(substring(filter_value,1,1)),substring(filter_value,2))

    # symbols
    sybs_search = setDT(fromJSON(sprintf(base_url, sprintf("series/search?search_text=%s&filter_variable=%s&filter_value=%s&", keywords, filter_variable, filter_value), key))[["seriess"]])[
        , .(symbol=id, name=title, from=observation_start, to=observation_end, frequency, units, seasonal_adjustment, last_updated, popularity)]
    
    return(sybs_search)
}

ed_symbol_fred_category = function() {
    . = name = popularity = series_count = frequency = id = title = observation_start = observation_end = seasonal_adjustment = last_updated = parent_id = sybs_search = NULL
    
    key = api_key("fred")
    base_url = "https://api.stlouisfed.org/fred/%sapi_key=%s&file_type=json"
    
    
    query_category = function(id) {
        setDT(fromJSON(sprintf(base_url, sprintf("category?category_id=%s&", unlist(id)), key))[["categories"]])[,.(id, name, parent_id)]
    }
    query_category_children = function(id) {
        dt = setDT(fromJSON(sprintf(base_url, sprintf("category/children?category_id=%s&", unlist(id)), key))[["categories"]])
        if (dt[,.N] > 0) dt = dt[,.(id, name, parent_id)]
        
        return(dt)
    }
    query_category_series = function(id) {
        . = title = observation_start = observation_end = frequency = seasonal_adjustment = last_updated = popularity = NULL
        
        setDT(fromJSON(sprintf(base_url, sprintf("category/series?category_id=%s&", unlist(id)), key))[["seriess"]])[, .(symbol=id, name=title, from=observation_start, to=observation_end, frequency, units, seasonal_adjustment, last_updated, popularity)]
    }
    
    # selecting symbols via category
    selecting = TRUE
    while (selecting) {
        ser_query = try(query_category_series(NULL), silent = TRUE)
        
        while(inherits(ser_query, "try-error")) {
            cate_query = try(rbindlist(lapply(list(32991, 10, 32992, 1, 32455, 32263, 3008, 33060), query_category)), silent = TRUE)
            
            while (!inherits(cate_query, "try-error") & nrow(cate_query)>0 & ("id" %in% names(cate_query))) {
                print(cate_query)
                
                sel_id = readline("select a category via (id) or ('r'+rowid): ")
                while (grepl("^r", sel_id)) {
                    row_id = as.integer(gsub("[^0-9]", "", sel_id))
                    if (row_id %in% cate_query[,.I]) {
                        sel_id = as.character(cate_query[["id"]][row_id]) 
                    } else {
                        sel_id = readline("select a category via (id) or ('r'+rowid): ")
                    }
                }
                
                cate_query = try(query_category_children(sel_id), silent = TRUE)
            }
            
            ser_query = try(query_category_series(sel_id), silent = TRUE)
        }
        selecting = menu(c("yes", "no"), title="Choose this category?")-1
    }
    
    return(ser_query)
}

#' query symbols of economic data in FRED
#' 
#' ed_fred_symbol provides an interface to search symbols of economic data in FRED by keywords or category.
#' 
#' @param keywords the query text. If it is NULL, the function will search symbols by category.
#' 
#' @examples 
#' \dontrun{
#' # search data by keywords
#' sybs1 = ed_fred_symbol("gdp china")
#' 
#' # search data by category
#' sybs1 = ed_fred_symbol()
#' }
#' 
#' @export
ed_fred_symbol = function(keywords = NULL) {
    . = symbol = name = last_updated = NULL 
    
    if (is.null(keywords)) {
        series = ed_symbol_fred_category()
    } else {
        series = ed_symbol_fred_keywords(keywords=keywords)
    }
    
    if (!is.null(series)) {
        print(setDF(copy(series)[,.(symbol, name, last_updated)]))
    }
    
    return(series)
}


