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
geted1_fred = function(symbol1, from="1776-07-04", to="9999-12-31") {
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
# dt = geted1_fred("GNPCA")

#' get economic data from FRED
#' 
#' geted_fred provides an interface to access the economic data provided by FRED (\url{https://fred.stlouisfed.org})
#' 
#' @param symbol symbol of indicators in FRED, which is available via function geted_FRED_symbol or its website. 
#' @param from the start date. Default is '1776-07-04'.
#' @param to the end date. Default is current system date.
#' @param print_step A non-negative integer, which will print symbol name by each print_step iteration. Default is 1. 
#' 
#' @examples 
#' \dontrun{
#' dat = geted_fred(c("A191RL1A225NBEA", "GDPCA"))
#' 
#' }
#' 
#' @export
geted_fred = function(symbol, from="1776-07-04", to="9999-12-31", print_step=1L) {
    # fromt to 
    from = check_fromto(from)
    to = check_fromto(to)
    
    # data list
    dat_list = load_dat_loop(symbol, "geted1_fred", args = list(from = from, to = to), print_step=print_step)
    
    return(dat_list)
}


#' @importFrom jsonlite fromJSON
#' @import data.table
geted_symbol_fred_keywords = function(keywords) {
    . = name = popularity = series_count = frequency = id = title = observation_start = observation_end = seasonal_adjustment = last_updated = NULL
    
    key = api_key("fred")
    base_url = "https://api.stlouisfed.org/fred/%sapi_key=%s&file_type=json"
    keywords = gsub("[^a-zA-Z]+", "%20", keywords)
    

    # filter via frequency
    freq_search = setDT(fromJSON(sprintf(base_url, sprintf(
        "series/search/tags?series_search_text=%s&tag_group_id=%s&", keywords, "freq"), key))[["tags"]])[,.(frequency=name, popularity, series_count)]
    
    if (freq_search[,.N] == 0) {
        cat("not series was found")
        return()
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

geted_symbol_fred_category = function() {
    . = name = popularity = series_count = frequency = id = title = observation_start = observation_end = seasonal_adjustment = last_updated = parent_id = sybs_search = NULL
    
    key = api_key("fred")
    base_url = "https://api.stlouisfed.org/fred/%sapi_key=%s&file_type=json"
    
    
    query_category = function(id) {
        setDT(fromJSON(sprintf(base_url, sprintf("category?category_id=%s&", id), key))[["categories"]])[,.(id, name, parent_id)]
    }
    query_category_children = function(id) {
        setDT(fromJSON(sprintf(base_url, sprintf("category/children?category_id=%s&", id), key))[["categories"]])[,.(id, name, parent_id)]
    }
    query_category_series = function(id) {
        . = title = observation_start = observation_end = frequency = seasonal_adjustment = last_updated = popularity = NULL
        
        setDT(fromJSON(sprintf(base_url, sprintf("category/series?category_id=%s&", id), key))[["seriess"]])[
            , .(symbol=id, name=title, from=observation_start, to=observation_end, frequency, units, seasonal_adjustment, last_updated, popularity)]
    }
    
    # selecting symbols via category
    selecting = TRUE
    while (selecting) {
        ser_query = try(query_category_series(NULL), silent = TRUE)
        ser_query_iserror = inherits(sybs_search, "try-error")
        
        while(ser_query_iserror) {
            cate_query = try(rbindlist(lapply(list(32991, 10, 32992, 1, 32455, 32263, 3008, 33060), query_category)), silent = TRUE)
            cate_query_iserror = inherits(cate_query, "try-error")
            
            while (!cate_query_iserror) {
                print(cate_query)
                
                sel_id = readline("select a category via (id) or ('r'+rowid): ")
                if (grepl("r", sel_id)) {
                    row_id = as.integer(gsub("^r", "", sel_id))
                    if (row_id %in% cate_query[,.I]) sel_id = cate_query[row_id, id]  
                }
                
                cate_query = try(query_category_children(as.character(sel_id)), silent = TRUE)
                cate_query_iserror = inherits(cate_query, "try-error")
            }
            
            ser_query = try(query_category_series(sel_id), silent = TRUE)
            ser_query_iserror = inherits(ser_query, "try-error")
        }
        selecting = menu(c("yes", "no"), title="Choose this category?")-1
    }
    
    return(ser_query)
}

#' get symbols of economic data in FRED
#' 
#' geted_fred_symbol provides an interface to search symbols of economic data in FRED by keywords or category.
#' 
#' @param keywords the query text. If it is NULL, the function will search symbols by category.
#' 
#' @examples 
#' \dontrun{
#' # search data by keywords
#' sybs1 = geted_fred_symbol("gdp china")
#' 
#' # search data by category
#' sybs1 = geted_fred_symbol()
#' }
#' 
#' @export
geted_fred_symbol = function(keywords = NULL) {
    . = symbol = name = last_updated = NULL 
    
    if (is.null(keywords)) {
        series = geted_symbol_fred_category()
    } else {
        series = geted_symbol_fred_keywords(keywords=keywords)
    }
    print(setDF(copy(series)[,.(symbol, name, last_updated)]))
    
    return(series)
}


