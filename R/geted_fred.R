#' by category,
#' 
#' tags:
#'     freq
#'     gen
#'     geo
#'     geot
#'     rls
#'     src
#'     seas
#' 
#' # fred
#' # http://fred.stlouisfed.org
#' 
#' "https://fred.stlouisfed.org/graph/fredgraph.csv?id=DTWEXB"
#' 
#' 
#' # oanda (forex & currency)
#' # https://www.oanda.com
#' 
#' 
#' 
#' fred_api = sprintf(, api_key)
#' 
#' # Categories
#' category = "https://api.stlouisfed.org/fred/category?category_id=%s&api_key=%s&file_type=json"
#' category_children = "https://api.stlouisfed.org/fred/category/children?category_id=%s&api_key=%s&file_type=json"
#' category_related = "https://api.stlouisfed.org/fred/category/related?category_id=32073&api_key=%s&file_type=json"
#' category_series = "https://api.stlouisfed.org/fred/category/series?category_id=%s&api_key=%s&file_type=json"
#' category_tags = "https://api.stlouisfed.org/fred/category/tags?category_id=%s&api_key=%s&file_type=json"
#' category_related_tags = "https://api.stlouisfed.org/fred/category/related_tags?category_id=125&tag_names=services;quarterly&api_key=%s&file_type=json"
#' 
#' # Releases
#' releases = "https://api.stlouisfed.org/fred/releases?api_key=%s&file_type=json"
#' releases_dates = "https://api.stlouisfed.org/fred/releases/dates?api_key=%s&file_type=json"
#' release = "https://api.stlouisfed.org/fred/release?release_id=53&api_key=%s&file_type=json"
#' release_dates = "https://api.stlouisfed.org/fred/release/dates?release_id=82&api_key=%s&file_type=json"
#' release_series = "https://api.stlouisfed.org/fred/release/series?release_id=51&api_key=%s&file_type=json"
#' release_sources = "https://api.stlouisfed.org/fred/release/sources?release_id=51&api_key=%s&file_type=json"
#' release_tags = "https://api.stlouisfed.org/fred/release/tags?release_id=86&api_key=%s"
#' release_related_tags = "https://api.stlouisfed.org/fred/release/related_tags?release_id=86&tag_names=sa;foreign&api_key=%s&file_type=json"
#' release_tables = "https://api.stlouisfed.org/fred/release/tables?release_id=53&api_key=%s&element_id=12886&file_type=json"
#' 
#' # Series
#' series = "https://api.stlouisfed.org/fred/series?series_id=GNPCA&api_key=%s&file_type=json"
#' series_categories = "https://api.stlouisfed.org/fred/series/categories?series_id=EXJPUS&api_key=%s&file_type=json"
#' series_observations = "https://api.stlouisfed.org/fred/series/observations?series_id=GNPCA&api_key=%s&file_type=json"
#' series_release = "https://api.stlouisfed.org/fred/series/release?series_id=IRA&api_key=%s&file_type=json"
#' series_search = "https://api.stlouisfed.org/fred/series/search?search_text=monetary+service+index&api_key=%s&file_type=json"
#' series_search_tags = "https://api.stlouisfed.org/fred/series/search/tags?series_search_text=monetary+service+index&api_key=%s&file_type=json"
#' series_search_related_tags = "https://api.stlouisfed.org/fred/series/search/related_tags?series_search_text=mortgage+rate&tag_names=30-year;frb&api_key=%s&file_type=json"
#' series_tags = "https://api.stlouisfed.org/fred/series/tags?series_id=STLFSI&api_key=%s&file_type=json"
#' series_updates = "https://api.stlouisfed.org/fred/series/updates?api_key=%s&file_type=json"
#' series_vintagedates = "https://api.stlouisfed.org/fred/series/vintagedates?series_id=GNPCA&api_key=%s&file_type=json"
#' # Sources
#' sources = "https://api.stlouisfed.org/fred/sources?api_key=%s&file_type=json"
#' source = "https://api.stlouisfed.org/fred/source?source_id=1&api_key=%s&file_type=json"
#' source_releases = "https://api.stlouisfed.org/fred/source/releases?source_id=1&api_key=%s&file_type=json"
#' 
#' # Tags
#' tags = "https://api.stlouisfed.org/fred/tags?api_key=%s&file_type=json"
#' related_tags = "https://api.stlouisfed.org/fred/related_tags?tag_names=monetary+aggregates;weekly&api_key=%s&file_type=json"
#' tags_series = "https://api.stlouisfed.org/fred/tags/series?tag_names=slovenia;food;oecd&api_key=%s&file_type=json"
#' 
#' 
#' 
#' 
#' dt = {}
#' pid0 = c(32991, 10, 32992, 1, 32455, 32263, 3008, 33060)
#' for (i in pid0) {
#'     dt_cat = try(fromJSON(sprintf(category, i, api_key)), silent = TRUE)
#'     dt = c(dt, dt_cat)
#' }
#' i = 32991
#' fromJSON(sprintf(category_children, i, api_key))
#' 
#' i = 94
#' fromJSON(sprintf(category_series, i, api_key))
#' fromJSON(sprintf(category_tags, i, api_key))
#' 
#' 
#' geted1_fred = function(symbol1, from="1776-07-04", to="9999-12-31") {
#'     key = api_key("fred")
#'     base_url = "https://api.stlouisfed.org/fred/%sapi_key=%s&file_type=json"
#' 
#'     # series of symbol1
#'     series = setDT(fromJSON(sprintf(base_url, sprintf("series?series_id=%s&", symbol1), key))[["seriess"]])
#' 
#'     # observation data of symbol1
#'     observ = setDT(fromJSON(sprintf(base_url, sprintf("series/observations?series_id=%s&observation_start=%s&observation_end=%s&", symbol1, from, to), key))[["observations"]])[,.(date=as.Date(date), symbol=symbol1, name=series[1,title], value=as.numeric(value), unit=series[1,units])]
#'     setkeyv(observ, "date")
#' 
#'     return(observ)
#' }
#' # dt = geted1_fred("GNPCA")
#' 
#' 
#' geted_fred = function(symbol, from="1776-07-04", to="9999-12-31", print_step=1L) {
#'     from = check_fromto(from)
#'     to = check_fromto(to)
#' 
#' 
#' 
#' 
#' 
#' }
#' 
#' 
#' #' @importFrom jsonlite fromJSON
#' #' @import data.table
#' geted_symbol_fred = function(keywords = NULL, category=NULL, tags=NULL) {
#'     key = api_key("fred")
#'     base_url = "https://api.stlouisfed.org/fred/%sapi_key=%s&file_type=json"
#' 
#' 
#' 
#'     sybs_search = setDT(fromJSON(sprintf(base_url, paste0("series/search?search_text=", keywords, "&"), key))[["seriess"]])
#' 
#' 
#' 
#' 
#'     # # tags
#'     # freq = Frequency
#'     # gen = General or Concept
#'     # geo = Geography
#'     # geot = Geography Type
#'     # rls = Release
#'     # seas = Seasonal Adjustment
#'     # src = Source
#' 
#'     # all_tags = setDT(fromJSON(sprintf(base_url, "tags?", key))[["tags"]])
#'     # all_sources = setDT(fromJSON(sprintf(base_url, "sources?", key))[["sources"]])
#'     # all_releases = setDT(fromJSON(sprintf(base_url, "releases?", key))[["releases"]])
#' 
#' 
#' 
#' 
#' 
#' }