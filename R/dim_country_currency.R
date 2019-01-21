# country code list
#
# Code list for the representation of names 
# of countries and their subdivisions.
#
# @docType data
# @keywords data
# @name code_country
# @usage data(code_country)
# @format A data frame with 249 rows and 4 columns.
# @source \url{https://en.wikipedia.org/wiki/ISO_3166-1}
# @examples
# # load code of country
# data(code_country)
# 
# NULL




# currency code list
#
# Current currency & funds code list.
#
# @docType data
# @keywords data
# @name code_currency
# @usage data(code_currency)
# @format A data frame with 278 rows and 3 columns.
# @source \url{https://en.wikipedia.org/wiki/ISO_4217}, \url{https://www.currency-iso.org/en/home/tables/table-a1.html}
# @examples
# # load code of currency
# data(code_currency)
# 
# NULL




# # import data.table rvest stringi
# dim_country_currency = function() {
#   read_html = stri_trans_totitle = country_name = numeric_code = read_xml = currency_code = NULL 
#   
#   # country ------
#   country_web = read_html('https://en.wikipedia.org/wiki/ISO_3166-1') %>% 
#     html_table(header=TRUE, fill=TRUE)
#   
#   
#   country = setDT(country_web[[2]])[,1:4,with=F]
#   setnames(country, c("country_name", "alpha2_code", "alpha3_code", "numeric_code"))
#   
#   country = country[,`:=`(
#     country_name = stri_trans_totitle(sub("\\[.+\\]$","",country_name)), 
#     numeric_code = sprintf("%03.f", numeric_code)
#   )][,country_name := gsub('\\(the ', "\\(",gsub("The","the",country_name))
#    ][,country_name := gsub("^\\s*|\\s*$","",country_name)]
#   
#   
#   # currency ------ 
#   currency_web = read_xml("http://www.currency-iso.org/dam/downloads/lists/list_one.xml") %>% html_nodes('CcyNtry')
#   
#   currency = data.table(
#     country_name = html_nodes(currency_web, "CtryNm") %>% html_text(),
#     currency = html_nodes(currency_web, "CcyNm") %>% html_text(),
#     currency_code = currency_web %>% html_text()
#   )[,`:=`(
#     currency_code = substr(currency_code,nchar(country_name)+nchar(currency)+1,nchar(country_name)+nchar(currency)+3), 
#     country_name = stri_trans_totitle(country_name)
#   )][,country_name := gsub('\\(The\\)', '',country_name)
#    ][,country_name := gsub('The', 'the',country_name)
#    ][,country_name := gsub('\\(the ', "\\(",country_name)
#    ][,country_name := gsub('Å', 'A',country_name)
#    ][,country_name := gsub("ô", "o",country_name)
#    ][,country_name := gsub('ç', 'c',country_name)
#    ][,country_name := gsub('é', 'e',country_name)
#    ][,country_name := gsub('\\[Malvinas\\]', '\\(Malvinas\\)',country_name)
#    ][,country_name := gsub(' \\(Province Of China\\)', ', Province Of China',country_name)
#    ][,country_name := gsub("’", "'",gsub(' +', ' ',country_name))  
#    ][,country_name := gsub("^\\s*|\\s*$","",country_name)]
#   
#   
#   # join country and currency 
#   # cc = merge(country, currency, by="country_name", all.x = TRUE)[,`:=`(
#   #   country_name = gsub(' And ', ' and ',gsub('Of', 'of',gsub('U.s.', 'U.S.', country_name))), 
#   #   currency_code = ifelse(currency_code == '', NA, currency_code)
#   # )]
#   
#   return(list(country=country, currency=currency))
# }










# library(data.table)
# library(rvest)
# library(stringi)
# dat = dim_country_currency()
# 
# code_country = setDF(dat$country) 
# save(code_country, file="./data/code_country.RData")
# 
# code_currency = setDF(dat$currency)
# save(code_currency, file="./data/code_currency.RData")



# reference urls ------ 
# urls = c(
#     country_code ='https://en.wikipedia.org/wiki/ISO_3166-1',
#     currency_code ='https://en.wikipedia.org/wiki/ISO_4217',
#     circulating_currency_A ='https://en.wikipedia.org/wiki/List_of_circulating_currencies', 
#     circulating_currency_B = 'http://www.casi.org.uk/info/1051list/annexd.html'
# )
