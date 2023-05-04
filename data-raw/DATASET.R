## code to prepare `DATASET` dataset goes here
library(pedquant)
library(data.table)
to = '2023-03-31'
dt_banks = md_stock(c('601988.SS', '601288.SS', '601398.SS', '601939.SS', '601328.SS'), to = to, date_range = '3y')
dt_ssec = md_stock(c('000001.SS'), to = to, date_range = '5y')

dt_banks = setDF(rbindlist(dt_banks))
dt_ssec = setDF(rbindlist(dt_ssec))

usethis::use_data(dt_banks, overwrite = TRUE)
usethis::use_data(dt_ssec, overwrite = TRUE)


# database urls 
if (FALSE) {
    library(rvest)
    'http://www.cciee.org.cn/zkdh.aspx?clmId=69' %>% 
        read_html() %>% 
        html_attr('')
        html_table() %>% 
        .[[1]]
        
        
}


