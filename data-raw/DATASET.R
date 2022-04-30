## code to prepare `DATASET` dataset goes here
library(pedquant)
library(data.table)
dt_ssec = md_stock('^000001', date_range = 'max', to = '2021-09-01', source = '163')
dt_ssec = setDF(dt_ssec)

usethis::use_data(dt_banks, overwrite = TRUE)
usethis::use_data(dt_ssec, overwrite = TRUE)

library(pedb)
ssecbank = db_query("select * from md_stock_history
                    where symbol in ('601988.SS', '601288.SS', '601398.SS', '601939.SS', '601328.SS', '000001.SS')")
ssec = setDF(ssecbank[date <= as.Date('2019-03-15') & date >= as.Date('2006-07-05')])




# database urls 
if (FALSE) {
    library(rvest)
    'http://www.cciee.org.cn/zkdh.aspx?clmId=69' %>% 
        read_html() %>% 
        html_attr('')
        html_table() %>% 
        .[[1]]
        
        
}


