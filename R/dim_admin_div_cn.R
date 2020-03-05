# admin_div_cn, administrative division in China
# ref: 
# http://www.stats.gov.cn/tjsj/tjbz
# http://www.mca.gov.cn/article/sj/xzqh/
# http://www.xzqh.org/html/
# https://github.com/eduosi/district

# province, city, county, town, village
# import RSelenium rvest httr data.table
admin_div_cn = function(admin_level=3) {
  read_html = html_attr = . = id = ad = remoteDriver = code = name = code_parent = NULL
  
  # function # url0
  url_latest = function(web_url="http://www.stats.gov.cn/tjsj/tjbz/tjyqhdmhcxhfdm/") {
    url_nbs = "http://www.stats.gov.cn"
    
    url0 = read_html(web_url) %>% 
      html_nodes("ul.center_list_contlist li a") %>% 
      html_attr('href') %>% 
      sub(url_nbs, '', .) %>% 
      sub("index.html", "", .) %>% 
      paste0(url_nbs, .) %>% 
      .[1]
    
    return(url0)
  }
  
  # function # name of admin div
  name_ad = function(url, url0) {
    data.table(
      id = 0:4, 
      ad = c("provincetr","citytr","countytr","towntr","villagetr")
    )[id==length(unlist(strsplit(sub(url0, "", url), "/"))), ad]
  }
  
  # remoteDriver
  # remDr <- remoteDriver(remoteServerAddr = "localhost", port = 4444L, browserName = "firefox")
  # remDr$open(silent = TRUE)
  remDr <- remoteDriver(port = 4445L, browserName = "chrome")
  remDr$open(silent = TRUE)
  
  # function to scrap dat/url
  scrap_daturl = function(url, ad, rem = FALSE) {
    html_nodes = `%>%` = html_attr = html_text = NULL
    
    read_web = function(url, rem=FALSE) {
      if (rem) {
        remDr$navigate(url)
        wb = read_html(remDr$getPageSource()[[1]])
      } else {
        wb = read_html(url)
      }
      
      return(wb)
    }
    
    # read web html
    wb = try(read_web(url, rem), silent = TRUE)
    if (inherits(wb, "try-error")) {
      wb = read_web(url, TRUE)
    }

    wb = html_nodes(wb, paste0(".", ad))
    
    # pcctv
    if (ad == "provincetr") {
      wb2 = html_nodes(wb, "td a")
      dat = data.table(
        code = wb2 %>% html_attr("href") %>% sub(".html","",.),
        name = wb2 %>% html_text()
      )
      
      urls = wb2 %>% html_attr("href") %>% paste0(url,.)
    } else if (ad == "villagetr") {
      dat = data.table(
        code = html_nodes(wb, "td:nth-child(1)") %>% html_text(),
        code_ur = html_nodes(wb, "td:nth-child(2)") %>% html_text(), 
        # code of urban rural
        name = html_nodes(wb, "td:nth-child(3)") %>% html_text()
      )
      
      urls = NULL
    } else {
      dat = data.table(
        code = html_nodes(wb, "td:nth-child(1)") %>% html_text(),
        name = html_nodes(wb, "td:nth-child(2)") %>% html_text()
      )
      
      urls = html_nodes(wb, "td:nth-child(1) a") %>% html_attr("href") %>% 
        paste0(sub("(.+/)(\\d+.html)", "\\1", url),.)
    }
    
    
    return(list(dat=dat, urls=urls))
  }
  
  
  # provincetr citytr countytr towntr villagetr ------
  
  # return data
  ret = NULL
  # param
  al = 1
  num = 1 
  # urls
  url0 = url_latest()
  urls = url0
  while (al <= admin_level) {
    urls2 = NULL
    for (i in urls) {
      admin_name = name_ad(i, url0)
      ad_code = sub(".*?(\\d+).html", "\\1", sub(url0, "", i))
      cat(num, admin_name, ad_code, "\n")
      
      if (num %% 100 == 0) {
        Sys.sleep(1)
        daturl = scrap_daturl(i, admin_name, rem=TRUE)
      } else {
        daturl = scrap_daturl(i, admin_name)
      }
      
      ret[[paste0(admin_name, ad_code)]] = daturl$dat
      urls2 = c(urls2, daturl$urls)
      
      num = num + 1
    }
    urls = urls2
    al = al+1
  }
  

  # close remDr
  remDr$close()
  
  ret_df = rbindlist(ret, fill = TRUE, idcol = "admin_level")[
    , `:=`(
      code_parent = sub("^([a-z]+)tr([0-9]*)$","\\2",admin_level),
      admin_level = sub("^([a-z]+)tr([0-9]*)$","\\1",admin_level)
    )][order(code)][,.(code, name, admin_level, code_parent)]
  # 11 12 31 50
  for (c in c("110100000000", "120100000000", "310100000000", "500100000000")) {
    ret_df[code == c, name := ret_df[code == substr(c,1,2), name]]
  }
  
  return(setDF(ret_df))
}

#' @importFrom rvest html_attr html_table
admin_mca = function() {
  . = X2 = X3 = admin_level = code = code_parent = name = NULL
  
  url = read_html('http://www.mca.gov.cn/article/sj/xzqh/1980/') %>% 
    html_nodes('td.arlisttd a') %>% #html_text()
    html_attr('href') %>%
    paste0('http://www.mca.gov.cn', .) %>%
    .[1] %>% # url0
    read_html() %>% 
    html_nodes('div.content p a') %>% #html_text()
    html_attr('href') %>%
    .[1] # %>% # url div wb
    
  dat = read_html(url) %>% 
    html_table() %>% 
    .[[1]] %>% 
    .[,c('X2', 'X3')]
  
  dat2 = setDT(copy(dat))[,.(
    code = X2, name = X3
  )][grepl('[0-9]{6}', code)
   ][grepl('.{4}0{2}', code), admin_level := 'city'
   ][grepl('.{2}0{4}', code), admin_level := 'province'
   ][is.na(admin_level), admin_level:= 'county']
  
  dat3 = rbind(
    dat2, 
    dat2[admin_level=='province' & grepl('\u5e02$',name) # https://tool.chinaz.com/tools/unicode.aspx
       ][,`:=`(
         code = paste0(substr(code,1,2),'0100'), 
         admin_level = 'city'
       )]
  )[order(code)
  ][admin_level != 'county', code_parent := code
  ][, code_parent := fillna(code_parent)
  ][admin_level == 'city', code_parent := paste0(substr(code,1,2),'0000')
  ][admin_level == 'province', code_parent := '']
  
  return(setDF(dat3))
}