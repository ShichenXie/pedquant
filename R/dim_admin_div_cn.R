# admin_div_cn, administrative division in China
# ref: 
# http://www.stats.gov.cn/tjsj/tjbz
# http://www.mca.gov.cn/article/sj/xzqh/
# http://www.xzqh.org/html/
# https://github.com/eduosi/district

# province, city, county, town, village
# import RSelenium rvest httr data.table
admin_nbs1y = function(y=2019, ad_level=3) {
  . = admin_level = code = NULL
  
  admin_list = c("provincetr","citytr","countytr","towntr","villagetr")
  url1y = sprintf('http://www.stats.gov.cn/tjsj/tjbz/tjyqhdmhcxhfdm/%s/', y)
  
  # function to scrap admin data
  scrap_daturl = function(url1, ad) {
    wb = #pedquant:::
      load_web_source2(url1) %>% 
      read_html() %>% 
      html_nodes(paste0(".", ad))
    
    # pcctv
    if (ad == "provincetr") {
      wb2 = html_nodes(wb, "td a")
      dat = data.table(
        code = wb2 %>% html_attr("href") %>% sub(".html","",.),
        name = wb2 %>% html_text(), 
        urls = wb2 %>% html_attr("href") %>% paste0(url1,.)
      )
      dat = rbind(dat, data.table(
        code = c('71','81','82'), 
        name = stringi::stri_unescape_unicode(c('\\u53f0\\u6e7e\\u7701', '\\u9999\\u6e2f\\u7279\\u522b\\u884c\\u653f\\u533a', '\\u6fb3\\u95e8\\u7279\\u522b\\u884c\\u653f\\u533a'))
      ), fill=TRUE)
    } else if (ad == "villagetr") {
      dat = data.table(
        code = html_nodes(wb,"td:nth-child(1)") %>% html_text(),
        code_ur = html_nodes(wb, "td:nth-child(2)") %>% html_text(), 
        # code of urban rural
        name = html_nodes(wb, "td:nth-child(3)") %>% html_text()
      )
    } else {
      dat = data.table(
        code = html_nodes(wb,"td:nth-child(1)") %>% html_text(),
        name = html_nodes(wb,"td:nth-child(2)") %>% html_text(),
        urls = html_nodes(wb, "td:nth-child(1) a") %>% html_attr("href") %>% 
          paste0(sub("(.+/)(\\d+.html)", "\\1", url1),.)
      )
    }
    dat$code_parent = sub('^.+/([0-9]+)/.*?$','\\1',url1)
    return(dat)
  }
  
  # provincetr citytr countytr towntr villagetr ------
  ret = list()
  al = 1
  urls = url1y
  while(al <= ad_level) {
    ad1 = admin_list[al]
    cat(sprintf('[%s] %s\n', al, ad1))
    
    dat1 = lapply(urls, function(u1) {
      scrap_daturl(u1, ad1)
    })
    ret[[ad1]] = rbindlist(dat1, fill = TRUE)
    
    urls = ret[[ad1]][!is.na(urls),urls]
    al = al + 1
  }
  
  retdat = rbindlist(
    ret, fill = TRUE, idcol = "admin_level"
  )[admin_level=='provincetr', code:=paste0(code,'0000000000')
  ][,`:=`(
    urls = NULL, 
    admin_level = sub('tr$', '', admin_level)
  )]
  
  return(retdat)
}

#' @importFrom rvest html_attr html_table
admin_mca1y = function(y=2020, print_info = FALSE) {
  
  urlst = fread(
'
year\turl
2020\thttp://www.mca.gov.cn/article/sj/xzqh/2020/2020/202003061536.html
2019\thttp://www.mca.gov.cn/article/sj/xzqh/1980/2019/202002281436.html
2018\thttp://www.mca.gov.cn/article/sj/xzqh/1980/201903/201903011447.html
2017\thttp://www.mca.gov.cn/article/sj/xzqh/1980/201803/201803131454.html
2016\thttp://www.mca.gov.cn/article/sj/xzqh/1980/201705/201705311652.html
2015\thttp://www.mca.gov.cn/article/sj/tjbz/a/2015/201706011127.html
2014\thttp://files2.mca.gov.cn/cws/201502/20150225163817214.html
2013\thttp://files2.mca.gov.cn/cws/201404/20140404125552372.htm
2012\thttp://www.mca.gov.cn/article/sj/tjbz/a/201713/201707271556.html
2011\thttp://www.mca.gov.cn/article/sj/tjbz/a/201713/201707271552.html
2010\thttp://www.mca.gov.cn/article/sj/xzqh/1980/201507/20150715854918.shtml
2009\thttp://www.mca.gov.cn/article/sj/xzqh/1980/201507/20150715854917.shtml
2008\thttp://www.mca.gov.cn/article/sj/xzqh/1980/201507/20150715854916.shtml
2007\thttp://www.mca.gov.cn/article/sj/xzqh/1980/201507/20150715854913.shtml
2006\thttp://www.mca.gov.cn/article/sj/xzqh/1980/201507/20150715854908.shtml
2005\thttp://www.mca.gov.cn/article/sj/xzqh/1980/201507/20150715854907.shtml
2004\thttp://www.mca.gov.cn/article/sj/xzqh/1980/201507/20150715854906.shtml
2003\thttp://www.mca.gov.cn/article/sj/xzqh/1980/201507/20150715854904.shtml
2002\thttp://www.mca.gov.cn/article/sj/xzqh/1980/201507/20150715854903.shtml
2001\thttp://www.mca.gov.cn/article/sj/xzqh/1980/201507/20150715854902.shtml
2000\thttp://www.mca.gov.cn/article/sj/xzqh/1980/201507/20150715854900.shtml
1999\thttp://www.mca.gov.cn/article/sj/xzqh/1980/201507/20150715854899.shtml
1998\thttp://www.mca.gov.cn/article/sj/xzqh/1980/201507/20150715854896.shtml
1997\thttp://www.mca.gov.cn/article/sj/xzqh/1980/201507/20150715854894.shtml
1996\thttp://www.mca.gov.cn/article/sj/xzqh/1980/201507/20150715854893.shtml
1995\thttp://www.mca.gov.cn/article/sj/xzqh/1980/201507/20150715854892.shtml
1994\thttp://www.mca.gov.cn/article/sj/xzqh/1980/201507/20150715854890.shtml
1993\thttp://www.mca.gov.cn/article/sj/xzqh/1980/201507/20150715854887.shtml
1992\thttp://www.mca.gov.cn/article/sj/xzqh/1980/201507/20150715854885.shtml
1991\thttp://www.mca.gov.cn/article/sj/xzqh/1980/201507/20150715854882.shtml
1990\thttp://www.mca.gov.cn/article/sj/xzqh/1980/201507/20150715854869.shtml
1989\thttp://www.mca.gov.cn/article/sj/xzqh/1980/201507/20150715854867.shtml
1988\thttp://www.mca.gov.cn/article/sj/xzqh/1980/201507/20150715854866.shtml
1987\thttp://www.mca.gov.cn/article/sj/xzqh/1980/201507/20150715854865.shtml
1986\thttp://www.mca.gov.cn/article/sj/xzqh/1980/201507/20150715854863.shtml
1985\thttp://www.mca.gov.cn/article/sj/xzqh/1980/201507/20150715854860.shtml
1984\thttp://www.mca.gov.cn/article/sj/xzqh/1980/201507/20150715854858.shtml
1983\thttp://www.mca.gov.cn/article/sj/xzqh/1980/201507/20150715854857.shtml
1982\thttp://www.mca.gov.cn/article/sj/xzqh/1980/201507/20150715854854.shtml
1981\thttp://www.mca.gov.cn/article/sj/xzqh/1980/201507/20150715854853.shtml
1980\thttp://www.mca.gov.cn/article/sj/xzqh/1980/201507/20150715854852.shtml
')
  urlst = urlst[, lapply(.SD, as.character)]
  # web = read_html('http://www.mca.gov.cn/article/sj/xzqh/') %>% 
  #   html_nodes('a.artitlelist') %>% 
  #   html_attr('href') %>%
  #   paste0('http://www.mca.gov.cn', .) %>%
  #   .[1] %>% # url0
  #   read_html() %>% 
  #   html_nodes('a.artitlelist') %>% #html_text()
  #   html_attr('href') %>%
  #   .[1] # %>% # url div wb
  urls <- as.list(urlst[year %in% as.character(y), url])
  names(urls) <- urlst[year %in% as.character(y), paste0('v',year)]
  
  lapply(
    urls,
    function(u) admin_mca1(u, print_info)
  )
}
#' 
#' @importFrom stats median setNames
admin_mca1 = function(url, print_info = FALSE) {
  if (print_info) print(url)
  . = X2 = X3 = admin_level = code = code_parent = name = NULL

  dat = load_web_source2(url) %>% # url %>% #
    read_html() %>% 
    html_table() %>% 
    .[[1]]
  
  colsel = sapply(dat, function(x) {
    median(nchar(x))
  })
  colsel = names(colsel)[which(colsel > 1)]
  
  dat = dat[, colsel]
  dat = setNames(dat, c('code', 'name'))
  
  dat2 = setDT(copy(dat))[
    grepl('[0-9]{6}', code)
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