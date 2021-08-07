# V8
# [From R Hub – JavaScript for the R package developer](https://www.r-consortium.org/blog/2020/08/30/from-r-hub-javascript-for-the-r-package-developer)

library(data.table)
library(webdriver)
library(rvest)


pbc_urlsyear = function() {
    
    urls = fread('
year, url
2020, /3959050/index.html
2019, /3750274/index.html
2018, /3471721/index.html
2017, /3245697/index.html
2016, /3013637/index.html
2015, /2161324/index.html
2014, /116348/index.html
2013, /116351/index.html
2012, /116354/index.html
2011, /116357/index.html
2010, /116360/index.html
2009, /116363/index.html
2008, /116366/index.html
2007, /116369/index.html
2006, /116372/index.html
2005, /116375/index.html
2004, /116378/index.html
2003, /116381/index.html
2002, /116384/index.html
2001, /116387/index.html
2000, /116390/index.html
1999, /116393/index.html
')[,url := paste0('http://www.pbc.gov.cn/diaochatongjisi/116219/116319',url)]
    
    if (year(Sys.Date()) > urls[,max(year)]) {
        wb = load_web_source('http://www.pbc.gov.cn/diaochatongjisi/116219/116319/index.html') %>%
            read_html() %>%
            html_nodes('div .wengao2 a')
        
        urls = data.table(
            year = html_text(wb), 
            url = html_attr(wb, 'href')
        )[,`:=`(
            year = gsub('[^0-9]', '', year), 
            url = paste0('http://www.pbc.gov.cn', url)
        )][]
    }
    
    return(urls)
}

pbc_urlstopic2008 = function(url, year) {
    wb = load_web_source(url) %>%
        read_html() %>%
        html_nodes('table .border_nr td') 
    
    urlstopic = data.table(
        subname = html_text(html_nodes(wb, 'a')), 
        url = html_attr(html_nodes(wb, 'a'), 'href')
    )[!(subname %in% c('xls', 'xml','pdf'))
    ][url %in% c('#', ''), url := NA
    ][!is.na(url) & !grepl('pbc.gov.cn', url), url := paste0('http://www.pbc.gov.cn', url)
    ][!(grepl('^[\n[:space:]]+$',subname) & is.na(url))]
    
    if (year <= 2014) {
        urlstopic = urlstopic[,.(
            name = gsub('^[\n[:space:]]+|[^\u4E00-\u9FFF]+$', '', subname),
            url
        )]
    } else {
        name = html_text(html_nodes(wb, 'div')) %>% 
            gsub('^[\n[:space:]]+|[^\u4E00-\u9FFF]+$', '', .) %>% 
            setdiff(., c('', '公布日程预告'))
        
        urlstopic = urlstopic[, .(
            name = c(name, rep(name[length(name)], nrow(urlstopic)-length(name))), subname, url
        )]
    }
    
    return(urlstopic)
}

pbc_urlstopic = function(url, year) {
    if (year <= 2005) {
        wb = load_web_source(url) %>%
            read_html() %>%
            html_nodes('div .portlet li a') 
        
        urlstopic = data.table(
            name = html_text(wb), 
            url = html_attr(wb, 'href')
        )[grepl('eportal/fileDir', url)]
    } else if (year <= 2007) {
        wb = load_web_source(url) %>%
            read_html() %>%
            html_nodes('div .portlet table td a') 
        
        urlstopic = data.table(
            name = html_text(wb), 
            url = html_attr(wb, 'href')
        )[grepl('eportal/fileDir', url)]
    } else {
        wb = load_web_source(url) %>%
            read_html() %>%
            html_nodes('td .lan14cu') 
        
        urlstopic0 = data.table(
            topic = html_text(wb), 
            url = html_attr(wb, 'href')
        )[, `:=`(
            topic = sub('[a-zA-Z ]+$', '', topic), 
            url = paste0('http://www.pbc.gov.cn', url)
        )][]
        
        urlstopic = rbindlist(lapply(seq_len(nrow(urlstopic0)), function(r) {
            pbc_urlstopic2008(urlstopic0[r,url], year)[,`:=`(
                topic = urlstopic0[r,topic], 
                topicurl = urlstopic0[r,url]
            )]
        }), fill = TRUE)
    }
    
    return(urlstopic)    
}

urls = pbc_urlsyear()
urlstopic = lapply(urls$year, function(y) {
    print(y)
    pbc_urlstopic(urls[year == y, url], y)
})
names(urlstopic) <- urls$year

urlstopic2 = rbindlist(
    urlstopic, fill = TRUE, id = 'year'
)[name == '', name := '资金流量表'
][name == '货币供应量表', name := '货币供应量'
][name == '城镇居民收入与物价指数表', name := '城镇储户收入与物价扩散指数表'
][name == '银行概览', name := '存款性公司概览']

# fwrite(urlstopic2, 'pbcurls.csv')

