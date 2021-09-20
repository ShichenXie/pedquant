# http://www.gov.cn/zfwj/bgtfd.htm
# http://www.gov.cn/zhengce/xxgkzl.htm
# http://sousuo.gov.cn/s.htm?t=paper&advance=true&sort=&title=年节假日安排&content=&puborg=&pcodeJiguan=&pcodeYear=&pcodeNum=&timetype=timeqb&mintime=&maxtime=&filetype=&childtype=&subchildtype=
# type: 1-workday; 3-weekend; 4-holiday;

ed_holiday = function(date_range = '3y', from = NULL, to = Sys.Date()) {
    # internet connection
    check_internet('www.gov.cn')
    # from/to
    ft = get_fromto(date_range, from, to, min_date = "1000-01-01", default_date_range = '3y')
    from = ft$f
    to = ft$t
    
    url0 ='http://sousuo.gov.cn/s.htm?q=&n=10&p=0&t=paper&advance=true&title=%E5%B9%B4%E8%8A%82%E5%81%87%E6%97%A5%E5%AE%89%E6%8E%92&content=&puborg=&pcodeJiguan=&pcodeYear=&pcodeNum=&childtype=&subchildtype=&filetype=&timetype=timeqb&mintime=&maxtime=&sort=&sortType=1&nocorrect='
    
    pagenum = read_html(url0) %>% 
        html_nodes(., 'div.page') %>% 
        html_text() %>% 
        sub('.+共([0-9]+)页.+', '\\1', .) %>% 
        as.numeric() %>% 
        seq_len()-1
    
    urlst = lapply(
        pagenum, 
        function(n) {
            datweb = read_html(sub('p=0', paste0('p=',n), url0))
            
            data.table(
                title = html_nodes(datweb, 'h3.res-sub-title') %>% html_text(), 
                url = html_nodes(datweb, 'h3.res-sub-title a') %>% html_attr( 'href')
            )[, .(yr = gsub('[^0-9]', '', title), url)][]
        }
    )
    urlst_sel = rbindlist(urlst)[order(-yr)][yr >= year(from) & yr <= year(to)]
    
    datlst = lapply(
        urlst_sel[,.I], 
        function(n) {
            u = urlst_sel[n, url]
            content_text = html_text(html_nodes(read_html(u), 'tbody p'))
            if (length(content_text) <=3 ) content_text = unlist(strsplit(content_text, '。\\s+'))
            
            data.table(
                yr = urlst_sel[n, yr], 
                content = content_text
            )
            
        }
    )
    
    dat_holidaynotice = rbindlist(datlst)[
        grepl('：[0-9]', content)
    ][, `:=`(
        name = sub('^.+?、(.+)：[0-9].+', '\\1', content), 
        holiday = sub('.+：(.*?[0-9]+月[0-9]+日).*放假.+', '\\1', content),
        holiday2 = sub('.+：.+放假.*([0-9]+月[0-9]+日).*补休.*', '\\1', content),
        days = as.integer(sub('.+共([0-9]+)天.+', '\\1', content)), 
        workday = sub('^.+?[。，](.+)上班.*?', '\\1', content)
    )][!grepl('上班', content), workday := NA
     ][!grepl('补休', content), holiday2 := NA
     ][, holiday := gsub('(至.+)|(—.+)|([0-9]+年)', '', holiday)
     ][, workday := gsub('(（.+?）)|(\\(.+\\))|([0-9]+年)|。', '', workday)
     ][!is.na(holiday), holiday_ := as.Date(paste0(yr, '-', sub('月', '-', sub('日', '', holiday))))
     ][!is.na(holiday2), holiday2_ := as.Date(paste0(yr, '-', sub('月', '-', sub('日', '', holiday2))))
     ][, rid := .I
     ]
    
    data_holiday1 = dat_holidaynotice[, .(
        yr, name, 
        date = as.Date(paste0(c(yr, unlist(strsplit(holiday, '[^0-9]'))), collapse = '-')), 
        days
    ), by = rid
    ][is.na(days), days := 1
    ][, unlist(date + (seq_len(days)-1)), by = c('yr', 'name')
    ][, .(yr, name, date = V1, type = 'holiday')]
    
    data_holiday2 = dat_holidaynotice[!is.na(holiday2), .(
        yr, name, 
        date= as.Date(paste(c(yr, unlist(strsplit(holiday2, '[^0-9]'))), collapse = '-')), 
        type = 'holiday'
    ), by = 'rid'][, .(yr, name, date, type)]
    
    data_workday = dat_holidaynotice[
        !is.na(workday),.(yr, mth = month(holiday_), name, workday)
    ][, unlist(strsplit(workday, '、', fixed = TRUE)), by = c('yr', 'mth', 'name')
    ][grepl('月', V1), mth2 := sub('([0-9]+)月.+', '\\1', V1)
    ][grepl('日', V1), day := sub('.*?([0-9]+)日.*', '\\1', V1)
    ][, mth2 := na.locf0(mth2), by = c('yr', 'mth', 'name')
    ][is.na(mth2), mth2 := mth
    ][, .(yr, name, date = as.Date(paste(yr, mth2, day, sep='-')), type = 'workday')]
    
    rbind(data_holiday1, data_holiday2, data_workday)[order(-date)]
}