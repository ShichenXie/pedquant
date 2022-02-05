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


## Internal data # http://r-pkgs.had.co.nz/data.html
if (FALSE) {
    
    # # china district
    # library(RSelenium); library(rvest); library(httr); library(data.table)
    # code_china_district = admin_div_cn(admin_level = 3)
    # code_china_district = admin_mca()
    
    # # country currency
    # cc = dim_country_currency()
    # code_country = cc$country
    # code_currency = cc$currency
    
    # # financial_statements_163
    # # prov_indu_163
    
    # # symbol_stock_163
    # # update on 20200403
    # syb_stock1 = pedquant:::md_stock_spotall_163(symbol = c('a', 'b', 'index', 'fund'), to_sysdata=TRUE)
    # syb_stock2 = syb_stock1[
    #     ,.(market, symbol = gsub('[^0-9]', '', symbol), name, province, sector, industry)
    # ][market == 'index', symbol := paste0('^',symbol)][, market := NULL]
    # # syb_stock3 = pedquant:::md_fund_spotall_163()[market == 'index', symbol := paste0('^',symbol)][,.(symbol, name)]
    # symbol_stock_163 = setDF(
    #     setDT(
    #         rbind(syb_stock2, symbol_stock_163, fill=TRUE)
    #     )[,.SD[1], keyby=symbol
    #     ][grepl('^c.+', province), province := NA] )
    
    # # symbol_future_sina
future1 = fread(
'exchange symbol name board unit 
DCE V0  PVC     energy  人民币/吨
DCE L0  乙烯塑料    energy  人民币/吨
DCE PP0 聚丙烯PP    energy  人民币/吨
DCE EB0 苯乙烯  energy  人民币/吨
DCE EG0 乙二醇  energy  人民币/吨
DCE J0  焦炭    energy  人民币/吨
DCE JM0 焦煤    energy  人民币/吨
DCE A0  豆一    grain   人民币/吨
DCE B0  豆二    grain   人民币/吨
DCE M0  豆粕    grain   人民币/吨
DCE Y0  豆油    grain   人民币/吨
DCE C0  玉米    grain   人民币/吨
DCE CS0 淀粉    grain   人民币/吨
DCE RR0 粳米    grain   人民币/吨
DCE JD0 鸡蛋    grain   人民币/500千克
DCE LH0 生猪    grain   人民币/吨
DCE I0  铁矿石  metal   人民币/吨
DCE FB0 纤维板  other   人民币/立方米
DCE BB0 胶合板  other   人民币/张
DCE P0  棕榈    soft    人民币/吨
SHFE    NR0 20号胶  other   人民币/吨
SHFE    SP0 纸浆    other   人民币/吨
SHFE    SC0 原油    energy  人民币/桶
SHFE    FU0 燃油    energy  人民币/吨
SHFE    LU0 低硫燃料油  energy  人民币/吨
SHFE    BU0 沥青    energy  人民币/吨
SHFE    RU0 橡胶    energy  人民币/吨
SHFE    AU0 黄金    metal   人民币/克
SHFE    AG0 白银    metal   人民币/千克
SHFE    BC0 国际铜  metal   人民币/吨
SHFE    CU0 沪铜    metal   人民币/吨
SHFE    SS0 不锈钢  metal   人民币/吨
SHFE    RB0 螺纹钢  metal   人民币/吨
SHFE    WR0 线材    metal   人民币/吨
SHFE    HC0 热轧卷板    metal   人民币/吨
SHFE    PB0 沪铅    metal   人民币/吨
SHFE    AL0 沪铝    metal   人民币/吨
SHFE    ZN0 沪锌    metal   人民币/吨
SHFE    SN0 沪锡    metal   人民币/吨
SHFE    NI0 沪镍    metal   人民币/吨
ZCE TA0 PTA     energy  人民币/吨
ZCE ZC0 动力煤  energy  人民币/吨
ZCE MA0 郑醇    energy  人民币/吨
ZCE AP0 苹果    grain   人民币/吨
ZCE CJ0 红枣    grain   人民币/吨
ZCE PK0 花生    grain   人民币/吨
ZCE RS0 菜籽    grain   人民币/吨
ZCE RM0 菜粕    grain   人民币/吨
ZCE OI0 菜油    grain   人民币/吨
ZCE WH0 强麦    grain   人民币/吨
ZCE JR0 粳稻    grain   人民币/吨
ZCE RI0 早籼稻  grain   人民币/吨
ZCE LR0 晚籼稻  grain   人民币/吨
ZCE SF0 硅铁    metal   人民币/吨
ZCE SM0 锰硅    metal   人民币/吨
ZCE PF0 短纤    other   人民币/吨
ZCE SA0 纯碱    other   人民币/吨
ZCE UR0 尿素    other   人民币/吨
ZCE FG0 玻璃    other   人民币/吨
ZCE SR0 白糖    soft    人民币/吨
ZCE CF0 棉花    soft    人民币/吨
ZCE CY0 棉纱    soft    人民币/吨
CFFEX   TS0 2年期国债   financial   百元净价报价
CFFEX   TF0 5年期国债   financial   百元净价报价
CFFEX   T0  10年期国债  financial   百元净价报价
CFFEX   IH0 上证50指数  financial   指数点
CFFEX   IF0 沪深300指数 financial   指数点
CFFEX   IC0 中证500指数 financial   指数点')
future2 = fread(
'exchange board symbol name unit
CME-CBOT grain C 美国玉米 美分/蒲式耳
CME-CBOT grain W 美国小麦 美分/蒲式耳
CME-CBOT grain S 美国大豆 美分/蒲式耳
CME-CBOT grain SM 美黄豆粉 美元/短吨
CME-CBOT grain BO 美黄豆油 美分/磅
CME grain LHC 美瘦猪肉 美分/磅
ICE-NYBOT soft CT 美国棉花 美分/磅
ICE-NYBOT soft KC 美国咖啡 美分/磅
ICE-NYBOT soft RS 美国原糖 
MDEX soft FCPO 马棕油 令吉/吨
CME-COMEX metal GC 纽约黄金 美元/盎司
CME-COMEX metal SI 纽约白银 美元/盎司
interbank metal XAG 伦敦银 美元/盎司
interbank metal XAU 伦敦金 美元/盎司
interbank metal XPD 钯金期货 美元/盎司
interbank metal XPT 铂金期货 美元/盎司
CME-COMEX metal HG 美铜 美分/磅
LME metal CAD 伦铜 美元/吨
LME metal AHD 伦铝 美元/吨
LME metal NID 伦镍 美元/吨
LME metal PBD 伦铅 美元/吨
LME metal SND 伦锡 美元/吨
LME metal ZSD 伦锌 美元/吨
CME-COMEX energy CL WTI纽约原油 美元/桶
ICE energy OIL 布伦特原油 美元/桶
CME-NYMEX energy NG 美国天然气 美元/mmbtu
ICE energy EUA 欧洲碳排放 欧元/吨
CME financial BTC CME比特币期货 USD/BTC
CME financial YM 道琼斯指数期货
CME financial ES 标普500指数期货
CME financial NQ 纳斯达克指数期货
SGX financial NK 日经225指数期货
SGX financial CHA50CFD 富时中国A50指数
HKEX financial HSI 恒生指数期货
HKEX financial MCA MSCI中国A50指数
NA financial VX VIX恐慌指数期货', fill = TRUE)
symbol_future_sina = setDF(rbind(future1, future2, fill=TRUE)[, exchange := toupper(exchange)])

    # # exchange
    # code_exchange_commodity = pedquant:::dim_exchange_commodity()
    # code_exchange_stock = pedquant:::dim_exchange_stock()
    
    # # holiday of china
    # holiday = setDF(ed_holiday(date_range = 'max'))
    
    # usethis::use_data(code_china_district, code_country, code_currency, code_exchange_commodity, code_exchange_stock, financial_statements_163, holiday, prov_indu_163, symbol_future_sina, symbol_stock_163, urls_pbc, internal = TRUE, overwrite = TRUE)
    
}


# database urls 
if (FALSE) {
    library(rvest)
    'http://www.cciee.org.cn/zkdh.aspx?clmId=69' %>% 
        read_html() %>% 
        html_attr('')
        html_table() %>% 
        .[[1]]
        
        
}


