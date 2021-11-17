library(readxl)
library(dplyr)
library(ggplot2)
library(ggimage)
library(quantmod)
library(lubridate)
library(tidyquant)
image = data.frame(image = c("https://s3-symbol-logo.tradingview.com/amazon--600.png",
                             "https://s3-symbol-logo.tradingview.com/alphabet--600.png",
                             "https://cdn.freelogovectors.net/wp-content/uploads/2020/11/booking-com-icon-logo.png",
                             "https://eleconomista.com.ar/wp-content/uploads/2020/03/Screenshot-2020-03-18-at-13.25.39.png",
                             "https://i.pinimg.com/originals/54/fe/dd/54fedd53b31b8b208c7c01af25b512ac.png",
                             "https://s3-symbol-logo.tradingview.com/asml--600.png",
                             "https://s3-symbol-logo.tradingview.com/charter--600.png",
                             "https://s3-symbol-logo.tradingview.com/tesla--600.png",
                             "https://s3-symbol-logo.tradingview.com/align-technology--600.png",
                             "https://s3-symbol-logo.tradingview.com/idexx-laboratories--600.png"))


#Yahoo Finance Verilerinin İndirilmesi
options("getSymbols.warning4.0" = F)
options("getSymbols.yahoo.warning" = F)
Symbols = c('AAPL','ADBE','ADI','ADP','ADSK','AEP','ALGN','AMAT',
            'AMD','AMGN','AMZN','ANSS','ASML','ATVI','AVGO','BIDU',
            'BIIB','BKNG','CDNS','CDW','CERN','CHKP','CHTR','CMCSA',
            'COST','CPRT','CRWD','CSCO','CSX','CTAS','CTSH','DLTR',
            'DOCU','DXCM','EA','EBAY','EXC','FAST','FB','FISV','FOX',
            'FOXA','GILD','GOOG','GOOGL','HON','IDXX','ILMN','INCY',
            'INTC','INTU','ISRG','JD','KDP','KHC','KLAC','LRCX','LULU',
            'MAR','MCHP','MDLZ','MELI','MNST','MRNA','MRVL','MSFT',
            'MTCH','MU','NFLX','NTES','NVDA','NXPI','OKTA','ORLY',
            'PAYX','PCAR','PDD','PEP','PTON','PYPL','QCOM','REGN',
            'ROST','SBUX','SGEN','SIRI','SNPS','SPLK','SWKS','TCOM',
            'TEAM','TMUS','TSLA','TXN','VRSK','VRSN','VRTX','WBA',
            'WDAY','XEL','XLNX','ZM')

#BIST 100
a = getSymbols("XU100.IS",from = today()-365,to = today(),warnings = F,auto.assign = T)

getSymbols("AAPL",from = today()-100,to = today(),warnings = F,auto.assign = T) # Tek bir hisse için
price_nasdaq = tq_get(Symbols,from = today()-1,to = today(),get = "stock.prices") #Birden fazla hisse
price_nasdaq = price_nasdaq%>%select(symbol,date,close,volume)%>%as.data.frame()
price_nasdaq
ordered_price = price_nasdaq[order(-price_nasdaq$close)[1:11],]
ordered_price = ordered_price[-3,]
ordered_price = ordered_price%>%mutate(image = image$image)
plot = ggplot(ordered_price,aes(reorder(symbol,close), close))+geom_segment(aes(x =reorder(symbol,close),xend = reorder(symbol,close),y=0,yend =  close),color = "grey")
plot+geom_point(size = 4,color ="orange",fill = alpha("orange",.5),alpha = .7,shape = 21,stroke = 2)+theme_minimal()+theme(
  panel.grid.major.x = element_blank(),
  panel.border = element_blank(),
  axis.ticks.x = element_blank()
)+geom_image(aes(y = -160,image=image), size=0.08)+xlab("Stock")+ylab("Price ($)")+ggtitle("Most Expensive Nasdaq-100 Stocks")+labs(subtitle = "Date : 2021-09-14",
                                                                                                                                    caption = "Source : Yahoo Finance")
#ggsave("nasdaq2.png")

