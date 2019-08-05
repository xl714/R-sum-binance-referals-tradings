# R sum binance referals tradings

## this script allow to know how much you earned from your Binance referals tradings

Update the dfPrices and dfExport tables with your values then copy-paste the script to https://rdrr.io/snippets/

Code :

```r

# Add and / or update prices with today prices 
# cf https://coinmarketcap.com/fr/currencies/ethereum/

dfPrices <- read.csv(text='Coin,Usdt
"USDT", 1
"ETH", 233.55
"BTC", 11759.28
"XLM", 0.083623')

#dfPrices[which(dfPrices$Coin == "ETH"), ]$Usdt

# Export from https://www.binance.com/fr/referral-details

dfExport <- read.csv(text='Commission,Coin,Email,Date (UTC)
"0.11997250","USDT","po***@***.com","2019-08-05 07:27:33"
"0.00235245","USDT","po***@***.com","2019-08-05 07:27:33"
"0.00031420","ETH","po***@***.com","2019-08-05 00:39:27"
"0.00001620","ETH","po***@***.com","2019-08-05 00:39:27"
"0.00020284","BTC","po***@***.com","2019-08-04 04:44:06"
"0.00000656","BTC","po***@***.com","2019-08-04 04:31:55"
"0.21778000","USDT","po***@***.com","2019-08-04 00:30:28"
"0.01492220","USDT","po***@***.com","2019-08-04 00:27:37"
"0.17546000","XLM","po***@***.com","2019-07-05 10:29:13"
"0.16160000","XLM","po***@***.com","2019-07-05 10:29:08"
"0.34136000","XLM","po***@***.com","2019-07-05 10:21:36"
"0.03598881","USDT","po***@***.com","2019-07-05 10:19:07"
"0.05536111","USDT","po***@***.com","2019-07-05 10:19:07"
"0.00042000","XLM","po***@***.com","2019-07-03 23:56:31"
"0.24278000","XLM","po***@***.com","2019-07-03 23:56:16"
"0.02570745","USDT","po***@***.com","2019-07-03 23:53:34"
"0.30820000","XLM","po***@***.com","2019-07-03 23:52:13"
"0.12347004","USDT","po***@***.com","2019-06-24 12:54:54"')

suppressMessages(library(dplyr))

df <- dfExport %>% select(Commission, Coin)

myFunction <- function(commissions, coins) {
    res = c()
    len = length(commissions)
    for(i in 1:len)
    {
        commission <- commissions[i]
        coin <- coins[i]
        if(identical(coin, "ETH")){
            usdt <- commission * currentPriceETH
        }
        else if(identical(coin, "BTC")){
            usdt <- commission * currentPriceBTC
        }
        else if(identical(coin, "XLM")){
            usdt <- commission * currentPriceXLM
        }
        else 
        #if(identical(coin, "USDT"))
        {
            usdt <- commission * 1
        }
        res <- c(res, usdt)
    }
    res
}


myFunction2 <- function(commissions, coins) {
    res = c()
    len = length(commissions)
    for(i in 1:len)
    {
        commission <- commissions[i]
        coin <- coins[i]
        price <- dfPrices[which(dfPrices$Coin == coin), ]$Usdt
        
        usdt <- commission * price
        res <- c(res, usdt)
    }
    res
}

# df <- mutate(df, usdt = myFunction(Commission, Coin))
df <- mutate(df, usdt = myFunction2(Commission, Coin))


head(df)

cat("\n\n")

dfSummarized <- df %>% 
  group_by(Coin) %>% 
  summarise(Commission = sum(Commission))

dfSummarized

cat("\n\n")

usdtTotal <- sum(df$usdt)

cat('>>>>>>>>>> USDT Total', usdtTotal)



              


```
