library(tidyverse)
library(quantmod)
library(tidyquant)
library(dplyr)
library(ggplot2)

#Strategy 1

sp<-tq_get("SPY", from = '1990-01-01', to='2020-04-23', get="stock.prices")
sp<-sp%>%mutate(SMA_50 = SMA(adjusted, 50))
spy_monthly<-sp%>%group_by(symbol)%>%tq_transmute(select = adjusted, mutate_fun = to.period, period = 'months')
spy_monthly<-spy_monthly[-c(1:3),]
spy_monthly_sma<-sp%>%group_by(symbol)%>%tq_transmute(select = SMA_50, mutate_fun = to.period, period = 'months')
spy_monthly<-spy_monthly%>%mutate(sma_50 = spy_monthly_sma$SMA_50)

#starting with a $1000 investment
spy_monthly<-spy_monthly%>%mutate(invested = 1000)

#investing 1000 every month
spy_monthly<-spy_monthly%>%mutate(invested = cumsum(invested))

#calculating the number of shares owned
for (row in 1:dim(spy_monthly)[1])
{
    if (row == 1)
    {
        spy_monthly[row,]$shares = 1000/(spy_monthly[row,]$adjusted)
    }
    else
    {
        spy_monthly[row,]$shares = (1000/(spy_monthly[row,]$adjusted)) + (spy_monthly[row-1,]$shares)
    }
}

#total value of shares
spy_monthly<-spy_monthly%>%mutate(total_shares_value = shares*adjusted)

#profit obtained
spy_monthly<-spy_monthly%>%mutate(profit = total_shares_value - invested)

#monthly plot of profit/return
ggplot(spy_monthly, aes(x = date, y = profit)) + geom_line()

#separated tibble for yearly profits
spy_yearly<-spy_monthly%>%group_by(symbol)%>%tq_transmute(select = c(adjusted, sma_50, invested, shares, total_shares_value, profit), mutate_fun = to.period, period = "years")

#plotting the yearly profits
ggplot(spy_yearly, aes(x = date, y = profit)) + geom_line()


#Strategy 2

spy_monthly_2<-select(spy_monthly, c(symbol, date, adjusted, sma_50))

#flag to indiacate buying or selling of stocks
# 0 for buy, 1 for sell
spy_monthly_2<-spy_monthly_2%>%mutate(buy_or_sell = 0)

#applying the rule,
#adjusted price >= SMA, then buy
#adjusted price < SMA, then sell
for(row in 1:dim(spy_monthly_2)[1])
{
    if(spy_monthly_2[row,]$adjusted<spy_monthly_2[row,]$sma_50)
    {
        spy_monthly_2[row,]$buy_or_sell = 1
    }
}
#initializing various components in the tibble
spy_monthly_2<-spy_monthly_2%>%mutate(shares_owned = 0)
spy_monthly_2<-spy_monthly_2%>%mutate(invested = 0)
spy_monthly_2<-spy_monthly_2%>%mutate(savings_account_balance = 1000)
spy_monthly_2<-spy_monthly_2%>%mutate(profit = 0)

#as the entire amount is invested in buying the stocks, account balance reduces to 0
for(row in 1:dim(spy_monthly_2)[1])
{
  if(spy_monthly_2[row,]$buy_or_sell == 0)
  {
    spy_monthly_2[row,]$savings_account_balance = 0
  }
}

#Calculating the shares owned and the amount invested each month
for(row in 1:dim(spy_monthly_2)[1])
{
    if(row!=1)
    {
        if(spy_monthly_2[row,]$buy_or_sell == 0)
        {
            invest = spy_monthly_2[row-1,]$savings_account_balance + 1000
            spy_monthly_2[row,]$shares_owned = (spy_monthly_2[row-1,]$shares_owned) + (invest/spy_monthly_2[row,]$adjusted)
            spy_monthly_2[row,]$invested = spy_monthly_2[row-1,]$invested + invest
        }
        else
        {
            spy_monthly_2[row,]$savings_account_balance = spy_monthly_2[row,]$savings_account_balance + ((spy_monthly_2[row-1,]$shares_owned)*(spy_monthly_2[row,]$adjusted)) + spy_monthly_2[row-1,]$savings_account_balance
        }
    }
    else
    {
        if(spy_monthly_2[row,]$buy_or_sell == 0)
        {
            invest = 1000
            spy_monthly_2[row,]$shares_owned = (spy_monthly_2[row,]$shares_owned) + (invest/spy_monthly_2[row,]$adjusted)
            spy_monthly_2[row,]$invested = invest
        }
    }
}

#Calculating profits
#Logic
#profit is calculated only after selling the stocks
for (row in 1:dim(spy_monthly_2)[1])
{
    if(row!=1)
    {
        if(spy_monthly_2[row,]$buy_or_sell == 0)
        {
            if(spy_monthly_2[row+1,]$buy_or_sell == 1)
            {
                spy_monthly_2[row+1,]$profit = ((spy_monthly_2[row+1,]$savings_account_balance)-1000)-(spy_monthly_2[row,]$invested)
            }
        }
    }
}

#generating separate tibble for monthly profits
spy_monthly_2_profit<-select(spy_monthly_2, c(symbol,date, profit))

#plotting the monthly profits
ggplot(spy_monthly_2_profit, aes(x = date, y = profit))+geom_line()

#generating separate tibble for yearly profits
spy_yearly_2_profit<-spy_monthly_2%>%group_by(year(date))%>%summarise(profit = sum(profit))

#plotting the yearly profits
ggplot(spy_yearly_2_profit, aes(x = `year(date)`, y = profit))+ geom_line()