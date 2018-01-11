#this is sumulate function to simulate and assess the performance of a 4 stock portfolio
#some assumptions:
#allocate some amount of value to each equity on the first day then hold investment for entire year
#use adjusted close data
# report statistic for the entire portfolio

#function input: allocation, symbols, startdata, enddate)
#function output: portfolio average daily return, portfolio std, portfolio sharp ratio,portfolio cumulative return) 


#import------------------------------------------------------------------------
# QSTK Imports
import QSTK.qstkutil.qsdateutil as du
import QSTK.qstkutil.tsutil as tsu
import QSTK.qstkutil.DataAccess as da
import numpy as np
from math import sqrt

# Third Party Imports
import datetime as dt
import matplotlib.pyplot as plt
import pandas as pd


#simulate function----------------------------------------------------------------------
def simulate(ls_symbols, allocation, starty,startm,startd, endy,endm,endd):
    
    dt_start = dt.datetime(starty,startm,startd)
    dt_end = dt.datetime(endy,endm,endd)

    #-----------------------------------------------------------------------------
    # fetch and clean data
    #
    #
    #
    #We need closing prices so the timestamp should be hours=16.
    dt_timeofday = dt.timedelta(hours=16)

    # Get a list of trading days between the start and the end.
    ldt_timestamps = du.getNYSEdays(dt_start, dt_end, dt_timeofday)

    # Creating an object of the dataaccess class with Yahoo as the source.
    c_dataobj = da.DataAccess('Yahoo')

    # Keys to be read from the data, it is good to read everything in one go.
    ls_keys = ['open', 'high', 'low', 'close', 'volume', 'actual_close']

    # Reading the data, now d_data is a dictionary with the keys above.
    # Timestamps and symbols are the ones that were specified before.
    ldf_data = c_dataobj.get_data(ldt_timestamps, ls_symbols, ls_keys)
    d_data = dict(zip(ls_keys, ldf_data))

     # Filling the data for NAN
    for s_key in ls_keys:
         d_data[s_key] = d_data[s_key].fillna(method='ffill')

    # create np 2Darray 
    na_price = d_data['close'].values
    #
    #--------------------------------------------------------------------------
    #
    normalized_price=na_price/na_price[0,:]
    normalized_price
    na_rets=normalized_price.copy()
    tsu.returnize0(na_rets)
    #---------------------------------------------------------------------------
    na_rets_new=na_rets*allocation
    # print 'na_rets_new
    # print na_rets_new
    #------------------------------------------------------------------------
    # calculate portfolio return
    portfolio_return=np.sum(na_rets_new,axis=1)
    #print 'portfolio return'
    #print portfolio_return

    #print' type portfolio return'
    #print type(portfolio_return)

    #-----------------------------------------------------------------------
    average_portfolio_return=np.average(portfolio_return)
   
    #----------------------------------------------------------------------
    #print 'Standard portfolio return'
    std_portfolio_return=np.std(portfolio_return)
    
    #print 'sharp ratio'
    sharp_ratio=(average_portfolio_return/std_portfolio_return)*sqrt(252)
    print 'Symbols: ' +str(ls_symbols)
    print 'Optimal Allocations: ' +str(allocation)
    
    print 'Volatility(stdev of daily returns): ' +str(std_portfolio_return)
    print 'Average Daily Return: '+str(average_portfolio_return)
    print 'Shape Ratio: '+str(sharp_ratio)
    

    #---------------------------------------------------------------------------
    plt.clf()
    plt.plot(ldt_timestamps,na_price)
    plt.legend(ls_symbols)
    plt.ylabel('Adjusted_Close')
    plt.xlabel('Date')
    plt.savefig('adjustedclose.pdf',format='pdf')

    Cumulative = (portfolio_return+1).prod()
    print 'Cumulative Return: '+str(Cumulative)
     

   

vol,daily_ret,sharpe,cum_ret =simulate(['AXP','HPQ','IBM','HNZ'],[0.0,0.0,0.0,0.1],2010,1,1,2010,12,31)

