# Hello!

You've stumbled onto some of my SVD VAR code from 2012!

## Huh?

This is illustrating how to...

1.  Take a large number of time series (such as all stocks on the NYSE)
2.  Perform SVD or PCA to reduce the dimensionality
3.  Forecast the factors that result from (2)
4.  Do some basic analysis about the correlations between the factors 
    and the series you're looking at.
5.  Forecast a particular series of interest based on (4).

At the time, I was comparing these series evolutions to options chains.  

## Wait, if you're so good at finance and internets why aren't you a zillionaire?

Again, this is an illustration (also kind of a messy one ;) ).  I used stock data
from yahoo finance because it's easy to obtain highly correlated data.  However,
unless you can trade at the speed of Citadel, none of this will actually be useful
to you in making trades---by the time you get the prices, the information you need
has already been incorporated in the price.  You'll never make enough beyond transactions
cost as a small time dude to make up for that.

## So why post it?

There's still some cool stuff here, such as the SVD for pretty sizable time series.  When I run
this, it really cooks my 8GB of RAM.  Also, a lot of people have asked me about the ggplot2 style
I use for plots on the [design and analytics sector forecasts](http://www.designandanalytics.com/ARIMA-Sectors), 
and this gives the basics.

(What's handy about the plotting is that it actually deals with market days in the future, 
which in 2012, neither timeSeries, xts, nor ts did with any easy setting I could find.)

If you're going to use my ggplot style, I'd appreciate if you'd change a color or a line width
here or there.  The license I'm using is please be kind.  ;)

I am also migrating computers soon, so I want to get this off my machine and post it to
pressure myself to at least tidy it up and make it presentable.

## Future

I am aiming to turn this into 3 repos that might be more useful to other folk:

1. A plotter that deals with the forecast bounds in an automatic, slightly handsomer than ggplot2 default way.
2. An easy time series dimensional reduction tool + diagnostics.
3. A library for backtesting these things.  (Easier now that it's like 2 years beyond the stored data here!)

I'll be wrapping each of these 3 up in proper R libraries, but haven't gotten around to retro-fitting it yet,
and if I get really ambitious, I'll make the whole suite into a really slick command-line callable thing.

On the note of old ambitions, you can see that of the 123 stocks originally used in this analysis back then (these were on weekly
options), at least 4 are no longer with us $APKT, $MMR, $NVLS, $RIMM (at least on weeklies).  

```
"Look on my works, ye Mighty and despair!"  
Nothing besides remains.
```

# Disclaimer!

Do not use this for trading or blame me for losses.  I'm using financial data not because that's a good idea,
but because it's free and clean.  If you're seriously thinking about trading on this, you should seriously read
_A Random Walk Down Wall Street_ pronto!
