library(tidyr); library(readxl); library(quantmod); library(ggplot2); library(dplyr); library(data.table)
library(BatchGetSymbols); library(xts)                                                                                          

df <- read_excel("data/Stock_tickers.xlsx")

### first ten rows

df_10 <- head(df, 10)


x = 1   # how many years back in time you want stock data

age <- seq.Date(as.Date(Sys.Date()),length.out=2,by=paste0("-", x, " years"))[2]

Stocks <- BatchGetSymbols(df$Symbol,
           first.date  = age)

### add name

df_flt <- df %>% 
        select(ticker = Symbol, Name)


Stocks2 <- Stocks$df.tickers

Stocks3 <- Stocks2 %>% 
        full_join(df_flt, by = "ticker")

#### plot

p<- 
ggplot(Stocks$df.tickers, aes(x = ref.date, y = price.close))+
        geom_line()+
        facet_wrap(~ticker, scales = 'free_y', ncol = 3)

ggsave("Stock_plots/first_100.pdf", plot = p, 
       width =30,  height = 200, dpi = 600, units = "cm", device = cairo_pdf, limitsize = FALSE)



#### plot with name instead of ticker



p2 <-
Stocks3 %>% 
        ggplot(aes(x = ref.date, y = price.close))+
        geom_line()+
        facet_wrap(~Name, scales = 'free_y', ncol = 3)

ggsave("Stock_plots/first_100_names.pdf", plot = p2, 
       width =30,  height = 200, dpi = 600, units = "cm", device = cairo_pdf, limitsize = FALSE)




#######################################################
#######################################################
#######################################################

#######################################################
### dividends

`getDividends` <-
        function(Symbol,from='1970-01-01',to=Sys.Date(),env=parent.frame(),src='yahoo',
                 auto.assign=FALSE,auto.update=FALSE,verbose=FALSE,split.adjust=TRUE,...,
                 curl.options=list()) {
                
                tmp.symbol <- Symbol
                if(missing(env)) {
                        env <- parent.frame(1)
                } else {
                        if(exists(Symbol, envir = env, inherits = FALSE)) {
                                tmp.symbol <- get(Symbol, envir = env)
                        }
                        if(!missing(auto.assign) && !isTRUE(auto.assign) && !is.null(env)) {
                                warning("ignoring 'auto.assign = FALSE' because 'env' is specified")
                        }
                        auto.assign <- TRUE
                }
                if(is.null(env))
                        auto.assign <- FALSE
                Symbol.name <- ifelse(!is.character(Symbol),
                                      deparse(substitute(Symbol)),
                                      as.character(Symbol))
                
                from.posix <- .dateToUNIX(from)
                to.posix <- .dateToUNIX(to)
                
                handle <- .getHandle()
                yahoo.URL <- .yahooURL(Symbol.name, from.posix, to.posix, "1d", "div")
                
                conn <- curl::curl(yahoo.URL,handle=handle)
                fr <- try(read.csv(conn, as.is=TRUE), silent = TRUE)
                
                if (inherits(fr, "try-error")) {
                        fr <- retry.yahoo(Symbol.name, from.posix, to.posix, "1d", "div", conn, warn = FALSE)
                }
                
                fr <- xts(fr[,2],as.Date(fr[,1]))
                colnames(fr) <- paste(Symbol.name,'div',sep='.')
                
                # dividends from Yahoo are not split-adjusted
                if(src[1] == "yahoo" && split.adjust) {
                        splits <- getSplits(Symbol.name, from="1900-01-01")
                        if(is.xts(splits) && is.xts(fr) && nrow(splits) > 0 && nrow(fr) > 0) {
                                fr <- fr * adjRatios(splits=merge(splits, index(fr)))[,1]
                        }
                }
                
                if(is.xts(tmp.symbol)) {
                        if(auto.update) {
                                xtsAttributes(tmp.symbol) <- list(dividends=fr)
                                assign(Symbol.name,tmp.symbol,envir=env)
                        }
                } else if(auto.assign) {
                        assign(paste(Symbol.name,'div',sep='.'),fr,envir=env)
                } else fr
        }


aapl_div <- getDividends(AAPL)





















getDividends(Symbol = tickers, 
              from = age,
              to = Sys.Date(), 
              src = "yahoo", 
              auto.assign = FALSE, 
              auto.update = FALSE, 
              verbose = FALSE,
              split.adjust = TRUE,
              curl.options = table(), 
             warnings = FALSE)

tickers <- as.character(df_10$Symbol)
tickers

dividends <- getDividends(Symbol = tickers)
             from = age)
             #
             to = Sys.Date(), 
             src = "yahoo", 
             auto.assign = FALSE, 
             auto.update = FALSE, 
             verbose = FALSE,
             split.adjust = TRUE,
             curl.options = table())

div <- as.numeric(tail((getDividends(Symbol = AAPL,
                        from = age,
                       to = Sys.Date(), 
                       src = "yahoo", 
                       auto.assign = FALSE, 
                       auto.update = FALSE, 
                       verbose = FALSE,
                       split.adjust = TRUE,
                       curl.options = table())), n = 1))

div_aapl <- as.numeric(tail((getDividends(AAPL)), n = 1))

AAPL_div <- getDividends(Symbol = AAPL,
                         from = age,
                         to = Sys.Date(), 
                         src = "yahoo", 
                         auto.assign = FALSE, 
                         auto.update = FALSE, 
                         verbose = FALSE,
                         split.adjust = TRUE,
                         curl.options = list())

aapl_div <- (tail(AAPL_div, n = 1))

aapl_div
div




#######################

### read excel screener

screener1 <- read_excel("data/yahoo_screener_dividendes_9%.xlsx", sheet = "1-100")
screener2 <- read_excel("data/yahoo_screener_dividendes_9%.xlsx", sheet = "101-200", head)

screener <- rbind(screener1, screener2)


screener %>% 
        mutate(dividends = as.numeric(tail((getDividends(symbol = Symbol)), n = 1))
               
         
as.numeric(tail((getDividends(symbol = screener$Symbol)), n = 1)






dividends <- getDividends(Symbol = DNB.ASA)

getSymbols("AAPL")

AAPL %>% 
        ggplot(aes(y=AAPL.Close))+
        geom_line()

row.names(AAPL$AAPL.Open)

rownames(AAPL)

AAPL_div <- getDividends("AAPL", 
             from = "1970-01-01",
             to = Sys.Date(), 
             src = "yahoo", 
             auto.assign = FALSE, 
             auto.update = FALSE, 
             verbose = FALSE,
             split.adjust = TRUE,
             curl.options = table())

plot(AAPL_div)

div <- getDividends(Symbol = tickers, 
                    from = "1970-01-01",
                    to = Sys.Date(), 
                    src = "yahoo", 
                    auto.assign = FALSE, 
                    auto.update = FALSE, 
                    verbose = FALSE,
                    split.adjust = TRUE)

time(AAPL_div)

AAPL_div %>% 
        mutate(date = time(AAPL_div))

df <- as.table(AAPL_div$AAPL.div, index(AAPL_div))

aapl_div <- as.data.table(AAPL_div, keep.rownames = TRUE) %>% 
        mutate(date = index) %>% 
        select(date, AAPL.div)

aapl_div %>% 
        ggplot(aes(date, AAPL.div))+
        geom_line()

getSymbols("AAPL")

AAPL2 <- as.data.table(AAPL, keep.rownames = TRUE)

AAPL2 %>% 
        ggplot(aes(index, AAPL.Close))+
        geom_line()+
        xlab("date")

getSymbols("MSFT")
MSFT2 <- as.data.table(MSFT, keep.rownames = TRUE)

div <- AAPL2 %>% 
        select(index, AAPL.Close)




##################################################################
##################################################################

getSymbols("AAPL")

AAPL.date <- as.data.table(AAPL, keep.rownames = TRUE)

AAPL_div <- as.data.table((getDividends("AAPL")), keep.rownames = TRUE)

AAPL_df <- inner_join(AAPL.date, AAPL_div, by = "index")

AAPL_df %>% 
        mutate(dividend.yield = (AAPL.div/AAPL.Close)*100) -> AAPL_df

getSymbols("AAPL")

AAPL.date <- as.data.table(AAPL, keep.rownames = TRUE)
