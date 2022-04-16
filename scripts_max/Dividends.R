library(tidyr); library(readxl); library(quantmod); library(ggplot2); library(dplyr); library(data.table)
                                                                                              

df <- read_excel("data/Stock_tickers.xlsx")


tickers <- df$Symbol

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