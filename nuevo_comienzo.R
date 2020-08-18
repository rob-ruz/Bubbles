# Este es un nuevo comienzo
library(readr)
library(slider)
library(dplyr)
library(ggplot2)
library(tsibble)
library(tidyr)
library(readxl)

datos <- read_csv(
    file = "Compustat_Global_Daily.csv",
    col_types = cols(
        datadate = col_date(format = "%Y%m%d"),
        sedol = col_character()
    )
)

fundamentals <- read_csv(
  file = "Compustat_Fundamentals.csv",
  col_types = cols(
    gvkey = col_character(),
    datadate = col_date(format = "%Y%m%d"),
    ceqq = col_number()
  ),
  na = c("N/A", "")
)

sector <-  read_xlsx(
  path = "Sectores_GICS_BMV.xlsx",
  sheet = "Sectores",
  range = "A1:B292",
  col_names = TRUE,
  col_types = c("text")
)

fundamentals %>%
    distinct(gvkey, prirow) %>%
    drop_na() %>%
    unite(gvkey_iid, gvkey, prirow)  %>%
    pull() -> primary

datos %>%
    filter(
        curcdd %in% c("MXN", "MXP"),
        monthend == 1
    ) -> datos_mth


datos_mth %>%
    mutate(
        prccd = if_else(curcdd == "MXP", prccd / 1000, prccd),
        adjusted_price = (prccd / ajexdi) * trfd,
        sector = vlookup(gvkey, sector, "gvkey", "sector"),
        date = yearmonth(datadate)
    )  %>%
    unite(gvkey_iid, gvkey, iid, remove = FALSE) -> datos_mth

# Para el caso de Industrias Resistol el cambio de moneda está
# contemplado por el ajexdi


datos_mth <- subset(datos_mth, !gvkey_iid %in% df$gvkey_iid)

datos_mth %>%
    filter(
        sector == "Consumer Staples"
    ) -> consumer_all

consumer_all %>%
    filter(
        datadate >= "2000-01-01"
    ) -> consumer_2000

#### funcionalizar
consumer_all %>%
    group_by(gvkey) %>%
    group_modify(~ if (!any(unique(.x$gvkey_iid) %in% primary) & n_distinct(.x$iid) > 1) {
        group_by(.x,iid) %>% mutate(freq=n()) %>% ungroup() %>% filter(freq == max(freq)) %>% select(-freq)
    } else if (any(unique(.x$gvkey_iid) %in% primary) & n_distinct(.x$iid) > 1) {
        filter(.x, gvkey_iid %in% primary)
    } else {
        .x
    }) -> consumer_all_filtered
  


consumer_all_filtered %>%
    ungroup() %>%
    ggplot(aes(datadate, adjusted_price, color = conm)) +
    geom_line() +
    facet_wrap(~conm, nrow = 8, ncol = 5) +
    theme(legend.position = "None")

consumer_all_filtered %>%
    tsibble(key = gvkey_iid, index = date) %>%
    fill_gaps()

#rellenar los de 6 meses o menos

# prueba de runups
bimbo <- filter(
    datos,
    conm == "GRUPO BIMBO SA DE CV",
    monthend == 1,
    datadate >= "2000/01/01",
    curcdd == "MXN"
) %>%
    mutate(
        adjusted_price = (prccd / ajexdi),
        log_return = c(NA, diff(log(adjusted_price))),
        gross_return = c(NA, adjusted_price[2:239] / adjusted_price[1:238]),
        net_return = gross_return - 1
    ) %>%
    select(datadate, adjusted_price, log_return, net_return, gross_return) %>%
    rename(date = datadate)

bimbo <- mutate(
    bimbo[-1, ],
    cum_log_ret = slide_dbl(
        .x = log_return,
        .f = sum,
        .before = 23,
        .complete = TRUE
    ),
    cum_ret = slide_dbl(
        .x = gross_return,
        .f = ~ prod(.x) - 1,
        .before = 23,
        .complete = TRUE
    )
)

bimbo %>%
    ggplot(aes(date, cum_ret)) +
    geom_line(color = "dodgerblue") +
    geom_hline(yintercept = 0)

    
vlookup <- function(this, df, key, value) {
    m <- match(this, df[[key]])
    df[[value]][m]
}

df <- aggregate(gvkey ~ gvkey_iid, datos_mth, length)
colnames(df)[colnames(df) == "gvkey"] <- "n"
df <- df[which(df$n < 12), ]
df <- df[order(df$n), ]
row.names(df) <- NULL
df$name <- vlookup(df$gvkey_iid, datos_mth, "gvkey_iid", "conm")
sum(df$n)
