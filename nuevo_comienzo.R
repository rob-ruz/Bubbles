# Este es un nuevo comienzo
library(readr)
library(slider)
library(dplyr)
library(ggplot2)

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

primary <- fundamentals %>%
    group_by(gvkey, conm) %>%
    distinct(prirow) %>%
    drop_na() %>%
    ungroup() %>%
    unite(gvkey_iid, gvkey, prirow, remove = FALSE)

sector <-  read_xlsx(
  path = "Sectores_GICS_BMV.xlsx",
  sheet = "Sectores",
  range = "A1:D292",
  col_names = TRUE,
  col_types = c("text")
)[, 1:2]

datos <- left_join(datos, sector, by = "gvkey")
datos <- mutate(datos, adjusted_price = (prccd / ajexdi) * trfd)

consumer_2000 <- filter(
    datos,
    sector == "Consumer Staples",
    datadate >= "2000-01-01",
    curcdd == "MXN",
    monthend == 1
)

consumer_all <- filter(
    datos,
    sector == "Consumer Staples",
    curcdd == "MXN" | curcdd == "MXP",
    monthend == 1
)

consumer_2000 <- unite(consumer_2000, gvkey_iid, gvkey, iid, remove = FALSE)
consumer_all <- unite(consumer_all, gvkey_iid, gvkey, iid, remove = FALSE)

consumer_2000 <- consumer_2000 %>% filter(gvkey_iid %in% fundamentals$gvkey_iid)

prueba %>%
    ungroup() %>%
    ggplot(aes(datadate, adjusted_price, color = conm)) +
    geom_point() +
    facet_wrap(~conm, nrow = 8, ncol = 5) +
    theme(legend.position = "None")

prueba <- consumer_all %>%
    group_by(gvkey) %>%
    group_modify(~ if (n_distinct(.x$iid) > 1) {
        filter(.x, gvkey_iid %in% primary$gvkey_iid)
    } else {
        .x
    })

tsibble(mutate(prueba, date = yearmonth(datadate)), key = gvkey_iid, index = date) %>% count_gaps()

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