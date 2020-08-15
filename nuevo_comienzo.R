# Este es un nuevo comienzo
library(readr)
library(slider)

datos <- read_csv(
    file = "Compustat_Global_Daily.csv",
    col_types = cols(
        datadate = col_date(format = "%Y%m%d"),
        sedol = col_character()
    )
)

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
    ggplot(aes(datadate, returns)) +
    geom_line(color = "dodgerblue") +
    geom_hline(yintercept = 0)