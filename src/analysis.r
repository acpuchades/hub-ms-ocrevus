library(ggplot2)

source("src/data.r")

dir.create("output", showWarnings = FALSE)

ggplot(relapse_data, aes(sample = cd19_count)) +
    geom_qq_line() +
    geom_qq()
ggsave("output/cd19-qqplot-r.png")

ggplot(nonrelapse_data, aes(sample = cd19_count)) +
    geom_qq_line() +
    geom_qq()
ggsave("output/cd19-qqplot-nr.png")

rdata <- relapse_data |>
    select(patient_id, cd19_count) |>
    drop_na()

nrdata <- nonrelapse_data |>
    select(patient_id, cd19_count) |>
    drop_na()

rdata$cd19_count |>
    wilcox.test(mu = 0, alternative = "greater", exact = FALSE)

nrdata$cd19_count |>
    wilcox.test(mu = 0, alternative = "greater", exact = FALSE)

data <- inner_join(rdata, nrdata, by = "patient_id", suffix = c("_r", "_nr")) |>
    select(patient_id, cd19_count_r, cd19_count_nr) |>
    drop_na()

wilcox.test(data$cd19_count_nr, data$cd19_count_r, paired = TRUE, alternative = "greater")
