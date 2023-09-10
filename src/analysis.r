library(dplyr)
library(ggplot2)
library(ggsci)
library(lubridate)
library(magrittr)
library(readxl)
library(rlang)
library(stringr)
library(tidyr)
library(writexl)

if (any(!file.exists(c("output/ocrevus-relapses.xlsx", "output/ocrevus-nonrelapses.xlsx")))) {
    source("src/data.r")
} else {
    relapse_data <- read_excel("output/ocrevus-relapses.xlsx")
    nonrelapse_data <- read_excel("output/ocrevus-nonrelapses.xlsx")
}

data_w <- relapse_data |>
    filter(time_between_relapse_and_last_assessment < 7 * 12) |>
    inner_join(nonrelapse_data, by = c("patient_id", "sap"), suffix = c("_r", "_nr")) %T>%
    write_xlsx("output/data.xlsx")

data_l <- bind_rows(
    relapse_data |>
        filter(time_between_relapse_and_last_assessment < 7 * 8) |>
        select(patient_id, ends_with("_count")) |>
        mutate(group = "R"),
    nonrelapse_data |>
        select(patient_id, ends_with("_count")) |>
        mutate(group = "NR")
) |>
    mutate(cd4_cd8_ratio = cd4_count / cd8_count) |>
    pivot_longer(
        c(cd4_cd8_ratio, ends_with("_count")),
        names_to = "celltype", values_to = "count"
    )

data_p <- data_w |>
    pivot_longer(ends_with(c("_r", "_nr")), names_to = "celltype", values_to = "count")

ggplot(data_l, aes(sample = count)) +
    geom_qq() +
    geom_qq_line() +
    facet_grid(group ~ celltype)
ggsave("output/celltype-qqplot.png")

ggplot(data_l, aes(group, count, fill = group)) +
    geom_boxplot() +
    facet_wrap(~celltype) +
    theme(legend.position = "none") +
    labs(fill = NULL)
ggsave("output/celltype-boxplot.png")

unlink("output/comparison-tests.txt")

for (name in unique(data_l$celltype)) {
    cdata_r <- data_l |> filter(celltype == name, group == "R")
    cdata_nr <- data_l |> filter(celltype == name, group == "NR")
    data_p <- cdata_r |>
        left_join(cdata_nr, by = "patient_id", suffix = c("_r", "_nr")) |>
        drop_na(count_r, count_nr)

    ggplot(data_p, aes(sample = count_r)) +
        geom_qq_line() +
        geom_qq()
    ggsave(str_glue("output/{name}-qqplot-r.png"))

    ggplot(data_p, aes(sample = count_nr)) +
        geom_qq_line() +
        geom_qq()
    ggsave(str_glue("output/{name}-qqplot-nr.png"))

    ggplot(data_p |> mutate(diff = count_r - count_nr), aes(sample = diff)) +
        geom_qq_line() +
        geom_qq()
    ggsave(str_glue("output/{name}-qqplot-diff.png"))

    if (name == "cd4_cd8_ratio") {
        title_label <- "CD4 / CD8"
        y_label <- NULL
    } else {
        title_label <- name %>%
            str_replace("_count$", "") %>%
            str_to_upper()
        y_label <- bquote("count / " ~ mm^3)
    }

    ggplot(data_l |> filter(celltype == name), aes(group, count, fill = group)) +
        geom_boxplot() +
        labs(title = title_label, fill = NULL, x = NULL, y = y_label) +
        theme(legend.position = "none", plot.title = element_text(hjust = 0.5))
    ggsave(str_glue("output/{name}-boxplot.png"))

    sink("output/comparison-tests.txt", append = TRUE)
    cat(str_glue("# {name} (group R vs NR)\n\n"))
    print(shapiro.test(data_p$count_r - data_p$count_nr))
    print(t.test(data_p$count_r, data_p$count_nr, paired = TRUE, alternative = "greater"))
    print(wilcox.test(data_p$count_r, data_p$count_nr, paired = TRUE, alternative = "greater"))
    sink()
}
