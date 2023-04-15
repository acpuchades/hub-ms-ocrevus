library(ggplot2)

source("src/data.r")

dir.create("output", showWarnings = FALSE)

ggplot(data, aes(sample = last_cd19_count)) +
    geom_qq_line() +
    geom_qq()
ggsave("output/last-cd19-qqplot.png")

cd19_test_result <- data$last_cd19_count |>
    wilcox.test(mu = 0, alternative = "greater", exact = FALSE)
