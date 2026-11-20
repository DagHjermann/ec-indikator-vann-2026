suppressPackageStartupMessages({
  library(sf)
  library(dplyr)
  library(writexl)
})

read_csv <- function(file = "data/regulerte_elver_kyst.csv") {
  read.csv(file, stringsAsFactors = FALSE)
}

compute_index <- function(data) {
  by_vannforekomst <- data %>%
    mutate(andel_qnormal_klasse = andel_qnormal_klasse / 100) %>%
    group_by(vannforekomstid,okoregion) %>%
    summarise(andel_qnormal_klasse = mean(andel_qnormal_klasse), .groups = "drop")
  
  by_region <- by_vannforekomst %>%
    filter(okoregion != "") %>%
    group_by(okoregion) %>%
    summarise(
      mean_andel_qnormal_klasse = mean(andel_qnormal_klasse, na.rm = TRUE),
      stdev_andel_qnormal_klasse = sd(andel_qnormal_klasse, na.rm = TRUE)
    )
  
  total <- by_vannforekomst %>%
    summarise(
      okoregion = "Hele landet",
      mean_andel_qnormal_klasse = mean(andel_qnormal_klasse, na.rm = TRUE),
      stdev_andel_qnormal_klasse = sd(andel_qnormal_klasse, na.rm = TRUE)
    )
  
  bind_rows(by_region, total)
}

write_excel <- function(index, file = "data/regulert_indeks.xlsx") {
  write_xlsx(index, file)
}
