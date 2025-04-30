#!/usr/bin/env Rscript

library(tidyverse)

args <- commandArgs(trailingOnly = TRUE)

if (length(args) != 2) {
  quit(save = "no", status = 1)
}

input_file <- args[1]
output_file <- args[2]

df <- tryCatch(read_csv(input_file, locale = locale(encoding = "UTF-8")), error = function(e) {
  return(NULL)
})

if (is.null(df) || nrow(df) == 0 || !"date" %in% colnames(df) || !"rating" %in% colnames(df)) {
  quit(save = "no", status = 0)
}

result <- df %>%
  group_by(date) %>%
  summarise(avg_rating = mean(rating, na.rm = TRUE), .groups = "drop") %>%
  mutate(studio = df$studio[1])

write_csv(result, output_file)
