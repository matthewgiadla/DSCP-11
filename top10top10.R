library(magick)
library(tidyverse)
library(patchwork)

studios <- readLines("top10_studios.txt")
posters = read_csv("posters.csv")
clean_movies = read_csv("cleaned_movies.csv")
cleaned_movies = clean_movies %>% left_join(posters, by='id')
for (stu in studios) {
  studio_data = cleaned_movies %>% filter(studio == stu) %>% arrange(rating)
  len = 0
  if (nrow(studio_data) > 10) {
    len = 10
  } else {
    len = nrow(studio_data)
  }
  for (i in 1:len) {
    rank = i
    mv_name = studio_data$name[i]
    year = studio_data$date[i]
    link = studio_data$link[i]
    studio = studio_data$studio[i]
    rating = studio_data$rating[i]
    
    img <- image_read(link)
    img_small <- image_resize(img, "100x100!")
    
    colors <- as.raster(img_small) %>%
      as.character() %>%
      substr(1, 7) %>%
      table() %>%
      as.data.frame()
    colnames(colors) = c("color", "count")
    top_colors <- colors %>%
      arrange(desc(count)) %>%
      head(1000) %>%
      mutate(color = factor(color, levels = color))
    
    pie_chart <- ggplot(top_colors, aes(x = "", y = count, fill = color)) +
      geom_bar(data = data.frame(x = "", count = sum(top_colors$count)), aes(x = x, y = count), stat = "identity", width = 1, fill = NA, color = "black")+
      geom_bar(width = 1, stat = "identity") +
      coord_polar("y") +
      scale_fill_identity() +
      theme_minimal(base_family = "Helvetica", base_line_size = 0) +
      labs(title = paste0(rank, ". ", mv_name, " (", year , ")"),
           x="Pixel Counts",
           y="",
           subtitle = paste0("Rating: ", rating)) +
      theme(
        plot.title = element_text(size = 20, face = "bold", hjust = 0.5),
        plot.subtitle = element_text(size = 15, hjust = 0.5),
        plot.background = element_rect(fill = "white", color = NA),
        text = element_text(color = "black", size = 17),
      )
    poster_gg <- grid::rasterGrob(as.raster(img), interpolate = TRUE)
    poster_plot <- ggplot() +
      annotation_custom(poster_gg, xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf) +
      theme_void()
    
    combined <- pie_chart + poster_plot + plot_layout(ncol = 2, widths = c(2, 1))
    
    dir.create(paste0("top10studios_posters/", studio), showWarnings = FALSE)
    ggsave(
      filename = paste0("top10studios_posters/", studio, "/", rank, "_", gsub("[^a-zA-Z0-9]", "_", mv_name), ".png"),
      plot = combined,
      width = 10,
      height = 6,
      dpi = 300
    )
  }
}