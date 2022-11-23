# function which checks for html img tag and replaces it with actual image filename
check_replace_imagefile <- function(x) {
  str_replace_all(
    x,
    "<img src=[\"'](.+?)[\"'].*?>",
    here("data/Shiny/\\1")
  )
}
# plotting of tables with images
get_image_table <- function(list_tables, rows) {
  list_tables %>%
    gt::gt() %>%
    cols_width(value ~ px(600)) %>%
    tab_style(
      style = cell_borders(
        sides = "all",
        color = "grey95",
        weight = px(1),
        style = "solid"
      ),
      locations = cells_body(
        columns = everything(),
        rows = everything()
      )
    ) %>%
    gt::tab_options(
      column_labels.hidden = TRUE,
      table.width = px(800),
      table.align = "center"
    ) %>%
    gt::text_transform(
      locations = gt::cells_body(columns = value, rows = rows),
      fn = function(x) {
        purrr::map_chr(
          x, ~ gt::local_image(filename = .x, height = gt::pct(100))
        )
      }
    )
}
# convert wide format of item into long format as well as filter missing values
convert_to_long <- function(x) {
  x %>%
    gather(data = x) %>%
    mutate(key = c(
      "Item",
      "Stimulus",
      "Antwortoption 1",
      "Antwortoption 2",
      "Antwortoption 3",
      "Antwortoption 4",
      "Korrekte Antwort",
      "Gewählte Antwort",
      "Korrektheit?",
      "Antwortzeit (in Sekunden)"
    )) %>%
    select(key, value) %>%
    # Drop rows where value is "NA"
    filter(!str_detect(value, "NA"))
}
# function that extract indices of rows where images should be located in the tables
get_image_indices <- function(x) {
  x %>%
    pull(value) %>%
    str_detect(., pattern = "(data/Shiny/www/)") %>%
    which()
}

# function for rendering the exam review script:
render_review <- function(filename_of_data) {
  # extract participant id and date for file naming:
  participant_id_date <- filename_of_data %>% 
    basename() %>% 
    str_remove_all("WS.RData")

  # render rmd:
  rmarkdown::render(
    here::here("R/exam_review.Rmd"),
    params = list(data = filename_of_data),
    output_format = "html_document",
  #  envir = new.env(),     
    output_file = paste0(
      here::here("R/"),
      participant_id_date,
      "exam_review.html"
      )
  )
}

#

replace_mediafile <- function(file) {
  replace_mediafile_ <- function(file, media_src = c("img", "audio", "video")) {
    rlang::arg_match(media_src)
    stringr::str_replace_all(
      string = file,
      pattern = glue::glue("<{media_src} src=[\"'](.+?)[\"'].*?>"),
      replacement = here::here("data/Shiny/\\1")
    )
  }
  replace_mediafile_(file, media_src = "img") %>% 
    replace_mediafile_(., media_src = "audio") %>% 
    replace_mediafile_(., media_src = "video")
}


html_tag_audio <- function(file) {
  file_ext_rgx <- stringr::regex("[\\/a-zA-Z0-9äöüÄÖÜ_-]*?\\.mp3|ogg|wav")
  file_extension <- str_match(file, file_ext_rgx)
  audio_validation_rgx <- stringr::regex("(([\\/a-zA-Z0-9äöüÄÖÜ_-]*?)\\.(mp3|ogg|wav))")
  stringr::str_replace_all(
    string = file,
    pattern = audio_validation_rgx,
    replacement = glue::glue('<audio controls> <source src="\\1" type="audio/{file_extension}"/></audio>')
  )
}

html_tag_video <- function(file) {
  file_ext_rgx <- regex(".*\\.(webm|mp4|mov)")
  file_extension <- str_replace_all(file, file_ext_rgx, "\\1")
  video_validation_rgx <- stringr::regex("(([\\/a-zA-Z0-9äöüÄÖÜ_-]*?)\\.(webm|mp4|mov))")
  stringr::str_replace_all(
    string = file,
    pattern = video_validation_rgx,
    replacement = glue::glue('<video controls>  <source src="\\1" type="video/{file_extension}"/></video>')
  )
}

html_tag_img <- function(file) {
  image_validation_rgx <- stringr::regex("(([\\/a-zA-Z0-9äöüÄÖÜ_-]*?)\\.(svg|pdf|png|jpeg))")
  stringr::str_replace_all(
    string = file,
    pattern = image_validation_rgx,
    replacement = glue::glue('<img src="\\1">')
  )
}

format_table <- function(.data) {
  gt::gt(.data) %>% 
    fmt_markdown(columns = everything())
}
