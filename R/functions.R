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


# dynamisch Erstellen mit case_when

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
    replacement = glue::glue('<video width="600px" controls> <source src="\\1" type="video/{file_extension}"/></video>')
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
  kableExtra::kbl(
    x = .data, 
    format = "html", 
    escape = FALSE,
    col.names = c("","")
  ) %>%
    kableExtra::kable_styling(
      full_width = FALSE,
      position = "center"
    )  %>% 
    kableExtra::column_spec(
      column = 1,
      width = "8cm"
    ) %>% 
    kableExtra::column_spec(
      column = 2,
      width = "16cm"
    ) 
}

# function to render the exam review script:
render_review <- function(filename_of_data) {
  # extract participant id and date for file naming:
  participant_id_date <- filename_of_data %>% 
    basename() %>% 
    str_remove_all("WS.RData")
  
  rmarkdown::render(
    input = here::here("R/exam_review.Rmd"),
    params = list(data = filename_of_data),
    output_format = "html_document",
    output_dir = here("out"),
    output_file = glue::glue('{here::here("R/")}{participant_id_date}exam_review.html')
  )
}


