yaml <- (function() {
  con <- url(
    "https://raw.githubusercontent.com/willnet/gimei/main/lib/data/names.yml",
    open = "r",
    encoding = "UTF-8"
  )
  on.exit(close(con))
  return(yaml::read_yaml(con))
})()

first_name_male <-
  purrr::map_dfr(
    yaml$first_name$male,
    ~ data.frame(
      type = "first",
      gender = "male",
      kanji = .[1],
      hiragana = .[2],
      katakana = .[3]
    )
  )

first_name_female <-
  purrr::map_dfr(
    yaml$first_name$female,
    ~ data.frame(
      type = "first",
      gender = "female",
      kanji = .[1],
      hiragana = .[2],
      katakana = .[3]
    )
  )

last_name <-
  purrr::map_dfr(
    yaml$last_name,
    ~ data.frame(
      type = "last",
      gender = NA_character_,
      kanji = .[1],
      hiragana = .[2],
      katakana = .[3]
    )
  )

library(magrittr)

GimeiNames <-
  dplyr::bind_rows(first_name_male, first_name_female, last_name) %>%
  dplyr::mutate(
    type = as.factor(type),
    gender = as.factor(gender)
  )

usethis::use_data(GimeiNames, overwrite = TRUE)
