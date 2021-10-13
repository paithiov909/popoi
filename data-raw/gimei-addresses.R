yaml <- (function() {
  con <- url(
    "https://raw.githubusercontent.com/willnet/gimei/main/lib/data/addresses.yml",
    open = "r",
    encoding = "UTF-8"
  )
  on.exit(close(con))
  return(yaml::read_yaml(con))
})()

pref <-
  purrr::map_dfr(
    yaml$addresses$prefecture,
    ~ data.frame(
      type = "prefecture",
      kanji = .[1],
      hiragana = .[2],
      katakana = .[3]
    )
  )

city <-
  purrr::map_dfr(
    yaml$addresses$city,
    ~ data.frame(
      type = "city",
      kanji = .[1],
      hiragana = .[2],
      katakana = .[3]
    )
  )

town <-
  purrr::map_dfr(
    yaml$addresses$town,
    ~ data.frame(
      type = "town",
      kanji = .[1],
      hiragana = .[2],
      katakana = .[3]
    )
  )

library(magrittr)

GimeiAddresses <-
  dplyr::bind_rows(pref, city, town) %>%
  dplyr::mutate(
    type = as.factor(type)
  )

usethis::use_data(GimeiAddresses, overwrite = TRUE)
