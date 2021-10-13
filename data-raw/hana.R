hana <- readr::read_lines("data-raw/hana.txt")
hana <- stringi::stri_split_boundaries(hana, type = "sentence") %>%
  purrr::flatten_chr()
hana <- stringi::stri_trans_nfkc(hana)
hana <- stringr::str_remove_all(hana, "[[:blank:]|\\.]+") # 三点リーダは半角ピリオドになる
hana <- hana[3:163]
usethis::use_data(hana, overwrite = TRUE)
