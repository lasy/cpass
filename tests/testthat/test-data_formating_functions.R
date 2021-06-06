test_that("format 'cycle_start' works", {
  sim_data =
    tidyr::expand_grid(
      SUBJECT = 1, ITEM = 1:24,
      date = seq(as.Date("2020-01-01"),as.Date("2020-03-05"), by = 1)) %>%
    dplyr::mutate(is_cycle_start =
                    dplyr::case_when(
                      date %in% as.Date(c("2020-01-15","2020-02-15")) ~ TRUE,
                      TRUE ~ FALSE
                    )
                  )

  set.seed(1)
  sim_data = sim_data %>%  dplyr::mutate(DRSP_score = sample(1:6, size = nrow(sim_data), replace = TRUE))
  formated_data = as_cpass_data(data = sim_data, sep_event = "ovulation",verbose = FALSE)
  cycles = unique(formated_data$CYCLE) %>% sort()
  expect_equal(cycles, 0:2)

  dx = CPASS(formated_data, silent = TRUE)
  expect_equal(dx$SUBJECT_level_diagnosis$dx, NA_character_)
})


test_that("format 'bleeding' works", {
  sim_data =
    tidyr::expand_grid(
      SUBJECT = 1, ITEM = 1:24,
      date = seq(as.Date("2020-01-01"),as.Date("2020-03-05"), by = 1)) %>%
    dplyr::mutate(
      bleeding =
        dplyr::case_when(
          date %in% as.Date(c("2020-01-15","2020-02-15")) ~ "heavy",
          date %in% as.Date(c("2020-01-16","2020-01-17","2020-01-18",
                              "2020-02-16","2020-02-17","2020-02-18")) ~ "medium",
          date %in% as.Date(c("2020-01-19","2020-01-20",
                              "2020-02-19","2020-02-20")) ~ "light",
          date %in% as.Date(c("2020-01-14","2020-01-21",
                              "2020-02-14","2020-02-21")) ~ "spotting",
          TRUE ~ NA_character_
        )
    )

  set.seed(1)
  sim_data = sim_data %>%  dplyr::mutate(DRSP_score = sample(1:6, size = nrow(sim_data), replace = TRUE))
  formated_data = as_cpass_data(data = sim_data, sep_event = "ovulation",verbose = FALSE)
  cycles = unique(formated_data$CYCLE) %>% sort()
  expect_equal(cycles, 0:2)

  dx = CPASS(formated_data, silent = TRUE)
  expect_equal(dx$SUBJECT_level_diagnosis$dx, NA_character_)
})

