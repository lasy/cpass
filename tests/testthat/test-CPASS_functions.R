test_that("CPASS with missing ITEMs", {

  d0 = PMDD_data %>%
    dplyr::filter(SUBJECT %in% c(2, 25, 44))

  # d1 has only the core emotional items
  d1 = d0 %>%
    dplyr::filter(ITEM %in% 1:8)

  # d2 has only 6 ITEMs
  d2 = d0 %>% dplyr::filter(ITEM %in% 1:6)

  # d3 has random missing items
  set.seed(14)
  j = sample(1:nrow(d0), 200)
  d3 = d0[-j,]

  # We run the CPASS function on these three datasets
  cpass_d0 = CPASS(data = d0, sep_event = "menses", silent = TRUE)
  cpass_d1 = CPASS(data = d1, sep_event = "menses", silent = TRUE)
  cpass_d2 = CPASS(data = d2, sep_event = "menses", silent = TRUE)
  cpass_d3 = CPASS(data = d3, sep_event = "menses", silent = TRUE)

  # We check the results
  expect_equal(cpass_d0$SUBJECT_level_diagnosis$dx, c("PMDD","MRMD","no diagnosis"))
  expect_equal(cpass_d1$SUBJECT_level_diagnosis$dx, c("MRMD","MRMD","no diagnosis"))
  expect_equal(cpass_d2$SUBJECT_level_diagnosis$dx, c("MRMD","MRMD","no diagnosis"))
  expect_equal(cpass_d3$SUBJECT_level_diagnosis$dx, c("MRMD","no diagnosis","no diagnosis"))


  # We also need to check the visualizations
  # TO DO
})
