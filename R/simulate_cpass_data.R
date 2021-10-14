
#' Simulates CPASS data
#'
#' This function simulates DRSP data for \code{n_subjects}.
#' Options allow to choose the number of cycles per subject (\code{n_cycles}),
#' the increase in symptoms rating (\code{pmdd_intensity}), the baseline levels
#' severity of problems for subjects (\code{baseline}), the number of items
#' varying with the menstrual cycle (\code{n_items}), and the missingness rate
#' (\code{missingness_rate}), which is the fraction of days for which data are
#' missing.
#'
#' @param n_subjects (positive integer) The number of subjects for which data
#' need to be simulated
#' @param n_cycles (positive integer(s)) The number of cycles for each subject.
#' This parameter (as well as all following parameters) can be of length
#' \code{1} (and all subjects have the same number of cycles) or of length
#' \code{n_subjects} to determine the number of cycles independently
#' for each subject.
#' @param baseline (positive values in 1-6) Baseline levels for severity of
#' problems for all subjects (if \code{baseline} is of length \code{1}) or
#' each subject (if \code{baseline} is of length \code{n_subjects}).
#' @param pmdd_intensity change from baseline during the pre-menstrual phase.
#' @param n_items (positive integers in 1-24) number of items for which there is
#' a variation in the pre-menstrual phase. Can be of length \code{1}
#' (same \code{n_items} for all subjects) or of length \code{n_subjects}
#' (specific \code{n_items} for each subject).
#' @param missingness_rate (positive values in 0-0.9) The fraction of missing
#' days for all (if of length \code{1}) or each (if of length \code{n_subjects})
#' subject.
#'
#' @return a \code{data.frame} providing the DRSP scores for each subject,
#' cycle, cycle day (within the pre- and post- menstrual phase) and item.
#'
#' @seealso \code{simulate_cycle}
#' @importFrom purrr map_dfr
#' @import dplyr
#' @export
simulate_cpass_data <-
  function(
    n_subjects = 1,
    n_cycles = 1, # number of cycle per subject
    baseline = 1, # baseline levels
    pmdd_intensity = 0.5, # increase in symptoms intensity between pre and post
    n_items = 24, # number of items for which there is a pmdd increase
    missingness_rate = 0 # rate of missing days
  ) {

    if (n_subjects < 1) stop("'n_subjects' must be 1 or greater.\n")
    if (as.integer(n_subjects) != n_subjects) {
      warning("'n_subjects' must be a positive integer.
              Provided values rounded to the previous integer.\n")
      n_subjects <- floor(n_subjects)
    }

    # check lengths
    n_cycles <-
      .check_input_length(n_cycles, n_subjects, "n_cycles")
    pmdd_intensity <-
      .check_input_length(pmdd_intensity, n_subjects, "pmdd_intensity")
    baseline <-
      .check_input_length(baseline, n_subjects, "baseline")
    missingness_rate <-
      .check_input_length(missingness_rate, n_subjects, "missingness_rate")
    n_items <-
      .check_input_length(n_items, n_subjects, "n_items")

    # check values
    if (any((n_cycles <= 0) | (n_cycles  != as.integer(n_cycles))))
      stop("`n_cycles` must be positive integers.\n")
    if(any((missingness_rate < 0) | (missingness_rate > 0.9)))
      stop("`missingness_rate` must be a double between 0 and 0.9.\n")
    if(any((n_items < 1) | (n_items > 24) | (n_items != as.integer(n_items))))
      stop("`n_items` must be an integer between 1 and 24.\n")

    map_dfr(
      .x = 1:n_subjects,
      .f = function(subject_id){
        map_dfr(
          .x = 1:n_cycles[subject_id],
          .f = simulate_cycle,
          pmdd_intensity = pmdd_intensity[subject_id],
          baseline = baseline[subject_id],
          items = sample(1:24, n_items[subject_id]),
          missingness_rate = missingness_rate[subject_id]
        ) %>%
          mutate(subject = subject_id)
      }
    ) %>%
      select(
        subject, cycle, phase, day, item, drsp_score
      ) %>%
      pivot_wider(
        names_from = item,
        values_from = drsp_score,
        names_prefix = "item_"
      )
  }



#' Simulates CPASS data for a single cycle.
#'
#' This function simulates DRSP data for a single cycle
#'
#' @param cycle_nb (positive integer) The cycle number.
#' @param baseline (positive value in 1-6) Baseline level for severity of
#' problems for all items.
#' @param pmdd_intensity change from baseline during the pre-menstrual phase.
#' @param items (positive integers in 1-24) items for which there is
#' a variation in the pre-menstrual phase.
#' @param missingness_rate (positive value in 0-0.9) The fraction of missing
#' days.
#'
#' @return a \code{data.frame} providing the DRSP scores for each cycle day
#' (within the pre- and post- menstrual phase) and item.
#'
#' @seealso \code{simulate_cycle}
#' @import dplyr
#' @import tidyr
#' @export
simulate_cycle <-
  function(cycle_nb, baseline, pmdd_intensity, items, missingness_rate){
    data <-
      expand_grid(
        cycle = cycle_nb,
        bind_rows(
          expand_grid(
            phase = "pre",
            day = -7:-1
          ),
          expand_grid(
            phase = "post",
            day = 4:10
          )
        )
      )
    data <-
      data %>%
      mutate(is_missing = runif(nrow(data),0, 1) < missingness_rate)
    data <-
      expand_grid(
        data,
        item = 1:24
      )

    data <-
      data %>%
      mutate(
        drsp_score =
          baseline +
          rnorm(n = nrow(data), sd = 0.5)
      )
    data <-
      data %>%
      mutate(
        drsp_score =
          case_when(
            (phase == "pre") &
              (item %in% items) ~
              (drsp_score + pmdd_intensity * 5),
            TRUE ~ drsp_score
          ),
        drsp_score =
          drsp_score %>% round() %>% pmin(6) %>% pmax(1),
      )
    # we replace the scores by NA where data is missing
    data <-
      data %>%
      mutate(
        drsp_score = ifelse(is_missing, NA, drsp_score)
      )
    # line below ensures that the range of each subject is 5
    j = sample(which(!data$is_missing), 2)
    data$drsp_score[j[1]] = 6
    data$drsp_score[j[2]] = 1
    data
  }


.check_input_length <- function(x, l, name){
  if (!(length(x) %in% c(1, l)))
    stop(paste0("'",name,"' must be a single number or
           a vector of length 'n_subjects'\n"))
  if (length(x) != l)
    x <- rep(x[1], l)
  x
}
