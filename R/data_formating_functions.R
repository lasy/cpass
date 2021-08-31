#' Supported data format for the \code{as_cpass_data} function.
#'
#' This function prints (or return) a list of the format currently
#' supported by the \code{cpass} package.
#' @param as_list (optional, default = \code{FALSE}) logical specifying if
#' the supported format should be printed to the console
#' or returned as a list object.
#' @keywords CPASS C-PASS PMDD MRMD
#' @return a list if \code{as_list = TRUE}.
#' @export
#' @importFrom magrittr %>%
#' @examples
#' supported_data_format()
#'
supported_data_format <- function(as_list = FALSE) {
  day_identifying_columns <- list(
    cpass_format = list(
      columns = c("cycle", "day"),
      description =
        list("cycle is the cycle number",
             "day is counted forward (1,2,3,...) for the post-menses phase;
             backward (-1,-2,...) for the pre-menses one.")
    ),
    first_day = list(
      columns = c("date", "is_cycle_start"),
      description =
        list(
          "`date` is an actual date
          (use 'as.Date' to convert character strings to dates)",
          "`is_cycle_start` is a logical which is TRUE
          if a menstrual cycle starts on the specified date."
        )
    ),
    bleeding = list(
      columns = c("date", "bleeding"),
      description =
        list(
          "`date` is an actual date
          (use 'as.Date' to convert character strings to dates)",
          "`bleeding` is a character describing vaginal bleeding.
          It can take the following values:
          'none','spotting','light', 'medium', 'heavy'.
          Missing values (NAs) are accepted."
        )
    )
  )

  not_yet_supported_format <-
    list(
      fw = list(
        columns = c("natural_cycle_number", "fw_day", "cycle_length"),
        description = ""
      ),
      bw = list(
        columns = c("natural_cycle_number", "bw_day", "cycle_length"),
        description = ""
      ),
      fw_bw = list(
        columns = c("natural_cycle_number", "fw_day", "bw_day"),
        description = ""
      )
    )

  if (as_list) return(day_identifying_columns)

  for (i in seq_along(day_identifying_columns)) {
    cat(paste0("\n", i, ") ",
               names(day_identifying_columns)[i] %>%
                 stringr::str_replace("_", " ") %>%
                 stringr::str_to_upper(), "\n"))
    cat("     columns: ",
        stringr::str_c(day_identifying_columns[[i]]$columns, collapse = ", "),
        "\n")
    cat("     description:\n")
    for (j in seq_along(day_identifying_columns[[i]]$description))
      cat("     * ", day_identifying_columns[[i]]$description[[j]], "\n")
    cat("\n")
  }
  invisible(day_identifying_columns)
}



#' Transforms into a "cpass" data.frame.
#'
#' This function checks if the data can be used by the CPASS functions
#' and transforms it into a "cpass" data.frame.
#' @param data a data.frame that contains symptoms reported subjects.
#'     The data must be in a long format,
#'     _i.e._ one row per subject, day (date or cycle + cycleday) and DRSP item.
#'     The data.frame must have the following columns:
#'     \code{subject}, \code{item}, \code{drsp_score}
#'     as well as two columns defining time.
#'     These two columns can be
#'     \code{cycle} + \code{day},
#'     \code{date} + \code{is_cycle_start}, or
#'     \code{date} + \code{bleeding}.
#'     See \code{supported_data_format} for details.
#' @param sep_event (required) character (\code{"menses"} or \code{"ovulation"})
#' specifying whether the menstrual event separating the two phases of interest
#' (the "pre" and "post" weeks) is the menses or ovulation.
#' If the menses is the separating event,
#' the week preceding the menstruation ("pre") is compared
#' to the week following the menstruation ("post").
#' In that case, the "pre" and the "post" week do NOT belong to the same cycle.
#' If ovulation is the separating event,
#' then the two phases belong to the same cycle.
#' @param silent (optional) logical, default = \code{FALSE}.
#' Should warning messages be printed?
#' @param verbose (optional) logical.
#' Should the function print additional information?
#' (mostly used for debugging purposes)
#' @keywords CPASS C-PASS PMDD MRMD
#' @return a data.frame.
#' @export
#' @importFrom magrittr %>%
#' @examples
#' random_data =
#'    expand.grid(
#'        subject = 1,
#'        cycle = 1:2,
#'        day = c(1:10,-10:-1),
#'        item = 1:24
#'        )
#' random_data$drsp_score = sample(1:6, nrow(random_data), replace = TRUE)
#' cpass_data = as_cpass_data(random_data, sep_event = "menses")
#' colnames(cpass_data)

as_cpass_data <-
  function(data, sep_event = NULL, silent = FALSE, verbose = TRUE) {

    d2 <-  data

    # 1. CHECKS

    # sep_event check
    possible_sep_event <-  c("ovulation", "menses")
    sep_event <-  sep_event[1]
    if (is.null(sep_event)) stop("'sep_event' must be specified.\n")
    sep_event <-  match.arg(sep_event, possible_sep_event)

    # data check: checking that all necessary columns are present
    required_columns <-  c("subject", "item", "drsp_score")
    if (!all(required_columns %in% colnames(d2)))
      stop(stringr::str_c("data must contain the following columns: ",
                          stringr::str_c(required_columns, collapse = "; ")))


    # data check: checking that time-stamp columns are present
    format <-  "error" # we initialize it to error
    # until data match a specific format in the loop below
    formats <-  supported_data_format(as_list = TRUE)
    for (f in names(formats)) {
      if (all(formats[[f]]$columns %in% colnames(d2))) { format <- f; break }
    }

    if (format == "error") {
      error_message <-
        paste0("cycles and cycledays are not formatted properly.\n",
               "Use the function `supported_data_format()` to check",
               "how to format your data.\n",
               "Note that R is case-sentitive.\n")
      stop(error_message)
    }

    if (verbose & (format != "cpass_format"))
      cat(
        paste0(
          "Using columns ",
          paste0(paste0("'", formats[[format]]$columns, "'"), collapse = ", "),
          " (format '", format, "') to define 'cycle' and 'day'.\n"
        )
      )

    # if format is different than cpass,
    # transform into cycle and day with sep_event (not implemented yet)

    if (format == "first_day") {
      d2 <-  .compute_phase_and_cycleday(data = d2, sep_event = sep_event)
    }
    if (format == "bleeding") {
      d2 <-  .identify_cycles_from_bleeding(data = d2)
      d2 <-  .compute_phase_and_cycleday(data = d2, sep_event = sep_event)
    }

    # CHECKING THE VALUES OF THE COLUMNS
    # cycle
    if ((!all(d2$cycle >= 0)) | (!all((d2$cycle %% 1) == 0)))
      stop("Numbers in the cycle column must be positive integers.\n")
    # day
    if (!all((d2$day %% 1) == 0, na.rm = TRUE))
      stop("Numbers in the day column must be integers.\n")
    if (any(d2$day == 0, na.rm = TRUE))
      stop(
        paste0("day must be counted forward from 1 ",
               "(day 1 = first day of the menses)",
               "and backward from -1 (day - 1 = last day before the menses).",
               "There is no day 0.\n"))
    if (all(d2$day > 0, na.rm = TRUE))
      warning(
        paste0("There are no negative days.\n",
               "This will be interpreted as ",
               "no data for the pre-menstrual phase.\n",
               "CPASS diagnosis cannot be ran."))
    # drsp
    if (!all(unique(d2$item) %in% 1:24))
      stop("item must be integers from 1:24.")
    # drsp_score
    if (!all(unique(d2$drsp_score[!is.na(d2$drsp_score)]) %in% 1:6))
      stop("Values of 'drsp_score' must be in 1:6.")

    # subjectS
    d2 <- d2 %>% dplyr::filter(!is.na(.data$subject))
    if (verbose)
      cat("Number of subjects: ", length(table(d2$subject)), "\n")

    # cycleS
    if (verbose)
      cat("Total number of cycles: ",
          length(table(paste0(d2$subject, "-", d2$cycle))), "\n")



    # FILLING MISSING VALUES
    d3 <-  tidyr::expand_grid(
      d2 %>% dplyr::select(subject, cycle) %>% dplyr::distinct(),
      day = c(-7:-1, 4:10),
      item = dsm5_dict$ITEM)

    d2 <-
      dplyr::left_join(
        d3, d2, by = c("subject", "cycle", "day", "item")
      )

    if (verbose)
      cat("Percentage of missing scores: ",
          round(mean(is.na(d2$drsp_score)) * 100, 2), "%\n")

    # DEFINING THE phase
    if ("phase" %in% colnames(d2) & !silent)
      warning("The 'phase' column will be over-written\n")

    d2 <-  d2 %>%
      dplyr::mutate(
        phase = dplyr::case_when(
          day %in% -7:-1 ~ "pre-menses",
          day %in% 4:10 ~ "post-menses",
          day %in% 1:3 ~ "menses",
          TRUE ~ "peri-ovulation"
        )
      )

    if (verbose)
      cat("Percentage of missing scores (in pre- & post-menstrual phases): ",
          d2$drsp_score[d2$phase %in% c("pre-menses", "post-menses")] %>%
            mean() %>%
            magrittr::multiply_by(100) %>%
            round(., 2), "%\n")


    if (sep_event == "menses") {
      phase_levels <- c("pre-menses", "menses", "post-menses", "peri-ovulation")
    }else{
      phase_levels <- c("menses", "post-menses", "peri-ovulation", "pre-menses")
    }
    d2 <-
      d2 %>%
      dplyr::mutate(phase = factor(phase, levels = phase_levels))


    d2 <-
      d2 %>%
      dplyr::select(subject, cycle, phase, day, item, drsp_score) %>%
      dplyr::arrange(subject, cycle, phase, day, item)
    d2
  }

#' Checks if data is a "CPASS" data.frame.
#'
#' @param data a data.frame that contains symptoms reported subjects.
#' The data must be in a long format and have the following columns:
#' \code{subject}, \code{cycle}, \code{day}, \code{DRSP}, \code{drsp_score}
#' @param silent (optional) logical, default = \code{TRUE}.
#' Should warning messages be printed?
#' @keywords CPASS C-PASS PMDD MRMD
#' @return a logical.
#' @export
#' @importFrom magrittr %>%
#' @examples
#' random_data =
#'   expand.grid(
#'      subject = 1,
#'      cycle = 1:2,
#'      day = c(1:10,-10:-1),
#'      item = 1:24)
#' random_data$drsp_score = sample(1:6, nrow(random_data), replace = TRUE)
#' cpass_data = as_cpass_data(random_data, sep_event = "menses")
#' colnames(cpass_data)
#' is_cpass_data(random_data)
#' is_cpass_data(cpass_data)

is_cpass_data <- function(data, silent = TRUE) {

  if (!any("phase" %in% colnames(data))) return(FALSE)

  if (!is.factor(data$phase)) return(FALSE)

  sep_event <-
    ifelse(levels(data$phase)[1] == "pre-menses", "menses", "ovulation")

  ok <-
    try(
      as_cpass_data(
        data = data,
        sep_event = sep_event,
        silent = silent,
        verbose = FALSE),
      silent = TRUE)

  if (
    (class(ok)[1] != "try-error") &&
    all.equal(ok, data)
  ) TRUE else FALSE

}




#' Compute cycledays and phases from dates and provided starts of cycles.
#'
#' @param data a data.frame that contains symptoms reported subjects.
#' The data must be in a long format and have the following columns:
#' \code{subject}, \code{item}, \code{drsp_score}
#' @param sep_event a character XXX
#' @keywords CPASS C-PASS PMDD MRMD
#' @return the same data.frame as data with additional columns.
#' @export
#' @import magrittr
#' @examples
#' random_data =
#'     expand.grid(
#'         subject = 1,
#'         item = 1:24,
#'         date = seq(as.Date("2020-01-01"), as.Date("2020-03-01"), by = 1)
#'         )
#' random_data$drsp_score <- sample(1:6, nrow(random_data), replace = TRUE)
#' random_data$is_cycle_start <-
#'     ifelse(
#'        random_data$date %in%
#'          c(as.Date("2020-01-15"), as.Date("2020-02-20")),
#'        TRUE, FALSE)
#' augmented_data <-
#'    .compute_phase_and_cycleday(random_data, sep_event = "ovulation")
#' colnames(augmented_data)
.compute_phase_and_cycleday <- function(data, sep_event) {
  d <- data
  d <- d %>%
    dplyr::arrange(subject, date, is_cycle_start) %>%
    dplyr::group_by(subject, date) %>%
    dplyr::summarize(is_cycle_start = any(is_cycle_start),
                     .groups = "drop") %>%
    dplyr::group_by(subject) %>%
    dplyr::mutate(natural_cycle_nb = cumsum(is_cycle_start),
                  n_cycles = max(natural_cycle_nb)) %>%
    dplyr::group_by(subject, natural_cycle_nb) %>%
    dplyr::mutate(cycle_start_date =
                    ifelse(natural_cycle_nb > 0, min(date), NA),
                  cycle_end_date =
                    ifelse(natural_cycle_nb < n_cycles, max(date), NA),
                  cycleday_fw =
                    (date - cycle_start_date) %>%
                    as.numeric(., units = "day") %>%
                    magrittr::add(1),
                  cycleday_bw =
                    (date - cycle_end_date) %>%
                    as.numeric(., units = "day") %>%
                    magrittr::add(-1)
    ) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(
      day =
        ifelse(
          (cycleday_bw %in% -14:-1) | is.na(cycleday_fw),
          cycleday_bw,
          cycleday_fw),
      menses_centered_cycle_nb =
        ifelse((day %in% -14:-1),
               natural_cycle_nb + 1,
               natural_cycle_nb))

  if (sep_event == "ovulation")
    d$cycle <- d$natural_cycle_nb else d$cycle <- d$menses_centered_cycle_nb

  d <-
    d %>%
    dplyr::select(subject, date, cycle, day,
                  natural_cycle_nb, menses_centered_cycle_nb,
                  cycleday_fw, cycleday_bw)

  data <- data %>%
    dplyr::left_join(., d, by = c("subject", "date")) %>%
    dplyr::select(subject, cycle, day, item, drsp_score,
                  date, is_cycle_start,
                  natural_cycle_nb, menses_centered_cycle_nb,
                  cycleday_fw, cycleday_bw)

  data
}




#' @importFrom  magrittr %>%
.identify_cycles_from_bleeding <- function(data) {

  d <-  data
  d <- data %>%
    dplyr::group_by(subject, date) %>%
    dplyr::mutate(
      bleeding = bleeding %>%
        factor(.,
               levels =
                 cycle_model$marg_em_probs$bleeding$params$values)
    ) %>%
    dplyr::summarize(bleeding = sort(unique(bleeding))[1], .groups = "drop")

  data4hsmm <-
    d %>%
    dplyr::mutate(
      seq_id = subject %>% as.character()
    ) %>%
    dplyr::group_by(seq_id) %>%
    dplyr::mutate(
      min_date = min(date),
      t = (date - min_date) %>% as.numeric(., unit = "days")
    ) %>%
    dplyr::ungroup()

  vit <-
    HiddenSemiMarkov::predict_states_hsmm(
      model = cycle_model, X = data4hsmm, method = "Viterbi")
  fwbw <-
    HiddenSemiMarkov::predict_states_hsmm(
      model = cycle_model, X = data4hsmm, method = "FwBw")

  decoding <-
    vit$state_seq %>%
    dplyr::select(seq_id, t, state) %>%
    dplyr::mutate(
      seq_id = seq_id %>% as.character()
    ) %>%
    dplyr::left_join(
      fwbw$probabilities %>%
        dplyr::select(seq_id, t, state, state_prob) %>%
        dplyr::mutate(seq_id = seq_id %>% as.character()),
      by = c("seq_id", "t", "state")) %>%
    dplyr::mutate(is_cycle_start = (state == 1))

  data4hsmm <-
    data4hsmm %>% dplyr::left_join(., decoding, by = c("seq_id", "t"))

  data <- data %>%
    dplyr::left_join(
      .,
      data4hsmm %>% dplyr::select(subject, date, is_cycle_start),
      by = c("subject", "date"))

  data
}
