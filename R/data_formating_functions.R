
supported_data_format = function(as.list = FALSE){
  day_identifying_columns = list(
    cpass_format = list(
      columns = c("CYCLE","DAY"),
      description = "CYCLE is the cycle number and DAY is counted forward for the post-menses period and backward for the pre-menses period."
    ),
    first_day = list(
      columns = c("date","is_cycle_start"),
      description = "`date` is an actual date (use the function 'as.Date' to convert character strings to dates) and `is_cycle_start` is a logical which is TRUE if a menstrual cycle starts on the specified date."
    ),
    bleeding = list(
      columns = c("date","bleeding"),
      description = "`date` is an actual date (use the function 'as.Date' to convert character strings to dates) and `bleeding` is a character describing vaginal bleeding and which can take the following values: 'none','spotting','light', 'medium', 'heavy'. Missing values are accepted."
    )
  )

  not_yet_supported_format =
    list(
    fw = list(
      columns = c("natural_cycle_number","fw_day","cycle_length"),
      description = ""
    ),
    bw = list(
      columns = c("natural_cycle_number","bw_day","cycle_length"),
      description = ""
    ),
    fw_bw = list(
      columns = c("natural_cycle_number","fw_day","bw_day"),
      description = ""
    )
  )

  if(as.list) return(day_identifying_columns)

  for(i in 1:length(day_identifying_columns)){
    cat(paste0("\n",i,") ",
               names(day_identifying_columns)[i] %>% stringr::str_replace("_"," ") %>%
                 stringr::str_to_upper(),"\n"))
    cat("     columns: ", stringr::str_c(day_identifying_columns[[i]]$columns, collapse = ", "),"\n")
    cat("     desc: ", day_identifying_columns[[i]]$description,"\n\n")
  }
  invisible(day_identifying_columns)
}




#' Transforms into a "cpass" data.frame.
#'
#' This function checks if the data can be used by the CPASS functions and transforms it into a "cpass" data.frame.
#' @param data a data.frame that contains symptoms reported subjects.
#'     The data must be in a long format, _i.e._ one row per subject, day (date or cycle + cycleday) and DRSP item.
#'     The data.frame must have the following columns: \code{SUBJECT}, \code{ITEM}, \code{DRSP_score} as well as two columns defining time.
#'     These two columns can be \code{CYCLE} + \code{DAY} or \code{date} + \code{is_cycle_start}.
#' @param sep_event (required) character (\code{"menses"} or \code{"ovulation"}) specifying whether the menstrual event separating the two phases of interest ("pre" and "post" week) is the menses or ovulation. If the menses is the separating event, the week preceding the menstruation ("pre") is compared to the week following the menstruation ("post"). In that case, the "pre" and the "post" week do NOT belong to the same cycle. If ovulation is the separating event, then the two phases belong to the same cycle.
#' @param silent (optional) logical, default = \code{FALSE}. Should warning messages be printed?
#' @param verbose (optional) logical. Should the function print additional information? (mostly used for debugging purposes)
#' @keywords CPASS C-PASS PMDD MRMD
#' @return a data.frame.
#' @export
#' @importFrom magrittr %>%
#' @examples
#' random_data = expand.grid(SUBJECT = 1, CYCLE = 1:2, DAY = c(1:10,-10:-1), ITEM = 1:24)
#' random_data$DRSP_score = sample(1:6, nrow(random_data), replace = TRUE)
#' cpass_data = as_cpass_data(random_data, sep_event = "menses")
#' colnames(cpass_data)

as_cpass_data = function(data, sep_event = NULL, silent = FALSE, verbose = TRUE){

  d2 = data

  # 1. CHECKS

  # sep_event check
  possible_sep_event = c("ovulation","menses")
  sep_event = sep_event[1]
  if(is.null(sep_event)){stop("'sep_event' must be specified.\n")}
  sep_event = match.arg(sep_event, possible_sep_event)

  # data check: checking that all necessary columns are present
  required_columns = c("SUBJECT", "ITEM", "DRSP_score")
  if(!all(required_columns %in% colnames(d2))){
    stop(stringr::str_c("data must contain the following columns: ",
                        stringr::str_c(required_columns, collapse = "; ")))}


  # data check: checking that time-stamp columns are present
  format = "error"
  formats = supported_data_format(as.list = TRUE)
  for(f in names(formats)){
    #cat(f,"\n")
    if(all(formats[[f]]$columns %in% colnames(d2))){format = f; break}
  }


  if(format == "error"){
    error_message =
      paste0("Cycles and cycledays are not formatted properly.\n",
             "Please use the function `supported_data_format()` to check how to format your data.\n")
    stop(error_message)
  }

  if(verbose & (format != "cpass_format"))
    cat(paste0("Using columns ",paste0( paste0("'",formats[[format]]$columns,"'"),collapse = ", "),
               " (format '",format,"') to define 'CYCLE' and 'DAY'.\n"))

  # if format is different than cpass, transform into CYCLE and DAY with sep_event (not implemented yet)
  if(format != "cpass_format"){
    if(format == "first_day"){
      d2 = .compute_phase_and_cycleday(data = d2, sep_event = sep_event)
    }
    if(format == "bleeding"){
      d2 = .identify_cycle_starts_from_bleeding(data = d2)
      d2 = .compute_phase_and_cycleday(data = d2, sep_event = sep_event)
    }
    if(!(format %in% c("first_day","bleeding"))){
      stop(
        paste0("The format '",format,
               "' (", paste0(formats[[format]]$columns, collapse = ", "),
               ") has not been implemented yet. Please provide processed columns 'CYCLE' and 'DAY'.")
      )
    }
  }




  # CHECKING THE VALUES OF THE COLUMNS
  # cycle
  if((!all(d2$CYCLE>=0))|(!all((d2$CYCLE%%1) == 0))) stop("Numbers in the CYCLE column must be positive integers.\n")
  # day
  if(!all((d2$DAY%%1) == 0, na.rm = TRUE)) stop("Numbers in the DAY column must be integers.\n")
  if(any(d2$DAY == 0, na.rm = TRUE)) stop("DAY must be counted forward from 1 (day 1 = first day of the menses) and backward from -1 (day - 1 = last day before the menses). There is no day 0.\n")
  if(all(d2$DAY>0, na.rm = TRUE)) warning("There are no negative DAY, which will be interpreted as no data for the pre-menstrual phase.")
  # drsp
  if(!all(unique(d2$ITEM) %in% 1:24)) stop("ITEM must be integers from 1:24")
  # DRSP_score
  if(!all(unique(d2$DRSP_score[!is.na(d2$DRSP_score)]) %in% 1:6)) stop("Values of 'DRSP_score' must be in 1:6")

  # SUBJECTS
  d2 = d2 %>% dplyr::filter(!is.na(SUBJECT))
  if(verbose) cat("Number of SUBJECTS: ", length(table(d2$SUBJECT)),"\n")

  # CYCLES
  if(verbose) cat("Total number of CYCLES: ", length(table(paste0(d2$SUBJECT,"-",d2$CYCLE))),"\n")



  # FILLING MISSING VALUES
  d3 = tidyr::expand_grid(d2 %>% dplyr::select(SUBJECT, CYCLE) %>% dplyr::distinct(),
                          DAY = c(-7:-1, 4:10),
                          ITEM = dsm5_dict$ITEM)

  d2 = dplyr::left_join(d3, d2, by = c("SUBJECT", "CYCLE", "DAY", "ITEM"))

  # DEFINING THE PHASE
  if("PHASE" %in% colnames(d2) & !silent) warning("The 'PHASE' column will be over-written\n")
  d2 = d2 %>%
    dplyr::mutate(
      PHASE = dplyr::case_when(
        DAY %in% -7:-1 ~ "pre-menses",
        DAY %in% 4:10 ~ "post-menses",
        DAY %in% 1:3 ~ "menses",
        TRUE ~ "peri-ovulation"
      )
    )

  if(sep_event == "menses"){
    phase_levels = c("pre-menses","menses","post-menses","peri-ovulation")
  }else{
    phase_levels = c("menses","post-menses","peri-ovulation","pre-menses")
  }
  d2 = d2 %>% dplyr::mutate(PHASE = factor(PHASE, levels = phase_levels))


  d2 = d2 %>% dplyr::select(SUBJECT, CYCLE, PHASE, DAY, ITEM, DRSP_score)
  d2
}

#' Checks if data is a "CPASS" data.frame.
#'
#' @param data a data.frame that contains symptoms reported subjects. The data must be in a long format and have the following columns: \code{SUBJECT}, \code{CYCLE}, \code{DAY}, \code{DRSP}, \code{DRSP_score}
#' @param silent (optional) logical, default = \code{FALSE}. Should warning messages be printed?
#' @keywords CPASS C-PASS PMDD MRMD
#' @return a logical.
#' @export
#' @importFrom magrittr %>%
#' @examples
#' random_data = expand.grid(SUBJECT = 1, CYCLE = 1:2, DAY = c(1:10,-10:-1), ITEM = 1:24)
#' random_data$DRSP_score = sample(1:6, nrow(random_data), replace = TRUE)
#' cpass_data = as_cpass_data(random_data, sep_event = "menses")
#' colnames(cpass_data)
#' is_cpass_data(random_data)
#' is_cpass_data(cpass_data)

is_cpass_data = function(data, silent = FALSE){
  ok = try(as_cpass_data(data = data, silent = silent, verbose = FALSE), silent = TRUE)
  if(
    (class(ok)[1] != "try-error") &&
    all(colnames(ok) %in% colnames(data))
  ) TRUE else FALSE
}




#' Compute cycledays and phases from dates and provided starts of cycles.
#'
#' @param data a data.frame that contains symptoms reported subjects. The data must be in a long format and have the following columns: \code{SUBJECT}, \code{ITEM}, \code{DRSP_score}
#' @param sep_event a character XXX
#' @keywords CPASS C-PASS PMDD MRMD
#' @return the same data.frame as data with additional columns.
#' @export
#' @import magrittr
#' @examples
#' random_data =
#'     expand.grid(
#'         SUBJECT = 1,
#'         ITEM = 1:24,
#'         date = seq(as.Date("2020-01-01"), as.Date("2020-03-01"), by = 1)
#'         )
#' random_data$DRSP_score = sample(1:6, nrow(random_data), replace = TRUE)
#' random_data$is_cycle_start = ifelse(random_data$date %in% c(as.Date("2020-01-15"), as.Date("2020-02-20")), TRUE, FALSE)
#' augmented_data = .compute_phase_and_cycleday(random_data, sep_event = "ovulation")
#' colnames(augmented_data)
.compute_phase_and_cycleday = function(data, sep_event){
  d = data
  d = d %>%
    dplyr::arrange(SUBJECT, date, is_cycle_start) %>%
    dplyr::group_by(SUBJECT, date) %>%
    dplyr::summarize(is_cycle_start = any(is_cycle_start), .groups = "drop") %>%
    dplyr::group_by(SUBJECT) %>%
    dplyr::mutate(natural_cycle_nb = cumsum(is_cycle_start),
                  n_cycles = max(natural_cycle_nb)) %>%
    dplyr::group_by(SUBJECT, natural_cycle_nb) %>%
    dplyr::mutate(cycle_start_date = ifelse(natural_cycle_nb > 0, min(date), NA),
                  cycle_end_date = ifelse(natural_cycle_nb < n_cycles, max(date), NA),
                  cycleday_fw = (date - cycle_start_date) %>% as.numeric(., units = "day") %>% magrittr::add(1),
                  cycleday_bw = (date - cycle_end_date) %>% as.numeric(., units = "day") %>% magrittr::add(-1)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(
      DAY =
        ifelse(
          (cycleday_bw %in% -14:-1) | is.na(cycleday_fw),
          cycleday_bw ,
          cycleday_fw),
      menses_centered_cycle_nb =
        ifelse((DAY %in% -14:-1),
               natural_cycle_nb +1 ,
               natural_cycle_nb))

  if(sep_event == "ovulation") d$CYCLE = d$natural_cycle_nb else d$CYCLE = d$menses_centered_cycle_nb

  d =
    d %>%
    dplyr::select(SUBJECT, date, CYCLE, DAY,
                  natural_cycle_nb, menses_centered_cycle_nb,
                  cycleday_fw, cycleday_bw)

  data = data %>%
    dplyr::left_join(., d, by = c("SUBJECT","date")) %>%
    dplyr::select(SUBJECT, CYCLE, DAY, ITEM, DRSP_score,
                  date, is_cycle_start,
                  natural_cycle_nb, menses_centered_cycle_nb,
                  cycleday_fw, cycleday_bw)

  data
}





.identify_cycle_starts_from_bleeding = function(data){

  d = data
  d = data %>%
    dplyr::group_by(SUBJECT, date) %>%
    dplyr::mutate(bleeding = bleeding %>% factor(., levels = cycle_model$marg_em_probs$bleeding$params$values)) %>%
    dplyr::summarize(bleeding = sort(unique(bleeding))[1], .groups = "drop")

  data4hsmm =
    d %>%
    dplyr::mutate(seq_id = SUBJECT %>% as.character()) %>%
    dplyr::group_by(seq_id) %>%
    dplyr::mutate(min_date = min(date),
                  t = (date - min_date) %>% as.numeric(., unit = "days")) %>%
    dplyr::ungroup()

  X = data4hsmm %>%
    dplyr::select(seq_id, t, bleeding)

  vit = HiddenSemiMarkov::predict_states_hsmm(model = cycle_model, X = data4hsmm, method = "Viterbi")
  fwbw = HiddenSemiMarkov::predict_states_hsmm(model = cycle_model, X = data4hsmm, method = "FwBw")

  decoding =
    vit$state_seq %>%
    dplyr::select(seq_id, t, state) %>%
    dplyr::mutate(seq_id = seq_id %>% as.character()) %>%
    dplyr::left_join(fwbw$probabilities %>%
                       dplyr::select(seq_id, t, state, state_prob) %>%
                       dplyr::mutate(seq_id = seq_id %>% as.character()) ,
                     by = c("seq_id", "t", "state")) %>%
    dplyr::mutate(is_cycle_start = (state == 1))

  data4hsmm =
    data4hsmm %>% dplyr::left_join(., decoding, by = c("seq_id","t"))

  data = data %>%
    dplyr::left_join(., data4hsmm %>% dplyr::select(SUBJECT, date, is_cycle_start), by = c("SUBJECT","date"))

  data
}
