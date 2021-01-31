
supported_data_format = function(as.list = FALSE){
  day_identifying_columns = list(
    cpass_format = list(
      columns = c("CYCLE","DAY"),
      description = "CYCLE is the cycle number and DAY is counted forward for the post-menses period and backward for the pre-menses period."
    ),
    first_day = list(
      columns = c("DATE","IS_FIRST_DAY"),
      description = "DATE is an actual date (use the function 'as.Date' to convert character strings to dates) and IS_FIRST_DAY is a logical which is TRUE if a menstrual cycle starts on the specified date."
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
    ),
    bleeding = list(
      columns = c("date","bleeding"),
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
#'     These two columns can be \code{CYCLE} + \code{DAY} or \code{DATE} + \code{IS_FIRST_DAY}.
#' @param sep_event an optional character \code{"menses"} (default) or \code{"ovulation"} which defines if the pre- and post-menstrual phases are separated by the menses or by ovulation within a cycle (values of the column \code{CYCLE} in the provided data).
#' @param verbose (optional) logical. Should the function print additional information? (mostly used for debugging purposes)
#' @keywords CPASS C-PASS PMDD MRMD
#' @return a data.frame.
#' @export
#' @importFrom magrittr %>%
#' @examples
#' random_data = expand.grid(SUBJECT = 1, CYCLE = 1:2, DAY = c(1:10,-10:-1), ITEM = 1:24)
#' random_data$DRSP_score = sample(1:6, nrow(random_data), replace = TRUE)
#' cpass_data = as_cpass_data(random_data)
#' colnames(cpass_data)

as_cpass_data = function(data, sep_event = c("menses", "ovulation"), verbose = TRUE){

  d2 = data

  possible_sep_event = c("ovulation","menses")
  if(!any(sep_event %in% possible_sep_event)) stop(paste0("sep_event must be ",paste0("'",possible_sep_event,"'",collapse = " or ")))
  sep_event = sep_event[1]

  # CHECKING that all the necessary columns are present
  required_columns = c("SUBJECT", "ITEM", "DRSP_score")
  if(!all(required_columns %in% colnames(d2))){
    stop(stringr::str_c("data must contain the following columns: ",
                        stringr::str_c(required_columns, collapse = "; ")))}



  format = "error"
  formats = supported_data_format(as.list = TRUE)
  for(format in names(formats))
    if(all(formats[[format]]$columns %in% colnames(d2))) break

  if(format == "error"){
    error_message =
      paste0("Cycles and cycledays are not formatted properly.\n",
             "Please use the function `supported_data_format()` to check how to format your data.\n")
    stop(error_message)
  }

  if(verbose & (format != "cpass_format"))
    cat(paste0("Using columns ",paste0(formats[[format]]$columns,collapse = ", "),
               " (format '",format,"') to define 'CYCLE' and 'DAY'."))

  # if format is different than cpass, transform into CYCLE and DAY with sep_event (not implemented yet)
  if(format != "cpass_format")
    stop(
      paste0("The format '",format,
             "' (", paste0(formats[[format]]$columns, collapse = ", "),
             ") has not been implemented yet. Please provide processed columns 'CYCLE' and 'DAY'.")
    )

  # DEFINING THE PHASE
  d2 = d2 %>%
    dplyr::mutate(
      PHASE = dplyr::case_when(
        DAY %in% -7:-1 ~ "pre",
        DAY %in% 4:10 ~ "post",
        DAY %in% 1:3 ~ "menses",
        TRUE ~ "peri-ovulatory"
      )
    )

  if(sep_event == "menses"){
    phase_levels = c("pre","menses","post","peri-ovulatory")
  }else{
    phase_levels = c("menses","post","peri-ovulatory","pre")
  }
  d2 = d2 %>% dplyr::mutate(PHASE = factor(PHASE, levels = phase_levels))


  # CHECKING THE VALUES OF THE COLUMNS
  # cycle
  if((!all(d2$CYCLE>0))|(!all((d2$CYCLE%%1) == 0))) stop("Numbers in the CYCLE column must be integers greater than 0.\n")
  # day
  if(!all((d2$DAY%%1) == 0)) stop("Numbers in the DAY column must be integers.\n")
  if(any(d2$DAY == 0)) stop("DAY must be counted forward from 1 (day 1 = first day of the menses) and backward from -1 (day - 1 = last day before the menses). There is no day 0.\n")
  if(all(d2$DAY>0)) warning("There are no negative DAY, which will be interpreted as no data for the pre-menstrual phase.")
  # drsp
  if(!all(unique(d2$ITEM) %in% 1:24)) stop("ITEM must be integers from 1:24")
  # DRSP_score
  if(!all(unique(d2$DRSP_score[!is.na(d2$DRSP_score)]) %in% 1:6)) stop("Values of 'DRSP_score' must be in 1:6")


  # SUBJECTS
  d2 = d2 %>% dplyr::filter(!is.na(SUBJECT))
  if(verbose) cat("Number of SUBJECTS: ", length(table(d2$SUBJECT)),"\n")

  # CYCLES
  if(verbose) cat("Total number of CYCLES: ", length(table(paste0(d2$SUBJECT,"-",d2$CYCLE))),"\n")

  d2 = d2 %>% dplyr::select(SUBJECT, CYCLE, PHASE, DAY, ITEM, DRSP_score)
  d2
}

#' Checks if data is a "CPASS" data.frame.
#'
#' @param data a data.frame that contains symptoms reported subjects. The data must be in a long format and have the following columns: \code{SUBJECT}, \code{CYCLE}, \code{DAY}, \code{DRSP}, \code{DRSP_score}
#' @keywords CPASS C-PASS PMDD MRMD
#' @return a logical.
#' @export
#' @importFrom magrittr %>%
#' @examples
#' random_data = expand.grid(SUBJECT = 1, CYCLE = 1:2, DAY = c(1:10,-10:-1), ITEM = 1:24)
#' random_data$DRSP_score = sample(1:6, nrow(random_data), replace = TRUE)
#' cpass_data = as_cpass_data(random_data)
#' colnames(cpass_data)
#' is_cpass_data(random_data)
#' is_cpass_data(cpass_data)

is_cpass_data = function(data){
  ok = try(as_cpass_data(data = data, verbose = FALSE), silent = TRUE)
  if(
    (class(ok)[1] != "try-error") &&
    all(colnames(ok) %in% colnames(data))
  ) TRUE else FALSE
}
