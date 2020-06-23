

#' Applies the C-PASS procedure for PMDD and MRMD diagnoses
#'
#' This function implements the C-PASS procedure and returns a list of 6 tables of summaries and diagnoses at different levels (subjects, cycles, domains, etc).
#' @param data a \code{cpass.data} object that contains the symptom scores reported by the subjects. To transform your data into a \code{cpass.data} object, use the function \code{as_cpass_data()}.
#'
#' @keywords CPASS C-PASS PMDD MRMD
#' @return The CPASS function returns a list with 6 elements; each of them is a table (\code{data.frame}):
#'
#' \describe{
#'   \item{\code{SUBJECT_level_diagnosis}}{The subject-level diagnoses}
#'   \item{\code{CYCLE_level_diagnosis}}{The cycle-level diagnoses}
#'   \item{\code{DSM5_DOMAINS_level_diagnosis}}{The diagnoses at the DSM5 DOMAINS level}
#'   \item{\code{DRSP_level_diagnosis}}{The diagnoses at the individual DRSP items level}
#'   \item{\code{daily_summary_DRSP}}{The daily summary of each DRSP for each subject}
#'   \item{\code{summary_DRSP}}{The percentage change in DRSP items between the pre- and post-menstrual phase}
#' }
#' @export
#' @importFrom magrittr %>%
#' @examples
#' data(PMDD_data)
#' cpass_input = as_cpass_data(PMDD_data)
#' output = CPASS(cpass_input)
#' head(output$SUBJECT_level_diagnosis)

CPASS = function(data){

  # Check the data
  if(!is_cpass_data(data)) stop("data must be of class 'cpass.data'. Use 'as_cpass_data(...)' to check the format of your data and transform them into 'cpass.data'.")

  # Only keep the pre and post-menstrual phase:
  data = data %>% dplyr::filter(PHASE %in% c("pre","post"))

  #### DRSP level diagnosis
  # enough observations in both phase
  n_obs_min = 4
  output_DRSP_level = data   %>%
    dplyr::group_by(SUBJECT, CYCLE, PHASE,DRSP) %>%
    dplyr::summarise(n_days_with_obs = sum(!is.na(score))) %>%
    dplyr::ungroup() %>%
    dplyr::group_by(SUBJECT, CYCLE, DRSP) %>%
    dplyr::summarise(at_least_n_obs = sum(n_days_with_obs >= n_obs_min, na.rm = TRUE)>=2)

  # max sev pre-phase
  output_abs_sev = data   %>%
    dplyr::filter(PHASE == "pre") %>%
    dplyr::group_by(SUBJECT, CYCLE, DRSP) %>%
    dplyr::summarise(max_sev_pre = max(score, na.rm = TRUE),
                     n_days_high_score = sum(score >= 4 , na.rm = TRUE))
  output_DRSP_level =
    dplyr::full_join(output_DRSP_level, output_abs_sev, by = c("SUBJECT","CYCLE","DRSP"))

  #score range
  output_range = data %>%
    dplyr::group_by(SUBJECT) %>%
    dplyr::summarise(range = max(score, na.rm = TRUE)-1)

  # relative change
  output_rel_change = data %>%
    dplyr::group_by(SUBJECT, CYCLE, PHASE, DRSP) %>%
    dplyr::summarise(mean = mean(score, na.rm = TRUE)) %>%
    dplyr::ungroup() %>%
    tidyr::pivot_wider(names_from = PHASE, values_from = mean, names_prefix = "mean_") %>%
    dplyr::mutate(raw_cyclical_change = mean_pre - mean_post)
  output_rel_change =
    dplyr::full_join(output_rel_change,output_range, by = "SUBJECT") %>%
    dplyr::mutate(percent_change = raw_cyclical_change/range * 100)

  output_DRSP_level = dplyr::full_join(output_DRSP_level, output_rel_change, by = c("SUBJECT","CYCLE","DRSP"))

  # clearance in the post-menstrual phase
  output_clearance = data   %>%
    dplyr::filter(PHASE == "post") %>%
    dplyr::group_by(SUBJECT, CYCLE, DRSP) %>%
    dplyr::summarise(max_sev_post = max(score, na.rm = TRUE))

  output_DRSP_level = dplyr::full_join(output_DRSP_level, output_clearance, by = c("SUBJECT","CYCLE","DRSP"))

  # DRSP level diagnosis for this cycle
  output_DRSP_level = output_DRSP_level %>%
    dplyr::mutate(DRSP_meets_criteria =
             at_least_n_obs & # enough observations
             (max_sev_pre >= 4) & # high score pre
             (n_days_high_score >= 2) & # enough days with high score pre
             (percent_change >= 30) &  # at least 30% change in average scores between pre and post
             (max_sev_post < 4) # clearance
    )

  output_DRSP_level =
    dplyr::full_join(output_DRSP_level, dsm5_dict %>% select(DRSP,DSM5_SYMPTOM_DOMAIN,SYMPTOM_CATEGORY), by = "DRSP")


  #### DSM-5 DOMAINS level diagnosis
  output_DSM5_DOMAINS_level = output_DRSP_level %>%
    dplyr::filter(!(DRSP %in% c(20, 22:24))) %>%
    dplyr::group_by(SUBJECT, CYCLE, DSM5_SYMPTOM_DOMAIN, SYMPTOM_CATEGORY) %>%
    dplyr::summarise(DSM5_criteria = any(DRSP_meets_criteria))

  #### CYCLE level
  # first we label cycles that have enough observation: we need at least X DRSP with at least nD observed days in each phase
  X = 1
  nD = 4 #

  output_CYCLE_level = data %>%
    dplyr::group_by(SUBJECT, CYCLE, PHASE, DAY) %>%
    dplyr::summarize(n_DRSP = sum(!is.na(score)),
              has_X_DRSP = n_DRSP >= X) %>%
    dplyr::ungroup() %>%
    dplyr::group_by(SUBJECT, CYCLE, PHASE) %>%
    dplyr::summarize(at_least_nD_days = sum(has_X_DRSP, na.rm = TRUE) >= nD)  %>%
    dplyr::ungroup() %>%
    dplyr::group_by(SUBJECT, CYCLE) %>%
    dplyr::summarize(at_least_nD_days_in_both_phases = all(at_least_nD_days)) %>%
    dplyr::mutate(included = at_least_nD_days_in_both_phases) %>%
    dplyr::select(SUBJECT, CYCLE, included)


  # any core emotional symptom
  output_core_emotional_symptom = output_DSM5_DOMAINS_level %>%
    dplyr::filter(SYMPTOM_CATEGORY == "Core Emotional Symptoms") %>%
    dplyr::group_by(SUBJECT, CYCLE) %>%
    dplyr::summarise(core_emotional_criteria = any(DSM5_criteria))

  output_CYCLE_level = dplyr::full_join(output_CYCLE_level,output_core_emotional_symptom, by =  c("SUBJECT", "CYCLE"))

  # DSM-A : at least one core emotional symptom meets criteria
  output_CYCLE_level = output_CYCLE_level %>%
    dplyr::mutate(DSM5_A = included & core_emotional_criteria)

  # DSM-B : 5 or more DMS-5 DOMAINS meet criteria
  output_DSM5_B = output_DSM5_DOMAINS_level %>%
    dplyr::group_by(SUBJECT, CYCLE) %>%
    dplyr::summarize(
      n_DSM5_DOMAINS_meeting_criteria = sum(DSM5_criteria, na.rm = TRUE),
      five_or_more_DSM5_DOMAINS = (n_DSM5_DOMAINS_meeting_criteria>=5))

  output_CYCLE_level = dplyr::full_join(output_CYCLE_level,output_DSM5_B, by =  c("SUBJECT", "CYCLE"))
  output_CYCLE_level = output_CYCLE_level %>%
    dplyr::mutate(DSM5_B = included & five_or_more_DSM5_DOMAINS)


  # PMDD or MRMD diagnosis for the cycle
  output_CYCLE_level = output_CYCLE_level %>%
    dplyr::mutate(diagnosis =
             case_when(
               !DSM5_A ~ "no diagnosis",
               DSM5_A & DSM5_B ~ "PMDD",
               DSM5_A & !DSM5_B ~ "MRMD")
    ) %>%
    dplyr::arrange(SUBJECT, CYCLE) %>%
    dplyr::select(SUBJECT, CYCLE, included, n_DSM5_DOMAINS_meeting_criteria, DSM5_A, DSM5_B, diagnosis)

  #### SUBJECT level

  output_SUBJECT_level = output_CYCLE_level %>%
    dplyr::group_by(SUBJECT) %>%
    dplyr::summarize(
      NCycles_tot = n(),
      NCycles = sum(included),
      N_PMDD = ifelse(NCycles>1, sum(diagnosis == "PMDD"), NA),
      N_MRMD = ifelse(NCycles>1, sum(DSM5_A), NA),
      pmddcycprop = N_PMDD/NCycles,
      mrmdcycprop = N_MRMD/NCycles,
      PMDD = (N_PMDD >= 2) & (pmddcycprop >= 0.5), # CHECK the 2nd part with Tory and Liisa
      MRMD = (N_MRMD >= 2) & (mrmdcycprop >= 0.5), # CHECK the 2nd part with Tory and Liisa
      dxcat = ifelse(NCycles == 1,
                     NA,
                     case_when(PMDD ~ 2,
                               (!PMDD & MRMD) ~ 1,
                               TRUE ~0)
                      ), #dxcat is the diagnosis category
      dx = c("no diagnosis","MRMD","PMDD")[dxcat+1],
      avgdsm5crit = sum(n_DSM5_DOMAINS_meeting_criteria * included, na.rm = TRUE)/NCycles
    )

  #### summaries of DRSP items
  data = dplyr::full_join(data, output_CYCLE_level %>%  select(SUBJECT, CYCLE, included), by = c("SUBJECT","CYCLE"))

  daily_summary_DRSP = data %>%
    dplyr::group_by(SUBJECT, DRSP, PHASE, DAY) %>%
    dplyr::summarise(ave = mean(score, na.rm  = TRUE),
              med = median(score, na.rm  = TRUE),
              min = min(score, na.rm  = TRUE),
              max = max(score, na.rm  = TRUE)
              )

  summary_DRSP = output_DRSP_level %>%
    dplyr::group_by(SUBJECT, DRSP) %>%
    dplyr::summarise(ave_perc_change = mean(percent_change, na.rm = TRUE))


  outputs = list(SUBJECT_level_diagnosis = output_SUBJECT_level,
                 CYCLE_level_diagnosis = output_CYCLE_level,
                 DSM5_DOMAINS_level_diagnosis = output_DSM5_DOMAINS_level,
                 DRSP_level_diagnosis = output_DRSP_level,
                 daily_summary_DRSP = daily_summary_DRSP,
                 summary_DRSP = summary_DRSP)

}


#' Transforms into a "cpass.data" object.
#'
#' This function transforms a data.frame into an object of class \code{"cpass.data"}.
#' @param data a data.frame that contains symptoms reported subjects. The data must be in a long format and have the following columns: \code{SUBJECT}, \code{CYCLE}, \code{DAY}, \code{DRSP}, \code{score}
#' @param sep_event an optional character \code{"menses"} (default) or \code{"ovulation"} which defines if the pre- and post-menstrual phases are separated by the menses or by ovulation within a cycle (values of the column \code{CYCLE} in the provided data).
#' @keywords CPASS C-PASS PMDD MRMD
#' @return an object of class \code{"cpass.data"}.
#' @export
#' @importFrom magrittr %>%
#' @examples
#' random_data = expand.grid(SUBJECT = 1, CYCLE = 1:2, DAY = c(1:10,-10:-1), DRSP = 1:24)
#' random_data$score = sample(1:6, nrow(random_data), replace = TRUE)
#' class(random_data)
#' cpass_data = as_cpass_data(random_data)
#' class(cpass_data)
#' colnames(cpass_data)


as_cpass_data = function(data, sep_event = c("menses", "ovulation"), verbose = TRUE){

  d2 = data

  possible_sep_event = c("ovulation","menses")
  if(!any(sep_event %in% possible_sep_event)) stop(paste0("sep_event must be ",paste0("'",possible_sep_event,"'",collapse = " or ")))
  sep_event = sep_event[1]

  # CHECKING that all the necessary columns are present
  required_columns = c("SUBJECT", "DRSP", "score")
  if(!all(required_columns %in% colnames(d2))){
    stop(stringr::str_c("data must contain the following columns: ",
                        stringr::str_c(required_columns, collapse = "; ")))}

  day_identifying_columns = list(cpass_format = c("CYCLE","DAY"),
                                 fw = c("natural_cycle_number","fw_day","cycle_length"),
                                 bw = c("natural_cycle_number","bw_day","cycle_length"),
                                 fw_bw = c("natural_cycle_number","fw_day","bw_day"),
                                 first_day = c("date","is_first_day"),
                                 bleeding = c("date","bleeding"))

  format = "error"
  for(format in names(day_identifying_columns))
    if(all(day_identifying_columns[[format]] %in% colnames(d2))) break

  if(format == "error"){
    error_message = paste0("Cycles and cycledays must be defined with any of these combinations of columns:\n",
                           paste0(day_identifying_columns, collapse = "\n"))
    stop(error_message)
  }

  if(verbose & (format != "cpass_format"))
    cat(paste0("Using columns ",paste0(day_identifying_columns[[format]],collapse = ", "),
        " (format '",format,"') to define 'CYCLE' and 'DAY'."))

  # if format is different than cpass, transform into CYCLE and DAY with sep_event (not implemented yet)
  if(format != "cpass_format")
    stop(
      paste0("The format '",format,
             "' (", paste0(day_identifying_columns[[format]], collapse = ", "),
             ") has not been implemented yet. Please provide processed columns 'CYCLE' and 'DAY'.")
      )

  # DEFINING THE PHASE
  d2 = d2 %>%
    dplyr::mutate(
      PHASE = case_when(
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
  if(!all(unique(d2$DRSP) %in% 1:24)) stop("DRSP must be integers from 1:24")
  # score
  if(!all(unique(d2$score[!is.na(d2$score)]) %in% 1:6)) stop("Values of 'score' must be in 1:6")


  # SUBJECTS
  d2 = d2 %>% dplyr::filter(!is.na(SUBJECT))
  if(verbose) cat("Number of SUBJECTS: ", length(table(d2$SUBJECT)),"\n")

  # CYCLES
  if(verbose) cat("Total number of CYCLES: ", length(table(paste0(d2$SUBJECT,"-",d2$CYCLE))),"\n")

  d2 = d2 %>% dplyr::select(SUBJECT, CYCLE, PHASE, DAY, DRSP, score)

  class(d2) <- append(class(d2),"cpass.data")
  d2
}

#' Checks if data is a "cpass.data" object.
#'
#' @param data a data.frame that contains symptoms reported subjects. The data must be in a long format and have the following columns: \code{SUBJECT}, \code{CYCLE}, \code{DAY}, \code{DRSP}, \code{score}
#' @keywords CPASS C-PASS PMDD MRMD
#' @return a logical.
#' @export
#' @importFrom magrittr %>%
#' @examples
#' random_data = expand.grid(SUBJECT = 1, CYCLE = 1:2, DAY = c(1:10,-10:-1), DRSP = 1:24)
#' random_data$score = sample(1:6, nrow(random_data), replace = TRUE)
#' class(random_data)
#' cpass_data = as_cpass_data(random_data)
#' class(cpass_data)
#' colnames(cpass_data)
#' is_cpass_data(random_data)
#' is_cpass_data(cpass_data)

is_cpass_data = function(data){
  if(!("cpass.data" %in% class(data))) return(FALSE)
  ok = try(as_cpass_data(data = data, verbose = FALSE), silent = TRUE)
  if(class(ok)[1] == "try-error") FALSE else TRUE
}

