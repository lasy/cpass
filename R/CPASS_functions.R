

#' Applies the C-PASS procedure for PMDD and MRMD diagnoses
#'
#' This function implements the C-PASS procedure and returns a list of 6 tables of summaries and diagnoses at different levels (subjects, cycles, domains, etc).
#' @param data a \code{cpass.data} object that contains the symptom DRSP scores reported by the subjects. To transform your data into a \code{cpass.data} object, use the function \code{as_cpass_data()}.
#' @param silent a \code{logical} specifying is the function should print messages or run silently. Default is \code{FALSE}.
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

CPASS = function(data, silent = FALSE){

  # Check the data
  if(!is_cpass_data(data)) stop("data must be of class 'cpass.data'. Use 'as_cpass_data(...)' to check the format of your data and transform them into 'cpass.data'.")

  # Only keep the pre and post-menstrual phase:
  data = data %>% dplyr::filter(PHASE %in% c("pre","post"))

  #### DRSP level diagnosis
  # enough observations in both phase
  n_obs_min = 4
  output_ITEM_level = data   %>%
    dplyr::group_by(SUBJECT, CYCLE, PHASE,ITEM) %>%
    dplyr::summarise(n_days_with_obs = sum(!is.na(DRSP_score))) %>%
    dplyr::ungroup() %>%
    dplyr::group_by(SUBJECT, CYCLE, ITEM) %>%
    dplyr::summarise(at_least_n_obs = sum(n_days_with_obs >= n_obs_min, na.rm = TRUE)>=2)

  # max sev pre-phase
  output_abs_sev = data   %>%
    dplyr::filter(PHASE == "pre") %>%
    dplyr::group_by(SUBJECT, CYCLE, ITEM) %>%
    dplyr::summarise(max_sev_pre = suppressWarnings(max(DRSP_score, na.rm = TRUE)),
                     max_sev_pre = ifelse(is.infinite(max_sev_pre),NA,max_sev_pre),
                     n_days_high_score = sum(DRSP_score >= 4 , na.rm = TRUE))
  output_ITEM_level =
    dplyr::full_join(output_ITEM_level, output_abs_sev, by = c("SUBJECT","CYCLE","ITEM"))

  #score range
  output_range = data %>%
    dplyr::group_by(SUBJECT) %>%
    dplyr::summarise(range = max(DRSP_score, na.rm = TRUE)-1)

  # relative change
  output_rel_change = data %>%
    dplyr::group_by(SUBJECT, CYCLE, PHASE, ITEM) %>%
    dplyr::summarise(mean = mean(DRSP_score, na.rm = TRUE)) %>%
    dplyr::ungroup() %>%
    tidyr::pivot_wider(names_from = PHASE, values_from = mean, names_prefix = "mean_") %>%
    dplyr::mutate(raw_cyclical_change = mean_pre - mean_post)
  output_rel_change =
    dplyr::full_join(output_rel_change,output_range, by = "SUBJECT") %>%
    dplyr::mutate(percent_change = raw_cyclical_change/range * 100)

  output_ITEM_level = dplyr::full_join(output_ITEM_level, output_rel_change, by = c("SUBJECT","CYCLE","ITEM"))

  # clearance in the post-menstrual phase
  output_clearance = data   %>%
    dplyr::filter(PHASE == "post") %>%
    dplyr::group_by(SUBJECT, CYCLE, ITEM) %>%
    dplyr::summarise(max_sev_post = suppressWarnings(max(DRSP_score, na.rm = TRUE))) %>%
    dplyr::mutate(max_sev_post = ifelse(is.infinite(max_sev_post),4,max_sev_post))

  output_ITEM_level = dplyr::full_join(output_ITEM_level, output_clearance, by = c("SUBJECT","CYCLE","ITEM"))

  # ITEM level diagnosis for this cycle
  output_ITEM_level = output_ITEM_level %>%
    dplyr::mutate(
      ITEM_meets_PMDD_criteria =
        at_least_n_obs & # enough observations
        (max_sev_pre >= 4) & # high score pre
        (n_days_high_score >= 2) & # enough days with high score pre
        (percent_change >= 30) &  # at least 30% change in average scores between pre and post
        (max_sev_post < 4), # clearance
      ITEM_meets_PME_criteria =
        at_least_n_obs & # enough observations
        (max_sev_pre >= 4) & # high score pre
        (n_days_high_score >= 2) & # enough days with high score pre
        (percent_change >= 30)  # at least 30% change in average scores between pre and post
    )

  output_ITEM_level =
    dplyr::full_join(output_ITEM_level, dsm5_dict %>% dplyr::select(ITEM,DSM5_SYMPTOM_DOMAIN,SYMPTOM_CATEGORY), by = "ITEM")


  #### DSM-5 DOMAINS level diagnosis
  output_DSM5_DOMAINS_level = output_ITEM_level %>%
    dplyr::filter(!(ITEM %in% c(20, 22:24))) %>%
    dplyr::group_by(SUBJECT, CYCLE, DSM5_SYMPTOM_DOMAIN, SYMPTOM_CATEGORY) %>%
    dplyr::summarise(DSM5_PMDD_criteria = any(ITEM_meets_PMDD_criteria),
                     PME_criteria = any(ITEM_meets_PME_criteria))

  #### CYCLE level
  # first we label cycles that have enough observation: we need at least X ITEM with at least nD observed days in each phase
  X = 1
  nD = 4 #

  output_CYCLE_level = data %>%
    dplyr::group_by(SUBJECT, CYCLE, PHASE, DAY) %>%
    dplyr::summarize(n_ITEM = sum(!is.na(DRSP_score)),
                     has_X_ITEM = n_ITEM >= X) %>%
    dplyr::ungroup() %>%
    dplyr::group_by(SUBJECT, CYCLE, PHASE) %>%
    dplyr::summarize(at_least_nD_days = sum(has_X_ITEM, na.rm = TRUE) >= nD)  %>%
    dplyr::ungroup() %>%
    dplyr::group_by(SUBJECT, CYCLE) %>%
    dplyr::summarize(at_least_nD_days_in_both_phases = all(at_least_nD_days)) %>%
    dplyr::mutate(included = at_least_nD_days_in_both_phases) %>%
    dplyr::select(SUBJECT, CYCLE, included)


  # any core emotional symptom
  output_core_emotional_symptom = output_DSM5_DOMAINS_level %>%
    dplyr::filter(SYMPTOM_CATEGORY == "Core Emotional Symptoms") %>%
    dplyr::group_by(SUBJECT, CYCLE) %>%
    dplyr::summarise(core_emotional_criteria = any(DSM5_PMDD_criteria))

  output_CYCLE_level = dplyr::full_join(
    output_CYCLE_level,
    output_core_emotional_symptom,
    by =  c("SUBJECT", "CYCLE"))

  # DSM-A : at least one core emotional symptom meets criteria
  output_CYCLE_level = output_CYCLE_level %>%
    dplyr::mutate(DSM5_A = included & core_emotional_criteria)

  # DSM-B : 5 or more DMS-5 DOMAINS meet criteria
  output_DSM5_B = output_DSM5_DOMAINS_level %>%
    dplyr::group_by(SUBJECT, CYCLE) %>%
    dplyr::summarize(
      n_DSM5_DOMAINS_meeting_PMDD_criteria = sum(DSM5_PMDD_criteria, na.rm = TRUE),
      five_or_more_DSM5_DOMAINS_PMDD = (n_DSM5_DOMAINS_meeting_PMDD_criteria>=5))

  output_CYCLE_level = dplyr::full_join(
    output_CYCLE_level,output_DSM5_B, by =  c("SUBJECT", "CYCLE"))
  output_CYCLE_level = output_CYCLE_level %>%
    dplyr::mutate(DSM5_B = included & five_or_more_DSM5_DOMAINS_PMDD)


  # PME
  output_PME = output_DSM5_DOMAINS_level %>%
    dplyr::group_by(SUBJECT, CYCLE) %>%
    dplyr::summarize(
      n_DSM5_DOMAINS_meeting_PME_criteria = sum(PME_criteria, na.rm = TRUE),
      five_or_more_DSM5_DOMAINS_PME = (n_DSM5_DOMAINS_meeting_PME_criteria>=5))

  output_CYCLE_level = dplyr::full_join(
    output_CYCLE_level,output_PME, by =  c("SUBJECT", "CYCLE"))
  output_CYCLE_level = output_CYCLE_level %>%
    dplyr::mutate(PME = included & five_or_more_DSM5_DOMAINS_PME)


  # PMDD or MRMD diagnosis for the cycle
  output_CYCLE_level = output_CYCLE_level %>%
    dplyr::mutate(diagnosis =
                    dplyr::case_when(
                      !PME & !DSM5_A ~ "no diagnosis",
                      PME & !DSM5_A ~ "PME",
                      DSM5_A & DSM5_B ~ "PMDD",
                      DSM5_A & !DSM5_B ~ "MRMD")
    ) %>%
    dplyr::arrange(SUBJECT, CYCLE) %>%
    dplyr::select(SUBJECT, CYCLE,
                  included,
                  n_DSM5_DOMAINS_meeting_PME_criteria, n_DSM5_DOMAINS_meeting_PMDD_criteria,
                  PME, DSM5_A, DSM5_B, diagnosis)

  #### SUBJECT level

  output_SUBJECT_level = output_CYCLE_level %>%
    dplyr::group_by(SUBJECT) %>%
    dplyr::summarize(
      NCycles_tot = dplyr::n(),
      NCycles = sum(included),
      N_PMDD = ifelse(NCycles>1, sum(diagnosis == "PMDD"), NA),
      N_MRMD = ifelse(NCycles>1, sum(DSM5_A), NA),
      N_PME = ifelse(NCycles>1, sum(PME), NA),
      pmddcycprop = N_PMDD/NCycles,
      mrmdcycprop = N_MRMD/NCycles,
      pmecycprop = N_PME/NCycles,
      PMDD = (N_PMDD >= 2) & (pmddcycprop >= 0.5), # CHECK the 2nd part with Tory and Liisa
      MRMD = (N_MRMD >= 2) & (mrmdcycprop >= 0.5), # CHECK the 2nd part with Tory and Liisa
      PME = (N_PME >= 2) & (pmecycprop >= 0.5), # CHECK the 2nd part with Tory and Liisa
      dxcat = ifelse(NCycles == 1,
                     NA,
                     dplyr::case_when(PMDD ~ 2,
                                      (!PMDD & MRMD) ~ 1,
                                      !PMDD & !MRMD & PME ~ 3,
                                      TRUE ~0)
      ), #dxcat is the diagnosis category
      dx = c("no diagnosis","MRMD","PMDD", "PME")[dxcat+1],
      avgdsm5crit = sum(n_DSM5_DOMAINS_meeting_PMDD_criteria * included, na.rm = TRUE)/NCycles
    )

  #### summaries of ITEM items
  data = dplyr::full_join(data, output_CYCLE_level %>%  dplyr::select(SUBJECT, CYCLE, included), by = c("SUBJECT","CYCLE"))

  daily_summary_ITEM = data %>%
    dplyr::group_by(SUBJECT, ITEM, PHASE, DAY) %>%
    dplyr::summarise(ave = mean(DRSP_score, na.rm  = TRUE),
                     med = median(DRSP_score, na.rm  = TRUE),
                     min = suppressWarnings(min(DRSP_score, na.rm  = TRUE)),
                     max = suppressWarnings(max(DRSP_score, na.rm  = TRUE))
    ) %>%
    dplyr::mutate(min = ifelse(is.infinite(min),NA, min),
                  max = ifelse(is.infinite(max),NA, max))

  summary_ITEM = output_ITEM_level %>%
    dplyr::group_by(SUBJECT, ITEM) %>%
    dplyr::summarise(ave_perc_change = mean(percent_change, na.rm = TRUE))


  if(!silent) message("PME diagnosis is still experimental and has not be validated clinically. Please, use with caution.\n")

  outputs = list(SUBJECT_level_diagnosis = output_SUBJECT_level,
                 CYCLE_level_diagnosis = output_CYCLE_level,
                 DSM5_DOMAINS_level_diagnosis = output_DSM5_DOMAINS_level,
                 ITEM_level_diagnosis = output_ITEM_level,
                 daily_summary_per_ITEM = daily_summary_ITEM,
                 summary_ITEM = summary_ITEM)

}


