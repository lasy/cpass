

#' Applies the C-PASS procedure for PMDD and MRMD diagnoses
#'
#' This function implements the C-PASS procedure and returns.
#' @param data a data.frame that contains the symptoms reported by the subjects. The data must be in a long format and have the following columns: SUBJECT, CYCLE, DAY, DRSP, score
#' @keywords CPASS C-PASS PMDD MRMD
#' @return 6 tables:
#' * (1) the subject-level diagnoses;
#' * (2) the cycle-level diagnoses; (3) the diagnoses at the DSM5 DOMAINS level; (4) the diagnoses at the individual DRSP items level; (5) the daily summary of each DRSP for each subject and (6) the percentage change in DRSP items between the pre- and post-menstrual phase
#' @export
#' @examples
#'


CPASS = function(data){ # , sep_event = c("menses","ovulation")

  # Check the data
  .check_input_data(data)
  # check PHASE is defined
  if(!("PHASE" %in% colnames(data))){
    # define PHASE
  }
  # ordering the PHASES
  sep_event = sep_event[1]
  if(sep_event == "menses"){data$PHASE = factor(data$PHASE, levels = c("pre","post"))}


  #### DRSP level diagnosis
  # enough observations in both phase
  n_obs_min = 4
  output_DRSP_level = data   %>%
    group_by(SUBJECT, CYCLE, PHASE,DRSP) %>%
    dplyr::summarise(n_days_with_obs = sum(!is.na(score))) %>%
    ungroup() %>%
    group_by(SUBJECT, CYCLE, DRSP) %>%
    dplyr::summarise(at_least_n_obs = sum(n_days_with_obs >= n_obs_min, na.rm = TRUE)>=2)

  # max sev pre-phase
  output_abs_sev = data   %>%
    filter(PHASE == "pre") %>%
    group_by(SUBJECT, CYCLE, DRSP) %>%
    dplyr::summarise(max_sev_pre = max(score, na.rm = TRUE),
                     n_days_high_score = sum(score >= 4 , na.rm = TRUE))
  output_DRSP_level = full_join(output_DRSP_level, output_abs_sev, by = c("SUBJECT","CYCLE","DRSP"))

  #score range
  output_range = data %>%
    group_by(SUBJECT) %>%
    dplyr::summarise(range = max(score, na.rm = TRUE)-1)

  # relative change
  output_rel_change = data %>%
    group_by(SUBJECT, CYCLE, PHASE, DRSP) %>%
    dplyr::summarise(mean = mean(score, na.rm = TRUE)) %>%
    ungroup() %>%
    tidyr::pivot_wider(names_from = PHASE, values_from = mean, names_prefix = "mean_") %>%
    mutate(raw_cyclical_change = mean_pre - mean_post) %>%
    full_join(.,output_range, by = "SUBJECT") %>%
    mutate(percent_change = raw_cyclical_change/range * 100)

  output_DRSP_level = full_join(output_DRSP_level, output_rel_change, by = c("SUBJECT","CYCLE","DRSP"))

  # clearance in the post-menstrual phase
  output_clearance = data   %>%
    filter(PHASE == "post") %>%
    group_by(SUBJECT, CYCLE, DRSP) %>%
    dplyr::summarise(max_sev_post = max(score, na.rm = TRUE))

  output_DRSP_level = full_join(output_DRSP_level, output_clearance, by = c("SUBJECT","CYCLE","DRSP"))

  # DRSP level diagnosis for this cycle
  output_DRSP_level = output_DRSP_level %>%
    mutate(DRSP_meets_criteria =
             at_least_n_obs & # enough observations
             (max_sev_pre >= 4) & # high score pre
             (n_days_high_score >= 2) & # enough days with high score pre
             (percent_change >= 30) &  # at least 30% change in average scores between pre and post
             (max_sev_post < 4) # clearance
    )

  output_DRSP_level = output_DRSP_level %>%
    full_join(., dsm5_dict %>% select(DRSP,DSM5_SYMPTOM_DOMAIN,SYMPTOM_CATEGORY), by = "DRSP")


  #### DSM-5 DOMAINS level diagnosis
  output_DSM5_DOMAINS_level = output_DRSP_level %>%
    filter(!(DRSP %in% c(20, 22:24))) %>%
    group_by(SUBJECT, CYCLE, DSM5_SYMPTOM_DOMAIN, SYMPTOM_CATEGORY) %>%
    summarise(DSM5_criteria = any(DRSP_meets_criteria))

  #### CYCLE level
  # first we label cycles that have enough observation: we need at least X DRSP with at least nD observed days in each phase
  X = 1
  nD = 4 #

  output_CYCLE_level = data %>%
    group_by(SUBJECT, CYCLE, PHASE, DAY) %>%
    summarize(n_DRSP = sum(!is.na(score)),
              has_X_DRSP = n_DRSP >= X) %>%
    ungroup() %>%
    group_by(SUBJECT, CYCLE, PHASE) %>%
    summarize(at_least_nD_days = sum(has_X_DRSP, na.rm = TRUE) >= nD)  %>%
    ungroup() %>%
    group_by(SUBJECT, CYCLE) %>%
    summarize(at_least_nD_days_in_both_phases = all(at_least_nD_days)) %>%
    mutate(included = at_least_nD_days_in_both_phases) %>%
    select(SUBJECT, CYCLE, included)


  # any core emotional symptom
  output_core_emotional_symptom = output_DSM5_DOMAINS_level %>%
    filter(SYMPTOM_CATEGORY == "Core Emotional Symptoms") %>%
    group_by(SUBJECT, CYCLE) %>%
    summarise(core_emotional_criteria = any(DSM5_criteria))

  output_CYCLE_level = full_join(output_CYCLE_level,output_core_emotional_symptom, by =  c("SUBJECT", "CYCLE"))

  # DSM-A : at least one core emotional symptom meets criteria
  output_CYCLE_level = output_CYCLE_level %>%
    mutate(DSM5_A = included & core_emotional_criteria)

  # DSM-B : 5 or more DMS-5 DOMAINS meet criteria
  output_DSM5_B = output_DSM5_DOMAINS_level %>%
    group_by(SUBJECT, CYCLE) %>%
    summarize(
      n_DSM5_DOMAINS_meeting_criteria = sum(DSM5_criteria, na.rm = TRUE),
      five_or_more_DSM5_DOMAINS = (n_DSM5_DOMAINS_meeting_criteria>=5))

  output_CYCLE_level = full_join(output_CYCLE_level,output_DSM5_B, by =  c("SUBJECT", "CYCLE"))
  output_CYCLE_level = output_CYCLE_level %>%
    mutate(DSM5_B = included & five_or_more_DSM5_DOMAINS)


  # PMDD or MRMD diagnosis for the cycle
  output_CYCLE_level = output_CYCLE_level %>%
    mutate(diagnosis =
             case_when(
               !DSM5_A ~ "no diagnosis",
               DSM5_A & DSM5_B ~ "PMDD",
               DSM5_A & !DSM5_B ~ "MRMD")
    ) %>%
    arrange(SUBJECT, CYCLE) %>%
    select(SUBJECT, CYCLE, included, n_DSM5_DOMAINS_meeting_criteria, DSM5_A, DSM5_B, diagnosis)

  #### SUBJECT level

  output_SUBJECT_level = output_CYCLE_level %>%
    group_by(SUBJECT) %>%
    summarize(
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
  data = full_join(data, output_CYCLE_level %>%  select(SUBJECT, CYCLE, included), by = c("SUBJECT","CYCLE"))

  daily_summary_DRSP = data %>%
    group_by(SUBJECT, DRSP, PHASE, DAY) %>%
    summarise(ave = mean(score, na.rm  = TRUE),
              med = median(score, na.rm  = TRUE),
              min = min(score, na.rm  = TRUE),
              max = max(score, na.rm  = TRUE)
              )

  summary_DRSP = output_DRSP_level %>%
    group_by(SUBJECT, DRSP) %>%
    summarise(ave_perc_change = mean(percent_change, na.rm = TRUE))


  outputs = list(SUBJECT_level_diagnosis = output_SUBJECT_level,
                 CYCLE_level_diagnosis = output_CYCLE_level,
                 DSM5_DOMAINS_level_diagnosis = output_DSM5_DOMAINS_level,
                 DRSP_level_diagnosis = output_DRSP_level,
                 daily_summary_DRSP = daily_summary_DRSP,
                 summary_DRSP = summary_DRSP)

}

.check_input_data = function(data){
  required_columns = c("SUBJECT","CYCLE","DAY", "DRSP","score")
  if(!all(c(required_columns %in% colnames(data)))){stop(str_c("data must contain the following columns: ", str_c(required_columns, collapse = "; ")))}
  if(!all(unique(data$DRSP) %in% 1:24)){stop("DRSP must be integers from 1:24")}

  return(TRUE)
}
