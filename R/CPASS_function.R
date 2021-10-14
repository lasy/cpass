

#' Applies the C-PASS procedure for PMDD and MRMD diagnoses
#'
#' This function implements the C-PASS procedure and
#' returns a list of 6 tables of summaries and
#' diagnoses at different levels (subjects, cycles, domains, etc).
#' @param data a \code{cpass.data} data frame
#' that contains the symptom DRSP scores reported by the subjects.
#' To transform your data into a \code{cpass.data} object,
#' use the function \code{as_cpass_data()}.
#' @param silent a \code{logical}
#' specifying is the function should print messages or run silently.
#' Default is \code{FALSE}.
#'
#' @return The cpass function returns a list with 6 elements;
#' each of them is a table (\code{data.frame}):
#' \describe{
#'   \item{\code{subject_level_diagnosis}}{The subject-level diagnoses}
#'   \item{\code{cycle_level_diagnosis}}{The cycle-level diagnoses}
#'   \item{\code{DSM5_domains_level_diagnosis}}{The diagnoses at the DSM5 DOMAINS level}
#'   \item{\code{DRSP_level_diagnosis}}{The diagnoses at the individual DRSP items level}
#'   \item{\code{daily_summary_DRSP}}{The daily summary of each DRSP for each subject}
#'   \item{\code{summary_DRSP}}{The percent change in DRSP items between the pre- and post-menstrual phase}
#' }
#' @keywords cpass C-PASS PMDD MRMD
#' @export
#' @importFrom magrittr %>%
#' @examples
#' data(PMDD_data)
#' cpass_input = as_cpass_data(PMDD_data, sep_event = "menses")
#' output = cpass(cpass_input)
#' head(output$subject_level_diagnosis)

cpass <- function(data, silent = FALSE) {

  if (!is_cpass_data(data, silent = TRUE))
    stop(
      paste0(
        "data must be 'cpass.data'.",
        "Use 'as_cpass_data(...)' to check the format of your data",
        "and transform them into 'cpass.data'.")
    )

  # Only keep the pre and post-menstrual phase:
  data <-
    data %>% dplyr::filter(.data$phase %in% c("pre-menses", "post-menses"))
  # we join the dsm5_dict
  data <-
    dplyr::left_join(
      data,
      dsm5_dict %>% dplyr::rename(item = ITEM),
      by = "item")

  #### DRSP level diagnosis
  # enough observations in both phase
  n_obs_min <-  4
  output_item_level <-
    data   %>%
    dplyr::group_by(subject, cycle, phase, item) %>%
    dplyr::summarise(n_days_with_obs = sum(!is.na(drsp_score)),
                     .groups = "drop") %>%
    dplyr::ungroup() %>%
    dplyr::group_by(subject, cycle, item) %>%
    dplyr::summarise(
      at_least_n_obs = sum(n_days_with_obs >= n_obs_min,
                           na.rm = TRUE) >= 2,
      .groups = "drop")

  # max sev pre-phase
  output_abs_sev <-
    data   %>%
    dplyr::filter(phase == "pre-menses") %>%
    dplyr::group_by(subject, cycle, item) %>%
    dplyr::summarise(
      max_sev_pre =
        suppressWarnings(max(drsp_score, na.rm = TRUE)),
      max_sev_pre =
        ifelse(is.infinite(max_sev_pre), NA, max_sev_pre),
      n_days_high_score = sum(drsp_score >= 4, na.rm = TRUE),
      .groups = "drop")

  output_item_level <-
    dplyr::full_join(
      output_item_level,
      output_abs_sev,
      by = c("subject", "cycle", "item"))

  #score range
  output_range <-
    data %>%
    dplyr::group_by(subject) %>%
    dplyr::summarise(range = max(drsp_score, na.rm = TRUE) - 1)

  # relative change
  output_rel_change <-
    data %>%
    dplyr::group_by(subject, cycle, phase, item) %>%
    dplyr::summarise(
      mean = mean(drsp_score, na.rm = TRUE), .groups = "drop") %>%
    dplyr::ungroup() %>%
    dplyr::mutate(phase = ifelse(phase == "pre-menses", "pre", "post")) %>%
    tidyr::pivot_wider(
      names_from = phase, values_from = mean, names_prefix = "mean_") %>%
    dplyr::mutate(raw_cyclical_change = mean_pre - mean_post)
  output_rel_change <-
    dplyr::full_join(output_rel_change, output_range, by = "subject") %>%
    dplyr::mutate(percent_change = raw_cyclical_change / range * 100)

  output_item_level <-
    dplyr::full_join(
      output_item_level,
      output_rel_change,
      by = c("subject", "cycle", "item"))

  # clearance in the post-menstrual phase
  output_clearance <-
    data   %>%
    dplyr::filter(phase == "post-menses") %>%
    dplyr::group_by(subject, cycle, item) %>%
    dplyr::summarise(
      max_sev_post = suppressWarnings(max(drsp_score, na.rm = TRUE)),
      .groups = "drop") %>%
    dplyr::mutate(
      max_sev_post = ifelse(is.infinite(max_sev_post), 4, max_sev_post))

  output_item_level <-
    dplyr::full_join(
      output_item_level,
      output_clearance,
      by = c("subject", "cycle", "item"))

  # item level diagnosis for this cycle
  output_item_level <-
    output_item_level %>%
    dplyr::mutate(
      item_meets_PMDD_criteria =
        at_least_n_obs & # enough observations
        (max_sev_pre >= 4) & # high score pre
        (n_days_high_score >= 2) & # enough days with high score pre
        (percent_change >= 30) &  # at least 30% change in average scores
        (max_sev_post < 4), # clearance
      item_meets_PME_criteria =
        at_least_n_obs & # enough observations
        (max_sev_pre >= 4) & # high score pre
        (n_days_high_score >= 2) & # enough days with high score pre
        (percent_change >= 30),  # at least 30% change in average scores
      item_meets_PMDD_criteria =
        ifelse(at_least_n_obs, item_meets_PMDD_criteria, NA),
      item_meets_PME_criteria =
        ifelse(at_least_n_obs, item_meets_PME_criteria, NA),
    )


  output_item_level <-
    dplyr::full_join(
      output_item_level,
      dsm5_dict %>%
        dplyr::select(ITEM, DSM5_SYMPTOM_DOMAIN, SYMPTOM_CATEGORY) %>%
        dplyr::rename(item = ITEM),
      by = "item")


  #### DSM-5 DOMAINS level diagnosis
  output_DSM5_domains_level <-
    output_item_level %>%
    dplyr::filter(!(item %in% c(20, 22:24))) %>%
    dplyr::group_by(subject, cycle, DSM5_SYMPTOM_DOMAIN, SYMPTOM_CATEGORY) %>%
    dplyr::summarise(DSM5_PMDD_criteria =
                       ifelse(
                         all(is.na(item_meets_PMDD_criteria)), NA,
                         any(item_meets_PMDD_criteria, na.rm = TRUE)),
                     PME_criteria =
                       ifelse(
                         all(is.na(item_meets_PME_criteria)), NA,
                         any(item_meets_PME_criteria, na.rm = TRUE)),
                     .groups = "drop")

  #### cycle level
  # first we label cycles that have enough observation:
  # we need at least X item with at least nD observed days in each phase
  X <-  1
  nD <-  4 #

  output_cycle_level <-
    data %>%
    dplyr::group_by(subject, cycle, phase, day) %>%
    dplyr::summarize(n_item = sum(!is.na(drsp_score)),
                     has_X_item = n_item >= X,
                     .groups = "drop") %>%
    dplyr::ungroup() %>%
    dplyr::group_by(subject, cycle, phase) %>%
    dplyr::summarize(at_least_nD_days = sum(has_X_item, na.rm = TRUE) >= nD,
                     .groups = "drop")  %>%
    dplyr::ungroup() %>%
    dplyr::group_by(subject, cycle) %>%
    dplyr::summarize(at_least_nD_days_in_both_phases = all(at_least_nD_days),
                     .groups = "drop") %>%
    dplyr::mutate(included = at_least_nD_days_in_both_phases) %>%
    dplyr::select(subject, cycle, included)


  # any core emotional symptom
  output_core_emotional_symptom <-
    output_DSM5_domains_level %>%
    dplyr::filter(SYMPTOM_CATEGORY == "Core Emotional Symptoms") %>%
    dplyr::group_by(subject, cycle) %>%
    dplyr::summarise(core_emotional_criteria =
                       ifelse(all(is.na(DSM5_PMDD_criteria)), NA,
                              any(DSM5_PMDD_criteria, na.rm = TRUE)),
                     .groups = "drop")

  output_cycle_level <-
    dplyr::full_join(
      output_cycle_level,
      output_core_emotional_symptom,
      by =  c("subject", "cycle"))

  # DSM-A : at least one core emotional symptom meets criteria
  output_cycle_level <-
    output_cycle_level %>%
    dplyr::mutate(DSM5_A = included & core_emotional_criteria)

  # DSM-B : 5 or more DMS-5 DOMAINS meet criteria
  output_DSM5_B <-
    output_DSM5_domains_level %>%
    dplyr::group_by(subject, cycle) %>%
    dplyr::summarize(
      n_DSM5_domains_meeting_PMDD_criteria =
        sum(DSM5_PMDD_criteria, na.rm = TRUE),
      five_or_more_DSM5_domains_PMDD =
        (n_DSM5_domains_meeting_PMDD_criteria >= 5),
      .groups = "drop")

  output_cycle_level <-
    dplyr::full_join(
      output_cycle_level, output_DSM5_B, by =  c("subject", "cycle"))
  output_cycle_level <-
    output_cycle_level %>%
    dplyr::mutate(DSM5_B = included & five_or_more_DSM5_domains_PMDD)


  # PME
  output_PME <-
    output_DSM5_domains_level %>%
    dplyr::group_by(subject, cycle) %>%
    dplyr::summarize(
      n_DSM5_domains_meeting_PME_criteria = sum(PME_criteria, na.rm = TRUE),
      five_or_more_DSM5_domains_PME =
        (n_DSM5_domains_meeting_PME_criteria >= 5),
      .groups = "drop")

  output_cycle_level <-
    dplyr::full_join(
      output_cycle_level, output_PME, by =  c("subject", "cycle"))
  output_cycle_level <-
    output_cycle_level %>%
    dplyr::mutate(PME = included & five_or_more_DSM5_domains_PME)


  # PMDD or MRMD diagnosis for the cycle
  output_cycle_level <-
    output_cycle_level %>%
    dplyr::mutate(diagnosis =
                    dplyr::case_when(
                      !PME & !DSM5_A ~ "no diagnosis",
                      PME & !DSM5_A ~ "PME",
                      DSM5_A & DSM5_B ~ "PMDD",
                      DSM5_A & !DSM5_B ~ "MRMD")
    ) %>%
    dplyr::arrange(subject, cycle) %>%
    dplyr::select(subject, cycle,
                  included,
                  n_DSM5_domains_meeting_PME_criteria,
                  n_DSM5_domains_meeting_PMDD_criteria,
                  PME, DSM5_A, DSM5_B, diagnosis)

  #### subject level
  dx_levels = c("no diagnosis", "MRMD", "PMDD", "PME")

  output_subject_level <-
    output_cycle_level %>%
    dplyr::group_by(subject) %>%
    dplyr::summarize(
      Ncycles_tot = dplyr::n(),
      Ncycles = sum(included, na.rm = TRUE),
      N_PMDD = ifelse(Ncycles > 1, sum(diagnosis == "PMDD"), NA),
      N_MRMD = ifelse(Ncycles > 1, sum(DSM5_A), NA),
      N_PME = ifelse(Ncycles > 1, sum(PME), NA),
      pmddcycprop = N_PMDD / Ncycles,
      mrmdcycprop = N_MRMD / Ncycles,
      pmecycprop = N_PME / Ncycles,
      PMDD = (N_PMDD >= 2) & (pmddcycprop >= 0.5),
      MRMD = (N_MRMD >= 2) & (mrmdcycprop >= 0.5),
      PME = (N_PME >= 2) & (pmecycprop >= 0.5),
      dxcat = ifelse(Ncycles == 1,
                     NA,
                     dplyr::case_when(PMDD ~ 2,
                                      (!PMDD & MRMD) ~ 1,
                                      !PMDD & !MRMD & PME ~ 3,
                                      TRUE ~0)
      ), #dxcat is the diagnosis category
      dx = dx_levels[dxcat + 1] %>% factor(., levels = dx_levels),
      avgdsm5crit =
        sum(n_DSM5_domains_meeting_PMDD_criteria * included,
            na.rm = TRUE) / Ncycles,
      .groups = "drop"
    )

  #### summaries of item items
  data <-
    dplyr::full_join(
      data,
      output_cycle_level %>%  dplyr::select(subject, cycle, included),
      by = c("subject", "cycle"))

  daily_summary_item <-
    data %>%
    dplyr::group_by(subject, item, phase, day) %>%
    dplyr::summarise(
      ave = mean(drsp_score, na.rm  = TRUE),
      med = median(drsp_score, na.rm  = TRUE),
      min = suppressWarnings(min(drsp_score, na.rm  = TRUE)),
      max = suppressWarnings(max(drsp_score, na.rm  = TRUE)),
      .groups = "drop"
    ) %>%
    dplyr::mutate(min = ifelse(is.infinite(min), NA, min),
                  max = ifelse(is.infinite(max), NA, max))

  summary_item <-
    output_item_level %>%
    dplyr::group_by(subject, item) %>%
    dplyr::summarise(ave_perc_change =
                       mean(percent_change, na.rm = TRUE),
                     .groups = "drop")


  if (!silent)
    message(
      paste0(
        "PME diagnosis is still experimental ",
        "and has not been validated clinically. ",
        "Please, use with caution.\n"
      )
    )

  outputs <-
    list(subject_level_diagnosis = output_subject_level,
         cycle_level_diagnosis = output_cycle_level,
         DSM5_domains_level_diagnosis = output_DSM5_domains_level,
         item_level_diagnosis = output_item_level,
         daily_summary_per_item = daily_summary_item,
         summary_item = summary_item)

  outputs
}
