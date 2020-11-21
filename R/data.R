#' PMDD DSM-5 items and their domains.
#'
#' A dictionary listing the 24 DSM-5 items for which subjects can report a DRSP (Daily Record of Severity of Problems) score
#'
#'
#' @format A data frame with 24 rows and 5 variables:
#' \describe{
#'   \item{ITEM}{item number}
#'   \item{ITEM_full_code}{item number with its names}
#'   \item{ITEM_desc}{item description}
#'   \item{DSM5_SYMPTOM_DOMAIN}{item domain, as defined in the DSM-5}
#'   \item{SYMPTOM_CATEGORY}{item category, as defined in the DSM-5. Can be a "Core Emotional Symptom", a "Secondary Symptom" or an "Interference".}
#' }
"dsm5_dict"



#' Example of PMDD data.
#'
#' A dataset with DRSP from 20 subjects
#'
#' @format A data frame with 12,432 rows and 6 variables:
#' \describe{
#'   \item{SUBJECT}{Subject unique idenfier}
#'   \item{CYCLE}{Cycle number. In this dataset, cycles are numbered from one ovulation to the next.}
#'   \item{PHASE}{\code{"pre"} or \code{"post"}: the phase in the menstrual cycle (pre-menstrual vs post-menstrual).}
#'   \item{DAY}{Cycle day. In this dataset, days in the pre-menstrual phase are counted backward from the onset of the menses. -1 is the day before the onset of the menses.
#'   Days in the post-menstrual phase are counted forward from the onset of the menses. Day 1 is the first day of the menses. Day 0 does not exist.}
#'   \item{DRSP}{Item number}
#'   \item{score}{Score reported for the user for the DRSP item, day and cycle of that row}
#' }
#' @source Dr. Tory Eisenlohr-Moul
"PMDD_data"
