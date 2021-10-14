
.get_strip_background_color <- function() return("gray80")

.get_summary_colors <-
  function() return(c("#F87875", "#659EED", "#33CC59", "#C77CFF"))



#' Visualize the subject diagnosis
#'
#' This function calls the \code{CPASS} function and
#' provides a visualization of the diagnosis.
#' @param data a \code{cpass} data frame
#' (use \code{as_cpass_data} to convert your data into \code{cpass} data)
#' that contains the symptoms reported by ONE subject.
#' @param color_summary string.
#' Either \code{"complementary"} (default) or \code{"rainbow"}
#' specifying the type of color scheme for the ITEM items
#' in the diagnosis summary.
#' If \code{"complementary"}, the colors are chosen complementary
#' within a domain;
#' if \code{"rainbow"}, the DRSP items colors are different for each item and
#' chosen from a rainbow palette.
#' @keywords CPASS C-PASS PMDD MRMD
#' @return a ggplot object
#' @export
#' @import ggplot2
#' @importFrom magrittr %>%
#' @examples
#'
#' library(magrittr)
#' library(dplyr)
#'
#' data(PMDD_data)
#' input <-
#'   PMDD_data %>%
#'   dplyr::filter(subject == 2) %>%
#'   as_cpass_data(., sep_event = "menses")
#' plot_subject_dx(data = input)


plot_subject_dx <-
  function(
    data = data.frame(),
    color_summary = c("complementary", "rainbow")) {

    if (!is_cpass_data(data, silent = TRUE))
      stop(
        paste0(
          "data must be 'cpass.data'.",
          "Use 'as_cpass_data(...)' to check the format of your data",
          "and transform them into 'cpass.data'.")
      )

    color_summary <- color_summary[1]
    if (! (color_summary %in%  c("complementary", "rainbow")))
      stop(
        paste0(
          "This color setting (color_summary = ",
          color_summary,
          ") has not been implemented yet.
          Please chose between 'complementary' (default) or 'rainbow'.")
      )

    subject <- unique(data$subject)
    if (length(subject) > 1)
      stop(
        paste0(
          "The data contains observations reported by more than one subject: ",
          paste0(subject, collapse = ", "))
      )

    subject_diagnosis <- cpass(data, silent = TRUE)

    gtitle <-  ggplot()
    gtitle <-
      gtitle +
      ggtitle(
        stringr::str_c(
          "SUBJECT : ", subject, "   ||   ",
          subject_diagnosis$subject_level_diagnosis$dx %>%
            tidyr::replace_na("Undefined diagnosis"), ""
        ),
        subtitle = stringr::str_c(
          "# CYCLES total: ",
          subject_diagnosis$subject_level_diagnosis$Ncycles_tot, "   ||   ",
          "# CYCLES incl.: ",
          subject_diagnosis$subject_level_diagnosis$Ncycles, "   ||   ",
          "# PMDD cycles: ",
          subject_diagnosis$subject_level_diagnosis$N_PMDD %>%
            tidyr::replace_na("Undefined"), "   ||   ",
          "# MRMD cycles: ",
          subject_diagnosis$subject_level_diagnosis$N_MRMD %>%
            tidyr::replace_na("Undefined")))

    daily_summary <- subject_diagnosis$daily_summary_per_item
    daily_summary <- daily_summary %>%
      dplyr::full_join(
        .,
        dsm5_dict %>%
          dplyr::select(ITEM, ITEM_desc, DSM5_SYMPTOM_DOMAIN, SYMPTOM_CATEGORY) %>%
          dplyr::rename(item = ITEM),
        by = "item") %>%
      dplyr::ungroup() %>%
      dplyr::mutate(
        item = factor(item, levels = dsm5_dict$ITEM),
        ITEM_desc = factor(ITEM_desc, levels = dsm5_dict$ITEM_desc),
        DSM5_SYMPTOM_DOMAIN =
          factor(DSM5_SYMPTOM_DOMAIN,
                 levels = unique(dsm5_dict$DSM5_SYMPTOM_DOMAIN)))

    if (color_summary == "complementary") {
      dsm5_dict <-
        dsm5_dict %>%
        dplyr::group_by(DSM5_SYMPTOM_DOMAIN) %>%
        dplyr::mutate(n_within_domain = dplyr::row_number())
      daily_summary <- daily_summary %>%
        dplyr::mutate(
          ITEM_color =
            dsm5_dict$n_within_domain[match(item, dsm5_dict$ITEM)] %>%
            factor(., levels = 1:max(dsm5_dict$n_within_domain)))
    }else{
      daily_summary <- daily_summary %>%
        dplyr::mutate(ITEM_color = ITEM_desc)
    }

    g_daily_summary <-
      ggplot(daily_summary, aes(x = day, group = ITEM_desc))
    g_daily_summary <-
      g_daily_summary +
      geom_hline(yintercept = 1, col = "gray60") +
      geom_hline(yintercept = 3.8, linetype = 2, col = "gray60") +
      geom_line(aes(y = ave, col = ITEM_color), na.rm = TRUE) +
      geom_area(
        data = daily_summary %>% dplyr::filter(!is.na(ave)),
        position = "identity",
        aes(y = ave, fill = ITEM_color, group = item),
        alpha = 0.3, na.rm = TRUE) +
      geom_point(aes(y = ave, col = ITEM_color), na.rm = TRUE) +
      scale_x_continuous(
        breaks = c(-7, -4, -1, 1, 4, 7, 10), minor_breaks = c(-7:-1, 4:10)) +
      scale_y_continuous(
        breaks = c(1, 4), minor_breaks = 1:6) +
      coord_cartesian(ylim = c(1, 6)) +
      ylab("") + xlab("") +
      facet_grid(DSM5_SYMPTOM_DOMAIN ~ phase, scales = "free_x", switch = "y") +
      theme_minimal() +
      theme(strip.text.y = element_text(angle = 0, hjust = 0.5, size = 7),
            strip.background.y =
              element_rect(fill = .get_strip_background_color(), color = NA),
            strip.placement = "outside",
            strip.text.x = element_blank(),
            panel.spacing.x = unit(30, "pt"),
            panel.spacing.y = unit(5, "pt"),
            legend.position = "none",
            plot.title.position = "plot"
      ) +
      ggtitle("ITEM daily averages")
    if (color_summary == "complementary")
      g_daily_summary <- g_daily_summary +
      scale_color_manual(values = .get_summary_colors()) +
      scale_fill_manual(values = .get_summary_colors())

    summary <- subject_diagnosis$summary_item
    summary <-
      dplyr::full_join(
        summary,
        dsm5_dict %>%
          dplyr::select(ITEM, ITEM_desc, DSM5_SYMPTOM_DOMAIN, SYMPTOM_CATEGORY) %>%
          dplyr::rename(item = ITEM),
        by = "item") %>%
      dplyr::ungroup() %>%
      dplyr::mutate(
        ITEM_fac = factor(item, levels = dsm5_dict$ITEM),
        ITEM_desc = factor(ITEM_desc, levels = dsm5_dict$ITEM_desc),
        DSM5_SYMPTOM_DOMAIN =
          factor(DSM5_SYMPTOM_DOMAIN,
                 levels = unique(dsm5_dict$DSM5_SYMPTOM_DOMAIN)))

    if (color_summary == "complementary") {
      dsm5_dict <- dsm5_dict %>%
        dplyr::group_by(DSM5_SYMPTOM_DOMAIN) %>%
        dplyr::mutate(n_within_domain = dplyr::row_number())
      summary <-
        summary %>%
        dplyr::mutate(
          ITEM_color =
            dsm5_dict$n_within_domain[match(item, dsm5_dict$ITEM)] %>%
            factor(., levels = 1:max(dsm5_dict$n_within_domain)))
    }else{
      summary <-  summary %>%
        dplyr::mutate(ITEM_color = ITEM_desc)
    }

    ITEM_desc <- factor(dsm5_dict$ITEM_desc, levels = dsm5_dict$ITEM_desc)
    g_summary <-
      ggplot(summary, aes(y = item, x = ave_perc_change, col = ITEM_color))
    g_summary <-
      g_summary +
      geom_vline(xintercept = 0, linetype = 1, col = "gray60") +
      geom_vline(xintercept = 30, linetype = 2, col = "gray60") +
      geom_segment(
        aes(y = item, yend = item, x = 0, xend = ave_perc_change),
        size = 1.2, na.rm = TRUE) +
      geom_point(size = 1.5, na.rm = TRUE) +
      ylab("") + xlab("") +
      scale_y_reverse(
        breaks = seq_along(ITEM_desc), labels = seq_along(ITEM_desc),
        minor_breaks = NULL,
        expand = expansion(add = 0.5),
        sec.axis =
          sec_axis(~., breaks = seq_along(ITEM_desc), labels = ITEM_desc)) +
      guides(col = "none") +
      facet_grid(DSM5_SYMPTOM_DOMAIN ~ ., scales = "free_y") +
      theme_minimal() +
      theme(strip.text = element_blank(),
            panel.spacing.y = unit(5, "pt")) +
      ggtitle("Av. % change")
    if (color_summary == "complementary")
      g_summary <-  g_summary +
      scale_color_manual(values = .get_summary_colors()) +
      scale_fill_manual(values = .get_summary_colors())


    #g_summary

    plots <-
      cowplot::plot_grid(
        plotlist = list(g_daily_summary, g_summary),
        ncol = 2, nrow = 1,  rel_widths = c(14, 13.5))
    plots_with_title <-
      cowplot::plot_grid(
        plotlist = list(gtitle, plots),
        ncol = 1, nrow = 2,  rel_heights = c(1, 10))

    return(plots_with_title)
  }




#' Visualize the scores of a subject's cycle
#'
#' This function visualizes the scores (raw data) of a single subject's cycle.
#' It also internally calls the \code{CPASS} function to provide
#' a visualization of the diagnosis at the ITEM, DSM5 DOMAIN and CYCLE level.
#' @param data a \code{cpass} data frame
#' (use \code{as_cpass_data} to convert your data into \code{cpass} data)
#' that contains the symptoms reported in ONE cycle by ONE subject.
#' @param add_diagnosis logical.
#' If \code{TRUE} (default), the diagnoses at the
#' ITEM, DSM5-DOMAIN and CYCLE levels are displayed together
#' with the subject's reported scores.
#' If \code{FALSE}, only the reported scores are displayed.
#' @param color_max_score string specifying the color of a score of 6
#' (the maximal score) reported by a subject.
#' Any standard color format specification is accepted,
#' i.e. one of the R built-in color names
#' (e.g. "tomato" (default); type \code{colors()}
#' to see the names of all R built-in colors),
#' an RGB hex code (e.g. "#AA2199")
#' or a color specified via one of the color/palette functions
#' (e.g. hsv(0.1,0.9,0.9))
#' @param silent a \code{logical}
#' specifying is the function should print messages or run silently.
#' Default is \code{FALSE}.
#' @keywords CPASS C-PASS PMDD MRMD
#' @return a ggplot object
#' @export
#' @import ggplot2
#' @importFrom magrittr %>%
#' @examples
#'
#' library(magrittr)
#' library(dplyr)
#'
#' data(PMDD_data)
#' input <-
#'   PMDD_data %>%
#'   dplyr::filter(subject == 2, cycle == 1)  %>%
#'   as_cpass_data(., sep_event = "menses")
#' plot_subject_cycle_obs(data = input)
#'


plot_subject_cycle_obs <-
  function(
    data = data.frame(),
    add_diagnosis = TRUE,
    color_max_score = "tomato", silent = FALSE) {

    add_legend <- FALSE # to change

    if (!is_cpass_data(data, silent = TRUE))
      stop(
        paste0(
          "data must be 'cpass.data'.",
          "Use 'as_cpass_data(...)' to check the format of your data",
          "and transform them into 'cpass.data'.")
      )

    if (length(unique(data$subject)) > 1)
      stop("The data must include the observations of only ONE subject")

    if (length(unique(data$cycle)) > 1)
      stop("The data must include the observations of only ONE cycle")

    if (add_diagnosis) {

      cycle_diagnosis <-  cpass(data, silent = TRUE)

      title_add <-
        stringr::str_c("     ",
                       ifelse(cycle_diagnosis$cycle_level_diagnosis$included,
                              cycle_diagnosis$cycle_level_diagnosis$diagnosis,
                              "not included"))

      ITEM_diagnosis <-
        cycle_diagnosis$item_level_diagnosis %>%
        dplyr::select(
          subject, cycle, item,
          DSM5_SYMPTOM_DOMAIN,
          item_meets_PMDD_criteria,
          item_meets_PME_criteria,
        ) %>%
        tidyr::pivot_longer(
          .,
          cols = c(item_meets_PMDD_criteria, item_meets_PME_criteria),
          values_to = "meets_criteria",
          names_to = "criteria") %>%
        dplyr::mutate(
          criteria =
            ifelse(stringr::str_detect(criteria, "PMDD"),
                   "PMDD", "PME") %>%
            factor(., levels = c("PME", "PMDD")),
          DSM5_SYMPTOM_DOMAIN = DSM5_SYMPTOM_DOMAIN %>%
            factor(., levels = dsm5_dict$DSM5_SYMPTOM_DOMAIN %>% unique()),
          meets_criteria = ifelse(item %in% c(20, 22:24), NA, meets_criteria)
        ) %>%
        dplyr::left_join(
          .,
          dsm5_dict %>%
            dplyr::ungroup() %>%
            dplyr::select(ITEM, ITEM_desc) %>%
            dplyr::rename(item = ITEM),
          by = "item") %>%
        dplyr::arrange(-item) %>%
        dplyr::mutate(ITEM_axis = paste0(item, ". ", ITEM_desc),
                      ITEM_axis = ITEM_axis %>%
                        factor(., levels = ITEM_axis %>%  unique()))

      g_item_names <-
        ggplot(ITEM_diagnosis, aes(x = 1, y = ITEM_axis)) +
        geom_blank() +
        facet_grid(DSM5_SYMPTOM_DOMAIN ~ ., scales = "free", space = "free") +
        ggtitle(" ") +
        xlab("") + ylab("") +
        scale_x_continuous(breaks = NULL) +
        theme_minimal() +
        theme(strip.text.y = element_blank(),
              title = element_text(size = 9, hjust = 0.5))


      g_item <-
        ggplot(ITEM_diagnosis,
               aes(x = 1, y = ITEM_axis, fill = meets_criteria))
      g_item <-
        g_item +
        geom_tile(col = "white", size = 0.5, na.rm = TRUE) +
        guides(fill = "none") +
        xlab("") + ylab("") +
        scale_x_continuous(breaks = NULL) +
        scale_fill_manual(
          values = c("gray90", color_max_score),
          breaks = c(FALSE, TRUE),
          na.value = "transparent") +
        facet_grid(DSM5_SYMPTOM_DOMAIN ~ criteria,
                   scales = "free", space = "free") +
        ggtitle("ITEM") +
        theme_minimal() +
        theme(strip.text.y = element_blank(),
              axis.text.y = element_blank(),
              strip.text.x = element_text(size = 7),
              plot.title = element_text(size = 9, hjust = 0.5))
      # g_item


      DOMAIN_diagnosis <-
        cycle_diagnosis$DSM5_domains_level_diagnosis %>%
        dplyr::select(
          subject, cycle, DSM5_SYMPTOM_DOMAIN,
          DSM5_PMDD_criteria, PME_criteria) %>%
        dplyr::bind_rows(
          .,
          data.frame(DSM5_SYMPTOM_DOMAIN = "INTERFERENCE")) %>%
        tidyr::pivot_longer(
          .,
          cols = c(DSM5_PMDD_criteria, PME_criteria),
          names_to = "criteria",
          values_to = "meets_criteria") %>%
        dplyr::mutate(
          criteria =
            ifelse(stringr::str_detect(criteria, "PMDD"),
                   "PMDD", "PME") %>%
            factor(., levels = c("PME", "PMDD"))) %>%
        dplyr::left_join(
          .,
          dsm5_dict %>%
            dplyr::select(ITEM, DSM5_SYMPTOM_DOMAIN) %>%
            dplyr::rename(item = ITEM),
          by = c("DSM5_SYMPTOM_DOMAIN")) %>%
        dplyr::mutate(
          item = item %>% factor(., levels = rev(dsm5_dict$ITEM)),
          DSM5_SYMPTOM_DOMAIN = DSM5_SYMPTOM_DOMAIN %>%
            factor(., levels = dsm5_dict$DSM5_SYMPTOM_DOMAIN %>% unique()),
          meets_criteria = ifelse(item %in% c(22:24), NA, meets_criteria)
        )

      g_domain <-
        ggplot(DOMAIN_diagnosis, aes(x = 1, y = item, fill = meets_criteria))
      g_domain <-
        g_domain +
        geom_tile(col = "transparent", na.rm = TRUE) +
        guides(fill = "none") +
        xlab("") + ylab("") +
        scale_x_continuous(breaks = NULL) +
        scale_fill_manual(
          values = c("gray90", color_max_score),
          breaks = c(FALSE, TRUE),
          na.value = "transparent") +
        facet_grid(
          DSM5_SYMPTOM_DOMAIN ~ criteria, scales = "free", space = "free") +
        ggtitle("DOMAIN") +
        theme_minimal() +
        theme(strip.text.y = element_blank(),
              strip.text.x = element_text(size = 7),
              plot.title = element_text(size = 9, hjust = 0.5),
              axis.text.y = element_blank())
      # g_domain


      CYCLE_diagnosis <-
        cycle_diagnosis$cycle_level_diagnosis %>%
        dplyr::select(subject, cycle, PME, DSM5_A, DSM5_B) %>%
        dplyr::rename(MRMD = DSM5_A, PMDD = DSM5_B) %>%
        tidyr::pivot_longer(
          ., cols = c(PME, PMDD, MRMD),
          names_to = "criteria",
          values_to = "meets_criteria") %>%
        dplyr::mutate(
          criteria = criteria %>% factor(., levels = c("PME", "MRMD", "PMDD")))

      g_cycle <-
        ggplot(CYCLE_diagnosis, aes(x = 1, y = 1, fill = meets_criteria))
      g_cycle <-
        g_cycle +
        geom_tile(col = NA, na.rm = TRUE, height = 1) +
        guides(fill = "none") +
        xlab("") + ylab("") +
        scale_x_continuous(breaks = NULL) +
        scale_y_continuous(
          breaks = NULL, limits = c(0.5, 1.5), expand = c(0, 0)) +
        scale_fill_manual(
          values = c("gray90", color_max_score),
          breaks = c(FALSE, TRUE),
          na.value = "gray45") +
        facet_grid(. ~ criteria, scales = "free", space = "free") +
        ggtitle("CYCLE") +
        theme_minimal() +
        theme(strip.text.y = element_blank(),
              strip.text.x = element_text(size = 7),
              plot.title = element_text(size = 9, hjust = 0.5)
        )
      # g_cycle

    } else {
      title_add <-  ""
    }


    obs <-  data
    if (!("DSM5_SYMPTOM_DOMAIN" %in% colnames(obs)))
      obs <-
      dplyr::full_join(
        obs, dsm5_dict %>% dplyr::rename(item = ITEM), by = c("item"))

    obs <-  obs %>%
      dplyr::mutate(
        item = item %>% factor(., levels = rev(dsm5_dict$ITEM)),
        DSM5_SYMPTOM_DOMAIN =
          DSM5_SYMPTOM_DOMAIN %>%
          factor(., levels = unique(dsm5_dict$DSM5_SYMPTOM_DOMAIN)))

    gcanvas <-
      ggplot(obs, aes(x = day, y = item,  fill = drsp_score))
    g <-
      gcanvas +
      geom_tile(col = "white", size = 0.5, na.rm = TRUE) +
      geom_text(aes(label = drsp_score), size = 2, na.rm = TRUE) +
      scale_fill_gradient(
        low = "gray90", high = color_max_score,
        breaks = 1:6, limits = c(1, 6), na.value = "transparent") +
      scale_alpha_continuous(
        na.value = 0, range = c(0.1, 1), limits = c(1, 6)) +
      scale_x_continuous(
        breaks = c(-7, -4, -1, 1, 4, 7, 10), minor_breaks = c(-7:-1, 4:10)) +
      guides(size = "none") +
      facet_grid(
        DSM5_SYMPTOM_DOMAIN ~ phase,
        scales = "free", space = "free", switch = "y") +
      theme_minimal() +
      theme(strip.text.y = element_text(angle = 0, hjust = 0.5, size = 7),
            strip.background.y =
              element_rect(fill = .get_strip_background_color(), color = NA),
            strip.placement = "outside",
            panel.spacing.x = unit(30, "pt"),
            panel.spacing.y = unit(2, "pt"),
            legend.position = ifelse(add_legend, "bottom", "none"),
            plot.title.position = "plot") +
      ylab("") +
      xlab("") +
      ggtitle(stringr::str_c("CYCLE: ", unique(data$cycle), title_add))
    # g

    if (add_diagnosis) {
      g <-
        cowplot::plot_grid(
          g,
          g_item_names,
          g_item, g_domain, g_cycle,
          align = "h",
          axis = "tb",
          nrow = 1,
          rel_widths = c(14, 3, 3, 3, 4.5)) # 2, 1.2
    }

    if (!silent & add_diagnosis)
      message(
        paste0(
          "PME diagnosis is still experimental and ",
          "has not be validated clinically. ",
          "Please, use with caution.\n")
      )

    return(g)
  }


#' Visualize all the observations of a single subject.
#'
#' This function visualizes the scores (raw data) reported by a subject.
#' It also internally calls the \code{cpass} function to provide
#' a visualization of the diagnosis at the ITEM, DSM5 DOMAIN and CYCLE level.
#' @param data a \code{cpass} data frame that
#' contains the symptoms reported by ONE subject.
#' @param add_diagnosis logical.
#' If \code{TRUE} (default), the diagnoses at the
#' ITEM, DSM5-DOMAIN and CYCLE levels are displayed together with
#' the subject's reported scores.
#' If \code{FALSE}, only the reported scores are displayed.
#' @param color_max_score string specifying the color of a score of 6
#' (the maximal score) reported by a subject.
#' Any standard color format specification is accepted,
#' i.e. one of the R built-in color names
#' (e.g. "tomato" (default); type \code{colors()}
#' to see the names of all R built-in colors),
#' an RGB hex code (e.g. "#AA2199") or
#' a color specified via one of the color/palette functions
#' (e.g. hsv(0.1,0.9,0.9))
#' @param silent a \code{logical} specifying if
#' the function should print messages or run silently.
#' Default is \code{FALSE}.
#' @keywords CPASS C-PASS PMDD MRMD
#' @return a ggplot object
#' @export
#' @import ggplot2
#' @importFrom magrittr %>%
#' @examples
#'
#' library(magrittr)
#' library(dplyr)
#'
#' data(PMDD_data)
#' input <-
#'   PMDD_data %>%
#'   dplyr::filter(subject == 2)  %>%
#'   as_cpass_data(., sep_event = "menses")
#' plot_subject_obs(data = input)
#'


plot_subject_obs <-
  function(
    data = data.frame(),
    add_diagnosis = TRUE,
    color_max_score = "tomato",
    silent = FALSE) {


    if (!is_cpass_data(data, silent = TRUE))
      stop(
        paste0(
          "data must be 'cpass.data'.",
          "Use 'as_cpass_data(...)' to check the format of your data",
          "and transform them into 'cpass.data'.")
      )


    if (length(unique(data$subject)) > 1)
      stop("The data must include the observations of only ONE subject")


    cycles <-  sort(unique(data$cycle))


    plotlist <-
      purrr::map(
        .x = cycles,
        .f = function(cycle) {
          this_cycle_data <-
            data[which(data$cycle == cycle), ]
          plot_subject_cycle_obs(
            data = this_cycle_data,
            add_diagnosis = add_diagnosis,
            color_max_score = color_max_score,
            silent = TRUE)
        })

    g_all_cycles <-
      cowplot::plot_grid(plotlist = plotlist, ncol = 1, align = "v")

    if (!silent & add_diagnosis)
      message(
        paste0(
          "PME diagnosis is still experimental ",
          "and has not be validated clinically. ",
          "Please, use with caution.\n")
      )

    return(g_all_cycles)
  }


#' Visualize all the observations and the diagnosis summary of a single subject.
#'
#' By default, this function saves the visualization in a pdf.
#' The pdf heigth scales with the number of cycles reported for the subject.
#'
#' @param data a \code{cpass} data.frame that
#'     contains the symptoms reported by ONE subject.
#' @param save_as_pdf logical.
#'     If \code{TRUE} (default), this function saves the visualization in a pdf.
#'     The pdf path and name can be specified with the arguments
#'     \code{pdf_path} and \code{pdf_name}.
#'     If \code{FALSE}, this function returns a ggplot object.
#'     The same object is also returned, but invisibly,
#'     when \code{save_as_pdf = TRUE} (see examples).
#' @param pdf_path string.
#'     Specifies the path to the folder in which the pdf should be saved.
#'     By default, the path is an empty string
#'     so the pdf is be saved in the current working directory.
#' @param pdf_name string. Specifies the name of the pdf.
#'     By default, the name of the pdf is \code{CPASS_SUBJECT_X.pdf},
#'     where \code{X} is replaced by the subject unique identifier.
#' @param color_max_score string specifying the color of a score of 6
#' (the maximal score) reported by a subject.
#' Any standard color format specification is accepted,
#' i.e. one of the R built-in color names
#'  (e.g. "tomato" (default);
#'  type \code{colors()} to see the names of all R built-in colors),
#'  an RGB hex code (e.g. "#AA2199") or
#'  a color specified via one of the color/palette functions
#'  (e.g. hsv(0.1,0.9,0.9))
#' @param color_summary string.
#' Either \code{"complementary"} (default) or \code{"rainbow"}
#' specifying the type of color scheme for the ITEM items
#' in the diagnosis summary.
#' If \code{"complementary"}, the colors are chosen complementary
#' within a domain; if \code{"rainbow"},
#' the item colors are different for each item and
#' chosen from a rainbow palette.
#'
#' @keywords CPASS C-PASS PMDD MRMD
#' @return a ggplot object if \code{save_as_pdf = FALSE}.
#' The same object is returned invisibly by default
#' (\code{save_as_pdf = TRUE}).
#' @export
#' @import ggplot2
#' @importFrom magrittr %>%
#' @examples
#'
#' library(magrittr)
#' library(dplyr)
#'
#' data(PMDD_data)
#' input =
#'   PMDD_data %>%
#'   dplyr::filter(subject == 2) %>%
#'   as_cpass_data(., sep_event = "menses")
#' p = plot_subject_data_and_dx(data = input)
#' p


plot_subject_data_and_dx <-
  function(
    data = data.frame(),
    save_as_pdf = TRUE, pdf_path = "", pdf_name = "",
    color_max_score = "tomato", color_summary = c("complementary", "rainbow")
  ) {


    if (!is_cpass_data(data, silent = TRUE))
      stop(
        paste0(
          "data must be 'cpass.data'.",
          "Use 'as_cpass_data(...)' to check the format of your data",
          "and transform them into 'cpass.data'.")
      )

    # the check for 1 subject is done in "plot_subject_dx"

    g_diagnosis_summary <-
      suppressWarnings(
        plot_subject_dx(data = data, color_summary = color_summary)
      )
    g_data <-
      suppressWarnings(
        plot_subject_obs(
          data = data,
          add_diagnosis = TRUE,
          color_max_score = color_max_score,
          silent = TRUE))
    n_cycles <-  length(g_data$layers)
    g <-
      suppressWarnings(
        cowplot::plot_grid(
          g_diagnosis_summary,
          g_data,
          ncol = 1, nrow = 2,
          rel_heights = c(1.2, n_cycles))
      )
    #g

    if (save_as_pdf) {
      if (pdf_name == "")
        pdf_name <-
          paste0("CPASS_SUBJECT_", unique(data$subject), ".pdf")
      if (pdf_path == "")
        pdf_path <-  getwd()
      if ((pdf_path != "") && (stringr::str_sub(pdf_path, -1) != "/"))
        pdf_path <-  stringr::str_c(pdf_path, "/")

      pdf_filename <-  stringr::str_c(pdf_path, pdf_name)
      ggsave(
        g,
        filename = pdf_filename,
        width = 75, height = 50 * (1.2 + length(g_data$layers)),
        units = "mm", scale = 3.7)
      cat("Subject summary saved in '", pdf_filename, "'\n")
      return(invisible(g))
    }

    message(
      paste0(
        "PME diagnosis is still experimental ",
        "and has not be validated clinically. ",
        "Please, use with caution.\n")
    )

    return(g)
  }
