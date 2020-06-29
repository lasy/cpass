
.get_strip_background_color = function() return("gray80")

.get_summary_colors = function() return(c("#F87875","#659EED","#33CC59","#C77CFF"))



#' Visualize the subject diagnosis
#'
#' This function calls the \code{CPASS} function and provides a visualization of the diagnosis.
#' @param data a data.frame that contains the symptoms reported by ONE subject. The data must be in a long format and have the following columns: \code{SUBJECT}, \code{CYCLE}, \code{DAY}, \code{DRSP}, \code{score}
#' @param colors_summary string. Either \code{"complementary"} (default) or \code{"rainbow"} specifying the type of color scheme for the DRSP items in the diagnosis summary. If \code{"complementary"}, the colors are chosen complementary within a domain; if \code{"rainbow"}, the DRSP colors are different for each item and chosen from a rainbow palette.
#'
#' @keywords CPASS C-PASS PMDD MRMD
#' @return a ggplot object
#' @export
#' @importFrom magrittr %>%
#' @examples
#' data(PMDD_data)
#' input = PMDD_data %>% filter(SUBJECT == 2) %>%  as_cpass_data()
#' plot_subject_diagnosis(data = input)


plot_subject_diagnosis = function(data = data.frame(), color_summary = c("complementary","rainbow")){

  color_summary = color_summary[1]
  if(! (color_summary %in%  c("complementary","rainbow"))) stop("This color setting (color_summary = ",color_summary,") has not been implemented yet. Please chose between 'complementary' (default) or 'rainbow'.")

  subject = unique(data$SUBJECT)
  if(length(subject)>1) stop(
    stringr::str_c("The data contains observations reported by more than one subject: ",
                   stringr::str_c(subject, collapse = ", "))
  )

  subject_diagnosis = CPASS(data)

  gtitle = ggplot()
  gtitle = gtitle +
    ggtitle(
      stringr::str_c(
        "SUBJECT : ",subject,"   ||   ",
        subject_diagnosis$SUBJECT_level_diagnosis$dx %>% tidyr::replace_na("Undefined diagnosis"),""
      ),
      subtitle = stringr::str_c(
        "# CYCLES total: ", subject_diagnosis$SUBJECT_level_diagnosis$NCycles_tot,"   ||   ",
        "# CYCLES incl.: ", subject_diagnosis$SUBJECT_level_diagnosis$NCycles,"   ||   ",
        "# PMDD cycles: ", subject_diagnosis$SUBJECT_level_diagnosis$N_PMDD %>% tidyr::replace_na("Undefined"),"   ||   ",
        "# MRMD cycles: ", subject_diagnosis$SUBJECT_level_diagnosis$N_MRMD %>% tidyr::replace_na("Undefined")))
  gtitle

  daily_summary = subject_diagnosis$daily_summary_DRSP
  daily_summary = daily_summary %>%
    dplyr::full_join(.,
              dsm5_dict %>%  select(DRSP,DRSP_desc, DSM5_SYMPTOM_DOMAIN, SYMPTOM_CATEGORY),
              by = "DRSP") %>%
    dplyr::ungroup() %>%
    dplyr::mutate(DRSP = factor(DRSP, levels = dsm5_dict$DRSP),
           DRSP_desc = factor(DRSP_desc, levels = dsm5_dict$DRSP_desc),
           DSM5_SYMPTOM_DOMAIN = factor(DSM5_SYMPTOM_DOMAIN, levels = unique(dsm5_dict$DSM5_SYMPTOM_DOMAIN)))

  if(color_summary == "complementary"){
    dsm5_dict = dsm5_dict %>% dplyr::group_by(DSM5_SYMPTOM_DOMAIN) %>% dplyr::mutate(n_within_domain = row_number())
    daily_summary = daily_summary %>%
      dplyr::mutate(DRSP_color = dsm5_dict$n_within_domain[match(DRSP, dsm5_dict$DRSP)] %>%
                      factor(., levels = 1:max(dsm5_dict$n_within_domain)))
  }else{
    daily_summary = daily_summary %>%
      dplyr::mutate(DRSP_color = DRSP_desc)
  }

  g_daily_summary = ggplot(daily_summary, aes(x = DAY, group = DRSP_desc))
  g_daily_summary = g_daily_summary +
    geom_hline(yintercept = 1, col = "gray60")+
    geom_hline(yintercept = 3.8, linetype = 2, col = "gray60")+
    #geom_ribbon(aes(ymin = min, ymax = max, fill = DRSP_desc), alpha = 0.2)+
    geom_line(aes(y = ave, col = DRSP_color))+
    geom_area(position = "identity",aes(y = ave, fill = DRSP_color), alpha = 0.3)+
    geom_point(aes(y = ave, col = DRSP_color))+
    scale_x_continuous(breaks = c(-7,-4,-1,1,4,7,10), minor_breaks = c(-7:-1,4:10))+
    scale_y_continuous(breaks = c(1,4), minor_breaks = 1:6)+
    coord_cartesian(ylim=c(1, 6))+
    ylab("")+xlab("")+
    facet_grid(DSM5_SYMPTOM_DOMAIN ~ PHASE, scale = "free_x", switch = "y")+
    theme_minimal()+
    theme(strip.text.y.left = element_text(angle = 0, hjust = 1),
          strip.background.y = element_rect(fill = .get_strip_background_color(), color = NA),
          strip.placement = "outside",
          strip.text.x = element_blank(),
          panel.spacing.x = unit(30,"pt"),
          panel.spacing.y = unit(5,"pt"),
          legend.position = "none",
          plot.title.position = "plot"
    )+
    ggtitle("DRSP daily averages")
  if(color_summary == "complementary")
    g_daily_summary = g_daily_summary +
    scale_color_manual(values = .get_summary_colors()) +
    scale_fill_manual(values = .get_summary_colors())

  summary = subject_diagnosis$summary_DRSP
  summary =
    dplyr::full_join(summary,
                             dsm5_dict %>%  select(DRSP,DRSP_desc, DSM5_SYMPTOM_DOMAIN, SYMPTOM_CATEGORY),
                             by = "DRSP") %>%
    dplyr::ungroup() %>%
    dplyr::mutate(DRSP_fac = factor(DRSP, levels = dsm5_dict$DRSP),
           DRSP_desc = factor(DRSP_desc, levels = dsm5_dict$DRSP_desc),
           DSM5_SYMPTOM_DOMAIN = factor(DSM5_SYMPTOM_DOMAIN, levels = unique(dsm5_dict$DSM5_SYMPTOM_DOMAIN)))

  if(color_summary == "complementary"){
    dsm5_dict = dsm5_dict %>% dplyr::group_by(DSM5_SYMPTOM_DOMAIN) %>% dplyr::mutate(n_within_domain = row_number())
    summary = summary %>%
      dplyr::mutate(DRSP_color = dsm5_dict$n_within_domain[match(DRSP, dsm5_dict$DRSP)]%>%
                      factor(., levels = 1:max(dsm5_dict$n_within_domain)))
  }else{
    summary = summary %>%
      dplyr::mutate(DRSP_color = DRSP_desc)
  }

  drsp_desc = factor(dsm5_dict$DRSP_desc, levels = dsm5_dict$DRSP_desc)
  g_summary = ggplot(summary, aes(y = DRSP, x = ave_perc_change, col = DRSP_color))
  g_summary = g_summary+
    geom_vline(xintercept = 0, linetype = 1, col = "gray60")+
    geom_vline(xintercept = 30, linetype = 2, col = "gray60")+
    geom_segment(aes(y = DRSP, yend = DRSP, x = 0, xend = ave_perc_change), size = 1.2)+
    geom_point(size = 1.5)+
    ylab("")+xlab("")+
    scale_y_reverse(breaks = 1:length(drsp_desc), labels = 1:length(drsp_desc),
                    minor_breaks = NULL,
                    expand = expansion(add = 0.5),
                    sec.axis = sec_axis(~.,breaks = 1:length(drsp_desc),labels = drsp_desc))+
    guides(col = FALSE)+
    facet_grid(DSM5_SYMPTOM_DOMAIN ~ ., scale = "free_y")+
    theme_minimal()+
    theme(strip.text = element_blank(),
          panel.spacing.y = unit(5,"pt"))+
    ggtitle("Av. % change")
  if(color_summary == "complementary")
    g_summary = g_summary +
    scale_color_manual(values = .get_summary_colors()) +
    scale_fill_manual(values = .get_summary_colors())


  #g_summary

  plots = cowplot::plot_grid(plotlist = list(g_daily_summary, g_summary), ncol = 2, nrow =1,  rel_widths = c(2,1.2))
  plots_with_title = cowplot::plot_grid(plotlist = list(gtitle, plots), ncol = 1, nrow =2,  rel_heights = c(1,10))

  return(plots_with_title)
}




#' Visualize the scores of a subject's cycle
#'
#' This function visualizes the scores (raw data) of a single subject's cycle. It also internally calls the \code{CPASS} function to provides a visualization of the diagnosis at the DRSP, DSM5 DOMAIN and CYCLE level.
#' @param data a data.frame that contains the symptoms reported in ONE cycle by ONE subject. The data must be in a long format and have the following columns: \code{SUBJECT}, \code{CYCLE}, \code{DAY}, \code{DRSP}, \code{score}
#' @param add_diagnosis logical. If \code{TRUE} (default), the diagnoses at the DRSP, DSM5-DOMAIN and CYCLE levels are displayed together with the subject's reported scores. If \code{FALSE}, only the reported scores are displayed.
#' @param color_max_score string specifying the color of a score of 6 (the maximal score) reported by a subject. Any standard color format specification is accepted, i.e. one of the R built-in color names (e.g. "tomato" (default); type \code{colors()} to see the names of all R built-in colors), an RGB hex code (e.g. "#AA2199") or a color specified via one of the color/palette functions (e.g. hsv(0.1,0.9,0.9))
#' @keywords CPASS C-PASS PMDD MRMD
#' @return a ggplot object
#' @export
#' @importFrom magrittr %>%
#' @examples
#' data(PMDD_data)
#' input = PMDD_data %>% filter(SUBJECT == 2, CYCLE == 1)  %>%  as_cpass_data()
#' plot_subject_cycle_obs(data = input)
#'


plot_subject_cycle_obs = function(data = data.frame(), add_diagnosis = TRUE, color_max_score = "tomato"){
  add_legend = FALSE # to change
  columns = c("CYCLE","DAY","PHASE","DRSP","score")
  if(any(!(columns %in% colnames(data)))){stop(stringr::str_c("The data must include the following columns:",columns))}
  if(("SUBJECT" %in% colnames(data)) & (length(unique(data$SUBJECT))>1)){stop("The data must include the observations of only ONE subject")}
  if(length(unique(data$CYCLE))>1){stop("The data must include the observations of only ONE cycle")}


  if(add_diagnosis){

    cycle_diagnosis = CPASS(data)

    title_add = stringr::str_c("     ",ifelse(cycle_diagnosis$CYCLE_level_diagnosis$included,cycle_diagnosis$CYCLE_level_diagnosis$diagnosis,"not included"))

    # preparing data to plot the diagnosis
    df = dplyr::full_join(cycle_diagnosis$DRSP_level_diagnosis %>%  select(SUBJECT, CYCLE, DRSP, DRSP_meets_criteria, DSM5_SYMPTOM_DOMAIN),
                   cycle_diagnosis$DSM5_DOMAINS_level_diagnosis %>%  select(SUBJECT, CYCLE, DSM5_SYMPTOM_DOMAIN, DSM5_criteria),
                   by = c("SUBJECT", "CYCLE", "DSM5_SYMPTOM_DOMAIN"))
    df = dplyr::full_join(df, cycle_diagnosis$CYCLE_level_diagnosis %>%  select(-n_DSM5_DOMAINS_meeting_criteria),
                   by = c("SUBJECT", "CYCLE"))
    df = df %>%  dplyr::ungroup()

    df_long = tidyr::pivot_longer(df %>%  select(DRSP, DSM5_SYMPTOM_DOMAIN, DRSP_meets_criteria, DSM5_criteria, DSM5_A, DSM5_B),
                           cols = c("DRSP_meets_criteria","DSM5_criteria","DSM5_A","DSM5_B"),
                           names_to = "level",
                           values_to = "meets_criteria")
    df_long = df_long %>%
      dplyr::mutate(
        level = case_when(
          level == "DRSP_meets_criteria" ~ "DRSP",
          level == "DSM5_criteria" ~ "domain",
          TRUE ~ level) %>%  factor(., levels = c("DRSP","domain","DSM5_A","DSM5_B")),
        DRSP = DRSP %>% factor(., levels = rev(dsm5_dict$DRSP)),
        DSM5_SYMPTOM_DOMAIN = DSM5_SYMPTOM_DOMAIN %>% factor(., levels = unique(dsm5_dict$DSM5_SYMPTOM_DOMAIN))
      )
    df_long$meets_criteria[(df_long$level != "DRSP") & (df_long$DRSP %in% c(20, 22:24))] = NA

    g_diagnosis = ggplot(df_long, aes(y = DRSP, x = 1, fill = meets_criteria))
    g_diagnosis = g_diagnosis+
      geom_tile(col = "white")+
      facet_grid(DSM5_SYMPTOM_DOMAIN ~ level, scale = "free_y", space = "free")+
      scale_fill_manual(values = c("gray90", color_max_score),na.value = "gray45")+
      guides(fill = FALSE)+
      xlab("")+ylab("")+
      scale_x_continuous(breaks = NULL)+
      theme_minimal()+
      theme(strip.text.y = element_blank())
    #g_diagnosis

  }else{
    title_add = ""
  }


  obs = data
  if(!("DSM5_SYMPTOM_DOMAIN" %in% colnames(obs))){obs = dplyr::full_join(obs, dsm5_dict, by = c("DRSP"))}

  obs = obs %>%
    dplyr::mutate(DRSP = DRSP %>% factor(.,levels = rev(dsm5_dict$DRSP)),
           DSM5_SYMPTOM_DOMAIN = DSM5_SYMPTOM_DOMAIN %>% factor(.,levels = unique(dsm5_dict$DSM5_SYMPTOM_DOMAIN)),
           PHASE = PHASE %>%  factor(.,levels = c("pre","post")))


  gcanvas = ggplot(obs, aes(x = DAY, y = DRSP,  fill = score))
  g = gcanvas +
    geom_tile(col = "white", size = 0.5)+
    geom_text(aes(label = score), size = 2)+
    scale_fill_gradient(low = "gray90", high = color_max_score, breaks = 1:6, limits = c(1,6), na.value = "transparent")+
    scale_alpha_continuous(na.value = 0, range = c(0.1,1), limits = c(1,6))+
    scale_x_continuous(breaks = c(-7,-4,-1,1,4,7,10), minor_breaks = c(-7:-1,4:10))+
    guides(size = FALSE)+ # col = FALSE
    facet_grid(DSM5_SYMPTOM_DOMAIN ~ PHASE, scale = "free", space = "free", switch = "y")+
    theme_minimal()+
    theme(strip.text.y.left = element_text(angle = 0, hjust = 1),
          strip.background.y = element_rect(fill = .get_strip_background_color(), color = NA),
          strip.placement = "outside",
          panel.spacing.x = unit(30,"pt"),
          panel.spacing.y = unit(2,"pt"),
          legend.position = ifelse(add_legend,"bottom","none"),
          plot.title.position = "plot")+
    ylab("")+
    xlab("")+
    ggtitle(stringr::str_c("CYCLE: ", unique(data$CYCLE),title_add))
  #g

  if(add_diagnosis){
    g = cowplot::plot_grid(g, g_diagnosis, align = "h", axis = "bt", ncol = 2, nrow = 1, rel_widths = c(2,1.2))
  }

  return(g)
}


#' Visualize all the observations of a single subject.
#'
#' This function visualizes the scores (raw data) reported by a subject. It also internally calls the \code{CPASS} function to provides a visualization of the diagnosis at the DRSP, DSM5 DOMAIN and CYCLE level.
#' @param data a data.frame that contains the symptoms reported by ONE subject. The data must be in a long format and have the following columns: \code{SUBJECT}, \code{CYCLE}, \code{DAY}, \code{DRSP}, \code{score}
#' @param add_diagnosis logical. If \code{TRUE} (default), the diagnoses at the DRSP, DSM5-DOMAIN and CYCLE levels are displayed together with the subject's reported scores. If \code{FALSE}, only the reported scores are displayed.
#' @param color_max_score string specifying the color of a score of 6 (the maximal score) reported by a subject. Any standard color format specification is accepted, i.e. one of the R built-in color names (e.g. "tomato" (default); type \code{colors()} to see the names of all R built-in colors), an RGB hex code (e.g. "#AA2199") or a color specified via one of the color/palette functions (e.g. hsv(0.1,0.9,0.9))
#' @keywords CPASS C-PASS PMDD MRMD
#' @return a ggplot object
#' @export
#' @importFrom magrittr %>%
#' @examples
#' data(PMDD_data)
#' input = PMDD_data %>% filter(SUBJECT == 2)  %>%  as_cpass_data()
#' plot_subject_obs(data = input)
#'


plot_subject_obs = function(data = data.frame(), add_diagnosis = TRUE, color_max_score = "tomato"){
  columns = c("CYCLE","DAY","PHASE","DRSP","score")
  if(any(!(columns %in% colnames(data)))){stop(stringr::str_c("The data must include the following columns:",columns))}
  if(("SUBJECT" %in% colnames(data)) & (length(unique(data$SUBJECT))>1)){stop("The data must include the observations of only ONE subject")}


  cycles = sort(unique(data$CYCLE))
  # plotlist = foreach(cycle = cycles) %do% {
  #   this_cycle_data = data %>% filter(CYCLE == cycle)
  #   g = plot_subject_cycle_obs(data = this_cycle_data, add_diagnosis = add_diagnosis)
  # }

  plotlist = purrr::map(.x = cycles,
                        .f = function(cycle){
                          this_cycle_data = data[which(data$CYCLE == cycle),] #data %>% dplyr::filter(CYCLE == cycle)
                          plot_subject_cycle_obs(data = this_cycle_data, add_diagnosis = add_diagnosis, color_max_score = color_max_score)
                        })

  g_all_cycles = cowplot::plot_grid(plotlist = plotlist, ncol=1, align="v")
  return(g_all_cycles)
}


#' Visualize all the observations and the diagnosis summary of a single subject.
#'
#' By default, this function saves the visualization in a pdf. The pdf heigth scales with the number of cycles reported for the subject.
#'
#' @param data a data.frame that contains the symptoms reported by ONE subject.
#'     The data must be in a long format and have the following columns: \code{SUBJECT}, \code{CYCLE}, \code{DAY}, \code{DRSP}, \code{score}
#' @param save_as_pdf logical.
#'     If \code{TRUE} (default), this function saves a pdf with the visualizations.
#'     The pdf path and name can be specified with the arguments \code{pdf_path} and \code{pdf_name}.
#'     If \code{FALSE}, this function returns a ggplot object. The same object is also returned, but invisibly, when \code{save_as_pdf = TRUE} (see examples).
#' @param pdf_path string. Specifies the path to the folder in which the pdf should be saved.
#'     By default, the path is an empty string so the pdf is be saved in the current working directory.
#' @param pdf_name string. Specifies the name of the pdf.
#'     By default, the name of the pdf is \code{CPASS_SUBJECT_X.pdf}, where \code{X} is replaced by the subject unique identifier.
#' @param color_max_score string specifying the color of a score of 6 (the maximal score) reported by a subject. Any standard color format specification is accepted, i.e. one of the R built-in color names (e.g. "tomato" (default); type \code{colors()} to see the names of all R built-in colors), an RGB hex code (e.g. "#AA2199") or a color specified via one of the color/palette functions (e.g. hsv(0.1,0.9,0.9))
#' @param colors_summary string. Either \code{"complementary"} (default) or \code{"rainbow"} specifying the type of color scheme for the DRSP items in the diagnosis summary. If \code{"complementary"}, the colors are chosen complementary within a domain; if \code{"rainbow"}, the DRSP colors are different for each item and chosen from a rainbow palette.
#'
#' @keywords CPASS C-PASS PMDD MRMD
#' @return a ggplot object if \code{save_as_pdf = FALSE}. The same object is returned invisibly by default (\code{save_as_pdf = TRUE}).
#' @export
#' @importFrom magrittr %>%
#' @examples
#'
#' data(PMDD_data)
#' input = PMDD_data %>% filter(SUBJECT == 2) %>%  as_cpass_data()
#' p = plot_subject_data_and_diagnosis(data = input)
#' p


plot_subject_data_and_diagnosis = function(data = data.frame(), save_as_pdf = TRUE, pdf_path = "", pdf_name = "", color_max_score = "tomato", color_summary = c("complementary","rainbow")){

  g_diagnosis_summary = suppressWarnings(plot_subject_diagnosis(data = data, color_summary = color_summary))
  g_data = suppressWarnings(plot_subject_obs(data = data, add_diagnosis = TRUE, color_max_score = color_max_score))
  n_cycles = length(g_data$layers)
  g = suppressWarnings(cowplot::plot_grid(g_diagnosis_summary, g_data, ncol = 1, nrow = 2, rel_heights = c(1.2, n_cycles)))
  #g

  if(save_as_pdf){
    if(pdf_name == ""){pdf_name = stringr::str_c("CPASS_SUBJECT_",unique(data$SUBJECT),".pdf")}
    if(pdf_path == ""){pdf_path = getwd()}
    if((pdf_path != "") && (str_sub(pdf_path, -1) != "/")){pdf_path = stringr::str_c(pdf_path, "/")}
    pdf_filename = stringr::str_c(pdf_path,pdf_name)
    ggsave(g, filename = pdf_filename, width = 75, height = 50* (1.2 + length(g_data$layers)), units = "mm", scale = 3)
    cat("Subject summary saved in '",pdf_filename,"'\n")
    return(invisible(g))
  }
  return(g)
}



