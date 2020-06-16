
#' Visualize the subject diagnosis
#'
#' This function calls the \code{CPASS} function and provides a visualization of the diagnosis.
#' @param data a data.frame that contains the symptoms reported by ONE subject. The data must be in a long format and have the following columns: \code{SUBJECT}, \code{CYCLE}, \code{DAY}, \code{DRSP}, \code{score}
#'
#' @keywords CPASS C-PASS PMDD MRMD
#' @return a ggplot object
#' @export
#' @examples
#' data(PMDD_data)
#' input = PMDD_data %>% filter(SUBJECT == 2)
#' plot_subject_diagnosis(data = input)
#'


plot_subject_diagnosis = function(data = data.frame()){

  #subject_diagnosis = compute_diagnosis(data)
  #subject_diagnosis = data.frame(NCycles = 2, dxcat = 2, dx = "PMDD", PMDD = 2, MRMD = 0)
  subject_diagnosis = CPASS(data)
  subject = unique(data$SUBJECT)

  gtitle = ggplot()
  gtitle = gtitle +
    ggtitle(str_c("SUBJECT : ",subject,"   ||   ",subject_diagnosis$SUBJECT_level_diagnosis$dx,""),
            subtitle = str_c(
              "# CYCLES total: ", subject_diagnosis$SUBJECT_level_diagnosis$NCycles_tot,"   ||   ",
              "# CYCLES incl.: ", subject_diagnosis$SUBJECT_level_diagnosis$NCycles,"   ||   ",
              "# PMDD cycles: ", subject_diagnosis$SUBJECT_level_diagnosis$N_PMDD,"   ||   ",
              "# MRMD cycles: ", subject_diagnosis$SUBJECT_level_diagnosis$N_MRMD))
  gtitle

  daily_summary = subject_diagnosis$daily_summary_DRSP
  daily_summary = full_join(daily_summary, dsm5_dict %>%  select(DRSP,DRSP_desc, DSM5_SYMPTOM_DOMAIN, SYMPTOM_CATEGORY), by = "DRSP") %>%
    ungroup() %>%
    mutate(DRSP = factor(DRSP, levels = dsm5_dict$DRSP),
           DRSP_desc = factor(DRSP_desc, levels = dsm5_dict$DRSP_desc),
           DSM5_SYMPTOM_DOMAIN = factor(DSM5_SYMPTOM_DOMAIN, levels = unique(dsm5_dict$DSM5_SYMPTOM_DOMAIN)))
  g_daily_summary = ggplot(daily_summary, aes(x = DAY))
  g_daily_summary = g_daily_summary +
    geom_hline(yintercept = 1, col = "gray60")+
    geom_hline(yintercept = 3.8, linetype = 2, col = "gray60")+
    #geom_ribbon(aes(ymin = min, ymax = max, fill = DRSP_desc), alpha = 0.2)+
    geom_line(aes(y = ave, col = DRSP_desc))+
    geom_area(position = "identity",aes(y = ave, fill = DRSP_desc), alpha = 0.3)+
    geom_point(aes(y = ave, col = DRSP_desc))+
    scale_x_continuous(breaks = c(-7,-4,-1,1,4,7,10), minor_breaks = c(-7:-1,4:10))+
    scale_y_continuous(breaks = c(1,4), minor_breaks = 1:6)+
    coord_cartesian(ylim=c(1, 6))+
    ylab("")+xlab("")+
    facet_grid(DSM5_SYMPTOM_DOMAIN ~ PHASE, scale = "free_x", switch = "y")+
    theme(strip.text.y.left = element_text(angle = 0, hjust = 1),
          strip.background.y = element_rect(fill = viz$strip.background.col, color = NA),
          strip.placement = "outside",
          strip.text.x = element_blank(),
          panel.spacing.x = unit(30,"pt"),
          panel.spacing.y = unit(5,"pt"),
          legend.position = "none",
          plot.title.position = "plot"
    )+
    ggtitle("DRSP daily averages and ranges")
  #g_daily_summary

  summary = subject_diagnosis$summary_DRSP
  summary = full_join(summary, dsm5_dict %>%  select(DRSP,DRSP_desc, DSM5_SYMPTOM_DOMAIN, SYMPTOM_CATEGORY), by = "DRSP") %>%
    ungroup() %>%
    mutate(DRSP_fac = factor(DRSP, levels = dsm5_dict$DRSP),
           DRSP_desc = factor(DRSP_desc, levels = dsm5_dict$DRSP_desc),
           DSM5_SYMPTOM_DOMAIN = factor(DSM5_SYMPTOM_DOMAIN, levels = unique(dsm5_dict$DSM5_SYMPTOM_DOMAIN)))

  drsp_desc = factor(dsm5_dict$DRSP_desc, levels = dsm5_dict$DRSP_desc)
  g_summary = ggplot(summary, aes(y = DRSP, x = ave_perc_change, col = DRSP_desc))
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
    theme(strip.text = element_blank(),
          panel.spacing.y = unit(5,"pt"))+
    ggtitle("Av. % change")
  #g_summary

  plots = plot_grid(plotlist = list(g_daily_summary, g_summary), ncol = 2, nrow =1,  rel_widths = c(2,1.2))
  plots_with_title = plot_grid(plotlist = list(gtitle, plots), ncol = 1, nrow =2,  rel_heights = c(1,10))

  return(plots_with_title)
}


plot_subject_cycle_obs = function(data = data.frame(), add_diagnosis = TRUE){
  add_legend = FALSE # to change
  columns = c("CYCLE","DAY","PHASE","DRSP","score")
  if(any(!(columns %in% colnames(data)))){stop(str_c("The data must include the following columns:",columns))}
  if(("SUBJECT" %in% colnames(data)) & (length(unique(data$SUBJECT))>1)){stop("The data must include the observations of only ONE subject")}
  if(length(unique(data$CYCLE))>1){stop("The data must include the observations of only ONE cycle")}


  if(add_diagnosis){

    cycle_diagnosis = CPASS(data)

    title_add = str_c("     ",ifelse(cycle_diagnosis$CYCLE_level_diagnosis$included,cycle_diagnosis$CYCLE_level_diagnosis$diagnosis,"not included"))

    # preparing data to plot the diagnosis
    df = full_join(cycle_diagnosis$DRSP_level_diagnosis %>%  select(SUBJECT, CYCLE, DRSP, DRSP_meets_criteria, DSM5_SYMPTOM_DOMAIN),
                   cycle_diagnosis$DSM5_DOMAINS_level_diagnosis %>%  select(SUBJECT, CYCLE, DSM5_SYMPTOM_DOMAIN, DSM5_criteria),
                   by = c("SUBJECT", "CYCLE", "DSM5_SYMPTOM_DOMAIN"))
    df = full_join(df, cycle_diagnosis$CYCLE_level_diagnosis %>%  select(-n_DSM5_DOMAINS_meeting_criteria),
                   by = c("SUBJECT", "CYCLE"))
    df = df %>%  ungroup()

    df_long = pivot_longer(df %>%  select(DRSP, DSM5_SYMPTOM_DOMAIN, DRSP_meets_criteria, DSM5_criteria, DSM5_A, DSM5_B),
                           cols = c("DRSP_meets_criteria","DSM5_criteria","DSM5_A","DSM5_B"),
                           names_to = "level",
                           values_to = "meets_criteria")
    df_long = df_long %>% mutate(
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
      scale_fill_manual(values = c("gray90",viz$high_score),na.value = "gray45")+
      guides(fill = FALSE)+
      xlab("")+ylab("")+
      scale_x_continuous(breaks = NULL)+
      theme(strip.text.y = element_blank())
    #g_diagnosis

  }else{
    title_add = ""
  }


  obs = data
  if(!("DSM5_SYMPTOM_DOMAIN" %in% colnames(obs))){obs = full_join(obs, dsm5_dict, by = c("DRSP"))}

  obs = obs %>%
    mutate(DRSP = DRSP %>% factor(.,levels = rev(dsm5_dict$DRSP)),
           DSM5_SYMPTOM_DOMAIN = DSM5_SYMPTOM_DOMAIN %>% factor(.,levels = unique(dsm5_dict$DSM5_SYMPTOM_DOMAIN)),
           PHASE = PHASE %>%  factor(.,levels = c("pre","post")))


  gcanvas = ggplot(obs, aes(x = DAY, y = DRSP,  fill = score))
  g = gcanvas +
    geom_tile(col = "white", size = 0.5)+
    geom_text(aes(label = score), size = 2)+
    scale_fill_gradient(low = "gray90", high = viz$high_score, breaks = 1:6, limits = c(1,6), na.value = "transparent")+
    scale_alpha_continuous(na.value = 0, range = c(0.1,1), limits = c(1,6))+
    scale_x_continuous(breaks = c(-7,-4,-1,1,4,7,10), minor_breaks = c(-7:-1,4:10))+
    guides(size = FALSE)+ # col = FALSE
    facet_grid(DSM5_SYMPTOM_DOMAIN ~ PHASE, scale = "free", space = "free", switch = "y")+
    theme(strip.text.y.left = element_text(angle = 0, hjust = 1),
          strip.background.y = element_rect(fill = viz$strip.background.col, color = NA),
          strip.placement = "outside",
          panel.spacing.x = unit(30,"pt"),
          panel.spacing.y = unit(2,"pt"),
          legend.position = ifelse(add_legend,"bottom","none"),
          plot.title.position = "plot")+
    ylab("")+
    xlab("")+
    ggtitle(str_c("CYCLE: ", unique(data$CYCLE),title_add))
  #g

  if(add_diagnosis){
    g = plot_grid(g, g_diagnosis, align = "h", axis = "bt", ncol = 2, nrow = 1, rel_widths = c(2,1.2))
  }

  return(g)
}

plot_subject_obs = function(data = data.frame(), add_diagnosis = TRUE){
  columns = c("CYCLE","DAY","PHASE","DRSP","score")
  if(any(!(columns %in% colnames(data)))){stop(str_c("The data must include the following columns:",columns))}
  if(("SUBJECT" %in% colnames(data)) & (length(unique(data$SUBJECT))>1)){stop("The data must include the observations of only ONE subject")}


  cycles = sort(unique(data$CYCLE))
  plotlist = foreach(cycle = cycles) %do% {
    this_cycle_data = data %>% filter(CYCLE == cycle)
    g = plot_subject_cycle_obs(data = this_cycle_data, add_diagnosis = add_diagnosis)
  }

  g_all_cycles = plot_grid(plotlist = plotlist, ncol=1, align="v")
  return(g_all_cycles)
}

plot_subject_data_and_diagnosis = function(data = data.frame(), save_as_pdf = TRUE, pdf_path = "", pdf_name = ""){
  # TO DO
  g_diagnosis_summary = suppressWarnings(plot_subject_diagnosis(data = data))
  g_data = suppressWarnings(plot_subject_obs(data = data, add_diagnosis = TRUE))
  g = suppressWarnings(plot_grid(g_diagnosis_summary, g_data, ncol = 1, nrow = 2, rel_heights = c(1.2,3)))
  #g

  if(save_as_pdf){
    if(pdf_name == ""){pdf_name = str_c("CPASS_SUBJECT_",unique(data$SUBJECT),".pdf")}
    if(pdf_path == ""){pdf_path = getwd()}
    if((pdf_path != "") && (str_sub(pdf_path, -1) != "/")){pdf_path = str_c(pdf_path, "/")}
    pdf_filename = str_c(pdf_path,pdf_name)
    ggsave(g, filename = pdf_filename, width = 75, height = 50*1.2 + 50*length(g_data$layers), units = "mm", scale = 3)
    cat("Subject summary saved in '",pdf_filename,"'\n")
    return(invisible(g))
  }
  return(g)
}



