
luteal_color_function = colorRampPalette(c("gold2", "orange"))


create_circular_transition_matrix =
  function(J){
    mat = matrix(0,nrow = J, ncol = J)
    for(j in 1:J){
      mat[j,ifelse(j == J, 1, j+1)] = 1
    }
    mat
  }


one_day_sojourn = list(type = "nonparametric",d = c(1,0,0))
menses_sojourn = list(type = "nonparametric", d = dgamma(1:10, shape = 2, scale = 1.5))
menses_foll = list(type = "nonparametric", d = dgamma(1:30, shape = 5, scale = 3))
menses_lut = list(type = "nonparametric", d = dgamma(1:10, shape = 8, scale = 1))

cycle_model =
  specify_hsmm(
    J = 12,
    state_names = c("day_1","menses","foll","ovu","lut","-7","-6","-5","-4","-3","-2","-1"),
    state_colors = c("red3","red2","skyblue1","black","gold1",luteal_color_function(7)),
    init = c(1, rep(0.1,11)),
    transition = create_circular_transition_matrix(J = 12),
    sojourn =
      list(
        one_day_sojourn, # day1
        menses_sojourn, # menses
        menses_foll, # foll
        one_day_sojourn, # ovu
        menses_lut, # lut
        one_day_sojourn, # -7
        one_day_sojourn, # -6
        one_day_sojourn, # -5
        one_day_sojourn, # -4
        one_day_sojourn, # -3
        one_day_sojourn, # -2
        one_day_sojourn # -1
      ),
    marg_em_probs =
      list(
        bleeding = list(
          type = "non-par",
          params = list(
            values = c("none","spotting","light","medium","heavy"),
            probs =
              rbind(
                # day_1 mens  foll, ovu,    lut,  -7,  -6,  -5,  -4,   -3,   -2,   -1
                c(0.001, 0.001, 0.95, 0.9,  0.95, 0.95, 0.93, 0.90, 0.86, 0.81, 0.75, 0.7), # none
                c(0.001, 0.1,   0.05, 0.1,  0.05, 0.05, 0.07, 0.10, 0.14, 0.19, 0.25, 0.3), # spotting
                c(0.3,   0.3,   0.01, 0.01, 0.01, 0.01, 0.01, 0.01, 0.01, 0.01, 0.05, 0.1), # light
                c(0.3,   0.3,   0.001,0.001,0.001,0.001,0.001,0.001,0.001,0.001,0.001,0.001), # medium
                c(0.3,   0.3,   0.001,0.001,0.001,0.001,0.001,0.001,0.001,0.001,0.001,0.001) # heavy
              )
          ),
          viz_options = list(colors = c("gray95","orange","tomato1","tomato2","tomato3"))
        )
      ),
    censoring_probs = list(p = c(0.01,0.3,0.5,0.5,0.5,0.45,0.4,0.35,0.3,0.25,0.2,0.15))

  )

usethis::use_data(cycle_model, overwrite = TRUE)
