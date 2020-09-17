# Model 2
en_LD_proc <- process.data(data = en_LD,
                           model = "Burnham",
                           groups="area",
                           initial.ages = 1)
en_LD_ddl <- make.design.data(en_LD_proc)
en_LD_ddl <- add.design.data(data = en_LD_proc,
                             ddl = en_LD_ddl,
                             parameter="S",
                             type = "age",
                             bins = c(1,2,4,5,9,28),
                             right = FALSE,
                             name = "age",
                             replace = TRUE)
en_LD_ddl <- add.design.data(data = en_LD_proc,
                             ddl = en_LD_ddl,
                             parameter="F",
                             type = "age",
                             bins = c(1,5,7,9,27),
                             right = FALSE,
                             name = "age_binned",
                             replace = TRUE)
en_LD_ddl <- add.design.data(data = en_LD_proc,
                             ddl = en_LD_ddl,
                             parameter="p",
                             type = "age",
                             bins = c(2,3,4,5,9,28),
                             right = FALSE,
                             name = "binned_age",
                             replace = TRUE)
S_sim <- list(formula=~age)
F_sim <- list(formula=~age_binned, fixed=list(index=c(1),value=1))
p_sim <- list(formula=~binned_age, fixed=list(index=c(1),value=0))
r_sim <- list(formula=~Time)
model_sim <- mark(data = en_LD_proc,
                  ddl = en_LD_ddl,
                  model.parameters = list(S = S_sim, p = p_sim,
                                          F =F_sim, r = r_sim),
                  invisible = FALSE,
                  default.fixed = TRUE,
                  model = "Burnham")
PIMS(model_sim,"S")
PIMS(model_sim,"p")
PIMS(model_sim,"F")
PIMS(model_sim,"r")
summary(model_sim,brief = TRUE,show.fixed = TRUE)
model_sim$results

# Model 4
S_sim_area <- list(formula=~age*area)
F_sim_area <- list(formula=~age_binned*area, fixed=list(index=c(1,2),value=1))
p_sim_area <- list(formula=~binned_age*area, fixed=list(index=c(1),value=0))
r_sim_area <- list(formula=~Time*area)
model_sim_area <- mark(data = en_LD_proc,
                       ddl = en_LD_ddl,
                       model.parameters = list(S = S_sim_area, p = p_sim_area,
                                               F =F_sim_area, r = r_sim_area),
                       invisible = FALSE,
                       default.fixed = TRUE,
                       model = "Burnham")
