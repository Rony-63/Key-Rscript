# Model 1
library(RMark)  
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
                             name = "binned_age",
                             replace = TRUE)
unclassed_binned_age <- unclass(en_LD_ddl$S$binned_age)
ref_s <- ifelse(unclassed_binned_age==1,
                paste0("t",en_LD_ddl$S$time,"b1"),
                paste0("ttt","b",unclassed_binned_age))
en_LD_ddl$S$ref <- factor(ref_s)
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
unclassed_binned_age_p <- unclass(en_LD_ddl$p$binned_age)
newcov_p <- ifelse(unclassed_binned_age_p==2,
                   paste0("t",00,"b2"),
                   ifelse(unclassed_binned_age_p==3,
                          paste0("t",en_LD_ddl$p$time,"b3"),
                          ifelse(unclassed_binned_age_p==4,
                                 paste0("t",en_LD_ddl$p$time,"b4"),
                                 ifelse(unclassed_binned_age_p > 4 & unclassed_binned_age_p < 10,
                                        paste0("t",en_LD_ddl$p$time,"b5"),
                                        paste0("ttt","b",unclassed_binned_age_p)))))
en_LD_ddl$p$new <- factor(newcov_p)
S_ref <- list(formula=~ref)
F_ref <- list(formula=~age_binned, fixed=list(index=c(1),value=1))
p_ref <- list(formula=~new*area, fixed=list(index=c(1,2),value=0))
r_ref <- list(formula=~Time)
model_ref <- mark(data = en_LD_proc,
                  ddl = en_LD_ddl,
                  model.parameters = list(S = S_ref, p = p_ref,
                                          F =F_ref, r = r_ref),
                  invisible = FALSE,
                  default.fixed = TRUE,
                  model = "Burnham")
PIMS(model_ref,"S")
PIMS(model_ref,"p")
PIMS(model_ref,"F")
PIMS(model_ref,"r")
summary(model_ref, brief = TRUE, show.fixed = TRUE)
summary(model_ref, show.fixed = TRUE)
model_ref$results

# Model 3
S_ref_area <- list(formula=~ref*area)
F_ref_area <- list(formula=~age_binned*area, fixed=list(index=c(1,2),value=1))
p_ref_area <- list(formula=~new*area, fixed=list(index=c(1,2),value=0))
r_ref_area <- list(formula=~Time*area)
model_ref_area <- mark(data = en_LD_proc,
                       ddl = en_LD_ddl,
                       model.parameters = list(S = S_ref_area, p = p_ref_area,
                                               F =F_ref_area, r = r_ref_area),
                       invisible = FALSE,
                       default.fixed = TRUE,
                       model = "Burnham")


