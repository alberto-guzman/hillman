




# first will need to fix the excel shifting issue

master_df |> vtable(missing = T)

master_df_fill |> vtable(missing = T)


library(MatchIt)

m.out <- matchit(treatment ~ gender + grade + age + gpa + sat_math + sat_verbal + sat_writing + psat_math + psat_verbal + psat_writing +
                   act_math + act_read + act_science + act_writing + stipend + house_size + first_gen + racially_marginalized +
                   bi_multi_racial + urban + suburban +
                   rural + disability + neg_school + us_citizen +
                   gender.NA + grade.NA + age.NA + gpa.NA + sat_math.NA + sat_verbal.NA + sat_writing.NA + psat_math.NA + psat_verbal.NA + psat_writing.NA +
                   act_math.NA + act_read.NA + act_science.NA + act_writing.NA + stipend.NA + house_size.NA + first_gen.NA +
                   disability.NA + neg_school.NA + us_citizen.NA,
                 data = master_df_fill,
                 method = "nearest", 
                 exact = ~ year + racially_marginalized,
                 distance = "glm", 
                 link = "probit",
                 discard = "control",
                 caliper = .50, std.caliper = TRUE, 
                 replace = TRUE)



plot(summary(m.out))

bal.plot(m.out, var.name = "distance", which = "both", colors = c("#003594", "#FFB81C")) +
  ggtitle("Propensity Score Distribution") + xlab("Propensity Score")


summary(m.out)

plot(m.out, type = "jitter", interactive = FALSE)
