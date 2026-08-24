# Generate the synthetic cohort used in the book practicals.
# The values are fictional and must not be used for clinical decisions.

set.seed(8311)
n_clinics <- 30
n_per_clinic <- 100
n <- n_clinics * n_per_clinic

clinic_frame <- data.frame(
  clinic_id = seq_len(n_clinics),
  clinic_support = rnorm(n_clinics, 0, 1),
  clinic_deprivation = rnorm(n_clinics, 0, 1)
)

demo <- merge(
  data.frame(
    id = seq_len(n),
    clinic_id = rep(seq_len(n_clinics), each = n_per_clinic)
  ),
  clinic_frame,
  by = "clinic_id",
  sort = FALSE
)
demo <- demo[order(demo$id), ]

demo$age <- round(pmin(65, pmax(30, rnorm(n, 47, 8))), 1)
demo$sex <- factor(rbinom(n, 1, 0.46), labels = c("female", "male"))
demo$socioeconomic_position <- round(
  rnorm(n, -0.35 * demo$clinic_deprivation, 1), 2
)
demo$asbestos_exposure <- rbinom(
  n, 1,
  plogis(-1.7 + 0.45 * (demo$sex == "male") -
           0.25 * demo$socioeconomic_position)
)

# Included only so the true data-generating process is documented.
# It is deliberately omitted from the main adjusted analyses.
demo$unmeasured_health_risk <- rbinom(
  n, 1, plogis(-0.8 - 0.3 * demo$socioeconomic_position)
)

nicotine_dependence <- rnorm(
  n,
  0.25 * demo$clinic_deprivation -
    0.25 * demo$socioeconomic_position +
    0.35 * demo$unmeasured_health_risk,
  1
)
demo$cigarettes_per_day <- round(pmax(1, 16 + 5 * nicotine_dependence + rnorm(n, 0, 3)))
demo$morning_urgency <- round(pmin(10, pmax(0, 5 + 2 * nicotine_dependence + rnorm(n))), 1)
demo$withdrawal_score <- round(pmin(20, pmax(0, 9 + 3 * nicotine_dependence + rnorm(n, 0, 2))), 1)
demo$previous_pack_years <- round(pmax(
  0,
  18 + 0.35 * (demo$age - 30) + 6 * nicotine_dependence + rnorm(n, 0, 6)
), 1)

demo$cessation <- rbinom(
  n, 1,
  plogis(-0.1 + 0.65 * demo$clinic_support +
           0.25 * demo$socioeconomic_position -
           0.7 * nicotine_dependence -
           0.25 * demo$unmeasured_health_risk)
)
demo$later_tobacco_exposure <- round(pmax(
  0,
  11 - 7.5 * demo$cessation + 3.5 * nicotine_dependence +
    0.12 * demo$previous_pack_years + rnorm(n, 0, 2.5)
), 1)

demo$lung_cancer_complete <- rbinom(
  n, 1,
  plogis(-4.1 - 0.20 * demo$cessation +
           0.095 * demo$later_tobacco_exposure +
           0.035 * demo$previous_pack_years +
           0.50 * demo$asbestos_exposure -
           0.30 * demo$cessation * demo$asbestos_exposure +
           0.018 * (demo$age - 45) +
           0.55 * demo$unmeasured_health_risk)
)

demo$followup_observed <- rbinom(
  n, 1,
  plogis(1.8 + 0.35 * demo$cessation -
           0.025 * demo$previous_pack_years -
           0.50 * demo$unmeasured_health_risk)
)
demo$lung_cancer <- demo$lung_cancer_complete
demo$lung_cancer[demo$followup_observed == 0] <- NA

write.csv(demo, "data/smoking_cohort_demo.csv", row.names = FALSE, na = "")

