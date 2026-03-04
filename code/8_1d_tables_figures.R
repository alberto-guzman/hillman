# =============================================================================
# 8_1d_tables_figures.R
# Publication-ready tables and figures, Meyer et al. (2024) conventions.
# =============================================================================
library(dplyr)
library(tidyr)
library(ggplot2)
library(gt)
library(here)
library(cobalt)
library(purrr)
library(stringr)
if (!dir.exists(here('output'))) {
  dir.create(here('output'))
}

sig_stars <- function(p) {
  dplyr::case_when(
    p < 0.001 ~ '***',
    p < 0.01 ~ '**',
    p < 0.05 ~ '*',
    p < 0.10 ~ '+',
    TRUE ~ ''
  )
}
fmt_coef <- function(est, p, d = 3) {
  paste0(sprintf(paste0('%.', d, 'f'), est), sig_stars(p))
}
fmt_se <- function(se, d = 3) {
  paste0('(', sprintf(paste0('%.', d, 'f'), se), ')')
}
notes_main <- function(sn = NULL) {
  base <- paste0(
    'Notes: Standard errors in parentheses. All models include cohort fixed effects. ',
    'ATT estimated via a doubly-robust linear probability model with propensity score ',
    'matching weights, nearest-neighbor matching with replacement, caliper = 0.5 SD, ',
    'exact match on application year. Models include controls for gender, grade, GPA, ',
    'PSAT math, stipend eligibility, race/ethnicity, geography, disability status, ',
    'negative school environment, and first-generation college student status, ',
    'with missing-data indicators for all covariates.'
  )
  if (!is.null(sn)) paste0(base, ' ', sn) else base
}
notes_sig <- '+p<0.10, *p<0.05, **p<0.01, ***p<0.001'

style_gt <- function(gt_tbl, n_rows) {
  gt_tbl |>
    tab_style(
      style = cell_borders(sides = 'top', color = 'black', weight = px(2)),
      locations = cells_body(rows = 1)
    ) |>
    tab_style(
      style = cell_borders(sides = 'bottom', color = 'black', weight = px(2)),
      locations = cells_body(rows = n_rows)
    ) |>
    cols_align(align = 'left', columns = 1) |>
    cols_align(align = 'center', columns = -1) |>
    tab_options(
      table.font.size = px(11),
      heading.title.font.size = px(13),
      heading.subtitle.font.size = px(11),
      heading.align = 'left',
      column_labels.font.weight = 'bold',
      column_labels.border.top.color = 'black',
      column_labels.border.top.width = px(2),
      column_labels.border.bottom.color = 'black',
      column_labels.border.bottom.width = px(1),
      table_body.border.bottom.color = 'black',
      table_body.border.bottom.width = px(2),
      row_group.background.color = 'gray97',
      row_group.border.top.color = 'black',
      row_group.border.top.width = px(1),
      row_group.border.bottom.color = 'gray80',
      table.border.top.color = 'white',
      table.border.bottom.color = 'white',
      source_notes.font.size = px(10),
      data_row.padding = px(3)
    )
}

# =============================================================================
# TABLE 1: BALANCE
# =============================================================================
message('\n=== Building Table 1: Balance (All-States) ===')

bal_vars <- c(
  'gender',
  'grade',
  'gpa',
  'psat_math',
  'stipend',
  'racially_marginalized',
  'bi_multi_racial',
  'urban',
  'suburban',
  'rural',
  'disability',
  'neg_school',
  'first_gen',
  'us_citizen'
)
bal_labels <- c(
  gender = 'Female',
  grade = 'Grade',
  gpa = 'High School GPA',
  psat_math = 'PSAT Math Score',
  stipend = 'Stipend Eligible',
  racially_marginalized = 'Racially Marginalized',
  bi_multi_racial = 'Bi/Multi-Racial',
  urban = 'Urban',
  suburban = 'Suburban',
  rural = 'Rural',
  disability = 'Documented Disability',
  neg_school = 'Negative School Environment',
  first_gen = 'First-Generation College Student',
  us_citizen = 'U.S. Citizen'
)

make_balance_table <- function(
  pre_data,
  matched_data,
  m_out,
  vars,
  labels,
  title_subtitle
) {
  # Pre-match means: one row per variable with control and treated side by side
  pre_ctrl <- pre_data |>
    filter(treated_in_year == 0) |>
    summarise(across(all_of(vars), ~ mean(., na.rm = TRUE))) |>
    pivot_longer(everything(), names_to = 'variable', values_to = 'pre_0')

  pre_trt <- pre_data |>
    filter(treated_in_year == 1) |>
    summarise(across(all_of(vars), ~ mean(., na.rm = TRUE))) |>
    pivot_longer(everything(), names_to = 'variable', values_to = 'pre_1')

  pre_means <- pre_ctrl |> left_join(pre_trt, by = 'variable')

  # Post-match weighted means
  post_ctrl <- matched_data |>
    filter(treated_in_year == 0) |>
    summarise(across(
      all_of(vars),
      ~ weighted.mean(., w = weights, na.rm = TRUE)
    )) |>
    pivot_longer(everything(), names_to = 'variable', values_to = 'post_0')

  post_trt <- matched_data |>
    filter(treated_in_year == 1) |>
    summarise(across(
      all_of(vars),
      ~ weighted.mean(., w = weights, na.rm = TRUE)
    )) |>
    pivot_longer(everything(), names_to = 'variable', values_to = 'post_1')

  post_means <- post_ctrl |> left_join(post_trt, by = 'variable')

  # SMDs from cobalt
  smds <- bal.tab(m_out, un = TRUE)$Balance |>
    tibble::rownames_to_column('variable') |>
    dplyr::select(variable, smd_pre = Diff.Un, smd_post = Diff.Adj) |>
    filter(variable %in% vars)

  tbl <- pre_means |>
    left_join(post_means, by = 'variable') |>
    left_join(smds, by = 'variable') |>
    filter(variable %in% vars) |>
    mutate(
      label = coalesce(labels[variable], variable),
      ctrl_mean = sprintf('%.3f', pre_0),
      trt_mean = sprintf('%.3f', pre_1),
      smd_pre_f = if_else(is.na(smd_pre), '—', sprintf('%.3f', smd_pre)),
      ctrl_post = sprintf('%.3f', post_0),
      trt_post = sprintf('%.3f', post_1),
      smd_post_f = if_else(is.na(smd_post), '—', sprintf('%.3f', smd_post))
    ) |>
    # Preserve variable ordering
    arrange(match(variable, vars)) |>
    dplyr::select(
      label,
      ctrl_mean,
      trt_mean,
      smd_pre_f,
      ctrl_post,
      trt_post,
      smd_post_f
    )

  n_rows <- nrow(tbl)

  tbl |>
    gt(rowname_col = 'label') |>
    tab_header(
      title = md(paste0('**', title_subtitle[1], '**')),
      subtitle = title_subtitle[2]
    ) |>
    tab_spanner(
      label = 'Before Matching',
      columns = c(ctrl_mean, trt_mean, smd_pre_f)
    ) |>
    tab_spanner(
      label = 'After Matching',
      columns = c(ctrl_post, trt_post, smd_post_f)
    ) |>
    cols_label(
      ctrl_mean = 'Control',
      trt_mean = 'Treated',
      smd_pre_f = 'SMD',
      ctrl_post = 'Control',
      trt_post = 'Treated',
      smd_post_f = 'SMD'
    ) |>
    tab_source_note(
      source_note = md(paste0(
        '*Notes:* SMD = standardized mean difference. |SMD| < 0.1 indicates good balance. ',
        'N before: ',
        nrow(pre_data),
        ' (',
        sum(pre_data$treated_in_year == 1),
        ' treated, ',
        sum(pre_data$treated_in_year == 0),
        ' control). ',
        'N after: ',
        nrow(matched_data),
        ' (',
        sum(matched_data$treated_in_year == 1),
        ' treated, ',
        sum(matched_data$treated_in_year == 0),
        ' control). ',
        'Post-match means weighted by matching weights.'
      ))
    ) |>
    tab_source_note(source_note = md(notes_sig)) |>
    style_gt(n_rows)
}
table_1 <- make_balance_table(
  matching_data_all,
  matched_data_all,
  m.out_all,
  bal_vars,
  bal_labels,
  c(
    'Table 1: Analytic Sample and Matching Balance',
    'All States - Propensity Score Matched Sample'
  )
)
table_1

# =============================================================================
# TABLES 2 & 3: MAIN ATT
# =============================================================================
# ctrl_mean comes directly from results (fit_att computed it within the correct
# subsample), so Panel C control means are denominated in the degree-eligible
# subsample — not the full matched sample.

make_main_table <- function(results, matched, title_subtitle, sn = NULL) {
  prep <- function(data, panel_label) {
    data |>
      mutate(
        coef_fmt = fmt_coef(att_pp / 100, pval),
        se_fmt = fmt_se(se_pp / 100),
        n_fmt = format(n_obs, big.mark = ',')
      ) |>
      rowwise() |>
      reframe(
        label = c(label, ''),
        ctrl_mean = c(sprintf('%.3f', ctrl_mean), ''),
        att = c(coef_fmt, se_fmt),
        n = c(n_fmt, ''),
        panel = panel_label
      )
  }

  tbl <- bind_rows(
    results |> filter(sample == 'full') |> prep('A'),
    results |> filter(sample == 'nsc_matched') |> prep('B'),
    results |> filter(sample == 'degree_eligible') |> prep('C')
  )

  n_rows <- nrow(tbl)

  tbl |>
    gt(rowname_col = 'label') |>
    tab_header(
      title = md(paste0('**', title_subtitle[1], '**')),
      subtitle = title_subtitle[2]
    ) |>
    tab_row_group(
      label = md(
        '**Panel C: Degree and Persistence Outcomes** *(non-censored subsample)*'
      ),
      rows = panel == 'C'
    ) |>
    tab_row_group(
      label = md(
        '**Panel B: Enrollment Outcomes** *(NSC-matched students only)*'
      ),
      rows = panel == 'B'
    ) |>
    tab_row_group(
      label = md('**Panel A: Enrollment Outcomes** *(full matched sample)*'),
      rows = panel == 'A'
    ) |>
    cols_hide(panel) |>
    cols_label(ctrl_mean = 'Control Mean', att = 'ATT', n = 'N') |>
    cols_align(align = 'left', columns = label) |>
    cols_align(align = 'center', columns = c(ctrl_mean, att, n)) |>
    tab_style(
      style = cell_text(color = 'gray50', size = px(10)),
      locations = cells_body(rows = grepl('^\\(', att), columns = att)
    ) |>
    tab_style(
      style = cell_borders(sides = 'top', color = 'black', weight = px(2)),
      locations = cells_body(rows = 1)
    ) |>
    tab_style(
      style = cell_borders(sides = 'bottom', color = 'black', weight = px(2)),
      locations = cells_body(rows = n_rows)
    ) |>
    tab_source_note(source_note = md(paste0('*', notes_main(sn), '*'))) |>
    tab_source_note(source_note = md(notes_sig)) |>
    tab_source_note(
      source_note = md(paste0(
        'Panel A: all matched students (N treated = ',
        sum(matched$treated_in_year == 1),
        '); students without NSC records coded as not enrolled. ',
        'Panel B: NSC-matched students only (robustness check). ',
        'Panel C: non-missing outcome subsample; sample size varies by outcome. ',
        'Control means in Panel C are computed within the non-censored subsample.'
      ))
    ) |>
    tab_options(
      table.font.size = px(11),
      heading.title.font.size = px(13),
      heading.subtitle.font.size = px(11),
      heading.align = 'left',
      column_labels.font.weight = 'bold',
      column_labels.border.top.color = 'black',
      column_labels.border.top.width = px(2),
      column_labels.border.bottom.color = 'black',
      column_labels.border.bottom.width = px(1),
      table_body.border.bottom.color = 'black',
      table_body.border.bottom.width = px(2),
      row_group.background.color = 'gray97',
      row_group.border.top.color = 'black',
      row_group.border.top.width = px(1),
      table.border.top.color = 'white',
      table.border.bottom.color = 'white',
      source_notes.font.size = px(10),
      data_row.padding = px(2)
    )
}
message('\n=== Building Table 2 ===')
table_2 <- make_main_table(
  results_all,
  matched_all,
  c(
    'Table 2: Effects of the Hillman Summer Program on College Outcomes',
    'All States - Propensity Score Matched Sample'
  )
)
table_2

message('\n=== Building Table 3 ===')
table_3 <- make_main_table(
  results_pa,
  matched_pa,
  c(
    'Table 3: Effects of the Hillman Summer Program on College Outcomes',
    'PA Public Schools - Propensity Score Matched Sample'
  ),
  sn = 'PA model additionally controls for school enrollment, % economically disadvantaged, % English learner, % special education, and % white.'
)
table_3

# =============================================================================
# FIGURES 1-3: SUBGROUP COEFFICIENT PLOTS
# =============================================================================
sg_cap <- paste0(
  'Notes: Each row reports the ATT from a separate regression limited to that subgroup, ',
  'including all baseline covariates and cohort fixed effects. ',
  'Filled circles indicate p < 0.10; open circles indicate p >= 0.10. ',
  'Horizontal bars are 95% confidence intervals. ',
  'Standard errors clustered by matched subclass. ',
  'Subgroups with fewer than 20 treated students suppressed.'
)

plot_coef <- function(
  data,
  outcome_var,
  sample_filter,
  title,
  subtitle,
  caption
) {
  df <- data |>
    filter(outcome == outcome_var, !suppressed, sample == sample_filter) |>
    mutate(
      subgroup = factor(subgroup, levels = rev(sort(unique(subgroup)))),
      sig = pval < 0.10,
      xmin_plot = pmax(conf_lo_pp, -65),
      xmax_plot = pmin(conf_hi_pp, 65)
    )

  x_lo <- floor(min(df$xmin_plot, na.rm = TRUE) / 10) * 10 - 5
  x_hi <- ceiling(max(df$xmax_plot, na.rm = TRUE) / 10) * 10 + 5

  ggplot(df, aes(x = att_pp, y = subgroup)) +
    geom_vline(
      xintercept = 0,
      linetype = 'dashed',
      color = 'gray50',
      linewidth = 0.5
    ) +
    geom_errorbarh(
      aes(xmin = xmin_plot, xmax = xmax_plot),
      height = 0.2,
      color = '#2c6e9e',
      linewidth = 0.7
    ) +
    geom_point(aes(shape = sig, fill = sig), size = 3.5, color = '#2c6e9e') +
    scale_shape_manual(values = c('FALSE' = 21, 'TRUE' = 19), guide = 'none') +
    scale_fill_manual(
      values = c('FALSE' = 'white', 'TRUE' = '#2c6e9e'),
      guide = 'none'
    ) +
    scale_x_continuous(
      limits = c(x_lo, x_hi),
      breaks = scales::pretty_breaks(n = 5),
      labels = function(x) paste0(ifelse(x > 0, '+', ''), round(x), ' pp'),
      expand = expansion(mult = 0)
    ) +
    labs(
      title = title,
      subtitle = subtitle,
      x = 'Treatment effect (percentage points)',
      y = NULL,
      caption = caption
    ) +
    theme_minimal(base_size = 12) +
    theme(
      plot.title = element_text(
        face = 'bold',
        size = 11,
        margin = margin(b = 4)
      ),
      plot.subtitle = element_text(
        color = 'gray40',
        size = 9,
        margin = margin(b = 8)
      ),
      plot.caption = element_text(
        color = 'gray40',
        size = 7.5,
        hjust = 0,
        margin = margin(t = 8),
        lineheight = 1.3
      ),
      axis.text.y = element_text(size = 11),
      axis.text.x = element_text(size = 9),
      axis.title.x = element_text(size = 9),
      panel.grid.minor = element_blank(),
      panel.grid.major.y = element_blank(),
      panel.grid.major.x = element_line(color = 'gray90'),
      plot.margin = margin(10, 10, 10, 10)
    )
}

sg_cap <- paste0(
  'Notes: Each row reports the ATT from a separate regression limited to that subgroup, ',
  'including all baseline covariates and cohort fixed effects. ',
  'Filled circles indicate p < 0.10; open circles indicate p >= 0.10. ',
  'Horizontal bars are 95% confidence intervals. ',
  'Standard errors clustered by matched subclass. ',
  'Subgroups with fewer than 20 treated students suppressed.'
)

message('\n=== Building Figures 1-3 ===')

figure_1 <- plot_coef(
  results_subgroup_all,
  'seamless_enroll',
  'full',
  title = 'Figure 1: Subgroup Effects on Seamless College Enrollment',
  subtitle = 'All States - ATT with 95% confidence intervals',
  caption = sg_cap
)
figure_1

figure_2 <- plot_coef(
  results_subgroup_all,
  'seamless_enroll_stem',
  'full',
  title = 'Figure 2: Subgroup Effects on Seamless STEM Enrollment',
  subtitle = 'All States - ATT with 95% confidence intervals',
  caption = sg_cap
)
figure_2

figure_3 <- plot_coef(
  results_subgroup_all,
  'reten_fall_enter2',
  'degree_eligible',
  title = 'Figure 3: Subgroup Effects on Retention into 2nd Year',
  subtitle = 'All States - ATT with 95% confidence intervals',
  caption = sg_cap
)
figure_3

# =============================================================================
# FIGURE 4: GPA HETEROGENEITY
# =============================================================================
message('\n=== Building Figure 4: GPA Heterogeneity ===')

gpa_fig_data <- results_gpa_het |>
  filter(!is.na(att_pp)) |>
  mutate(
    gpa_bin = str_replace_all(gpa_bin, '\u2013', '-'),
    subsample = factor(
      subsample,
      levels = c('All States', 'PA Public Schools')
    ),
    outcome_label = factor(
      outcome_label,
      levels = c('Seamless STEM Enrollment', 'Retained into 2nd Year')
    ),
    sig = pval < 0.10
  )

gpa_levels <- c('3.0 - 3.4', '3.5 - 3.9', '4.0+')
gpa_levels_present <- gpa_levels[gpa_levels %in% unique(gpa_fig_data$gpa_bin)]

gpa_fig_data <- gpa_fig_data |>
  mutate(
    gpa_bin = factor(gpa_bin, levels = rev(gpa_levels_present)),
    y_num = as.numeric(gpa_bin),
    y_dodge = y_num + if_else(subsample == 'All States', 0.18, -0.18)
  )

y_breaks <- seq_along(gpa_levels_present)
y_labels <- rev(gpa_levels_present)

figure_4 <- ggplot(
  gpa_fig_data,
  aes(x = att_pp, y = y_dodge, color = subsample)
) +
  geom_vline(
    xintercept = 0,
    linetype = 'dashed',
    color = 'gray50',
    linewidth = 0.5
  ) +
  geom_errorbarh(
    aes(xmin = conf_lo_pp, xmax = conf_hi_pp),
    height = 0.15,
    linewidth = 0.7
  ) +
  geom_point(aes(shape = sig, fill = interaction(subsample, sig)), size = 3.5) +
  scale_color_manual(
    values = c('All States' = '#2c6e9e', 'PA Public Schools' = '#e07b39'),
    name = NULL
  ) +
  scale_shape_manual(values = c('FALSE' = 21, 'TRUE' = 19), guide = 'none') +
  scale_fill_manual(
    values = c(
      'All States.FALSE' = 'white',
      'All States.TRUE' = '#2c6e9e',
      'PA Public Schools.FALSE' = 'white',
      'PA Public Schools.TRUE' = '#e07b39'
    ),
    guide = 'none'
  ) +
  scale_x_continuous(
    breaks = scales::pretty_breaks(n = 5),
    labels = function(x) paste0(ifelse(x > 0, '+', ''), round(x), ' pp'),
    expand = expansion(mult = 0.2)
  ) +
  scale_y_continuous(
    breaks = y_breaks,
    labels = y_labels,
    expand = expansion(add = 0.6)
  ) +
  facet_wrap(~outcome_label, ncol = 2, scales = 'free_x') +
  labs(
    title = 'Figure 4: Heterogeneous Treatment Effects by High School GPA',
    subtitle = 'ATT with 95% confidence intervals - All States vs. PA Public Schools',
    x = 'Treatment effect (percentage points)',
    y = 'High School GPA',
    caption = paste0(
      'Notes: Each point reports the ATT from a separate regression limited to students ',
      'in that GPA bin, including all baseline covariates and cohort fixed effects. ',
      'Filled circles indicate p < 0.10; open circles p >= 0.10. ',
      'PA 4.0+ bin suppressed (fewer than 20 treated). ',
      'Standard errors clustered by matched subclass where identified; HC3 otherwise. ',
      'Points dodged vertically for legibility.'
    )
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = 'bold', size = 11, margin = margin(b = 4)),
    plot.subtitle = element_text(
      color = 'gray40',
      size = 9,
      margin = margin(b = 8)
    ),
    plot.caption = element_text(
      color = 'gray40',
      size = 7.5,
      hjust = 0,
      margin = margin(t = 8),
      lineheight = 1.3
    ),
    legend.position = 'bottom',
    legend.text = element_text(size = 10),
    axis.text = element_text(size = 10),
    axis.title.x = element_text(size = 9),
    axis.title.y = element_text(size = 9),
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.major.x = element_line(color = 'gray90'),
    strip.text = element_text(face = 'bold', size = 10),
    strip.background = element_rect(fill = 'gray95', color = NA),
    plot.margin = margin(10, 10, 10, 10)
  )
figure_4

# =============================================================================
# APPENDIX TABLE A1: BALANCE (PA)
# =============================================================================
message('\n=== Building Appendix Table A1: Balance (PA) ===')

pa_bal_vars <- c(
  bal_vars,
  'school_enrollment',
  'school_pct_econ_disadvantaged',
  'school_pct_english_learner',
  'school_pct_special_ed',
  'school_pct_white'
)
pa_bal_labels <- c(
  bal_labels,
  school_enrollment = 'School Enrollment',
  school_pct_econ_disadvantaged = 'School % Economically Disadvantaged',
  school_pct_english_learner = 'School % English Learner',
  school_pct_special_ed = 'School % Special Education',
  school_pct_white = 'School % White'
)
appendix_a1 <- make_balance_table(
  matching_data_pa,
  matched_data_pa,
  m.out_pa,
  pa_bal_vars,
  pa_bal_labels,
  c(
    'Appendix Table A1: Analytic Sample and Matching Balance',
    'PA Public Schools - Propensity Score Matched Sample'
  )
)
appendix_a1

# =============================================================================
# APPENDIX TABLE A2: SAMPLE SIZES BY COHORT
# =============================================================================
message('\n=== Building Appendix Table A2: Sample Sizes by Cohort ===')

make_cohort_table <- function(pre_all, pre_pa, matched_all, matched_pa) {
  sc <- function(pre, matched, label) {
    pre |>
      group_by(year) |>
      summarise(
        pre_trt = sum(treated_in_year == 1),
        pre_ctrl = sum(treated_in_year == 0),
        .groups = 'drop'
      ) |>
      left_join(
        matched |>
          group_by(year) |>
          summarise(
            post_trt = sum(treated_in_year == 1),
            post_ctrl = sum(treated_in_year == 0),
            .groups = 'drop'
          ),
        by = 'year'
      ) |>
      mutate(sample = label, year = as.character(year))
  }
  tbl_all <- sc(pre_all, matched_all, 'All States')
  tbl_pa <- sc(pre_pa, matched_pa, 'PA Public Schools')
  totals <- bind_rows(tbl_all, tbl_pa) |>
    group_by(sample) |>
    summarise(
      year = 'Total',
      across(c(pre_trt, pre_ctrl, post_trt, post_ctrl), sum),
      .groups = 'drop'
    )
  tbl <- bind_rows(tbl_all, tbl_pa, totals) |>
    mutate(
      year = factor(year, levels = c('2017', '2018', '2019', '2021', 'Total'))
    )
  n_rows <- nrow(tbl)
  tbl |>
    gt(rowname_col = 'year', groupname_col = 'sample') |>
    tab_header(
      title = md('**Appendix Table A2: Sample Sizes by Application Cohort**'),
      subtitle = 'Before and after propensity score matching, by sample'
    ) |>
    tab_spanner(label = 'Before Matching', columns = c(pre_trt, pre_ctrl)) |>
    tab_spanner(label = 'After Matching', columns = c(post_trt, post_ctrl)) |>
    cols_label(
      pre_trt = 'Treated',
      pre_ctrl = 'Control',
      post_trt = 'Treated',
      post_ctrl = 'Control'
    ) |>
    tab_style(
      style = cell_text(weight = 'bold'),
      locations = cells_body(rows = year == 'Total')
    ) |>
    tab_style(
      style = cell_borders(sides = 'top', color = 'black', weight = px(1.5)),
      locations = cells_body(rows = year == 'Total')
    ) |>
    tab_source_note(
      source_note = md(paste0(
        '*Notes:* Before-matching counts exclude year 2020 (COVID), year 2022 ',
        '(insufficient follow-up), confirmed non-citizens, and students treated before 2017. ',
        'After-matching counts are unique matched units; with replacement matching, ',
        'the effective sample size of controls is lower than shown.'
      ))
    ) |>
    cols_align(align = 'center', columns = everything()) |>
    style_gt(n_rows)
}

appendix_a2 <- make_cohort_table(
  matching_data_all,
  matching_data_pa,
  matched_data_all,
  matched_data_pa
)
appendix_a2

# =============================================================================
# SAVE ALL
# =============================================================================
message('\n=== Saving Tables ===')
tbl_names <- list(
  table_1 = 'table_1_balance',
  table_2 = 'table_2_att_all_states',
  table_3 = 'table_3_att_pa',
  appendix_a1 = 'appendix_table_a1_balance_pa',
  appendix_a2 = 'appendix_table_a2_sample_by_cohort'
)
for (nm in names(tbl_names)) {
  gtsave(get(nm), here('output', paste0(tbl_names[[nm]], '.html')))
  gtsave(get(nm), here('output', paste0(tbl_names[[nm]], '.tex')))
}
message('Saved all tables.')
message('\n=== Saving Figures ===')
figs <- list(
  figure_1_subgroup_enrollment = figure_1,
  figure_2_subgroup_stem = figure_2,
  figure_3_subgroup_retention = figure_3,
  figure_4_gpa_heterogeneity = figure_4
)
dims <- list(c(7, 4.5), c(7, 4.5), c(7, 4.5), c(8, 5.5))
for (i in seq_along(figs)) {
  nm <- names(figs)[i]
  fig <- figs[[i]]
  d <- dims[[i]]
  ggsave(
    here('output', paste0(nm, '.pdf')),
    fig,
    width = d[1],
    height = d[2],
    dpi = 300
  )
  ggsave(
    here('output', paste0(nm, '.png')),
    fig,
    width = d[1],
    height = d[2],
    dpi = 300
  )
}
message('Saved all figures.')
message('\n=== All tables and figures complete ===')
