# ============================================================
# THYROIDLAB | DSP VISUAL ANALYTICS DASHBOARD
# CityPulse 360-inspired interface with the same demo login.
#
# Install packages once:
# install.packages(c("shiny", "ggplot2", "plotly", "DT", "scales", "rpart", "randomForest"))
#
# Run this file in RStudio:
# shiny::runApp("dsp.R")
#
# Demo login:
# Username: adithya
# Password: 123
#
# NOTE: The built-in dataset is synthetic demonstration data.
# It is not clinical evidence and must not be used for diagnosis.
# ============================================================

library(shiny)
library(ggplot2)
library(plotly)
library(DT)
library(scales)

options(shiny.maxRequestSize = 30 * 1024^2)
set.seed(360)

# --------------------------- DATA -----------------------------

create_sample_data <- function(n = 600) {
  age <- sample(18:85, n, replace = TRUE)
  gender <- sample(c("Male", "Female", "Other"), n, replace = TRUE,
                   prob = c(0.48, 0.48, 0.04))
  bmi <- pmax(15, pmin(50, round(rnorm(n, 26, 4.5), 1)))
  tsh <- round(rlnorm(n, log(2.2), 0.65), 2)
  free_t3 <- round(pmax(0.5, rnorm(n, 3.1, 0.65)), 2)
  free_t4 <- round(pmax(0.3, rnorm(n, 1.15, 0.25)), 2)
  tpo <- round(rlnorm(n, log(35), 1.1), 1)
  hdl <- round(pmax(15, rnorm(n, 52, 13)), 1)
  total_cholesterol <- round(pmax(80, rnorm(n, 195, 35)), 1)
  systolic_bp <- round(pmax(75, rnorm(n, 124, 17)))
  diastolic_bp <- round(pmax(40, rnorm(n, 79, 11)))
  diabetes <- sample(c("Yes", "No"), n, replace = TRUE, prob = c(.15, .85))
  smoking <- sample(c("Yes", "No"), n, replace = TRUE, prob = c(.25, .75))
  thyroid_status <- sample(c("Normal", "Hypothyroid", "Hyperthyroid"),
                           n, replace = TRUE, prob = c(.72, .20, .08))

  score <- -4 + .045 * age + .045 * (bmi - 25) +
    .018 * (systolic_bp - 120) + .7 * (diabetes == "Yes") +
    .45 * (smoking == "Yes") + .25 * (thyroid_status != "Normal")
  cvd <- rbinom(n, 1, plogis(score))

  data.frame(
    Patient_ID = sprintf("P%04d", seq_len(n)),
    Age = age, Gender = gender, BMI = bmi, TSH = tsh,
    Free_T3 = free_t3, Free_T4 = free_t4, TPO = tpo,
    HDL = hdl, Total_Cholesterol = total_cholesterol,
    SYS_BP = systolic_bp, DIA_BP = diastolic_bp,
    Diabetes = diabetes, Smoking = smoking,
    Thyroid_Status = thyroid_status,
    Cardiovascular_Disease = factor(cvd, levels = c(0, 1),
                                    labels = c("No", "Yes")),
    stringsAsFactors = FALSE
  )
}

sample_data <- create_sample_data()
numeric_vars <- c("Age", "BMI", "TSH", "Free_T3", "Free_T4", "TPO",
                  "HDL", "Total_Cholesterol", "SYS_BP", "DIA_BP")
pretty_names <- c(
  Age = "Age (years)", BMI = "Body Mass Index", TSH = "TSH",
  Free_T3 = "Free T3", Free_T4 = "Free T4", TPO = "TPO antibodies",
  HDL = "HDL cholesterol", Total_Cholesterol = "Total cholesterol",
  SYS_BP = "Systolic BP", DIA_BP = "Diastolic BP"
)
# Shiny choices: labels are displayed, raw column names are returned as input values.
var_choices <- stats::setNames(names(pretty_names), unname(pretty_names))
accent <- c("#25D9C4", "#5B8CFF", "#A879FF", "#FFB86B", "#F472B6", "#70E7FF")

# --------------------------- THEME ----------------------------

theme_dsp <- function() {
  theme_minimal(base_size = 12) +
    theme(
      text = element_text(colour = "#DCE7FB"),
      plot.title = element_text(colour = "#FFFFFF", face = "bold", size = 15),
      plot.subtitle = element_text(colour = "#9EB0CE", size = 10),
      axis.title = element_text(colour = "#C5D5F4", face = "bold"),
      axis.text = element_text(colour = "#B7C6E2"),
      panel.grid.major = element_line(colour = "#263957", linewidth = .35),
      panel.grid.minor = element_blank(),
      panel.background = element_rect(fill = "transparent", colour = NA),
      plot.background = element_rect(fill = "transparent", colour = NA),
      legend.background = element_rect(fill = "transparent", colour = NA),
      legend.title = element_text(colour = "#E6EEFF"),
      legend.text = element_text(colour = "#C5D2EA"),
      strip.text = element_text(colour = "#FFFFFF", face = "bold"),
      plot.margin = margin(12, 16, 12, 12)
    )
}

metric_card <- function(label, value, detail, colour = "#25D9C4") {
  div(class = "metric-card",
      style = paste0("border-top:3px solid ", colour, ";"),
      div(class = "metric-label", label),
      div(class = "metric-value", value),
      div(class = "metric-detail", detail))
}

panel <- function(title, subtitle, content) {
  div(class = "panel",
      div(class = "panel-title", title),
      div(class = "panel-subtitle", subtitle),
      content)
}

# ----------------------------- UI -----------------------------

ui <- fluidPage(
  tags$head(
    tags$title("ThyroidLab | DSP Analytics"),
    tags$style(HTML("
      @import url('https://fonts.googleapis.com/css2?family=DM+Sans:wght@400;500;600;700&family=Space+Grotesk:wght@500;600;700&display=swap');
      :root { --bg:#070B14; --card:#101B2D; --border:rgba(255,255,255,.09);
              --cyan:#25D9C4; --muted:#94A3B8; --text:#F1F5F9; }
      body { background:radial-gradient(circle at 10% 0%,#14213a 0,#070B14 48%,#050810 100%) fixed;
             color:var(--text); font-family:'DM Sans',Arial,sans-serif; }
      .container-fluid { max-width:1600px; padding:22px 28px 40px; }
      .login-shell { position:fixed; inset:0; z-index:9999; display:flex; align-items:center;
        justify-content:center; padding:20px; background:linear-gradient(125deg,#050914,#0b1426,#07151a,#0a1020);
        background-size:300% 300%; animation:bgshift 18s ease infinite; }
      .login-shell:before { content:''; position:absolute; inset:0; pointer-events:none;
        background:radial-gradient(circle at 18% 28%,rgba(49,91,234,.32),transparent 28%),
        radial-gradient(circle at 78% 68%,rgba(32,214,199,.22),transparent 30%); }
      @keyframes bgshift { 0%,100%{background-position:0% 50%} 50%{background-position:100% 50%} }
      .login-card { position:relative; width:100%; max-width:460px; padding:36px;
        border-radius:25px; background:rgba(15,23,42,.97); border:1px solid rgba(255,255,255,.13);
        box-shadow:0 24px 70px rgba(0,0,0,.6),0 0 35px rgba(32,214,199,.10); }
      .login-brand,.brand { font-family:'Space Grotesk',sans-serif; font-weight:700; letter-spacing:-.8px; }
      .login-brand { font-size:30px; color:#fff; }
      .login-brand span,.brand span { color:var(--cyan); }
      .login-kicker { font-size:10px; letter-spacing:2px; color:var(--cyan); font-weight:700; margin:12px 0; }
      .login-card h2 { color:#fff; font-size:28px; margin:12px 0; }
      .login-description,.tagline { color:var(--muted); font-size:13px; line-height:1.7; }
      .login-hint { margin-top:16px; text-align:center; font-size:12px; color:#CBD5E1;
        background:rgba(37,217,196,.08); padding:10px; border-radius:10px; }
      .login-footer { text-align:center; color:#64748B; font-size:10px; margin-top:20px; }
      .login-error { color:#FDA4AF; background:rgba(244,63,94,.10); border:1px solid rgba(244,63,94,.25);
        padding:10px 12px; border-radius:10px; font-size:12px; margin-top:12px; }
      .top-header { padding:23px 30px; margin-bottom:24px; border-radius:0 0 24px 24px;
        background:linear-gradient(120deg,rgba(6,9,19,.98),rgba(15,23,42,.96) 55%,rgba(12,43,53,.95));
        border:1px solid var(--border); box-shadow:0 14px 40px rgba(0,0,0,.35); }
      .brand { font-size:29px; color:#fff; }
      .metric-card,.panel { background:linear-gradient(145deg,rgba(18,35,57,.96),rgba(12,24,41,.96));
        border:1px solid var(--border); border-radius:18px; box-shadow:0 12px 32px rgba(0,0,0,.22); }
      .metric-card { padding:17px; min-height:108px; margin-bottom:12px; transition:all .18s ease; }
      .metric-card:hover,.panel:hover { border-color:rgba(37,217,196,.3); }
      .metric-label { color:#9EB2D2; font-size:10px; text-transform:uppercase; letter-spacing:1.2px; font-weight:700; }
      .metric-value { color:#fff; font-family:'Space Grotesk',sans-serif; font-size:29px; font-weight:700; margin:10px 0 4px; }
      /* Risk estimate colours are set independently for each model result. */
      .risk-result-content .risk-percentage { color: var(--risk-colour, inherit) !important; }
      .risk-result-content .risk-level-badge { display:inline-block; }
      .risk-result-content .risk-progress-fill { min-width:0; }
      .metric-detail,.panel-subtitle,.small-note { color:#91A4C4; font-size:11px; line-height:1.6; }
      .formula-box { width:100%; max-width:900px; margin-left:auto; margin-right:auto; text-align:center; background: rgba(37,217,196,.07); border: 1px solid rgba(37,217,196,.35); border-radius: 12px; padding: 18px 20px; margin: 14px auto 4px; color: #DCE7FB; line-height: 1.9; text-align:center; font-size:15px; overflow-wrap:anywhere; box-shadow:0 8px 26px rgba(0,0,0,.16); }
  .panel { padding:18px; margin-bottom:14px; transition:all .18s ease; }
      .panel-title { color:#fff; font-family:'Space Grotesk',sans-serif; font-weight:700; font-size:16px; margin-bottom:4px; }
      .panel-subtitle { margin-bottom:14px; }
      .hero { padding:34px; margin-bottom:22px; border-radius:22px; border:1px solid var(--border);
        background:radial-gradient(circle at 85% 20%,rgba(32,214,199,.18),transparent 35%),
        radial-gradient(circle at 12% 90%,rgba(139,92,246,.18),transparent 42%),linear-gradient(135deg,#101d32,#0a1222); }
      .hero-label { color:var(--cyan); font-size:10px; letter-spacing:2px; font-weight:700; }
      .hero h1 { font-family:'Space Grotesk',sans-serif; font-size:clamp(28px,3vw,43px); color:#fff; font-weight:700; }
      .hero p { color:#CBD5E1; line-height:1.8; max-width:850px; }
      .notice { background:rgba(255,184,77,.08); border:1px solid rgba(255,184,77,.25);
        border-left:4px solid #FFB86B; padding:12px 15px; border-radius:11px; color:#FFE5B7;
        font-size:12px; line-height:1.6; margin-bottom:20px; }
      .nav-pills { display:flex; flex-wrap:wrap; gap:7px; margin-bottom:18px; }
      .nav-pills>li>a { color:#AEBFDA!important; background:#0E1B2E; border:1px solid #263957!important;
        border-radius:11px!important; padding:10px 14px; font-weight:700; font-size:12px; }
      .nav-pills>li>a:hover { color:#fff!important; background:#1B3151!important; }
      .nav-pills>li.active>a,.nav-pills>li.active>a:hover,.nav-pills>li.active>a:focus {
        color:#07111F!important; background:linear-gradient(100deg,#25D9C4,#70E7FF)!important; border-color:transparent!important; }
      .form-control,.selectize-input,.selectize-control.single .selectize-input {
        background:#091629!important; color:#EDF4FF!important; border:1px solid #2B4263!important;
        border-radius:10px!important; min-height:39px; }
      label,.control-label,h2,h3,h4 { color:#EAF2FF!important; }
      .btn { border-radius:10px!important; font-weight:700!important; transition:all .2s ease!important; }
      .btn-primary,.btn-success { color:#07111F!important; background:linear-gradient(100deg,#25D9C4,#5B8CFF)!important; border:0!important; }
      .btn-info { color:#fff!important; background:linear-gradient(100deg,#5B8CFF,#A879FF)!important; border:0!important; }
      .btn:hover { transform:translateY(-2px); box-shadow:0 8px 24px rgba(37,217,196,.18); }
      .well { background:#0D1A2D!important; border:1px solid #263A59!important; border-radius:15px!important; }
      table.dataTable,table.dataTable thead th,table.dataTable tbody td { color:#DCE7FB!important; border-color:#263957!important; }
      table.dataTable thead th { background:#162944!important; }
      table.dataTable tbody tr,table.dataTable.stripe tbody tr.odd,table.dataTable.stripe tbody tr.even { background:#0E1B2E!important; }
      .dataTables_wrapper { color:#C7D5EC; }
      .dataTables_filter input,.dataTables_length select { color:#EDF4FF!important; background:#091629!important; }
      @media(max-width:768px) { .container-fluid{padding:12px} .login-card{padding:23px} .hero{padding:22px} .top-header{padding:20px} }
    "))
  ),

  conditionalPanel(
    condition = "output.logged_in != 'true'",
    div(class = "login-shell",
      div(class = "login-card",
        div(class = "login-brand", HTML("Thyroid<span>Lab</span>")),
        div(class = "login-kicker", "DSP • DATA SCIENCE & VISUAL ANALYTICS"),
        h2("Welcome back"),
        p(class = "login-description",
          "Explore thyroid biomarkers, cardiovascular factors and statistical patterns through an interactive analytics workspace."),
        textInput("login_username", "Username", placeholder = "Enter username"),
        passwordInput("login_password", "Password", placeholder = "Enter password"),
        actionButton("login_btn", "Sign in to dashboard", class = "btn btn-primary btn-block"),
        uiOutput("login_message"),
        div(class = "login-footer", "Username : adithya ● password : 123")
      )
    )
  ),

  conditionalPanel(
    condition = "output.logged_in == 'true'",
    div(class = "top-header",
      fluidRow(
        column(8,
          div(class = "brand", HTML("Thyroid<span>Lab</span>")),
          div(class = "tagline", "DSP ANALYTICS  |  DISTRIBUTION • RELATIONSHIPS • MULTIVARIATE INSIGHTS")
        ),
        column(4, align = "right",
          div(style = "color:#25D9C4;font-size:12px;font-weight:700;", "● ANALYTICS ONLINE"),
          div(style = "color:#C2CEE5;font-size:12px;margin-top:7px;", textOutput("today", inline = TRUE)),
          uiOutput("welcome_user"),
          actionButton("logout_btn", "Log out", class = "btn btn-default")
        )
      )
    ),

    div(class = "notice",
      strong("Research note: "),
      "The default records are synthetic and generated for demonstration. Patterns, correlations and the illustrative outcome are not validated clinical findings and must not be used for diagnosis or treatment."
    ),

    tabsetPanel(id = "main_tabs", type = "pills",

      tabPanel("⌂ Overview", value = "overview",
        br(),
        div(class = "hero",
          div(class = "hero-label", "THYROID & CARDIOVASCULAR DATA • DSP PROJECT"),
          h1(HTML("Turn raw data into <span style='color:#25D9C4'>clear insights.</span>")),
          p("A polished interactive dashboard for exploratory data analysis, distribution analysis, group comparisons, correlation discovery and multivariate visualization. Filter the data, switch chart techniques and export the records for further analysis."),
          actionButton("go_explorer", "Explore visualizations →", class = "btn btn-primary")
        ),
        uiOutput("overview_metrics"),
        fluidRow(
          column(6, panel("Numeric data completeness", "Percentage of valid values for each numeric variable.", plotlyOutput("outcome_plot", height = "330px"))),
          column(6, panel("Thyroid status composition", "Relative distribution of thyroid status categories.", plotlyOutput("thyroid_plot", height = "330px")))
        ),
        fluidRow(
          column(6, panel("Age profile", "Histogram with density overlay.", plotlyOutput("age_distribution", height = "320px"))),
          column(6, panel("BMI by outcome", "Compare central tendency and spread.", plotlyOutput("bmi_outcome", height = "320px")))
        ),
        panel("Dataset preview", "First records from the active dataset.", DTOutput("overview_table"))
      ),

      tabPanel("◈ Distribution Lab", value = "distribution",
        br(),
        fluidRow(
          column(4,
            div(class = "panel",
              h4("Chart controls"),
              selectInput("dist_var", "Numeric variable", choices = var_choices, selected = "TSH"),
              selectInput("dist_type", "Visualization technique",
                choices = c("Histogram" = "hist", "Density curve" = "density",
                  "Box plot" = "box", "Violin plot" = "violin",
                  "ECDF" = "ecdf", "Q–Q plot" = "qq",
                  "Rug + density" = "rug", "Frequency polygon" = "freqpoly")),
              selectInput("dist_group", "Optional group comparison",
                choices = c("No grouping" = "None", "Cardiovascular outcome" = "Cardiovascular_Disease",
                            "Thyroid status" = "Thyroid_Status", "Gender" = "Gender")),
              checkboxInput("show_mean", "Show mean reference", TRUE),
              checkboxInput("show_points", "Show individual observations", FALSE),
              p(class = "small-note", "Use distribution plots to inspect skew, spread, outliers and potential deviations from normality.")
            )
          ),
          column(8,
            panel("Distribution explorer", "Interactive plot; hover to inspect values.", plotlyOutput("distribution_plot", height = "480px")),
            uiOutput("distribution_stats"),
            panel("Formula reference", "Useful for interpreting the selected distribution.", uiOutput("distribution_formula"))
          )
        )
      ),

      tabPanel("⇄ Relationship Lab", value = "relationships",
        br(),
        fluidRow(
          column(4,
            div(class = "panel",
              h4("Relationship controls"),
              selectInput("x_var", "X-axis variable", choices = var_choices, selected = "Age"),
              selectInput("y_var", "Y-axis variable", choices = var_choices, selected = "BMI"),
              selectInput("rel_group", "Colour by", choices = c("Cardiovascular outcome" = "Cardiovascular_Disease",
                "Thyroid status" = "Thyroid_Status", "Gender" = "Gender", "Diabetes" = "Diabetes",
                "Smoking" = "Smoking", "No grouping" = "None")),
              selectInput("rel_type", "Chart technique",
                choices = c("Scatter plot" = "scatter", "Scatter + regression" = "smooth",
                            "Hexbin-style binned scatter" = "bin", "2D density contours" = "contour",
                            "Jitter plot" = "jitter")),
              checkboxInput("rel_marginals", "Add marginal distributions (scatter only)", FALSE)
            )
          ),
          column(8,
            panel("Bivariate analysis", "Explore association, clusters, trends and unusual observations.", plotlyOutput("relationship_plot", height = "500px")),
            uiOutput("relationship_stats"),
            panel("Formula reference", "How the relationship statistic is calculated.", uiOutput("relationship_formula"))
          )
        ),
        fluidRow(
          column(6, panel("Age vs systolic blood pressure", "Regression line shows a descriptive trend only.", plotlyOutput("age_bp_plot", height = "330px"))),
          column(6, panel("TSH vs Free T4", "Thyroid marker relationship by thyroid status.", plotlyOutput("tsh_ft4_plot", height = "330px")))
        )
      ),

      tabPanel("▦ Correlation & Multivariate", value = "multivariate",
        br(),
        fluidRow(
          column(6, tagList(panel("Correlation heatmap", "Pearson correlation coefficients for numeric variables.", plotlyOutput("correlation_plot", height = "500px")), panel("Formula reference", "How Pearson correlation is calculated.", uiOutput("correlation_formula")))),
          column(6, panel("Correlation ranking", "Strongest absolute pairwise correlations.", plotlyOutput("correlation_rank", height = "500px")))
        ),
        fluidRow(
          column(7, panel("Parallel coordinates", "Compare several normalized measurements across records.", plotlyOutput("parallel_plot", height = "420px"))),
          column(5, panel("Radar profile", "Mean standardized profile for selected groups.", plotlyOutput("radar_plot", height = "420px")))
        ),
        fluidRow(
          column(6, panel("Pairwise scatter matrix", "Inspect multiple variable pairs at once.", plotlyOutput("pairs_plot", height = "500px"))),
          column(6, panel("Group-wise summary", "Mean and standard deviation by thyroid status.", DTOutput("group_summary")))
        )
      ),

      tabPanel("♥ Risk Estimator", value = "risk",
        br(),
        div(class = "notice", "Educational estimates only. These models are not medical devices or validated clinical risk scores. Synthetic or uploaded data may not represent real-world health outcomes."),
        fluidRow(
          column(6,
            div(class = "panel risk-profile-panel",
              h4("Cardiovascular profile inputs"),
              numericInput("risk_age", "Age (years)", value = 45, min = 18, max = 100),
              numericInput("risk_bmi", "BMI", value = 27, min = 10, max = 80, step = 0.1),
              numericInput("risk_sysbp", "Systolic blood pressure (mmHg)", value = 125, min = 60, max = 280),
              numericInput("risk_hdl", "HDL cholesterol", value = 50, min = 5, max = 150),
              selectInput("risk_diabetes", "Diabetes", choices = c("No", "Yes")),
              selectInput("risk_smoking", "Smoking", choices = c("No", "Yes")),
              actionButton("calculate_cardio_risk", "Estimate cardiovascular risk", class = "btn btn-primary", width = "100%"),
              p(class = "small-note", "Uses age, BMI, systolic BP, HDL, diabetes and smoking from the active dataset.")
            )
          ),
          column(6,
            div(class = "panel risk-profile-panel",
              h4("Thyroid profile inputs"),
              numericInput("thyroid_risk_age", "Age (years)", value = 45, min = 18, max = 100),
              numericInput("thyroid_risk_tsh", "TSH", value = 2.2, min = 0.01, max = 100, step = 0.1),
              numericInput("thyroid_risk_ft3", "Free T3", value = 3.1, min = 0.1, max = 20, step = 0.1),
              numericInput("thyroid_risk_ft4", "Free T4", value = 1.15, min = 0.1, max = 10, step = 0.05),
              numericInput("thyroid_risk_tpo", "TPO antibodies", value = 35, min = 0, max = 10000, step = 1),
              actionButton("calculate_thyroid_risk", "Estimate thyroid risk", class = "btn btn-info", width = "100%"),
              p(class = "small-note", "Estimates the probability of an abnormal thyroid-status label (hypothyroid or hyperthyroid) versus normal in the active dataset.")
            )
          )
        ),
        fluidRow(
          column(6, div(class = "risk-result-panel", panel("Cardiovascular risk estimate", "Separate logistic-regression result.", uiOutput("cardio_risk_result")))),
          column(6, div(class = "risk-result-panel", panel("Thyroid risk estimate", "Separate logistic-regression result.", uiOutput("thyroid_risk_result"))))
        ),
        fluidRow(
          column(6, panel("Cardiovascular model formula", "Illustrative model only.", div(class = "formula-box", HTML("<b>p = 1 / (1 + e<sup>−η</sup>)</b><br><b>η = β₀ + β₁Age + β₂BMI + β₃SYS_BP + β₄HDL + β₅Diabetes + β₆Smoking</b>")))),
          column(6, panel("Thyroid model formula", "Abnormal versus normal thyroid-status label.", div(class = "formula-box", HTML("<b>p = 1 / (1 + e<sup>−η</sup>)</b><br><b>η = β₀ + β₁Age + β₂TSH + β₃Free_T3 + β₄Free_T4 + β₅TPO</b>"))))
        ),
        panel("Interpretation and limitations", "For software demonstration only.", uiOutput("risk_limitations"))
      ),

      tabPanel("▤ Data Explorer", value = "data",
        br(),
        div(class = "panel",
          h4("Import and filter data"),
          p(class = "small-note", "Upload a CSV containing columns that match the project dataset schema, or restore the built-in sample."),
          fileInput("upload_csv", "Upload CSV file", accept = ".csv"),
          actionButton("restore_sample", "Restore sample dataset", class = "btn btn-info"),
          uiOutput("upload_status")
        ),
        fluidRow(
          column(4, selectInput("filter_outcome", "Cardiovascular outcome", choices = c("All records", "No", "Yes"))),
          column(4, selectInput("filter_thyroid", "Thyroid status", choices = c("All statuses", "Normal", "Hypothyroid", "Hyperthyroid"))),
          column(4, selectInput("table_var", "Sort/inspect variable", choices = var_choices, selected = "Age"))
        ),
        panel("Filtered dataset", "Search, sort, paginate and export records from the interactive table.", DTOutput("data_table")),
        br(),
        downloadButton("download_data", "Download filtered CSV", class = "btn btn-primary")
      ),

      tabPanel("◎ Statistical Summary", value = "stats",
        br(),
        fluidRow(
          column(6, panel("Descriptive statistics", "Mean, median, standard deviation, quartiles and range.", DTOutput("descriptive_table"))),
          column(6, panel("Missingness profile", "Missing values and completeness by variable.", plotlyOutput("missing_plot", height = "350px")))
        ),
        fluidRow(
          column(6, panel("Outcome comparison", "Numeric distributions split by outcome.", plotlyOutput("outcome_boxplot", height = "390px"))),
          column(6, panel("Empirical cumulative distribution", "Compare empirical distributions across outcome groups.", plotlyOutput("outcome_ecdf", height = "390px")))
        )
      ),

      tabPanel("✦ Data Quality", value = "quality",
        br(),
        div(class = "notice", "Preprocessing creates a separate analysis dataset. The raw imported data remains unchanged until you choose to download the cleaned version."),
        fluidRow(
          column(4, div(class = "panel",
            h4("Preprocessing controls"),
            selectInput("missing_method", "Missing-value treatment", choices = c("Keep missing values" = "keep", "Remove rows with missing values" = "drop", "Impute numeric median / categorical mode" = "impute"), selected = "impute"),
            checkboxInput("remove_duplicates", "Remove duplicate records", TRUE),
            selectInput("outlier_method", "Outlier detection", choices = c("IQR rule" = "iqr", "Z-score rule" = "zscore", "Do not flag outliers" = "none")),
            sliderInput("outlier_threshold", "Outlier threshold", min = 1.5, max = 4, value = 1.5, step = .1),
            selectInput("outlier_action", "Outlier handling", choices = c("Flag only" = "flag", "Replace with boundary (winsorize)" = "winsor", "Remove rows with outliers" = "remove")),
            selectInput("scale_method", "Numeric scaling", choices = c("No scaling" = "none", "Min–max normalization [0,1]" = "minmax", "Z-score standardization" = "standard")),
            actionButton("apply_preprocessing", "Apply preprocessing", class = "btn btn-primary"),
            actionButton("reset_preprocessing", "Reset cleaned data", class = "btn btn-info")
          )),
          column(8,
            panel("Before-and-after quality summary", "Missingness, duplicate rows and flagged outliers are calculated from the active source data.", DTOutput("quality_summary")),
            panel("Preprocessing log", "Actions applied to the current analysis dataset.", verbatimTextOutput("preprocess_log")),
            panel("Cleaned data preview", "Preview of the transformed analysis data.", DTOutput("cleaned_preview")),
            downloadButton("download_cleaned", "Download cleaned CSV", class = "btn btn-primary")
          )
        )
      ),

      tabPanel("∑ Statistical Tests", value = "tests",
        br(),
        fluidRow(
          column(4, div(class = "panel",
            h4("Test configuration"),
            selectInput("test_numeric", "Numeric variable", choices = var_choices, selected = "Age"),
            selectInput("test_group", "Grouping variable", choices = c("Cardiovascular outcome" = "Cardiovascular_Disease", "Thyroid status" = "Thyroid_Status", "Gender" = "Gender", "Diabetes" = "Diabetes", "Smoking" = "Smoking")),
            selectInput("group_test", "Group comparison test", choices = c("Automatic (2 groups: t / Mann–Whitney; 3+: ANOVA / Kruskal–Wallis)" = "auto", "Welch independent t-test" = "ttest", "Mann–Whitney U / Wilcoxon rank-sum" = "wilcox", "One-way ANOVA" = "anova", "Kruskal–Wallis" = "kruskal")),
            selectInput("corr_x", "Correlation variable X", choices = var_choices, selected = "Age"),
            selectInput("corr_y", "Correlation variable Y", choices = var_choices, selected = "BMI"),
            selectInput("corr_method", "Correlation method", choices = c("Pearson" = "pearson", "Spearman" = "spearman")),
            selectInput("chi_x", "Chi-square variable 1", choices = c("Gender", "Diabetes", "Smoking", "Thyroid_Status", "Cardiovascular_Disease")),
            selectInput("chi_y", "Chi-square variable 2", choices = c("Cardiovascular_Disease", "Thyroid_Status", "Gender", "Diabetes", "Smoking"), selected = "Cardiovascular_Disease"),
            p(class = "small-note", "Interpret p-values in context; statistical significance does not imply causation.")
          )),
          column(8,
            panel("Numeric correlation", "Coefficient, p-value and confidence interval when available.", verbatimTextOutput("correlation_test_result")),
            panel("Group comparison", "Automatic mode selects a two-group or multi-group test based on observed groups.", verbatimTextOutput("group_test_result")),
            panel("Categorical association", "Pearson chi-square test of independence with contingency table.", verbatimTextOutput("chi_square_result")),
            panel("Interpretation", "Plain-English summary of the selected tests.", uiOutput("test_interpretation")),
            panel("Formula reference", "The dashboard displays the formulas used by each analysis.", uiOutput("formula_reference"))
          )
        )
      ),

      tabPanel("✧ Machine Learning", value = "ml",
        br(),
        div(class = "notice", "Machine-learning outputs are educational demonstrations. Synthetic sample data cannot validate real-world or clinical prediction."),
        fluidRow(
          column(4, div(class = "panel",
            h4("Model controls"),
            selectInput("ml_target", "Classification target (binary categorical only)", choices = c("Cardiovascular_Disease", "Diabetes", "Smoking")),
            selectizeInput("ml_features", "Predictor variables", choices = c(numeric_vars, "Gender", "Diabetes", "Smoking", "Thyroid_Status"), selected = c("Age", "BMI", "TSH", "SYS_BP", "Diabetes", "Smoking"), multiple = TRUE, options = list(plugins = list("remove_button"))),
            sliderInput("ml_split", "Training data (%)", min = 60, max = 85, value = 75, step = 5),
            sliderInput("ml_seed", "Reproducible random seed", min = 1, max = 999, value = 42),
            selectInput("ml_model", "Classifier", choices = c("Logistic regression" = "glm", "Decision tree" = "tree", "Random forest" = "rf")),
            actionButton("run_ml", "Train / evaluate model", class = "btn btn-primary"),
            p(class = "small-note", "Requires rpart for decision trees and randomForest for random forest. Missing predictor rows are removed for model fitting.")
          )),
          column(8,
            panel("Model evaluation", "Train/test holdout metrics and confusion matrix.", verbatimTextOutput("ml_metrics")),
            fluidRow(column(6, panel("Confusion matrix", "Actual vs predicted test labels.", plotlyOutput("ml_confusion_plot", height = "330px"))),
                     column(6, panel("Feature importance", "Model-based importance where supported; absolute logistic coefficients otherwise.", plotlyOutput("ml_importance_plot", height = "330px")))),
            panel("ROC curve", "Binary classification discrimination on the held-out test set.", plotlyOutput("ml_roc_plot", height = "350px")),
            panel("Cross-validation", "K-fold cross-validation summary for the selected classifier where supported.", verbatimTextOutput("ml_cv_result"))
          )
        )
      ),

      tabPanel("✦ Insights & Report", value = "reports",
        br(),
        panel("Automated insights", "Data-driven summaries of correlations, distributions, outliers and group differences.", uiOutput("auto_insights")),
        fluidRow(
          column(6, panel("Analysis snapshot", "A compact summary of the active dataset and preprocessing state.", DTOutput("report_snapshot"))),
          column(6, div(class = "panel",
            h4("Export report"),
            p(class = "small-note", "PDF includes dataset metadata, quality checks, preprocessing log, descriptive statistics, test summaries and limitations. Charts are exported separately from their interactive on-screen versions."),
            downloadButton("download_pdf_report", "Download PDF report", class = "btn btn-primary"), br(), br(),
            downloadButton("download_insights", "Download insights TXT", class = "btn btn-info"), br(), br(),
            downloadButton("download_cleaned_report", "Download cleaned data CSV", class = "btn btn-success")
          ))
        ),
        panel("Formula guide", "The project exposes the formulas used for its principal DSP analyses.", uiOutput("all_formulas"))
      )
    )
  )
)

# ---------------------------- SERVER ---------------------------


# ------------------ DATA NORMALIZATION HELPERS ------------------
normalize_project_data <- function(d) {
  names(d) <- trimws(sub("^\ufeff", "", names(d)))
  # Map common alternative labels to the canonical names used by this app.
  aliases <- c(
    "cardiovascular disease" = "Cardiovascular_Disease",
    "cardiovascular_disease" = "Cardiovascular_Disease",
    "cvd" = "Cardiovascular_Disease",
    "cvd_label" = "Cardiovascular_Disease",
    "outcome" = "Cardiovascular_Disease",
    "gender" = "Gender", "sex" = "Gender",
    "tsh" = "TSH", "free_t3" = "Free_T3", "free t3" = "Free_T3",
    "free_t4" = "Free_T4", "free t4" = "Free_T4",
    "total_cholesterol" = "Total_Cholesterol",
    "sys_bp" = "SYS_BP", "systolic_bp" = "SYS_BP",
    "dia_bp" = "DIA_BP", "diastolic_bp" = "DIA_BP",
    "thyroid status" = "Thyroid_Status", "thyroid_status" = "Thyroid_Status"
  )
  for (nm in names(d)) {
    key <- tolower(gsub("[[:space:]]+", " ", trimws(nm)))
    key2 <- tolower(trimws(nm))
    if (key %in% names(aliases)) names(d)[names(d) == nm] <- unname(aliases[[key]])
    else if (key2 %in% names(aliases)) names(d)[names(d) == nm] <- unname(aliases[[key2]])
  }
  if (!"Cardiovascular_Disease" %in% names(d)) {
    # If no explicit outcome exists, create a neutral unavailable label rather than crashing charts.
    d$Cardiovascular_Disease <- factor(rep("Not supplied", nrow(d)))
  } else {
    cv <- trimws(tolower(as.character(d$Cardiovascular_Disease)))
    cv[cv %in% c("0", "false", "no", "negative", "absent")] <- "No"
    cv[cv %in% c("1", "true", "yes", "positive", "present")] <- "Yes"
    cv[is.na(cv) | !nzchar(cv)] <- "Unknown"
    d$Cardiovascular_Disease <- factor(cv, levels = unique(c("No", "Yes", "Unknown", setdiff(cv, c("No", "Yes", "Unknown")))))
  }
  if (!"Thyroid_Status" %in% names(d)) d$Thyroid_Status <- factor(rep("Not supplied", nrow(d)))
  for (v in intersect(numeric_vars, names(d))) d[[v]] <- suppressWarnings(as.numeric(as.character(d[[v]])))
  d
}

server <- function(input, output, session) {
  authenticated <- reactiveVal(FALSE)
  login_message <- reactiveVal("")
  data_state <- reactiveVal(normalize_project_data(sample_data))
  source_state <- reactiveVal("Synthetic sample data")

  output$logged_in <- renderText(if (isTRUE(authenticated())) "true" else "false")
  outputOptions(output, "logged_in", suspendWhenHidden = FALSE)

  output$login_message <- renderUI({
    msg <- login_message()
    if (!nzchar(msg)) return(NULL)
    div(class = "login-error", msg)
  })

  observeEvent(input$login_btn, {
    username <- trimws(if (is.null(input$login_username)) "" else input$login_username)
    password <- if (is.null(input$login_password)) "" else input$login_password
    if (identical(tolower(username), "adithya") && identical(password, "123")) {
      authenticated(TRUE)
      login_message("")
      showNotification("Login successful. Welcome to ThyroidLab!", type = "message")
    } else {
      authenticated(FALSE)
      login_message("Invalid username or password. Please try again.")
    }
  })

  observeEvent(input$logout_btn, {
    authenticated(FALSE)
    updateTextInput(session, "login_username", value = "")
    updateTextInput(session, "login_password", value = "")
    login_message("")
  })

  output$today <- renderText(format(Sys.Date(), "%d %B %Y"))
  output$welcome_user <- renderUI({
    if (!authenticated()) return(NULL)
    div(style = "color:#25D9C4;font-size:12px;font-weight:700;margin:7px 0;",
        "SIGNED IN • adithya")
  })

  observeEvent(input$go_explorer, updateTabsetPanel(session, "main_tabs", selected = "distribution"))

  observeEvent(input$upload_csv, {
    req(input$upload_csv$datapath)
    uploaded <- tryCatch(read.csv(input$upload_csv$datapath, stringsAsFactors = FALSE,
                                  check.names = FALSE), error = function(e) e)
    if (inherits(uploaded, "error")) {
      output$upload_status <- renderUI(div(class = "login-error", "Could not read the CSV file."))
      return()
    }
    # Normalize common header whitespace/BOM issues without changing column meaning.
    names(uploaded) <- trimws(sub("^\\ufeff", "", names(uploaded)))
    # Convert numeric columns explicitly: CSV imports can otherwise leave them as character.
    required_cols <- names(sample_data)
    missing_cols <- setdiff(required_cols, names(uploaded))
    if (length(missing_cols)) {
      output$upload_status <- renderUI(div(class = "login-error",
        paste("CSV schema mismatch. Missing columns:", paste(missing_cols, collapse = ", "),
              ". Required columns:", paste(required_cols, collapse = ", "))))
      return()
    }
    uploaded <- uploaded[, required_cols, drop = FALSE]
    for (v in numeric_vars) uploaded[[v]] <- suppressWarnings(as.numeric(as.character(uploaded[[v]])))
    uploaded$Age <- suppressWarnings(as.numeric(as.character(uploaded$Age)))
    cvd_text <- trimws(tolower(as.character(uploaded$Cardiovascular_Disease)))
    cvd_text[cvd_text %in% c("0", "false", "no", "negative")] <- "No"
    cvd_text[cvd_text %in% c("1", "true", "yes", "positive")] <- "Yes"
    uploaded$Cardiovascular_Disease <- factor(cvd_text, levels = c("No", "Yes"))
    thyroid_text <- trimws(tolower(as.character(uploaded$Thyroid_Status)))
    thyroid_text[thyroid_text %in% c("normal", "reference_range", "reference range", "euthyroid")] <- "Normal"
    thyroid_text[thyroid_text %in% c("hypothyroid", "possible_hypothyroid", "possible hypothyroid")] <- "Hypothyroid"
    thyroid_text[thyroid_text %in% c("hyperthyroid", "possible_hyperthyroid", "possible hyperthyroid")] <- "Hyperthyroid"
    uploaded$Thyroid_Status <- factor(thyroid_text, levels = c("Normal", "Hypothyroid", "Hyperthyroid"))
    uploaded$Gender <- as.character(uploaded$Gender)
    uploaded$Diabetes <- as.character(uploaded$Diabetes)
    uploaded$Smoking <- as.character(uploaded$Smoking)
    # Drop rows with no usable target only for plots that require a target; retain them in data.
    data_state(normalize_project_data(uploaded))
    cleaned_state(uploaded)
    preprocessing_log("Uploaded CSV loaded. Apply preprocessing to create a cleaned analysis dataset.")
    source_state("Uploaded CSV")
    output$upload_status <- renderUI(div(class = "small-note", paste(nrow(uploaded), "records loaded successfully.")))
    showNotification("Dataset imported successfully.", type = "message")
  })

  observeEvent(input$restore_sample, {
    data_state(normalize_project_data(sample_data))
    cleaned_state(sample_data)
    preprocessing_log("Synthetic sample dataset restored. No preprocessing changes applied.")
    source_state("Synthetic sample data")
    output$upload_status <- renderUI(div(class = "small-note", "Built-in synthetic dataset restored."))
  })

  filtered_data <- reactive({
    d <- data_state()
    if (!is.null(input$filter_outcome) && input$filter_outcome != "All records")
      d <- d[as.character(d$Cardiovascular_Disease) == input$filter_outcome, , drop = FALSE]
    if (!is.null(input$filter_thyroid) && input$filter_thyroid != "All statuses")
      d <- d[as.character(d$Thyroid_Status) == input$filter_thyroid, , drop = FALSE]
    d
  })

  output$overview_metrics <- renderUI({
    d <- data_state()
    n <- nrow(d)
    positives <- sum(d$Cardiovascular_Disease == "Yes", na.rm = TRUE)
    avg_age <- if (n) round(mean(d$Age, na.rm = TRUE), 1) else NA
    avg_bmi <- if (n) round(mean(d$BMI, na.rm = TRUE), 1) else NA
    fluidRow(
      column(3, metric_card("Dataset records", comma(n), "Active observations", accent[1])),
      column(3, metric_card("Variables", ncol(d), "Demographic + biomarkers", accent[2])),
      column(3, metric_card("Outcome = Yes", comma(positives), "Synthetic outcome labels", accent[3])),
      column(3, metric_card("Mean age", avg_age, paste("Mean BMI:", avg_bmi), accent[4]))
    )
  })

  output$outcome_plot <- renderPlotly({
    d <- data_state()
    vars <- intersect(numeric_vars, names(d))
    validate(need(length(vars) > 0, "No numeric biomarker columns are available. Upload a dataset with numeric variables."))
    quality <- data.frame(
      Variable = vars,
      Valid = vapply(vars, function(v) sum(is.finite(suppressWarnings(as.numeric(as.character(d[[v]]))))), numeric(1)),
      Total = nrow(d),
      stringsAsFactors = FALSE
    )
    quality$Missing <- pmax(0, quality$Total - quality$Valid)
    quality$Completeness <- ifelse(quality$Total > 0, 100 * quality$Valid / quality$Total, 0)
    quality$Variable <- factor(quality$Variable, levels = quality$Variable[order(quality$Completeness)])
    p <- ggplot(quality, aes(x = Variable, y = Completeness,
                             text = paste0("Variable: ", Variable,
                                           "<br>Valid: ", Valid,
                                           "<br>Missing: ", Missing,
                                           "<br>Completeness: ", round(Completeness, 1), "%"))) +
      geom_col(fill = "#25D9C4", alpha = .9, width = .72) +
      coord_flip() + scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, 20),
                                        labels = function(x) paste0(x, "%")) +
      labs(title = "Numeric data completeness", x = NULL, y = "Valid observations") +
      theme_dsp() + theme(panel.grid.major.y = element_blank())
    ggplotly(p, tooltip = "text") %>% layout(margin = list(l = 110, r = 18, t = 48, b = 45),
      paper_bgcolor = "rgba(0,0,0,0)", plot_bgcolor = "rgba(0,0,0,0)")
  })

  output$thyroid_plot <- renderPlotly({
    d <- data_state()
    if (!"Thyroid_Status" %in% names(d)) d$Thyroid_Status <- "Not supplied"
    st <- as.character(d$Thyroid_Status)
    st[is.na(st) | !nzchar(st)] <- "Unknown"
    counts <- as.data.frame(table(Status = st), stringsAsFactors = FALSE)
    counts <- counts[counts$Freq > 0, , drop = FALSE]
    if (!nrow(counts)) counts <- data.frame(Status = "No data", Freq = 1)
    names(counts) <- c("Status", "Count")
    plot_ly(counts, labels = ~Status, values = ~Count, type = "pie", hole = .58,
            textinfo = "label+percent", sort = FALSE,
            marker = list(colors = rep(accent, length.out = nrow(counts)))) %>%
      layout(showlegend = TRUE, paper_bgcolor = "rgba(0,0,0,0)",
             plot_bgcolor = "rgba(0,0,0,0)", font = list(color = "#DCE7FB"),
             margin = list(t = 18, b = 18, l = 8, r = 8))
  })

  output$age_distribution <- renderPlotly({
    d <- data_state()
    p <- ggplot(d, aes(Age)) +
      geom_histogram(aes(y = after_stat(density)), bins = 24,
                     fill = "#5B8CFF", colour = "#101B2D", alpha = .8) +
      geom_density(colour = "#25D9C4", linewidth = 1.1, na.rm = TRUE) +
      labs(title = "Age distribution", x = "Age (years)", y = "Density") + theme_dsp()
    ggplotly(p, tooltip = "all") %>% config(displaylogo = FALSE, toImageButtonOptions = list(format = "png", filename = "dsp_chart", width = 2400, height = 1600, scale = 2))
  })

  output$bmi_outcome <- renderPlotly({
    d <- data_state()
    if (!"BMI" %in% names(d)) {
      validate(need(FALSE, "BMI is not present in the uploaded dataset. Choose another numeric variable in Distribution Lab."))
    }
    d$BMI <- suppressWarnings(as.numeric(as.character(d$BMI)))
    d <- d[is.finite(d$BMI), , drop = FALSE]
    validate(need(nrow(d) > 1, "BMI requires at least two valid numeric observations."))
    if ("Cardiovascular_Disease" %in% names(d) &&
        length(unique(na.omit(as.character(d$Cardiovascular_Disease)))) > 1) {
      d$Outcome <- as.factor(d$Cardiovascular_Disease)
      p <- ggplot(d, aes(x = Outcome, y = BMI, fill = Outcome)) +
        geom_violin(alpha = .38, trim = FALSE, na.rm = TRUE) +
        geom_boxplot(width = .16, alpha = .9, outlier.alpha = .3, na.rm = TRUE) +
        labs(title = "BMI distribution by outcome", x = "Outcome", y = "BMI")
    } else {
      p <- ggplot(d, aes(x = BMI)) +
        geom_histogram(bins = 26, fill = "#5B8CFF", colour = "#101B2D", alpha = .9) +
        geom_density(aes(y = after_stat(count)), colour = "#25D9C4", linewidth = 1, na.rm = TRUE) +
        labs(title = "BMI distribution", x = "BMI", y = "Count")
    }
    p <- p + theme_dsp() + theme(legend.position = "none")
    ggplotly(p, tooltip = "all") %>% layout(
      margin = list(l = 55, r = 18, t = 45, b = 55),
      paper_bgcolor = "rgba(0,0,0,0)", plot_bgcolor = "rgba(0,0,0,0)")
  })

  output$overview_table <- renderDT({
    datatable(head(data_state(), 12), options = list(scrollX = TRUE, pageLength = 6),
              rownames = FALSE, class = "compact stripe hover")
  })

  output$distribution_plot <- renderPlotly({
    d <- data_state()
    v <- input$dist_var
    req(v %in% names(d))
    group <- input$dist_group %||% "None"
    d[[v]] <- suppressWarnings(as.numeric(as.character(d[[v]])))
    d <- d[is.finite(d[[v]]), , drop = FALSE]
    validate(need(nrow(d) > 1, "This variable has fewer than two valid numeric observations."))
    if (group != "None" && group %in% names(d)) d[[group]] <- as.factor(d[[group]])
    p <- ggplot(d, aes(x = .data[[v]]))
    if (group != "None") p <- ggplot(d, aes(x = .data[[v]], fill = .data[[group]], colour = .data[[group]]))
    type <- input$dist_type
    if (type == "hist") {
      p <- p + geom_histogram(aes(y = after_stat(density)), bins = 28, alpha = .65, position = "identity")
      if (group == "None") p <- p + geom_density(colour = "#25D9C4", linewidth = 1)
    } else if (type == "density") {
      p <- p + geom_density(alpha = .25, linewidth = 1)
    } else if (type == "box") {
      p <- ggplot(d, aes(x = if (group == "None") "All records" else .data[[group]], y = .data[[v]], fill = if (group == "None") "All records" else .data[[group]])) +
        geom_boxplot(alpha = .8, outlier.alpha = .35)
    } else if (type == "violin") {
      p <- ggplot(d, aes(x = if (group == "None") "All records" else .data[[group]], y = .data[[v]], fill = if (group == "None") "All records" else .data[[group]])) +
        geom_violin(alpha = .55, trim = FALSE) + geom_boxplot(width = .12, alpha = .8)
    } else if (type == "ecdf") {
      p <- p + stat_ecdf(geom = "step", linewidth = 1.1)
    } else if (type == "qq") {
      p <- ggplot(d, aes(sample = .data[[v]])) + stat_qq(colour = "#5B8CFF", alpha = .65) +
        stat_qq_line(colour = "#25D9C4", linewidth = 1)
    } else if (type == "rug") {
      p <- p + geom_density(colour = "#25D9C4", linewidth = 1.1) +
        geom_rug(alpha = .25, sides = "b")
    } else if (type == "freqpoly") {
      p <- p + geom_freqpoly(bins = 28, linewidth = 1.1)
    }
    if (isTRUE(input$show_mean) && type %in% c("hist", "density", "rug", "freqpoly"))
      p <- p + geom_vline(xintercept = mean(d[[v]], na.rm = TRUE), colour = "#FFB86B",
                          linetype = "dashed", linewidth = .8)
    p <- p + labs(title = paste(type, "—", pretty_names[[v]]),
                  x = pretty_names[[v]], y = if (type == "ecdf") "Cumulative proportion" else "Value / density") +
      theme_dsp() + scale_fill_manual(values = accent, na.value = "#5B8CFF") +
      scale_colour_manual(values = accent, na.value = "#5B8CFF")
    ggplotly(p, tooltip = "all") %>% config(displaylogo = FALSE, toImageButtonOptions = list(format = "png", filename = "dsp_chart", width = 2400, height = 1600, scale = 2))
  })

  output$distribution_formula <- renderUI({
    v <- input$dist_type
    formula <- switch(v,
      hist = "Histogram: bin count = number of observations in each interval.",
      density = HTML("Kernel density: <b>f̂(x) = (1 / nh) Σ K((x − xᵢ) / h)</b>"),
      box = HTML("IQR = <b>Q3 − Q1</b>; outlier fences: <b>Q1 − 1.5×IQR</b> and <b>Q3 + 1.5×IQR</b>"),
      violin = HTML("Violin width represents estimated <b>kernel density</b>; centre box shows median and quartiles."),
      ecdf = HTML("Empirical CDF: <b>Fₙ(x) = (number of xᵢ ≤ x) / n</b>"),
      qq = HTML("Q–Q plot compares sample quantiles with theoretical normal quantiles; near-linear points indicate approximate normality."),
      rug = HTML("Rug marks show individual observations; density curve estimates the distribution."),
      freqpoly = "Frequency polygon connects the counts of adjacent histogram bins.")
    div(class = "formula-box", formula)
  })
  output$relationship_formula <- renderUI({
    type <- input$rel_type
    formula <- if (type == "smooth") HTML("Linear regression: <b>ŷ = β₀ + β₁x</b>; residual = <b>y − ŷ</b>.") else if (type == "bin") "Binned scatter groups observations into 2D bins; colour represents count per bin." else if (type == "contour") "2D density contours connect locations with similar estimated joint density." else if (type == "jitter") "Jitter adds small random offsets to reveal overlapping observations." else HTML("Pearson correlation: <b>r = cov(X,Y) / (sₓ sᵧ)</b>; −1 ≤ r ≤ 1.")
    div(class = "formula-box", formula)
  })
  output$correlation_formula <- renderUI({
    div(class = "formula-box", HTML("Pearson correlation: <b>r = Σ[(xᵢ − x̄)(yᵢ − ȳ)] / √[Σ(xᵢ − x̄)² Σ(yᵢ − ȳ)²]</b><br/>Values range from −1 to +1. Correlation describes association, not causation."))
  })

  output$distribution_stats <- renderUI({
    d <- data_state()
    v <- input$dist_var
    x <- suppressWarnings(as.numeric(as.character(d[[v]])))
    x <- x[is.finite(x)]
    validate(need(length(x) > 0, "No valid numeric values available for this variable."))
    div(class = "panel",
      div(class = "panel-title", "Quick descriptive readout"),
      fluidRow(
        column(3, metric_card("Valid n", comma(length(x)), "Non-missing values", accent[1])),
        column(3, metric_card("Mean", round(mean(x), 3), "Arithmetic average", accent[2])),
        column(3, metric_card("Median", round(median(x), 3), "50th percentile", accent[3])),
        column(3, metric_card("Std. deviation", round(sd(x), 3), "Sample spread", accent[4]))
      )
    )
  })

  output$relationship_plot <- renderPlotly({
    d <- data_state()
    x <- input$x_var; y <- input$y_var
    g <- input$rel_group %||% "None"
    req(x %in% names(d), y %in% names(d))
    d[[x]] <- suppressWarnings(as.numeric(as.character(d[[x]])))
    d[[y]] <- suppressWarnings(as.numeric(as.character(d[[y]])))
    d <- d[is.finite(d[[x]]) & is.finite(d[[y]]), , drop = FALSE]
    validate(need(nrow(d) > 1, "Select variables with at least two complete numeric observations."))
    p <- ggplot(d, aes(x = .data[[x]], y = .data[[y]]))
    if (g != "None") p <- ggplot(d, aes(x = .data[[x]], y = .data[[y]], colour = .data[[g]]))
    type <- input$rel_type
    if (type == "scatter") p <- p + geom_point(alpha = .65, size = 2)
    if (type == "smooth") p <- p + geom_point(alpha = .45, size = 1.8) +
      geom_smooth(method = "lm", se = TRUE, colour = "#25D9C4", fill = "#25D9C4", alpha = .12)
    if (type == "bin") p <- p + geom_bin2d(bins = 28) + scale_fill_gradient(low = "#13243B", high = "#25D9C4")
    if (type == "contour") p <- p + stat_density_2d(aes(colour = after_stat(level)), linewidth = .7)
    if (type == "jitter") p <- p + geom_jitter(alpha = .5, width = .15, height = .15, size = 2)
    p <- p + labs(title = paste(pretty_names[[x]], "vs", pretty_names[[y]]),
                  x = pretty_names[[x]], y = pretty_names[[y]], colour = g) + theme_dsp()
    ggplotly(p, tooltip = "all") %>% config(displaylogo = FALSE, toImageButtonOptions = list(format = "png", filename = "dsp_chart", width = 2400, height = 1600, scale = 2))
  })

  output$relationship_stats <- renderUI({
    d <- data_state()
    x <- input$x_var; y <- input$y_var
    xx <- suppressWarnings(as.numeric(as.character(d[[x]])))
    yy <- suppressWarnings(as.numeric(as.character(d[[y]])))
    ok <- is.finite(xx) & is.finite(yy)
    r <- if (sum(ok) > 1 && stats::sd(xx[ok]) > 0 && stats::sd(yy[ok]) > 0) suppressWarnings(cor(xx[ok], yy[ok])) else NA_real_
    div(class = "panel",
      div(class = "panel-title", "Pearson correlation"),
      p(style = "font-size:24px;color:#25D9C4;font-weight:700;",
        ifelse(is.finite(r), round(r, 3), "Not available")),
      p(class = "small-note", "Correlation ranges from −1 to +1. It describes linear association, not causation.")
    )
  })

  output$age_bp_plot <- renderPlotly({
    d <- data_state()
    p <- ggplot(d, aes(Age, SYS_BP, colour = Cardiovascular_Disease)) +
      geom_point(alpha = .55) + geom_smooth(method = "lm", se = FALSE, linewidth = .9) +
      scale_colour_manual(values = c("No" = "#5B8CFF", "Yes" = "#25D9C4")) +
      labs(title = "Age and systolic BP", x = "Age (years)", y = "Systolic BP (mmHg)", colour = "Outcome") + theme_dsp()
    ggplotly(p, tooltip = "all") %>% config(displaylogo = FALSE, toImageButtonOptions = list(format = "png", filename = "dsp_chart", width = 2400, height = 1600, scale = 2))
  })

  output$tsh_ft4_plot <- renderPlotly({
    d <- data_state()
    p <- ggplot(d, aes(TSH, Free_T4, colour = Thyroid_Status)) +
      geom_point(alpha = .65, size = 2) + scale_x_log10() +
      scale_colour_manual(values = c("Normal" = "#25D9C4", "Hypothyroid" = "#A879FF", "Hyperthyroid" = "#FFB86B")) +
      labs(title = "TSH and Free T4", x = "TSH (log scale)", y = "Free T4", colour = "Thyroid status") + theme_dsp()
    ggplotly(p, tooltip = "all") %>% config(displaylogo = FALSE, toImageButtonOptions = list(format = "png", filename = "dsp_chart", width = 2400, height = 1600, scale = 2))
  })

  corr_matrix <- reactive({
    d <- data_state()[, intersect(numeric_vars, names(data_state())), drop = FALSE]
    d[] <- lapply(d, function(x) suppressWarnings(as.numeric(as.character(x))))
    keep <- vapply(d, function(x) sum(is.finite(x)) >= 2 && is.finite(stats::sd(x, na.rm = TRUE)) && stats::sd(x, na.rm = TRUE) > 0, logical(1))
    d <- d[, keep, drop = FALSE]
    validate(need(ncol(d) >= 2, "At least two numeric variables with non-zero variance are required for correlation analysis."))
    cm <- suppressWarnings(cor(d, use = "pairwise.complete.obs", method = "pearson"))
    cm[!is.finite(cm)] <- NA_real_
    cm
  })

  output$correlation_plot <- renderPlotly({
    cm <- corr_matrix()
    plot_ly(x = colnames(cm), y = rownames(cm), z = cm, type = "heatmap",
      colorscale = list(
        list(0, "#5B8CFF"), list(0.5, "#101B2D"), list(1, "#25D9C4")
      ),
      zmin = -1, zmax = 1, reversescale = FALSE,
      hovertemplate = "%{y} × %{x}<br>Correlation: %{z:.3f}<extra></extra>") %>%
      layout(paper_bgcolor = "rgba(0,0,0,0)", plot_bgcolor = "rgba(0,0,0,0)",
        font = list(color = "#DCE7FB"), xaxis = list(tickangle = -35), yaxis = list(autorange = "reversed"),
        margin = list(l = 120, b = 100, t = 25, r = 20))
  })

  output$correlation_rank <- renderPlotly({
    cm <- corr_matrix()
    idx <- which(upper.tri(cm), arr.ind = TRUE)
    dd <- data.frame(Variable_1 = rownames(cm)[idx[, 1]], Variable_2 = colnames(cm)[idx[, 2]],
                     Correlation = cm[idx], stringsAsFactors = FALSE)
    dd <- dd[is.finite(dd$Correlation), , drop = FALSE]
    validate(need(nrow(dd) > 0, "No valid pairwise correlations are available."))
    dd <- dd[order(abs(dd$Correlation), decreasing = TRUE), , drop = FALSE]
    dd <- head(dd, 18)
    dd$Pair <- paste(dd$Variable_1, "↔", dd$Variable_2)
    p <- ggplot(dd, aes(reorder(Pair, Correlation), Correlation, fill = Correlation,
                        text = paste(Pair, "<br>r =", round(Correlation, 3)))) +
      geom_col() + coord_flip() + scale_fill_gradient2(low = "#5B8CFF", mid = "#17243A", high = "#25D9C4", midpoint = 0) +
      labs(x = NULL, y = "Pearson r", title = "Top pairwise relationships") + theme_dsp() + theme(legend.position = "none")
    ggplotly(p, tooltip = "text") %>% config(displaylogo = FALSE, toImageButtonOptions = list(format = "png", filename = "dsp_chart", width = 2400, height = 1600, scale = 2))
  })

  output$parallel_plot <- renderPlotly({
    d <- data_state()
    vars <- c("Age", "BMI", "TSH", "Free_T3", "Free_T4", "SYS_BP")
    norm <- as.data.frame(lapply(d[, vars, drop = FALSE], function(x) {
      rng <- range(x, na.rm = TRUE); if (diff(rng) == 0) return(rep(0, length(x)))
      (x - rng[1]) / diff(rng)
    }))
    norm$Outcome <- d$Cardiovascular_Disease
    plot_ly(type = "parcoords", line = list(color = as.numeric(norm$Outcome),
      colorscale = list(c(0, "#5B8CFF"), c(1, "#25D9C4"))),
      dimensions = lapply(vars, function(v) list(label = v, values = norm[[v]]))) %>%
      layout(paper_bgcolor = "rgba(0,0,0,0)", font = list(color = "#DCE7FB"))
  })

  output$radar_plot <- renderPlotly({
    d <- data_state()
    vars <- c("Age", "BMI", "TSH", "Free_T3", "Free_T4", "SYS_BP")
    means <- aggregate(d[, vars], list(Status = d$Thyroid_Status), mean, na.rm = TRUE)
    scale01 <- function(x) { r <- range(x, na.rm = TRUE); if (diff(r) == 0) return(rep(.5, length(x))); (x-r[1])/diff(r) }
    vals <- as.data.frame(lapply(means[, vars, drop = FALSE], scale01))
    plot <- plot_ly(type = "scatterpolar", mode = "lines+markers")
    cols <- c("#25D9C4", "#A879FF", "#FFB86B")
    for (i in seq_len(nrow(vals))) {
      rr <- as.numeric(vals[i, ])
      plot <- add_trace(plot, r = c(rr, rr[1]), theta = c(vars, vars[1]),
                        name = means$Status[i], fill = "toself",
                        line = list(color = cols[i]), opacity = .7)
    }
    plot %>% layout(polar = list(bgcolor = "rgba(0,0,0,0)",
      radialaxis = list(visible = TRUE, range = c(0, 1), gridcolor = "#263957"),
      angularaxis = list(gridcolor = "#263957")),
      paper_bgcolor = "rgba(0,0,0,0)", font = list(color = "#DCE7FB"),
      legend = list(orientation = "h", y = -0.15))
  })

  output$pairs_plot <- renderPlotly({
    d <- data_state()
    vars <- c("Age", "BMI", "TSH", "Free_T4", "SYS_BP")
    plot_ly(d, type = "splom", dimensions = lapply(vars, function(v) list(label = v, values = d[[v]])),
            showupperhalf = FALSE, diagonal = list(visible = TRUE),
            marker = list(color = ifelse(d$Cardiovascular_Disease == "Yes", "#25D9C4", "#5B8CFF"),
                          size = 4, opacity = .55)) %>%
      layout(paper_bgcolor = "rgba(0,0,0,0)", plot_bgcolor = "rgba(0,0,0,0)",
             font = list(color = "#DCE7FB"), margin = list(l = 35, r = 20, t = 20, b = 35))
  })

  output$group_summary <- renderDT({
    d <- data_state()
    out <- do.call(rbind, lapply(split(d, d$Thyroid_Status), function(g) {
      data.frame(Thyroid_Status = as.character(g$Thyroid_Status[1]),
        Records = nrow(g),
        Mean_Age = round(mean(g$Age, na.rm = TRUE), 2),
        Mean_BMI = round(mean(g$BMI, na.rm = TRUE), 2),
        Mean_TSH = round(mean(g$TSH, na.rm = TRUE), 2),
        Mean_Free_T4 = round(mean(g$Free_T4, na.rm = TRUE), 2),
        Mean_SYS_BP = round(mean(g$SYS_BP, na.rm = TRUE), 2))
    }))
    datatable(out, rownames = FALSE, options = list(pageLength = 5, scrollX = TRUE), class = "compact stripe")
  })

  output$data_table <- renderDT({
    d <- filtered_data()
    datatable(d, rownames = FALSE, filter = "top",
      options = list(pageLength = 12, scrollX = TRUE, autoWidth = TRUE,
                     dom = "Bfrtip", buttons = c("copy", "csv", "excel")),
      extensions = "Buttons", class = "compact stripe hover")
  })

  output$descriptive_table <- renderDT({
    d <- data_state()
    result <- do.call(rbind, lapply(numeric_vars, function(v) {
      x <- d[[v]]
      data.frame(Variable = pretty_names[[v]], N = sum(!is.na(x)),
        Missing = sum(is.na(x)), Mean = round(mean(x, na.rm = TRUE), 3),
        Median = round(median(x, na.rm = TRUE), 3), SD = round(sd(x, na.rm = TRUE), 3),
        Min = round(min(x, na.rm = TRUE), 3), Q1 = round(quantile(x, .25, na.rm = TRUE), 3),
        Q3 = round(quantile(x, .75, na.rm = TRUE), 3), Max = round(max(x, na.rm = TRUE), 3))
    }))
    datatable(result, rownames = FALSE, options = list(pageLength = 10, scrollX = TRUE), class = "compact stripe")
  })

  output$missing_plot <- renderPlotly({
    d <- data_state()
    miss <- data.frame(Variable = names(d), Missing = sapply(d, function(x) sum(is.na(x))))
    miss$Complete <- nrow(d) - miss$Missing
    p <- ggplot(miss, aes(reorder(Variable, Missing), Missing,
                          text = paste(Variable, "<br>Missing:", Missing,
                                       "<br>Complete:", Complete))) +
      geom_col(fill = "#A879FF", alpha = .9) + coord_flip() +
      labs(title = "Missing observations by variable", x = NULL, y = "Missing count") + theme_dsp()
    ggplotly(p, tooltip = "text") %>% config(displaylogo = FALSE, toImageButtonOptions = list(format = "png", filename = "dsp_chart", width = 2400, height = 1600, scale = 2))
  })

  output$outcome_boxplot <- renderPlotly({
    d <- data_state()
    long <- do.call(rbind, lapply(numeric_vars, function(v) {
      data.frame(Variable = pretty_names[[v]], Value = d[[v]],
                 Outcome = d$Cardiovascular_Disease)
    }))
    p <- ggplot(long, aes(Outcome, Value, fill = Outcome)) +
      geom_boxplot(alpha = .75, outlier.alpha = .18) + facet_wrap(~Variable, scales = "free", ncol = 2) +
      scale_fill_manual(values = c("No" = "#5B8CFF", "Yes" = "#25D9C4")) +
      labs(title = "Numeric measures by outcome", x = "Outcome", y = "Value") +
      theme_dsp() + theme(legend.position = "none", strip.text = element_text(size = 9))
    ggplotly(p, tooltip = "all") %>% config(displaylogo = FALSE, toImageButtonOptions = list(format = "png", filename = "dsp_chart", width = 2400, height = 1600, scale = 2))
  })

  output$outcome_ecdf <- renderPlotly({
    d <- data_state()
    p <- ggplot(d, aes(Age, colour = Cardiovascular_Disease)) +
      stat_ecdf(linewidth = 1.1) +
      scale_colour_manual(values = c("No" = "#5B8CFF", "Yes" = "#25D9C4")) +
      labs(title = "Age ECDF by outcome", x = "Age (years)", y = "Cumulative proportion", colour = "Outcome") + theme_dsp()
    ggplotly(p, tooltip = "all") %>% config(displaylogo = FALSE, toImageButtonOptions = list(format = "png", filename = "dsp_chart", width = 2400, height = 1600, scale = 2))
  })

  # ---------------- ADVANCED PREPROCESSING ----------------
  cleaned_state <- reactiveVal(sample_data)
  preprocessing_log <- reactiveVal("No preprocessing applied yet.")
  ml_state <- reactiveVal(NULL)

  quality_metrics <- function(d) {
    num <- d[vapply(d, is.numeric, logical(1))]
    outlier_count <- 0L
    if (nrow(d) > 0 && length(num)) {
      for (v in names(num)) {
        x <- num[[v]]; x <- x[is.finite(x)]
        if (length(x) < 4) next
        if (!is.null(input$outlier_method) && input$outlier_method == "zscore") {
          z <- abs((x - mean(x)) / stats::sd(x))
          outlier_count <- outlier_count + sum(z > if (is.null(input$outlier_threshold)) 3 else input$outlier_threshold, na.rm = TRUE)
        } else if (is.null(input$outlier_method) || input$outlier_method == "iqr") {
          q <- stats::quantile(x, c(.25, .75), na.rm = TRUE, names = FALSE)
          lim <- if (is.null(input$outlier_threshold)) 1.5 else input$outlier_threshold
          outlier_count <- outlier_count + sum(x < q[1] - lim * (q[2]-q[1]) | x > q[2] + lim * (q[2]-q[1]), na.rm = TRUE)
        }
      }
    }
    data.frame(Metric = c("Rows", "Columns", "Missing cells", "Duplicate rows", "Flagged numeric outlier values", "Completeness (%)"),
      Value = c(nrow(d), ncol(d), sum(is.na(d)), sum(duplicated(d)), outlier_count,
        if (length(d) && nrow(d) * ncol(d) > 0) round(100 * (1 - sum(is.na(d))/(nrow(d)*ncol(d))), 2) else 100),
      check.names = FALSE)
  }

  observeEvent(input$apply_preprocessing, {
    d <- data_state(); before <- quality_metrics(d)
    log_lines <- character()
    original_rows <- nrow(d)
    if (isTRUE(input$remove_duplicates)) {
      dup <- duplicated(d); d <- d[!dup, , drop = FALSE]
      log_lines <- c(log_lines, paste("Removed", sum(dup), "duplicate rows."))
    }
    if (identical(input$missing_method, "drop")) {
      old <- nrow(d); d <- d[stats::complete.cases(d), , drop = FALSE]
      log_lines <- c(log_lines, paste("Removed", old - nrow(d), "rows containing missing values."))
    } else if (identical(input$missing_method, "impute")) {
      filled <- 0L
      for (v in names(d)) {
        if (!anyNA(d[[v]])) next
        if (is.numeric(d[[v]])) {
          med <- suppressWarnings(stats::median(d[[v]], na.rm = TRUE))
          if (is.finite(med)) { filled <- filled + sum(is.na(d[[v]])); d[[v]][is.na(d[[v]])] <- med }
        } else {
          vals <- d[[v]][!is.na(d[[v]]) & nzchar(as.character(d[[v]]))]
          if (length(vals)) {
            mode_val <- names(sort(table(vals), decreasing = TRUE))[1]
            miss <- is.na(d[[v]]) | !nzchar(as.character(d[[v]]))
            filled <- filled + sum(miss); d[[v]][miss] <- mode_val
          }
        }
      }
      log_lines <- c(log_lines, paste("Imputed", filled, "missing values using numeric medians and categorical modes where possible."))
    } else log_lines <- c(log_lines, "Missing values were retained.")

    flagged_rows <- rep(FALSE, nrow(d)); outlier_cells <- 0L
    numeric_names <- names(d)[vapply(d, is.numeric, logical(1))]
    if (!identical(input$outlier_method, "none")) for (v in numeric_names) {
      x <- d[[v]]; valid <- is.finite(x)
      if (sum(valid) < 4) next
      if (identical(input$outlier_method, "zscore")) {
        sdv <- stats::sd(x[valid]); if (!is.finite(sdv) || sdv == 0) next
        idx <- valid & abs((x - mean(x[valid])) / sdv) > input$outlier_threshold
        lo <- mean(x[valid]) - input$outlier_threshold * sdv; hi <- mean(x[valid]) + input$outlier_threshold * sdv
      } else {
        qs <- stats::quantile(x[valid], c(.25, .75), na.rm = TRUE, names = FALSE); spread <- qs[2]-qs[1]
        lo <- qs[1] - input$outlier_threshold*spread; hi <- qs[2] + input$outlier_threshold*spread
        idx <- valid & (x < lo | x > hi)
      }
      outlier_cells <- outlier_cells + sum(idx, na.rm = TRUE); flagged_rows <- flagged_rows | idx
      if (identical(input$outlier_action, "winsor")) {
        d[[v]][valid & x < lo] <- lo
        d[[v]][valid & x > hi] <- hi
      }
    }
    if (identical(input$outlier_action, "remove") && any(flagged_rows)) {
      d <- d[!flagged_rows, , drop = FALSE]
      log_lines <- c(log_lines, paste("Removed", sum(flagged_rows), "rows containing at least one flagged numeric outlier."))
    } else if (identical(input$outlier_action, "winsor")) {
      # Bounds are applied variable-by-variable above.
      log_lines <- c(log_lines, paste("Winsorization requested; flagged numeric values were clamped to the selected rule boundaries (", outlier_cells, " flagged cells)."))
    } else log_lines <- c(log_lines, paste("Flagged", outlier_cells, "numeric outlier values; values were not altered."))

    if (identical(input$scale_method, "minmax") || identical(input$scale_method, "standard")) {
      for (v in numeric_names) {
        x <- d[[v]]; if (!any(is.finite(x))) next
        if (identical(input$scale_method, "minmax")) {
          rng <- range(x, na.rm = TRUE); if (diff(rng) > 0) d[[v]] <- (x-rng[1])/diff(rng)
        } else {
          mu <- mean(x, na.rm = TRUE); sig <- stats::sd(x, na.rm = TRUE)
          if (is.finite(sig) && sig > 0) d[[v]] <- (x-mu)/sig
        }
      }
      log_lines <- c(log_lines, if (identical(input$scale_method, "minmax")) "Applied min-max normalization to numeric columns." else "Applied z-score standardization to numeric columns.")
    } else log_lines <- c(log_lines, "No numeric scaling applied.")
    cleaned_state(d)
    preprocessing_log(paste(c(paste("Source rows:", original_rows), log_lines, paste("Final rows:", nrow(d))), collapse = "\n"))
    showNotification("Preprocessing completed. The cleaned analysis data is ready.", type = "message")
  })

  observeEvent(input$reset_preprocessing, {
    cleaned_state(data_state()); preprocessing_log("Analysis data reset to the active source dataset; no preprocessing changes applied.")
  })

  output$quality_summary <- renderDT({
    before <- quality_metrics(data_state()); after <- quality_metrics(cleaned_state())
    out <- merge(before, after, by = "Metric", all = TRUE, suffixes = c("_before", "_after"))
    names(out) <- c("Quality metric", "Before", "After")
    datatable(out, rownames = FALSE, options = list(dom = "t", pageLength = 10))
  })
  output$preprocess_log <- renderText(preprocessing_log())
  output$cleaned_preview <- renderDT(datatable(cleaned_state(), options = list(pageLength = 8, scrollX = TRUE), rownames = FALSE))
  output$download_cleaned <- downloadHandler(filename = function() paste0("dsp_cleaned_", Sys.Date(), ".csv"), content = function(file) write.csv(cleaned_state(), file, row.names = FALSE, na = ""))

  # ---------------- STATISTICAL TESTING ----------------
  correlation_result <- reactive({
    d <- cleaned_state(); x <- suppressWarnings(as.numeric(d[[input$corr_x]])); y <- suppressWarnings(as.numeric(d[[input$corr_y]]))
    ok <- is.finite(x) & is.finite(y); x <- x[ok]; y <- y[ok]
    validate(need(length(x) >= 3, "At least three complete numeric pairs are required."))
    validate(need(length(unique(x)) > 1 && length(unique(y)) > 1, "Both variables must vary."))
    stats::cor.test(x, y, method = input$corr_method, exact = FALSE)
  })
  output$correlation_test_result <- renderPrint({
    tryCatch({ ct <- correlation_result(); cat("Method:", ct$method, "\nCoefficient:", round(unname(ct$estimate), 4), "\np-value:", format.pval(ct$p.value, digits = 4), "\n")
      if (!is.null(ct$conf.int)) cat("95% confidence interval:", paste(round(ct$conf.int, 4), collapse = " to "), "\n") else cat("95% confidence interval: not returned for this test/method.\n")
      cat("Complete pairs:", sum(complete.cases(cleaned_state()[, c(input$corr_x, input$corr_y)])), "\n")
    }, error = function(e) cat("Unable to calculate correlation:", conditionMessage(e)))
  })
  group_test_object <- reactive({
    d <- cleaned_state(); x <- suppressWarnings(as.numeric(d[[input$test_numeric]])); g <- as.factor(d[[input$test_group]])
    ok <- is.finite(x) & !is.na(g); x <- x[ok]; g <- droplevels(g[ok]); k <- nlevels(g)
    validate(need(k >= 2, "At least two groups with numeric observations are required."))
    chosen <- input$group_test
    if (chosen == "auto") chosen <- if (k == 2) "ttest" else "kruskal"
    if (chosen %in% c("ttest", "wilcox") && k != 2) stop("This test requires exactly two observed groups.")
    if (chosen %in% c("anova", "kruskal") && k < 2) stop("This test requires at least two observed groups.")
    if (chosen == "ttest") { test <- stats::t.test(x ~ g); label <- "Welch independent t-test" }
    else if (chosen == "wilcox") { test <- stats::wilcox.test(x ~ g, exact = FALSE, conf.int = TRUE); label <- "Mann–Whitney / Wilcoxon rank-sum" }
    else if (chosen == "anova") { test <- stats::oneway.test(x ~ g, var.equal = TRUE); label <- "One-way ANOVA" }
    else { test <- stats::kruskal.test(x ~ g); label <- "Kruskal–Wallis" }
    list(test = test, label = label, groups = k, n = length(x), means = tapply(x, g, mean), medians = tapply(x, g, median))
  })
  output$group_test_result <- renderPrint({
    tryCatch({ z <- group_test_object(); print(z$test); cat("\nSelected test:", z$label, "\nGroups:", z$groups, "\nN:", z$n, "\nGroup means:\n"); print(round(z$means, 3)); cat("Group medians:\n"); print(round(z$medians, 3)) }, error = function(e) cat("Unable to run group comparison:", conditionMessage(e)))
  })
  output$chi_square_result <- renderPrint({
    d <- cleaned_state(); a <- as.factor(d[[input$chi_x]]); b <- as.factor(d[[input$chi_y]])
    ok <- !is.na(a) & !is.na(b); tab <- table(a[ok], b[ok])
    if (nrow(tab) < 2 || ncol(tab) < 2) { cat("Chi-square requires at least two categories in both variables."); return() }
    cat("Contingency table:\n"); print(tab)
    test <- tryCatch(suppressWarnings(chisq.test(tab, correct = FALSE)), error = function(e) e)
    if (inherits(test, "error")) cat("\nTest error:", conditionMessage(test)) else {
      cat("\nPearson chi-square:", round(unname(test$statistic), 4), "\nDegrees of freedom:", unname(test$parameter), "\np-value:", format.pval(test$p.value, digits = 4), "\n")
      if (any(test$expected < 5)) cat("Caution: some expected cell counts are below 5; consider an exact test or combining sparse categories.\n")
    }
  })
  output$test_interpretation <- renderUI({
    tryCatch({ ct <- correlation_result(); gz <- group_test_object(); p1 <- ct$p.value; p2 <- gz$test$p.value
      div(class = "small-note", tags$ul(
        tags$li(paste0(input$corr_method, " correlation between ", input$corr_x, " and ", input$corr_y, " is ", ifelse(abs(unname(ct$estimate)) >= .7, "strong", ifelse(abs(unname(ct$estimate)) >= .3, "moderate", "weak")), " (r/rho = ", round(unname(ct$estimate), 3), "). ", ifelse(p1 < .05, "The association is statistically significant at alpha = 0.05.", "The association is not statistically significant at alpha = 0.05."))),
        tags$li(paste0(gz$label, " for ", input$test_numeric, " across ", input$test_group, ": p = ", format.pval(p2, digits = 3), ". ", ifelse(p2 < .05, "At least one group difference is supported by this test.", "The test did not detect a statistically significant group difference."))),
        tags$li("These are associations/comparisons, not evidence of causation. Check assumptions and data quality before drawing conclusions.")))
    }, error = function(e) div(class = "small-note", paste("Interpretation unavailable:", conditionMessage(e))))
  })
  output$formula_reference <- renderUI({
    div(class = "small-note", tags$ul(
      tags$li(HTML("Pearson correlation: <b>r = cov(X,Y) / (s<sub>X</sub>s<sub>Y</sub>)</b>")),
      tags$li(HTML("Spearman correlation: Pearson correlation of the ranked values, <b>r<sub>s</sub> = cor(rank(X), rank(Y))</b>")),
      tags$li(HTML("Welch t-test: <b>t = (x&#772;<sub>1</sub> − x&#772;<sub>2</sub>) / √(s<sub>1</sub><sup>2</sup>/n<sub>1</sub> + s<sub>2</sub><sup>2</sup>/n<sub>2</sub>)</b>")),
      tags$li(HTML("Chi-square: <b>χ² = Σ (O − E)² / E</b>, where E = (row total × column total) / grand total")),
      tags$li(HTML("Z-score: <b>z = (x − μ) / σ</b>; min-max: <b>x' = (x − min(x)) / (max(x) − min(x))</b>")),
      tags$li(HTML("IQR outlier fences: <b>[Q1 − k·IQR, Q3 + k·IQR]</b>, where IQR = Q3 − Q1"))))
  })

  # ---------------- MACHINE LEARNING ----------------
  observeEvent(input$run_ml, {
    tryCatch({
      d <- cleaned_state(); target <- input$ml_target; features <- unique(input$ml_features)
      features <- setdiff(features, target)
      validate(need(length(features) >= 1, "Select at least one predictor."))
      validate(need(target %in% names(d), "Target is not present in the dataset."))
      d[[target]] <- as.factor(d[[target]])
      d <- d[, c(target, intersect(features, names(d))), drop = FALSE]
      d <- d[complete.cases(d), , drop = FALSE]
      d <- droplevels(d)
      validate(need(nrow(d) >= 30, "At least 30 complete records are recommended for the demonstration model."))
      validate(need(nlevels(d[[target]]) == 2, "Choose a target with exactly two observed classes."))
      set.seed(input$ml_seed)
      idx <- unlist(lapply(split(seq_len(nrow(d)), d[[target]]), function(ii) sample(ii, max(1, floor(length(ii)*input$ml_split/100)))))
      idx <- sort(unique(idx)); train <- d[idx, , drop = FALSE]; test <- d[-idx, , drop = FALSE]
      validate(need(nrow(test) > 0 && all(table(train[[target]]) > 0), "Train/test split left an empty class or test set; adjust split or dataset."))
      form <- stats::reformulate(setdiff(names(d), target), response = target)
      model_type <- input$ml_model
      if (model_type == "glm") {
        model <- stats::glm(form, data = train, family = stats::binomial())
        probs <- as.numeric(stats::predict(model, newdata = test, type = "response")); positive <- levels(train[[target]])[2]
        pred <- factor(ifelse(probs >= .5, positive, levels(train[[target]])[1]), levels = levels(train[[target]]))
        imp <- sort(abs(stats::coef(model)[-1]), decreasing = TRUE); imp <- imp[is.finite(imp)]
        importance <- data.frame(Feature = names(imp), Importance = as.numeric(imp))
      } else if (model_type == "tree") {
        if (!requireNamespace("rpart", quietly = TRUE)) stop("Install rpart first: install.packages('rpart')")
        model <- rpart::rpart(form, data = train, method = "class", control = rpart::rpart.control(cp = .01))
        pr <- predict(model, newdata = test, type = "prob"); positive <- levels(train[[target]])[2]
        probs <- as.numeric(pr[, positive]); pred <- predict(model, newdata = test, type = "class")
        vi <- model$variable.importance; importance <- if (length(vi)) data.frame(Feature = names(vi), Importance = as.numeric(vi)) else data.frame(Feature = character(), Importance = numeric())
      } else {
        if (!requireNamespace("randomForest", quietly = TRUE)) stop("Install randomForest first: install.packages('randomForest')")
        model <- randomForest::randomForest(form, data = train, ntree = 300, importance = TRUE)
        pr <- predict(model, newdata = test, type = "prob"); positive <- levels(train[[target]])[2]
        probs <- as.numeric(pr[, positive]); pred <- predict(model, newdata = test, type = "class")
        vi <- randomForest::importance(model); importance <- data.frame(Feature = rownames(vi), Importance = if ("MeanDecreaseGini" %in% colnames(vi)) vi[, "MeanDecreaseGini"] else vi[, 1])
      }
      actual <- factor(test[[target]], levels = levels(train[[target]])); pred <- factor(pred, levels = levels(train[[target]])); cm <- table(Actual = actual, Predicted = pred)
      tp <- sum(actual == positive & pred == positive); fp <- sum(actual != positive & pred == positive); fn <- sum(actual == positive & pred != positive); tn <- sum(actual != positive & pred != positive)
      precision <- ifelse(tp+fp == 0, NA, tp/(tp+fp)); recall <- ifelse(tp+fn == 0, NA, tp/(tp+fn)); f1 <- ifelse(is.na(precision) || is.na(recall) || precision+recall == 0, NA, 2*precision*recall/(precision+recall))
      auc <- NA_real_
      ord <- order(probs); ybin <- as.numeric(actual[ord] == positive)
      if (length(unique(ybin)) == 2) { ranks <- rank(probs, ties.method = "average"); auc <- (sum(ranks[actual == positive]) - sum(actual == positive)*(sum(actual == positive)+1)/2)/(sum(actual == positive)*sum(actual != positive)) }
      # 5-fold cross-validation on the training partition; fold metrics are descriptive.
      cv_scores <- numeric(0)
      try({
        set.seed(input$ml_seed + 1)
        folds <- rep(1:5, length.out = nrow(train))
        folds <- sample(folds)
        for (fold in 1:5) {
          tr <- train[folds != fold, , drop = FALSE]; va <- train[folds == fold, , drop = FALSE]
          if (nrow(va) == 0 || nlevels(droplevels(tr[[target]])) < 2) next
          cv_model <- if (model_type == "glm") stats::glm(form, data = tr, family = stats::binomial()) else if (model_type == "tree" && requireNamespace("rpart", quietly = TRUE)) rpart::rpart(form, data = tr, method = "class") else if (model_type == "rf" && requireNamespace("randomForest", quietly = TRUE)) randomForest::randomForest(form, data = tr, ntree = 150) else NULL
          if (is.null(cv_model)) next
          cv_pred <- tryCatch(predict(cv_model, newdata = va, type = "class"), error = function(e) NULL)
          if (!is.null(cv_pred)) cv_scores <- c(cv_scores, mean(as.character(cv_pred) == as.character(va[[target]])))
        }
      }, silent = TRUE)
      cv_summary <- if (length(cv_scores)) paste0("5-fold CV accuracy: ", round(mean(cv_scores), 4), " (SD ", round(stats::sd(cv_scores), 4), "); folds completed: ", length(cv_scores), "/5. CV was performed on the training partition.") else "Cross-validation could not be completed for this dataset/model; use the held-out test metrics cautiously."
      ml_state(list(model = model, model_type = model_type, target = target, features = features, train_n = nrow(train), test_n = nrow(test), actual = actual, predicted = pred, probs = probs, positive = positive, cm = cm, accuracy = mean(actual == pred), precision = precision, recall = recall, f1 = f1, auc = auc, importance = importance, cv = cv_summary))
      showNotification("Model trained and evaluated on the held-out test set.", type = "message")
    }, error = function(e) { ml_state(list(error = conditionMessage(e))); showNotification(conditionMessage(e), type = "error", duration = 8) })
  })
  output$ml_metrics <- renderText({
    z <- ml_state(); if (is.null(z)) return("Choose a target, predictors and model, then click Train / evaluate model.")
    if (!is.null(z$error)) return(paste("Model error:", z$error))
    paste0("Model: ", z$model_type, "\nTarget: ", z$target, "\nPredictors: ", paste(z$features, collapse = ", "), "\nTrain rows: ", z$train_n, "\nTest rows: ", z$test_n, "\nAccuracy: ", round(z$accuracy, 4), "\nPrecision: ", round(z$precision, 4), "\nRecall: ", round(z$recall, 4), "\nF1-score: ", round(z$f1, 4), "\nROC-AUC (rank-based): ", round(z$auc, 4), "\n\nConfusion matrix:\n", paste(capture.output(print(z$cm)), collapse = "\n"), "\n\nMetrics depend on the chosen threshold (0.5) and random split; they are not clinical performance claims.")
  })
  output$ml_confusion_plot <- renderPlotly({
    z <- ml_state(); validate(need(!is.null(z) && is.null(z$error), "Train a model to view this chart."))
    dd <- as.data.frame(z$cm); p <- ggplot(dd, aes(Predicted, Actual, fill = Freq, text = paste("Actual:", Actual, "<br>Predicted:", Predicted, "<br>Count:", Freq))) + geom_tile(colour = "#263957") + geom_text(aes(label = Freq), colour = "white", size = 6) + scale_fill_gradient(low = "#152943", high = "#25D9C4") + labs(title = "Confusion matrix", x = "Predicted", y = "Actual") + theme_dsp()
    ggplotly(p, tooltip = "text") %>% config(displaylogo = FALSE, toImageButtonOptions = list(format = "png", filename = "dsp_chart", width = 2400, height = 1600, scale = 2))
  })
  output$ml_importance_plot <- renderPlotly({
    z <- ml_state(); validate(need(!is.null(z) && is.null(z$error), "Train a model to view feature importance."))
    dd <- z$importance; validate(need(nrow(dd) > 0, "This model did not provide feature importance.")); dd <- dd[order(dd$Importance, decreasing = TRUE), , drop = FALSE]; dd <- head(dd, 15)
    p <- ggplot(dd, aes(reorder(Feature, Importance), Importance, text = paste(Feature, "<br>Importance:", signif(Importance, 4)))) + geom_col(fill = "#25D9C4") + coord_flip() + labs(title = "Feature importance", x = NULL, y = "Importance") + theme_dsp()
    ggplotly(p, tooltip = "text") %>% config(displaylogo = FALSE, toImageButtonOptions = list(format = "png", filename = "dsp_chart", width = 2400, height = 1600, scale = 2))
  })
  output$ml_roc_plot <- renderPlotly({
    z <- ml_state(); validate(need(!is.null(z) && is.null(z$error), "Train a model to view ROC curve."))
    y <- as.numeric(z$actual == z$positive); validate(need(length(unique(y)) == 2, "Both classes must occur in test data."))
    thresholds <- sort(unique(c(Inf, z$probs, -Inf)), decreasing = TRUE)
    roc <- do.call(rbind, lapply(thresholds, function(th) { pr <- z$probs >= th; data.frame(FPR = sum(pr & y == 0)/sum(y == 0), TPR = sum(pr & y == 1)/sum(y == 1)) }))
    p <- ggplot(roc, aes(FPR, TPR)) + geom_line(colour = "#25D9C4", linewidth = 1.2) + geom_abline(slope = 1, intercept = 0, linetype = "dashed", colour = "#7183A2") + coord_equal() + labs(title = paste("ROC curve · AUC", round(z$auc, 3)), x = "False positive rate", y = "True positive rate") + theme_dsp()
    ggplotly(p, tooltip = "all") %>% config(displaylogo = FALSE, toImageButtonOptions = list(format = "png", filename = "dsp_chart", width = 2400, height = 1600, scale = 2))
  })
  output$ml_cv_result <- renderText({ z <- ml_state(); if (is.null(z)) "Run a model to generate evaluation results." else if (!is.null(z$error)) z$error else z$cv })

  # ---------------- SEPARATE CARDIOVASCULAR AND THYROID RISK ESTIMATORS ----------------
  cardio_risk_state <- reactiveVal(NULL)
  thyroid_risk_state <- reactiveVal(NULL)

  observeEvent(input$calculate_cardio_risk, {
    tryCatch({
      d <- data_state()
      needed <- c("Cardiovascular_Disease", "Age", "BMI", "SYS_BP", "HDL", "Diabetes", "Smoking")
      missing <- setdiff(needed, names(d))
      if (length(missing)) stop(paste("Dataset is missing required cardiovascular columns:", paste(missing, collapse = ", ")))
      d <- d[, needed, drop = FALSE]
      for (v in c("Age", "BMI", "SYS_BP", "HDL")) d[[v]] <- suppressWarnings(as.numeric(as.character(d[[v]])))
      cv <- trimws(tolower(as.character(d$Cardiovascular_Disease)))
      cv[cv %in% c("0", "false", "no", "negative", "absent")] <- "No"
      cv[cv %in% c("1", "true", "yes", "positive", "present")] <- "Yes"
      d$Cardiovascular_Disease <- factor(cv, levels = c("No", "Yes"))
      for (v in c("Diabetes", "Smoking")) {
        vals <- trimws(tolower(as.character(d[[v]])))
        vals[vals %in% c("0", "false", "no")] <- "No"
        vals[vals %in% c("1", "true", "yes")] <- "Yes"
        d[[v]] <- factor(vals, levels = c("No", "Yes"))
      }
      d <- d[complete.cases(d), , drop = FALSE]
      d <- droplevels(d)
      if (nrow(d) < 40) stop("At least 40 complete rows are required for the cardiovascular demonstration model.")
      if (nlevels(d$Cardiovascular_Disease) != 2) stop("The dataset must contain both No and Yes cardiovascular outcome labels.")
      if (nlevels(d$Diabetes) < 2 || nlevels(d$Smoking) < 2) stop("Diabetes and Smoking must each contain both No and Yes values in the active dataset.")
      fit <- stats::glm(Cardiovascular_Disease ~ Age + BMI + SYS_BP + HDL + Diabetes + Smoking, data = d, family = stats::binomial())
      profile <- data.frame(Age = as.numeric(input$risk_age), BMI = as.numeric(input$risk_bmi),
        SYS_BP = as.numeric(input$risk_sysbp), HDL = as.numeric(input$risk_hdl),
        Diabetes = factor(input$risk_diabetes, levels = levels(d$Diabetes)),
        Smoking = factor(input$risk_smoking, levels = levels(d$Smoking)))
      pr <- as.numeric(stats::predict(fit, newdata = profile, type = "response"))
      if (length(pr) != 1 || !is.finite(pr)) stop("The cardiovascular model could not calculate a finite probability. Check the active dataset.")
      cardio_risk_state(list(probability = min(1, max(0, pr)), n = nrow(d)))
    }, error = function(e) cardio_risk_state(list(error = conditionMessage(e))))
  })

  observeEvent(input$calculate_thyroid_risk, {
    tryCatch({
      d <- data_state()
      needed <- c("Thyroid_Status", "Age", "TSH", "Free_T3", "Free_T4", "TPO")
      missing <- setdiff(needed, names(d))
      if (length(missing)) stop(paste("Dataset is missing required thyroid columns:", paste(missing, collapse = ", ")))
      d <- d[, needed, drop = FALSE]
      for (v in c("Age", "TSH", "Free_T3", "Free_T4", "TPO")) d[[v]] <- suppressWarnings(as.numeric(as.character(d[[v]])))
      status <- trimws(tolower(as.character(d$Thyroid_Status)))
      status[status %in% c("normal", "reference_range", "reference range", "euthyroid")] <- "Normal"
      status[status %in% c("hypothyroid", "possible_hypothyroid", "possible hypothyroid")] <- "Hypothyroid"
      status[status %in% c("hyperthyroid", "possible_hyperthyroid", "possible hyperthyroid")] <- "Hyperthyroid"
      d$Thyroid_Abnormal <- ifelse(status == "Normal", 0, ifelse(status %in% c("Hypothyroid", "Hyperthyroid"), 1, NA_real_))
      d <- d[, c("Thyroid_Abnormal", "Age", "TSH", "Free_T3", "Free_T4", "TPO"), drop = FALSE]
      d <- d[complete.cases(d), , drop = FALSE]
      if (nrow(d) < 40) stop("At least 40 complete rows with known thyroid status are required for the thyroid demonstration model.")
      if (length(unique(d$Thyroid_Abnormal)) != 2) stop("The active dataset must contain both Normal and abnormal thyroid-status labels (Hypothyroid/Hyperthyroid).")
      fit <- stats::glm(Thyroid_Abnormal ~ Age + TSH + Free_T3 + Free_T4 + TPO, data = d, family = stats::binomial())
      profile <- data.frame(Age = as.numeric(input$thyroid_risk_age), TSH = as.numeric(input$thyroid_risk_tsh),
        Free_T3 = as.numeric(input$thyroid_risk_ft3), Free_T4 = as.numeric(input$thyroid_risk_ft4), TPO = as.numeric(input$thyroid_risk_tpo))
      pr <- as.numeric(stats::predict(fit, newdata = profile, type = "response"))
      if (length(pr) != 1 || !is.finite(pr)) stop("The thyroid model could not calculate a finite probability. Check the active dataset.")
      thyroid_risk_state(list(probability = min(1, max(0, pr)), n = nrow(d)))
    }, error = function(e) thyroid_risk_state(list(error = conditionMessage(e))))
  })

  risk_result_ui <- function(z, label) {
    if (is.null(z)) return(div(class = "small-note", paste("Enter the", label, "profile and click its estimate button.")))
    if (!is.null(z$error)) return(div(class = "login-error", z$error))
    p <- z$probability
    # Display bands are illustrative UI thresholds, not clinically validated cut-offs.
    risk_level <- if (p < 0.30) "Low risk" else if (p < 0.60) "Medium risk" else "High risk"
    risk_colour <- if (p < 0.30) "#22C55E" else if (p < 0.60) "#FACC15" else "#EF4444"
    div(class = paste0("risk-result-content risk-", if (p < 0.30) "low" else if (p < 0.60) "medium" else "high"),
      style = "text-align:center;padding:20px 8px;",
      div(style = "font-size:12px;color:#9EB2D2;text-transform:uppercase;letter-spacing:1.2px;", "Model-estimated probability"),
      div(class = "risk-percentage", style = paste0("font-family:'Space Grotesk',sans-serif;font-size:48px;font-weight:700;color:", risk_colour, " !important;margin:10px 0;"), paste0(round(100 * p, 1), "%")),
      div(class = "risk-level-badge", style = paste0("display:inline-block;padding:7px 18px;margin:0 0 14px;border-radius:999px;font-weight:800;font-size:14px;color:", risk_colour, " !important;background-color:", risk_colour, "26;border:2px solid ", risk_colour, ";"), risk_level),
      div(style = "max-width:540px;margin:0 auto;background:#091629;border:1px solid #263957;border-radius:999px;height:12px;overflow:hidden;",
        div(class = "risk-progress-fill", style = paste0("height:100%;width:", max(0, min(100, 100 * p)), "%;background-color:", risk_colour, " !important;transition:width .3s ease;"))),
      p(class = "small-note", paste("Fitted on", comma(z$n), "complete rows in the active dataset. Illustrative only; not a validated individual risk estimate.")),
      p(class = "small-note", "Display bands: Low < 30%; Medium 30% to < 60%; High 60% or above. These are demonstration thresholds, not clinical cut-offs."))
  }
  output$cardio_risk_result <- renderUI({ risk_result_ui(cardio_risk_state(), "cardiovascular") })
  output$thyroid_risk_result <- renderUI({ risk_result_ui(thyroid_risk_state(), "thyroid") })
  output$risk_limitations <- renderUI({
    div(class = "small-note", tags$ul(
      tags$li("Cardiovascular and thyroid estimates use separate outcome labels, predictor sets, profile inputs and fitted models."),
      tags$li("Thyroid risk means the model-estimated probability of an abnormal status label (hypothyroid or hyperthyroid) versus normal in the active dataset."),
      tags$li("Synthetic data and model outputs are for demonstrating software functionality only; they are not clinical evidence."),
      tags$li("Do not use either percentage for diagnosis, treatment, or health decisions. Real clinical models require representative data and independent validation.")))
  })

  # ---------------- AUTOMATED INSIGHTS & REPORTS ----------------
  insight_text <- reactive({
    d <- cleaned_state(); lines <- c(paste("Dataset contains", nrow(d), "rows and", ncol(d), "columns."), paste("Missing cells:", sum(is.na(d)), "| duplicate rows:", sum(duplicated(d)), "."))
    nums <- names(d)[vapply(d, is.numeric, logical(1))]
    if (length(nums) >= 2) {
      cm <- suppressWarnings(cor(d[nums], use = "pairwise.complete.obs", method = "pearson")); cm[!is.finite(cm)] <- NA
      if (sum(is.finite(cm[upper.tri(cm)])) > 0) {
        vals <- cm; vals[lower.tri(vals, diag = TRUE)] <- NA; ix <- which(abs(vals) == max(abs(vals), na.rm = TRUE), arr.ind = TRUE)[1, ]
        lines <- c(lines, paste0("Strongest absolute Pearson correlation: ", nums[ix[1]], " vs ", nums[ix[2]], " (r = ", round(cm[ix[1], ix[2]], 3), "). Correlation is not causation."))
      }
      for (v in head(nums, 4)) {
        x <- d[[v]]; x <- x[is.finite(x)]; if (length(x) < 5) next
        sk <- mean((x-mean(x))^3)/(stats::sd(x)^3)
        lines <- c(lines, paste0(v, " distribution: mean ", round(mean(x), 3), ", median ", round(median(x), 3), ", skewness ", round(sk, 3), if (abs(sk) > 1) " (strong skew; inspect a density plot)." else " (no extreme skew by this simple heuristic)."))
      }
    }
    if ("Cardiovascular_Disease" %in% names(d) && "Age" %in% names(d)) {
      means <- tapply(d$Age, d$Cardiovascular_Disease, mean, na.rm = TRUE)
      if (length(means) >= 2) lines <- c(lines, paste0("Mean age by cardiovascular outcome: ", paste(names(means), round(means, 2), collapse = "; "), ". This is descriptive, not causal."))
    }
    lines <- c(lines, paste("Preprocessing log:", gsub("\n", " | ", preprocessing_log())))
    lines
  })
  output$auto_insights <- renderUI({ div(class = "small-note", tags$ul(lapply(insight_text(), tags$li))) })
  output$report_snapshot <- renderDT({ d <- cleaned_state(); desc <- data.frame(Item = c("Data source", "Rows", "Columns", "Missing cells", "Duplicate rows", "Numeric columns", "Categorical columns"), Value = c(source_state(), nrow(d), ncol(d), sum(is.na(d)), sum(duplicated(d)), sum(vapply(d, is.numeric, logical(1))), sum(!vapply(d, is.numeric, logical(1))))); datatable(desc, rownames = FALSE, options = list(dom = "t")) })
  output$all_formulas <- renderUI({
    div(class = "small-note", tags$ul(
      tags$li(HTML("Mean: <b>x&#772; = Σxᵢ / n</b>; variance: <b>s² = Σ(xᵢ − x&#772;)² / (n − 1)</b>")),
      tags$li(HTML("IQR outlier fences: <b>Q1 − 1.5×IQR</b> and <b>Q3 + 1.5×IQR</b>; IQR = Q3 − Q1")),
      tags$li(HTML("Z-score: <b>z = (x − μ) / σ</b>; min-max scaling: <b>(x − min) / (max − min)</b>")),
      tags$li(HTML("Pearson correlation: <b>r = Σ[(xᵢ − x&#772;)(yᵢ − ȳ)] / √[Σ(xᵢ − x&#772;)² Σ(yᵢ − ȳ)²]</b>")),
      tags$li(HTML("Chi-square independence: <b>χ² = Σ (Oᵢⱼ − Eᵢⱼ)² / Eᵢⱼ</b>; <b>Eᵢⱼ = row total × column total / grand total</b>")),
      tags$li(HTML("Logistic regression: <b>p = 1 / (1 + e<sup>−(β₀ + β₁x₁ + … + βₖxₖ)</sup>)</b>")),
      tags$li(HTML("Accuracy = (TP + TN)/(TP + TN + FP + FN); precision = TP/(TP+FP); recall = TP/(TP+FN); F1 = 2PR/(P+R)")),
      tags$li(HTML("ROC-AUC estimates ranking discrimination across thresholds; it does not establish calibration or clinical utility."))))
  })
  output$download_insights <- downloadHandler(filename = function() paste0("dsp_insights_", Sys.Date(), ".txt"), content = function(file) writeLines(c("DSP ANALYTICS REPORT - INSIGHTS", paste("Generated:", Sys.time()), "", insight_text(), "", "Limitations: Synthetic data is illustrative; associations are not causal; model metrics depend on the split and selected features."), file))
  output$download_cleaned_report <- downloadHandler(filename = function() paste0("dsp_cleaned_", Sys.Date(), ".csv"), content = function(file) write.csv(cleaned_state(), file, row.names = FALSE, na = ""))
  output$download_pdf_report <- downloadHandler(filename = function() paste0("dsp_analytics_report_", Sys.Date(), ".pdf"), content = function(file) {
    grDevices::pdf(file, width = 10, height = 8.2, onefile = TRUE)
    on.exit(grDevices::dev.off(), add = TRUE)
    write_page <- function(title, lines) { plot.new(); par(mar = c(0,0,0,0)); text(.04, .95, title, adj = c(0,1), cex = 1.5, font = 2, col = "#12345A"); text(.04, .87, paste(lines, collapse = "\n"), adj = c(0,1), cex = .83, family = "mono") }
    d <- cleaned_state()
    write_page("DSP Analytics | Project Report", c(paste("Generated:", Sys.time()), paste("Source:", source_state()), paste("Rows:", nrow(d), "Columns:", ncol(d)), paste("Missing cells:", sum(is.na(d))), paste("Duplicate rows:", sum(duplicated(d))), "", "Dataset and preprocessing overview", strwrap(preprocessing_log(), 100), "", "Limitations", strwrap("Built-in records are synthetic demonstration data. Findings do not establish causation or clinical validity. Missingness and preprocessing choices can affect results. Model metrics are from a single holdout split and are not deployment evidence.", 100)))
    nums <- names(d)[vapply(d, is.numeric, logical(1))]
    if (length(nums)) {
      desc <- do.call(rbind, lapply(nums, function(v) { x <- d[[v]]; data.frame(Variable = v, N = sum(is.finite(x)), Mean = mean(x, na.rm=TRUE), Median = median(x, na.rm=TRUE), SD = sd(x, na.rm=TRUE), Min = min(x, na.rm=TRUE), Max = max(x, na.rm=TRUE)) }))
      write_page("Descriptive statistics", capture.output(print(desc, row.names = FALSE, digits = 3)))
    }
    write_page("Automated insights", strwrap(insight_text(), 105))
    stat_lines <- c("Pearson correlation: cor.test(x, y, method='pearson')", "Spearman correlation: cor.test(x, y, method='spearman')", "Chi-square: chisq.test(table(x, y))", "Two-group comparison: Welch t-test or Wilcoxon rank-sum", "Multi-group comparison: one-way ANOVA or Kruskal-Wallis", "", "Selected correlation test:")
    stat_lines <- c(stat_lines, tryCatch(capture.output(print(correlation_result())), error = function(e) paste("Unavailable:", conditionMessage(e))), "", "Selected group comparison:")
    stat_lines <- c(stat_lines, tryCatch(capture.output(print(group_test_object()$test)), error = function(e) paste("Unavailable:", conditionMessage(e))))
    write_page("Statistical testing and results", stat_lines)
    # Include static chart pages in the PDF report as well as the interactive dashboard charts.
    if ("Age" %in% names(d) && is.numeric(d$Age)) {
      print(ggplot(d, aes(Age)) + geom_histogram(bins = 25, fill = "#25D9C4", colour = "#12345A") + labs(title = "Age distribution", x = "Age", y = "Count") + theme_minimal(base_size = 12))
    }
    if (all(c("BMI", "Cardiovascular_Disease") %in% names(d))) {
      print(ggplot(d, aes(Cardiovascular_Disease, BMI, fill = Cardiovascular_Disease)) + geom_boxplot(alpha = .8) + labs(title = "BMI by cardiovascular outcome", x = "Outcome", y = "BMI") + theme_minimal(base_size = 12) + theme(legend.position = "none"))
    }
    if (length(nums) >= 2) {
      cm <- suppressWarnings(cor(d[nums], use = "pairwise.complete.obs")); cm[!is.finite(cm)] <- 0
      cm_df <- as.data.frame(as.table(cm)); names(cm_df) <- c("Variable1", "Variable2", "Correlation")
      print(ggplot(cm_df, aes(Variable1, Variable2, fill = Correlation)) + geom_tile() + scale_fill_gradient2(low = "#5B8CFF", mid = "white", high = "#25D9C4", midpoint = 0) + labs(title = "Numeric correlation heatmap", x = NULL, y = NULL) + theme_minimal(base_size = 9) + theme(axis.text.x = element_text(angle = 45, hjust = 1)))
    }
    z <- ml_state()
    if (!is.null(z) && is.null(z$error)) write_page("Machine-learning evaluation", c(paste("Model:", z$model_type), paste("Target:", z$target), paste("Predictors:", paste(z$features, collapse = ", ")), paste("Train/test rows:", z$train_n, "/", z$test_n), paste("Accuracy:", round(z$accuracy, 4)), paste("Precision:", round(z$precision, 4)), paste("Recall:", round(z$recall, 4)), paste("F1:", round(z$f1, 4)), paste("ROC-AUC:", round(z$auc, 4)), "", "Confusion matrix:", capture.output(print(z$cm)), "", "Model results are exploratory and depend on preprocessing, predictors, threshold and random split."))
  })

  output$download_data <- downloadHandler(
    filename = function() paste0("thyroidlab_filtered_", Sys.Date(), ".csv"),
    content = function(file) write.csv(filtered_data(), file, row.names = FALSE)
  )
}

shinyApp(ui = ui, server = server)
