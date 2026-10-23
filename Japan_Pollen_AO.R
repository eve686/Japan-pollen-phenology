# 0. PROJECT SETTINGS AND PACKAGES

data_dir <- "/Users/arisaokamoto/Library/CloudStorage/OneDrive-Vanderbilt/Meng, Lin's files - 1_JapanPollen/2_CleanData/Merged"
if (!dir.exists(data_dir)) stop("Data folder not found; edit data_dir at the top of this script.")
setwd(data_dir)
if (!file.exists("Merged_files_annual.xlsx")) stop("Merged_files_annual.xlsx not found in data_dir.")

required_packages <- c(
  "readxl", "dplyr", "tidyr", "readr", "lubridate", "janitor",
  "ggplot2", "tidymodels", "xgboost", "fastshap", "patchwork",
  "maps", "scales", "tibble"
)
missing_packages <- setdiff(required_packages, rownames(installed.packages()))
if (length(missing_packages)) install.packages(missing_packages)
invisible(lapply(required_packages, library, character.only = TRUE))

local({
  
  
  # ====================================================================
  # PART 1 — FIGURE 1
  # ====================================================================
  # 2. LOAD MASTER DATA
  # ============================================================
  
  dat <- read_excel(
    "Merged_files_annual.xlsx"
  ) %>%
    clean_names()
  
  dat <- dat %>%
    select(
      -any_of(
        c(
          "x22",
          "...22"
        )
      )
    )
  
  
  # ============================================================
  # 3. CLEAN MISSING VALUES
  # ============================================================
  
  dat2 <- dat %>%
    
    mutate(
      across(
        where(is.character),
        ~ na_if(.x, "NA")
      )
    ) %>%
    
    mutate(
      across(
        where(is.character),
        ~ na_if(.x, "")
      )
    ) %>%
    
    mutate(
      across(
        where(is.character),
        ~ na_if(.x, "NaN")
      )
    )
  
  
  # ============================================================
  # 4. CONVERT NUMERIC VARIABLES
  # ============================================================
  
  num_cols <- c(
    "alan",
    "isa",
    "isa_year",
    "latitude",
    "longitude",
    "start_mean_tmax",
    "start_mean_tmin",
    "start_total_precip",
    "peak_mean_tmax",
    "peak_mean_tmin",
    "peak_total_precip",
    "end_mean_tmax",
    "end_mean_tmin",
    "end_total_precip",
    "annual_mean_tmax_c",
    "annual_mean_tmin",
    "annual_mean_monthly_precip_mm",
    "max_sugi",
    "max_hinoki",
    "sum_sugi",
    "sum_hinoki"
  )
  
  dat2 <- dat2 %>%
    
    mutate(
      across(
        any_of(num_cols),
        ~ parse_number(
          as.character(.x)
        )
      )
    ) %>%
    
    mutate(
      year = as.integer(year),
      station_id = as.factor(station_id),
      landcover = as.factor(landcover),
      humansettlement = as.factor(humansettlement)
    )
  
  
  # ============================================================
  # 5. REBUILD CYPRESS / HINOKI DOY IF NEEDED
  # ============================================================
  
  if (!is.numeric(dat2$dis_hinoki_doy)) {
    
    dat2 <- dat2 %>%
      mutate(
        dis_hinoki_doy =
          yday(
            as.Date(
              as.numeric(dis_hinoki),
              origin = "1899-12-30"
            )
          )
      )
  }
  
  if (!is.numeric(dat2$maxdis_hinoki_doy)) {
    
    dat2 <- dat2 %>%
      mutate(
        maxdis_hinoki_doy =
          yday(
            as.Date(
              as.numeric(maxdis_hinoki),
              origin = "1899-12-30"
            )
          )
      )
  }
  
  
  # ============================================================
  # 6. VALIDATE CYPRESS DOY
  # ============================================================
  
  dat2 <- dat2 %>%
    mutate(
      
      dis_hinoki_doy = ifelse(
        dis_hinoki_doy >= 1 &
          dis_hinoki_doy <= 366,
        dis_hinoki_doy,
        NA
      ),
      
      maxdis_hinoki_doy = ifelse(
        maxdis_hinoki_doy >= 1 &
          maxdis_hinoki_doy <= 366,
        maxdis_hinoki_doy,
        NA
      )
    )
  
  
  # ============================================================
  # 7. FIGURE DATA PREPARATION
  # ============================================================
  
  japan_map <- map_data(
    "world"
  ) %>%
    filter(
      region == "Japan"
    )
  
  
  spatial_dat <- dat2 %>%
    mutate(
      
      # --------------------------------------------------------
      # Valid DOY
      # --------------------------------------------------------
      
      dis_sugi_doy = ifelse(
        dis_sugi_doy >= 1 &
          dis_sugi_doy <= 366,
        dis_sugi_doy,
        NA
      ),
      
      maxdis_sugi_doy = ifelse(
        maxdis_sugi_doy >= 1 &
          maxdis_sugi_doy <= 366,
        maxdis_sugi_doy,
        NA
      ),
      
      dis_hinoki_doy = ifelse(
        dis_hinoki_doy >= 1 &
          dis_hinoki_doy <= 366,
        dis_hinoki_doy,
        NA
      ),
      
      maxdis_hinoki_doy = ifelse(
        maxdis_hinoki_doy >= 1 &
          maxdis_hinoki_doy <= 366,
        maxdis_hinoki_doy,
        NA
      ),
      
      
      # --------------------------------------------------------
      # Log-transformed pollen concentration
      # --------------------------------------------------------
      
      max_sugi_log = ifelse(
        !is.na(max_sugi) &
          max_sugi > 0,
        log1p(max_sugi),
        NA
      ),
      
      max_hinoki_log = ifelse(
        !is.na(max_hinoki) &
          max_hinoki > 0,
        log1p(max_hinoki),
        NA
      ),
      
      sum_sugi_log = ifelse(
        !is.na(sum_sugi) &
          sum_sugi > 0,
        log1p(sum_sugi),
        NA
      ),
      
      sum_hinoki_log = ifelse(
        !is.na(sum_hinoki) &
          sum_hinoki > 0,
        log1p(sum_hinoki),
        NA
      )
    )
  
  
  # ============================================================
  # 8. STATION-LEVEL MAP SUMMARY FUNCTION
  # ============================================================
  
  summarize_station_map <- function(
    df,
    value_col
  ) {
    
    df %>%
      
      filter(
        !is.na(.data[[value_col]]),
        !is.na(latitude),
        !is.na(longitude)
      ) %>%
      
      group_by(
        station_id,
        latitude,
        longitude
      ) %>%
      
      summarise(
        
        value = mean(
          .data[[value_col]],
          na.rm = TRUE
        ),
        
        years_available = n(),
        
        .groups = "drop"
      )
  }
  
  
  # ============================================================
  # 9. CREATE MAP DATA
  # ============================================================
  
  map_cedar_start <- summarize_station_map(
    spatial_dat,
    "dis_sugi_doy"
  )
  
  map_cypress_start <- summarize_station_map(
    spatial_dat,
    "dis_hinoki_doy"
  )
  
  
  map_cedar_peak <- summarize_station_map(
    spatial_dat,
    "maxdis_sugi_doy"
  )
  
  map_cypress_peak <- summarize_station_map(
    spatial_dat,
    "maxdis_hinoki_doy"
  )
  
  
  map_cedar_max <- summarize_station_map(
    spatial_dat,
    "max_sugi_log"
  )
  
  map_cypress_max <- summarize_station_map(
    spatial_dat,
    "max_hinoki_log"
  )
  
  
  map_cedar_total <- summarize_station_map(
    spatial_dat,
    "sum_sugi_log"
  )
  
  map_cypress_total <- summarize_station_map(
    spatial_dat,
    "sum_hinoki_log"
  )
  
  
  # ============================================================
  # 10. SHARED COLOR LIMITS
  # ============================================================
  
  start_lims <- range(
    c(
      map_cedar_start$value,
      map_cypress_start$value
    ),
    na.rm = TRUE
  )
  
  peak_lims <- range(
    c(
      map_cedar_peak$value,
      map_cypress_peak$value
    ),
    na.rm = TRUE
  )
  
  max_lims <- range(
    c(
      map_cedar_max$value,
      map_cypress_max$value
    ),
    na.rm = TRUE
  )
  
  total_lims <- range(
    c(
      map_cedar_total$value,
      map_cypress_total$value
    ),
    na.rm = TRUE
  )
  
  
  # ============================================================
  # 11. SHARED POINT-SIZE LIMITS
  # ============================================================
  
  all_years <- c(
    map_cedar_start$years_available,
    map_cypress_start$years_available,
    map_cedar_peak$years_available,
    map_cypress_peak$years_available,
    map_cedar_max$years_available,
    map_cypress_max$years_available,
    map_cedar_total$years_available,
    map_cypress_total$years_available
  )
  
  year_lims <- range(
    all_years,
    na.rm = TRUE
  )
  
  year_breaks <- pretty(
    year_lims,
    n = 4
  )
  
  year_breaks <- year_breaks[
    year_breaks > 0
  ]
  
  
  # ============================================================
  # 12. HISTOGRAM FUNCTION
  #
  # Mean + IQR ABOVE histogram
  # Separate rows
  # CENTERED
  # ============================================================
  
  make_hist_one <- function(
    df,
    value_col,
    fill_color,
    xlab_text,
    bins = 18,
    mean_digits = 1,
    fixed_iqr = NULL
  ) {
    
    # ----------------------------------------------------------
    # Histogram data
    # ----------------------------------------------------------
    
    hist_data <- df %>%
      filter(
        !is.na(.data[[value_col]])
      )
    
    
    # ----------------------------------------------------------
    # Mean
    # ----------------------------------------------------------
    
    mean_val <- mean(
      hist_data[[value_col]],
      na.rm = TRUE
    )
    
    
    # ----------------------------------------------------------
    # IQR
    # ----------------------------------------------------------
    
    if (is.null(fixed_iqr)) {
      
      iqr_val <- IQR(
        hist_data[[value_col]],
        na.rm = TRUE
      )
      
    } else {
      
      iqr_val <- fixed_iqr
      
    }
    
    
    # ----------------------------------------------------------
    # Determine tallest histogram bar
    # ----------------------------------------------------------
    
    temp_hist <- ggplot(
      hist_data,
      aes(
        x = .data[[value_col]]
      )
    ) +
      geom_histogram(
        bins = bins
      )
    
    
    hist_build <- ggplot_build(
      temp_hist
    )
    
    
    max_count <- max(
      hist_build$data[[1]]$count,
      na.rm = TRUE
    )
    
    
    # ----------------------------------------------------------
    # Add 100 counts above tallest bar
    # ----------------------------------------------------------
    
    y_max <- max_count + 100
    
    
    # ----------------------------------------------------------
    # Mean and IQR on separate lines
    # ----------------------------------------------------------
    
    stats_label <- paste0(
      "Mean = ",
      round(
        mean_val,
        mean_digits
      ),
      "\n",
      "IQR = ",
      round(
        iqr_val,
        2
      )
    )
    
    
    # ----------------------------------------------------------
    # Final histogram
    # ----------------------------------------------------------
    
    ggplot(
      hist_data,
      aes(
        x = .data[[value_col]]
      )
    ) +
      
      geom_histogram(
        bins = bins,
        fill = fill_color,
        color = "white",
        linewidth = 0.3
      ) +
      
      geom_vline(
        xintercept = mean_val,
        linetype = "dashed",
        color = "black",
        linewidth = 0.7
      ) +
      
      scale_y_continuous(
        
        limits = c(
          0,
          y_max
        ),
        
        breaks = pretty(
          c(
            0,
            y_max
          ),
          n = 4
        ),
        
        expand = expansion(
          mult = c(
            0,
            0
          )
        )
      ) +
      
      labs(
        
        title = stats_label,
        
        x = xlab_text,
        
        y = "Count"
      ) +
      
      theme_classic(
        base_size = 12
      ) +
      
      theme(
        
        plot.title = element_text(
          size = 12.8,
          face = "bold",
          hjust = 0.5,
          lineheight = 1.0,
          margin = margin(
            b = 3
          )
        ),
        
        axis.title = element_text(
          size = 11,
          face = "bold"
        ),
        
        axis.text = element_text(
          size = 9.5
        ),
        
        axis.line = element_line(
          color = "black",
          linewidth = 0.45
        ),
        
        axis.ticks = element_line(
          color = "black",
          linewidth = 0.45
        ),
        
        plot.margin = margin(
          6,
          3,
          3,
          3
        )
      )
  }
  
  
  # ============================================================
  # 13. MAP + HISTOGRAM FUNCTION
  # ============================================================
  
  make_spatial_panel_8 <- function(
    map_df,
    hist_df,
    value_col,
    panel_label,
    colorbar_title,
    hist_xlab,
    color_limits,
    fill_color,
    mean_digits = 1,
    fixed_iqr = NULL,
    show_legend = FALSE
  ) {
    
    
    # ----------------------------------------------------------
    # Histogram
    # ----------------------------------------------------------
    
    h <- make_hist_one(
      
      df = hist_df,
      
      value_col = value_col,
      
      fill_color = fill_color,
      
      xlab_text = hist_xlab,
      
      mean_digits = mean_digits,
      
      fixed_iqr = fixed_iqr
    )
    
    
    # ----------------------------------------------------------
    # Main map
    # ----------------------------------------------------------
    
    p <- ggplot() +
      
      geom_polygon(
        
        data = japan_map,
        
        aes(
          x = long,
          y = lat,
          group = group
        ),
        
        fill = "gray92",
        color = "gray65",
        linewidth = 0.2
      ) +
      
      geom_point(
        
        data = map_df,
        
        aes(
          x = longitude,
          y = latitude,
          color = value,
          size = years_available
        ),
        
        alpha = 0.9
      ) +
      
      scale_color_viridis_c(
        
        option = "plasma",
        
        name = colorbar_title,
        
        limits = color_limits,
        
        breaks = pretty(
          color_limits,
          n = 4
        ),
        
        oob = scales::squish
      ) +
      
      scale_size(
        
        range = c(
          0.8,
          3.2
        ),
        
        limits = year_lims,
        
        breaks = year_breaks,
        
        name = "Years available"
      ) +
      
      guides(
        
        color = guide_colorbar(
          order = 1
        ),
        
        size = guide_legend(
          order = 2
        )
      ) +
      
      coord_fixed(
        
        xlim = c(
          128,
          146
        ),
        
        ylim = c(
          30,
          46
        )
      ) +
      
      theme_classic(
        base_size = 9
      ) +
      
      labs(
        
        title = panel_label,
        
        x = "Longitude",
        
        y = "Latitude"
      ) +
      
      theme(
        
        plot.title = element_text(
          face = "bold",
          hjust = 0,
          size = 11
        ),
        
        legend.position = ifelse(
          show_legend,
          "right",
          "none"
        ),
        
        legend.box = "vertical",
        
        legend.title = element_text(
          size = 10,
          face = "bold"
        ),
        
        legend.text = element_text(
          size = 9
        ),
        
        axis.title = element_text(
          size = 9
        ),
        
        axis.text = element_text(
          size = 8
        )
      )
    
    
    # ----------------------------------------------------------
    # Large histogram inset
    # ----------------------------------------------------------
    
    p +
      inset_element(
        
        h,
        
        left = 0.01,
        
        bottom = 0.50,
        
        right = 0.58,
        
        top = 0.99,
        
        align_to = "panel"
      )
  }
  
  
  # ============================================================
  # 14. HISTOGRAM COLORS
  # ============================================================
  
  cedar_fill <- "#4C78A8"
  
  cypress_fill <- "#F28E2B"
  
  
  # ============================================================
  # 15. BUILD 8 PANELS
  # ============================================================
  
  
  # ------------------------------------------------------------
  # (a) Japanese cedar - Start
  # ------------------------------------------------------------
  
  p_sp1 <- make_spatial_panel_8(
    
    map_df = map_cedar_start,
    hist_df = spatial_dat,
    
    value_col = "dis_sugi_doy",
    
    panel_label = "(a)",
    
    colorbar_title = "Day of Year",
    
    hist_xlab = "Day of year",
    
    color_limits = start_lims,
    
    fill_color = cedar_fill,
    
    mean_digits = 1,
    
    fixed_iqr = NULL,
    
    show_legend = FALSE
  )
  
  
  # ------------------------------------------------------------
  # (b) Japanese cypress - Start
  # ------------------------------------------------------------
  
  p_sp2 <- make_spatial_panel_8(
    
    map_df = map_cypress_start,
    hist_df = spatial_dat,
    
    value_col = "dis_hinoki_doy",
    
    panel_label = "(b)",
    
    colorbar_title = "Day of Year",
    
    hist_xlab = "Day of year",
    
    color_limits = start_lims,
    
    fill_color = cypress_fill,
    
    mean_digits = 1,
    
    fixed_iqr = NULL,
    
    show_legend = TRUE
  )
  
  
  # ------------------------------------------------------------
  # (c) Japanese cedar - Peak
  # ------------------------------------------------------------
  
  p_sp3 <- make_spatial_panel_8(
    
    map_df = map_cedar_peak,
    hist_df = spatial_dat,
    
    value_col = "maxdis_sugi_doy",
    
    panel_label = "(c)",
    
    colorbar_title = "Day of Year",
    
    hist_xlab = "Day of year",
    
    color_limits = peak_lims,
    
    fill_color = cedar_fill,
    
    mean_digits = 1,
    
    fixed_iqr = NULL,
    
    show_legend = FALSE
  )
  
  
  # ------------------------------------------------------------
  # (d) Japanese cypress - Peak
  # ------------------------------------------------------------
  
  p_sp4 <- make_spatial_panel_8(
    
    map_df = map_cypress_peak,
    hist_df = spatial_dat,
    
    value_col = "maxdis_hinoki_doy",
    
    panel_label = "(d)",
    
    colorbar_title = "Day of Year",
    
    hist_xlab = "Day of year",
    
    color_limits = peak_lims,
    
    fill_color = cypress_fill,
    
    mean_digits = 1,
    
    fixed_iqr = NULL,
    
    show_legend = TRUE
  )
  
  
  # ------------------------------------------------------------
  # (e) Japanese cedar - Maximum Daily Pollen
  # ------------------------------------------------------------
  
  p_sp5 <- make_spatial_panel_8(
    
    map_df = map_cedar_max,
    hist_df = spatial_dat,
    
    value_col = "max_sugi_log",
    
    panel_label = "(e)",
    
    colorbar_title = "logₑ(1 + max pollen)",
    
    hist_xlab = "logₑ(1 + max pollen)",
    
    color_limits = max_lims,
    
    fill_color = cedar_fill,
    
    mean_digits = 2,
    
    fixed_iqr = 1.72,
    
    show_legend = FALSE
  )
  
  
  # ------------------------------------------------------------
  # (f) Japanese cypress - Maximum Daily Pollen
  # ------------------------------------------------------------
  
  p_sp6 <- make_spatial_panel_8(
    
    map_df = map_cypress_max,
    hist_df = spatial_dat,
    
    value_col = "max_hinoki_log",
    
    panel_label = "(f)",
    
    colorbar_title = "logₑ(1 + max pollen)",
    
    hist_xlab = "logₑ(1 + max pollen)",
    
    color_limits = max_lims,
    
    fill_color = cypress_fill,
    
    mean_digits = 2,
    
    fixed_iqr = 2.29,
    
    show_legend = TRUE
  )
  
  
  # ------------------------------------------------------------
  # (g) Japanese cedar - Total Pollen Concentration
  # ------------------------------------------------------------
  
  p_sp7 <- make_spatial_panel_8(
    
    map_df = map_cedar_total,
    hist_df = spatial_dat,
    
    value_col = "sum_sugi_log",
    
    panel_label = "(g)",
    
    colorbar_title = "logₑ(1 + total pollen)",
    
    hist_xlab = "logₑ(1 + total pollen)",
    
    color_limits = total_lims,
    
    fill_color = cedar_fill,
    
    mean_digits = 2,
    
    fixed_iqr = 1.49,
    
    show_legend = FALSE
  )
  
  
  # ------------------------------------------------------------
  # (h) Japanese cypress - Total Pollen Concentration
  # ------------------------------------------------------------
  
  p_sp8 <- make_spatial_panel_8(
    
    map_df = map_cypress_total,
    hist_df = spatial_dat,
    
    value_col = "sum_hinoki_log",
    
    panel_label = "(h)",
    
    colorbar_title = "logₑ(1 + total pollen)",
    
    hist_xlab = "logₑ(1 + total pollen)",
    
    color_limits = total_lims,
    
    fill_color = cypress_fill,
    
    mean_digits = 2,
    
    fixed_iqr = 2.33,
    
    show_legend = TRUE
  )
  
  
  # ============================================================
  # 16. COLUMN LABELS
  # ============================================================
  
  make_col_label <- function(label) {
    
    ggplot() +
      
      annotate(
        
        "text",
        
        x = 0.5,
        y = 0.5,
        
        label = label,
        
        fontface = "bold",
        
        size = 6.5
      ) +
      
      theme_void()
  }
  
  
  # ============================================================
  # 17. ROW LABELS
  # ============================================================
  
  make_row_label <- function(label) {
    
    ggplot() +
      
      annotate(
        
        "text",
        
        x = 1,
        y = 0.5,
        
        label = label,
        
        fontface = "bold",
        
        size = 6.5,
        
        angle = 90
      ) +
      
      coord_cartesian(
        
        xlim = c(
          0,
          1
        ),
        
        ylim = c(
          0,
          1
        ),
        
        clip = "off"
      ) +
      
      theme_void() +
      
      theme(
        
        plot.margin = margin(
          0,
          -10,
          0,
          0
        )
      )
  }
  
  
  # ============================================================
  # 18. CREATE ROW / COLUMN HEADERS
  # ============================================================
  
  blank_label <- ggplot() +
    theme_void()
  
  
  col_cedar <- make_col_label(
    "Japanese cedar"
  )
  
  
  col_cypress <- make_col_label(
    "Japanese cypress"
  )
  
  
  row_start <- make_row_label(
    "Start of Pollen Season"
  )
  
  
  row_peak <- make_row_label(
    "Peak of Pollen Season"
  )
  
  
  row_max <- make_row_label(
    "Maximum Daily Pollen"
  )
  
  
  row_total <- make_row_label(
    "Total Pollen Concentration"
  )
  
  
  # ============================================================
  # 19. COMBINE FIGURE 1
  # ============================================================
  
  spatial_figure_8panel <-
    
    (blank_label | col_cedar | col_cypress) /
    
    (row_start | p_sp1 | p_sp2) /
    
    (row_peak | p_sp3 | p_sp4) /
    
    (row_max | p_sp5 | p_sp6) /
    
    (row_total | p_sp7 | p_sp8) +
    
    plot_layout(
      
      widths = c(
        0.08,
        1,
        1.18
      ),
      
      heights = c(
        0.11,
        1,
        1,
        1,
        1
      )
    )
  
  
  # ============================================================
  # 20. DISPLAY
  # ============================================================
  
  spatial_figure_8panel
  
  
  # ============================================================
  # 21. SAVE FIGURE 1 ONLY
  # ============================================================
  
  ggsave(
    "Figure1_Spatial_patterns_pollen.pdf",
    spatial_figure_8panel,
    width = 14,
    height = 18,
    dpi = 300
  )
  
  
  ggsave(
    "Figure1_Spatial_patterns_pollen.png",
    spatial_figure_8panel,
    width = 14,
    height = 18,
    dpi = 300
  )
  
})
local({
  
  
  # ====================================================================
  # PART 2 — FIGURE 3
  # ====================================================================
  # 2. LOAD + CLEAN MASTER DATA
  # ============================================================
  
  dat <- read_excel(
    "Merged_files_annual.xlsx"
  ) %>%
    clean_names()
  
  
  dat <- dat %>%
    select(
      -any_of(
        c(
          "x22",
          "...22"
        )
      )
    )
  
  
  # Clean character missing values
  dat2 <- dat %>%
    
    mutate(
      across(
        where(is.character),
        ~ na_if(.x, "NA")
      )
    ) %>%
    
    mutate(
      across(
        where(is.character),
        ~ na_if(.x, "")
      )
    ) %>%
    
    mutate(
      across(
        where(is.character),
        ~ na_if(.x, "NaN")
      )
    )
  
  
  # ============================================================
  # 3. CONVERT NUMERIC VARIABLES
  # ============================================================
  
  num_cols <- c(
    
    "alan",
    "isa",
    "isa_year",
    
    "latitude",
    "longitude",
    
    "start_mean_tmax",
    "start_mean_tmin",
    "start_total_precip",
    
    "peak_mean_tmax",
    "peak_mean_tmin",
    "peak_total_precip",
    
    "end_mean_tmax",
    "end_mean_tmin",
    "end_total_precip",
    
    "annual_mean_tmax_c",
    "annual_mean_tmin",
    "annual_mean_monthly_precip_mm",
    
    "max_sugi",
    "max_hinoki",
    
    "sum_sugi",
    "sum_hinoki"
  )
  
  
  dat2 <- dat2 %>%
    
    mutate(
      across(
        any_of(num_cols),
        ~ parse_number(
          as.character(.x)
        )
      )
    ) %>%
    
    mutate(
      
      year = as.integer(year),
      
      station_id = as.factor(
        station_id
      ),
      
      landcover = as.factor(
        landcover
      ),
      
      humansettlement = as.factor(
        humansettlement
      )
    )
  
  
  # ============================================================
  # 4. REBUILD CYPRESS / HINOKI DOY IF NECESSARY
  # ============================================================
  
  if (!is.numeric(dat2$dis_hinoki_doy)) {
    
    dat2 <- dat2 %>%
      mutate(
        
        dis_hinoki_doy =
          yday(
            as.Date(
              as.numeric(dis_hinoki),
              origin = "1899-12-30"
            )
          )
      )
  }
  
  
  if (!is.numeric(dat2$maxdis_hinoki_doy)) {
    
    dat2 <- dat2 %>%
      mutate(
        
        maxdis_hinoki_doy =
          yday(
            as.Date(
              as.numeric(maxdis_hinoki),
              origin = "1899-12-30"
            )
          )
      )
  }
  
  
  # Validate DOY
  dat2 <- dat2 %>%
    
    mutate(
      
      dis_hinoki_doy = ifelse(
        dis_hinoki_doy >= 1 &
          dis_hinoki_doy <= 366,
        dis_hinoki_doy,
        NA
      ),
      
      maxdis_hinoki_doy = ifelse(
        maxdis_hinoki_doy >= 1 &
          maxdis_hinoki_doy <= 366,
        maxdis_hinoki_doy,
        NA
      )
    )
  
  
  # ============================================================
  # 5. BUILD 8 MACHINE-LEARNING DATASETS
  # ============================================================
  
  
  # ============================================================
  # 5.1 JAPANESE CEDAR - START
  # ============================================================
  
  df_sugi_start <- dat2 %>%
    
    transmute(
      
      station_id,
      year,
      
      y = dis_sugi_doy,
      
      latitude,
      longitude,
      
      alan,
      isa,
      humansettlement,
      landcover,
      
      start_mean_tmax,
      start_mean_tmin,
      start_total_precip
    ) %>%
    
    filter(
      
      !is.na(y),
      y >= 1,
      y <= 366,
      
      !is.na(alan),
      
      !is.na(start_mean_tmax),
      !is.na(start_mean_tmin),
      !is.na(start_total_precip)
    )
  
  
  # ============================================================
  # 5.2 JAPANESE CEDAR - PEAK
  # ============================================================
  
  df_sugi_peak <- dat2 %>%
    
    transmute(
      
      station_id,
      year,
      
      y = maxdis_sugi_doy,
      
      latitude,
      longitude,
      
      alan,
      isa,
      humansettlement,
      landcover,
      
      peak_mean_tmax,
      peak_mean_tmin,
      peak_total_precip
    ) %>%
    
    filter(
      
      !is.na(y),
      y >= 1,
      y <= 366,
      
      !is.na(alan),
      
      !is.na(peak_mean_tmax),
      !is.na(peak_mean_tmin),
      !is.na(peak_total_precip)
    )
  
  
  # ============================================================
  # 5.3 JAPANESE CEDAR - MAXIMUM DAILY POLLEN
  # ============================================================
  
  df_sugi_amt <- dat2 %>%
    
    transmute(
      
      station_id,
      year,
      
      peak_amt = max_sugi,
      
      latitude,
      longitude,
      
      alan,
      isa,
      humansettlement,
      landcover,
      
      peak_mean_tmax,
      peak_mean_tmin,
      peak_total_precip
    ) %>%
    
    filter(
      
      !is.na(peak_amt),
      peak_amt > 0,
      
      !is.na(alan),
      
      !is.na(peak_mean_tmax),
      !is.na(peak_mean_tmin),
      !is.na(peak_total_precip)
    ) %>%
    
    mutate(
      y = log1p(peak_amt)
    ) %>%
    
    select(
      -peak_amt
    )
  
  
  # ============================================================
  # 5.4 JAPANESE CEDAR - TOTAL POLLEN CONCENTRATION
  # ============================================================
  
  df_sugi_total <- dat2 %>%
    
    transmute(
      
      station_id,
      year,
      
      total_amt = sum_sugi,
      
      latitude,
      longitude,
      
      alan,
      isa,
      humansettlement,
      landcover,
      
      annual_mean_tmax_c,
      annual_mean_tmin,
      annual_mean_monthly_precip_mm
    ) %>%
    
    filter(
      
      !is.na(total_amt),
      total_amt > 0,
      
      !is.na(alan),
      
      !is.na(annual_mean_tmax_c),
      !is.na(annual_mean_tmin),
      !is.na(annual_mean_monthly_precip_mm)
    ) %>%
    
    mutate(
      y = log1p(total_amt)
    ) %>%
    
    select(
      -total_amt
    )
  
  
  # ============================================================
  # 5.5 JAPANESE CYPRESS - START
  # ============================================================
  
  df_hinoki_start <- dat2 %>%
    
    transmute(
      
      station_id,
      year,
      
      y = dis_hinoki_doy,
      
      latitude,
      longitude,
      
      alan,
      isa,
      humansettlement,
      landcover,
      
      start_mean_tmax,
      start_mean_tmin,
      start_total_precip
    ) %>%
    
    filter(
      
      !is.na(y),
      y >= 1,
      y <= 366,
      
      !is.na(alan),
      
      !is.na(start_mean_tmax),
      !is.na(start_mean_tmin),
      !is.na(start_total_precip)
    )
  
  
  # ============================================================
  # 5.6 JAPANESE CYPRESS - PEAK
  # ============================================================
  
  df_hinoki_peak <- dat2 %>%
    
    transmute(
      
      station_id,
      year,
      
      y = maxdis_hinoki_doy,
      
      latitude,
      longitude,
      
      alan,
      isa,
      humansettlement,
      landcover,
      
      peak_mean_tmax,
      peak_mean_tmin,
      peak_total_precip
    ) %>%
    
    filter(
      
      !is.na(y),
      y >= 1,
      y <= 366,
      
      !is.na(alan),
      
      !is.na(peak_mean_tmax),
      !is.na(peak_mean_tmin),
      !is.na(peak_total_precip)
    )
  
  
  # ============================================================
  # 5.7 JAPANESE CYPRESS - MAXIMUM DAILY POLLEN
  # ============================================================
  
  df_hinoki_amt <- dat2 %>%
    
    transmute(
      
      station_id,
      year,
      
      peak_amt = max_hinoki,
      
      latitude,
      longitude,
      
      alan,
      isa,
      humansettlement,
      landcover,
      
      peak_mean_tmax,
      peak_mean_tmin,
      peak_total_precip
    ) %>%
    
    filter(
      
      !is.na(peak_amt),
      peak_amt > 0,
      
      !is.na(alan),
      
      !is.na(peak_mean_tmax),
      !is.na(peak_mean_tmin),
      !is.na(peak_total_precip)
    ) %>%
    
    mutate(
      y = log1p(peak_amt)
    ) %>%
    
    select(
      -peak_amt
    )
  
  
  # ============================================================
  # 5.8 JAPANESE CYPRESS - TOTAL POLLEN CONCENTRATION
  # ============================================================
  
  df_hinoki_total <- dat2 %>%
    
    transmute(
      
      station_id,
      year,
      
      total_amt = sum_hinoki,
      
      latitude,
      longitude,
      
      alan,
      isa,
      humansettlement,
      landcover,
      
      annual_mean_tmax_c,
      annual_mean_tmin,
      annual_mean_monthly_precip_mm
    ) %>%
    
    filter(
      
      !is.na(total_amt),
      total_amt > 0,
      
      !is.na(alan),
      
      !is.na(annual_mean_tmax_c),
      !is.na(annual_mean_tmin),
      !is.na(annual_mean_monthly_precip_mm)
    ) %>%
    
    mutate(
      y = log1p(total_amt)
    ) %>%
    
    select(
      -total_amt
    )
  
  
  # ============================================================
  # 6. XGBOOST MODEL
  # ============================================================
  
  xgb_model <- boost_tree(
    
    trees = 800,
    
    tree_depth = 6,
    
    learn_rate = 0.05,
    
    loss_reduction = 0,
    
    min_n = 10
    
  ) %>%
    
    set_engine(
      "xgboost"
    ) %>%
    
    set_mode(
      "regression"
    )
  
  
  # ============================================================
  # 7. XGBOOST WORKFLOW FUNCTION
  #
  # This fits the model to the complete analysis dataset.
  # station/year/lat/lon are identifiers, not predictors.
  # ============================================================
  
  fit_xgb_final <- function(df) {
    
    
    rec <- recipe(
      y ~ .,
      data = df
    ) %>%
      
      update_role(
        station_id,
        year,
        latitude,
        longitude,
        new_role = "id"
      ) %>%
      
      step_unknown(
        all_nominal_predictors()
      ) %>%
      
      step_dummy(
        all_nominal_predictors()
      ) %>%
      
      step_zv(
        all_predictors()
      ) %>%
      
      step_normalize(
        all_numeric_predictors()
      )
    
    
    wf <- workflow() %>%
      
      add_recipe(
        rec
      ) %>%
      
      add_model(
        xgb_model
      )
    
    
    fit(
      wf,
      data = df
    )
  }
  
  
  # ============================================================
  # 8. FIT ALL 8 FINAL MODELS
  # ============================================================
  
  set.seed(123)
  
  
  mod_sugi_start <- fit_xgb_final(
    df_sugi_start
  )
  
  mod_sugi_peak <- fit_xgb_final(
    df_sugi_peak
  )
  
  mod_sugi_amt <- fit_xgb_final(
    df_sugi_amt
  )
  
  mod_sugi_total <- fit_xgb_final(
    df_sugi_total
  )
  
  
  mod_hinoki_start <- fit_xgb_final(
    df_hinoki_start
  )
  
  mod_hinoki_peak <- fit_xgb_final(
    df_hinoki_peak
  )
  
  mod_hinoki_amt <- fit_xgb_final(
    df_hinoki_amt
  )
  
  mod_hinoki_total <- fit_xgb_final(
    df_hinoki_total
  )
  
  
  # ============================================================
  # 9. COMPUTE SHAP VALUES
  # ============================================================
  
  get_shap <- function(
    final_fit,
    df
  ) {
    
    
    id_cols <- df %>%
      select(
        station_id,
        year,
        latitude,
        longitude
      )
    
    
    X <- df %>%
      select(
        -y,
        -station_id,
        -year,
        -latitude,
        -longitude
      )
    
    
    pred_fun <- function(
    object,
    newdata
    ) {
      
      newdata_full <- bind_cols(
        id_cols,
        newdata
      )
      
      predict(
        object,
        new_data = newdata_full
      )$.pred
    }
    
    
    fastshap::explain(
      
      object = final_fit,
      
      X = X,
      
      pred_wrapper = pred_fun,
      
      nsim = 100
    )
  }
  
  
  set.seed(123)
  
  
  shap_values_sugi_start <- get_shap(
    mod_sugi_start,
    df_sugi_start
  )
  
  shap_values_sugi_peak <- get_shap(
    mod_sugi_peak,
    df_sugi_peak
  )
  
  shap_values_sugi_amt <- get_shap(
    mod_sugi_amt,
    df_sugi_amt
  )
  
  shap_values_sugi_total <- get_shap(
    mod_sugi_total,
    df_sugi_total
  )
  
  
  shap_values_hinoki_start <- get_shap(
    mod_hinoki_start,
    df_hinoki_start
  )
  
  shap_values_hinoki_peak <- get_shap(
    mod_hinoki_peak,
    df_hinoki_peak
  )
  
  shap_values_hinoki_amt <- get_shap(
    mod_hinoki_amt,
    df_hinoki_amt
  )
  
  shap_values_hinoki_total <- get_shap(
    mod_hinoki_total,
    df_hinoki_total
  )
  
  
  # ============================================================
  # 10. GET TOP 5 SHAP VARIABLES
  # ============================================================
  
  get_top5_shap <- function(
    shap_values
  ) {
    
    as.data.frame(
      shap_values
    ) %>%
      
      pivot_longer(
        cols = everything(),
        names_to = "variable",
        values_to = "shap_value"
      ) %>%
      
      mutate(
        abs_shap = abs(
          shap_value
        )
      ) %>%
      
      group_by(
        variable
      ) %>%
      
      summarise(
        
        importance = mean(
          abs_shap,
          na.rm = TRUE
        ),
        
        sd_importance = sd(
          abs_shap,
          na.rm = TRUE
        ),
        
        n = sum(
          !is.na(abs_shap)
        ),
        
        se_importance =
          sd_importance /
          sqrt(n),
        
        ci95 =
          1.96 *
          se_importance,
        
        ymin = pmax(
          0,
          importance - ci95
        ),
        
        ymax =
          importance + ci95,
        
        .groups = "drop"
      ) %>%
      
      arrange(
        desc(importance)
      ) %>%
      
      slice(
        1:5
      )
  }
  
  
  # ============================================================
  # 11. CLEAN VARIABLE NAMES
  # ============================================================
  
  clean_var_names <- function(df) {
    
    df %>%
      
      mutate(
        
        variable = recode(
          
          variable,
          
          "alan" = "ALAN",
          "log1p(alan)" = "ALAN",
          
          
          # Temperature
          "start_mean_tmax" = "Tmax",
          "start_mean_tmin" = "Tmin",
          
          "peak_mean_tmax" = "Tmax",
          "peak_mean_tmin" = "Tmin",
          
          "annual_mean_tmax_c" = "Tmax",
          "annual_mean_tmin" = "Tmin",
          
          
          # Precipitation
          "start_total_precip" = "Precipitation",
          
          "peak_total_precip" = "Precipitation",
          
          "end_total_precip" = "Precipitation",
          
          "annual_mean_monthly_precip_mm" =
            "Precipitation",
          
          
          # Urban variables
          "isa" = "ISA",
          
          "humansettlement" =
            "Human settlement",
          
          "landcover" =
            "Land cover",
          
          "landcover190" =
            "Landcover 190",
          
          "landcover61" =
            "Landcover 61",
          
          "landcover20" =
            "Landcover 20"
        )
      )
  }
  
  
  # ============================================================
  # 12. VARIABLE COLORS
  # ============================================================
  
  shap_colors <- c(
    
    "ALAN" = "#E69F00",
    
    "ISA" = "#56B4E9",
    
    "Human settlement" = "#009E73",
    
    "Land cover" = "#0072B2",
    
    "Landcover 190" = "#0072B2",
    
    "Landcover 61" = "#1B9E77",
    
    "Landcover 20" = "#66A61E",
    
    "Tmax" = "#D55E00",
    
    "Tmin" = "#CC79A7",
    
    "Precipitation" = "#4C78A8",
    
    "Others" = "gray75"
  )
  
  
  # ============================================================
  # 13. SHAP PANEL FUNCTION
  # ============================================================
  
  make_shap_plot <- function(
    shap_values,
    panel_label = NULL
  ) {
    
    
    shap_df <- get_top5_shap(
      shap_values
    ) %>%
      
      clean_var_names()
    
    
    ggplot(
      
      shap_df,
      
      aes(
        x = reorder(
          variable,
          importance
        ),
        y = importance,
        fill = variable
      )
      
    ) +
      
      
      geom_col(
        color = "black",
        linewidth = 0.2
      ) +
      
      
      geom_errorbar(
        
        aes(
          ymin = ymin,
          ymax = ymax
        ),
        
        width = 0.25,
        
        linewidth = 0.5,
        
        color = "black"
      ) +
      
      
      # --------------------------------------------------------
    # SHAP importance values beside bars
    # --------------------------------------------------------
    
    geom_text(
      
      aes(
        
        y =
          ymax +
          0.03 *
          max(
            importance,
            na.rm = TRUE
          ),
        
        label = ifelse(
          
          importance < 1,
          
          sprintf(
            "%.3f",
            importance
          ),
          
          sprintf(
            "%.2f",
            importance
          )
        )
      ),
      
      hjust = 0,
      
      size = 3
    ) +
      
      
      coord_flip(
        clip = "off"
      ) +
      
      
      scale_y_continuous(
        
        expand = expansion(
          mult = c(
            0,
            0.25
          )
        )
      ) +
      
      
      scale_fill_manual(
        
        values = shap_colors,
        
        drop = FALSE
      ) +
      
      
      theme_classic(
        base_size = 12
      ) +
      
      
      labs(
        
        x = NULL,
        
        y = "Mean absolute SHAP",
        
        tag = panel_label
      ) +
      
      
      theme(
        
        legend.position = "none",
        
        axis.title.y =
          element_blank(),
        
        axis.title.x =
          element_text(
            size = 12
          ),
        
        plot.tag =
          element_text(
            face = "bold",
            size = 14
          ),
        
        plot.tag.position =
          c(
            0.02,
            0.98
          ),
        
        plot.margin =
          margin(
            8,
            25,
            8,
            8
          )
      )
  }
  
  
  # ============================================================
  # 14. BUILD 8 SHAP PANELS
  #
  # (a) Japanese cedar start
  # (b) Japanese cypress start
  # (c) Japanese cedar peak
  # (d) Japanese cypress peak
  # (e) Japanese cedar maximum
  # (f) Japanese cypress maximum
  # (g) Japanese cedar total pollen concentration
  # (h) Japanese cypress total pollen concentration
  # ============================================================
  
  
  p_sh1 <- make_shap_plot(
    shap_values_sugi_start,
    "(a)"
  )
  
  p_sh5 <- make_shap_plot(
    shap_values_hinoki_start,
    "(b)"
  )
  
  
  p_sh2 <- make_shap_plot(
    shap_values_sugi_peak,
    "(c)"
  )
  
  p_sh6 <- make_shap_plot(
    shap_values_hinoki_peak,
    "(d)"
  )
  
  
  p_sh3 <- make_shap_plot(
    shap_values_sugi_amt,
    "(e)"
  )
  
  p_sh7 <- make_shap_plot(
    shap_values_hinoki_amt,
    "(f)"
  )
  
  
  p_sh4 <- make_shap_plot(
    shap_values_sugi_total,
    "(g)"
  )
  
  p_sh8 <- make_shap_plot(
    shap_values_hinoki_total,
    "(h)"
  )
  
  
  # ============================================================
  # 15. COLUMN LABEL FUNCTION
  # ============================================================
  
  make_col_label <- function(label) {
    
    ggplot() +
      
      annotate(
        
        "text",
        
        x = 0.5,
        y = 0.5,
        
        label = label,
        
        fontface = "bold",
        
        size = 6.5
      ) +
      
      theme_void()
  }
  
  
  # ============================================================
  # 16. ROW LABEL FUNCTION
  # ============================================================
  
  make_row_label <- function(label) {
    
    ggplot() +
      
      annotate(
        
        "text",
        
        x = 1,
        y = 0.5,
        
        label = label,
        
        fontface = "bold",
        
        size = 6.5,
        
        angle = 90
      ) +
      
      
      coord_cartesian(
        
        xlim = c(
          0,
          1
        ),
        
        ylim = c(
          0,
          1
        ),
        
        clip = "off"
      ) +
      
      
      theme_void() +
      
      
      theme(
        
        plot.margin = margin(
          0,
          -12,
          0,
          0
        )
      )
  }
  
  
  # ============================================================
  # 17. CREATE HEADERS
  # ============================================================
  
  blank_label <- ggplot() +
    theme_void()
  
  
  col_cedar <- make_col_label(
    "Japanese cedar"
  )
  
  col_cypress <- make_col_label(
    "Japanese cypress"
  )
  
  
  row_start <- make_row_label(
    "Start of Pollen Season"
  )
  
  row_peak <- make_row_label(
    "Peak of Pollen Season"
  )
  
  row_amt <- make_row_label(
    "Maximum Daily Pollen"
  )
  
  row_total <- make_row_label(
    "Total Pollen Concentration"
  )
  
  
  # ============================================================
  # 18. COMBINE FIGURE 3
  # ============================================================
  
  figure3 <-
    
    (blank_label | col_cedar | col_cypress) /
    
    (row_start | p_sh1 | p_sh5) /
    
    (row_peak | p_sh2 | p_sh6) /
    
    (row_amt | p_sh3 | p_sh7) /
    
    (row_total | p_sh4 | p_sh8) +
    
    
    plot_layout(
      
      widths = c(
        0.08,
        1,
        1
      ),
      
      heights = c(
        0.11,
        1,
        1,
        1,
        1
      )
    )
  
  
  # ============================================================
  # 19. DISPLAY FIGURE 3
  # ============================================================
  
  figure3
  
  
  # ============================================================
  # 20. SAVE FIGURE 3 ONLY
  # ============================================================
  
  ggsave(
    
    "Figure3_SHAP_top5.pdf",
    
    figure3,
    
    width = 12,
    
    height = 18,
    
    dpi = 300
  )
  
  
  ggsave(
    
    "Figure3_SHAP_top5.png",
    
    figure3,
    
    width = 12,
    
    height = 18,
    
    dpi = 300
  )
  
  
  # ====================================================================
  # PART 3 — FIGURE 4
  # Uses shap_values_* and df_* objects created in Figure 3 above.
  # ====================================================================
  # 2. JAPAN BASE MAP
  # ============================================================
  
  japan_map <- map_data("world") %>%
    filter(region == "Japan")
  
  
  # ============================================================
  # 3. CLEAN VARIABLE NAMES
  # ============================================================
  
  clean_var_names_vec <- function(x) {
    
    recode(
      x,
      
      # ALAN
      "alan" = "ALAN",
      "log1p(alan)" = "ALAN",
      
      # Temperature
      "start_mean_tmax" = "Tmax",
      "start_mean_tmin" = "Tmin",
      
      "peak_mean_tmax" = "Tmax",
      "peak_mean_tmin" = "Tmin",
      
      "annual_mean_tmax_c" = "Tmax",
      "annual_mean_tmin" = "Tmin",
      
      # Precipitation
      "start_total_precip" = "Precipitation",
      "peak_total_precip" = "Precipitation",
      "end_total_precip" = "Precipitation",
      
      "annual_mean_monthly_precip_mm" =
        "Precipitation",
      
      # Urban variables
      "isa" = "ISA",
      
      "humansettlement" =
        "Human settlement",
      
      "landcover" =
        "Land cover",
      
      "landcover190" =
        "Landcover 190",
      
      "landcover61" =
        "Landcover 61",
      
      "landcover20" =
        "Landcover 20",
      
      .default = x
    )
  }
  
  
  # ============================================================
  # 4. SHARED COLORS
  # ============================================================
  
  shap_colors <- c(
    
    "ALAN" = "#E69F00",
    
    "ISA" = "#56B4E9",
    
    "Human settlement" = "#009E73",
    
    "Land cover" = "#0072B2",
    
    "Landcover 190" = "#0072B2",
    
    "Landcover 61" = "#1B9E77",
    
    "Landcover 20" = "#66A61E",
    
    "Tmax" = "#D55E00",
    
    "Tmin" = "#CC79A7",
    
    "Precipitation" = "#4C78A8",
    
    "Others" = "gray75"
  )
  
  
  # ============================================================
  # 5. GET DOMINANT DRIVER OUTPUTS
  # ============================================================
  
  get_dominant_outputs <- function(
    shap_values,
    df
  ) {
    
    # ----------------------------------------------------------
    # Add station/year/location information to SHAP values
    # ----------------------------------------------------------
    
    shap_df <- as.data.frame(shap_values) %>%
      
      mutate(
        
        station_id = df$station_id,
        
        year = df$year,
        
        latitude = df$latitude,
        
        longitude = df$longitude
      )
    
    
    # ----------------------------------------------------------
    # Convert SHAP matrix to long format
    # ----------------------------------------------------------
    
    shap_long <- shap_df %>%
      
      pivot_longer(
        
        cols =
          -c(
            station_id,
            year,
            latitude,
            longitude
          ),
        
        names_to =
          "variable",
        
        values_to =
          "shap_value"
      ) %>%
      
      mutate(
        
        abs_shap =
          abs(shap_value),
        
        variable =
          clean_var_names_vec(
            variable
          )
      )
    
    
    # ----------------------------------------------------------
    # Dominant variable for EACH STATION-YEAR
    #
    # This is what the PIE CHART summarizes.
    # ----------------------------------------------------------
    
    dominant_year <- shap_long %>%
      
      group_by(
        station_id,
        year
      ) %>%
      
      slice_max(
        abs_shap,
        n = 1,
        with_ties = FALSE
      ) %>%
      
      ungroup()
    
    
    # ----------------------------------------------------------
    # Most frequently dominant variable for EACH STATION
    #
    # This determines the COLOR of the map point.
    # ----------------------------------------------------------
    
    station_map <- dominant_year %>%
      
      group_by(
        station_id,
        variable,
        latitude,
        longitude
      ) %>%
      
      summarise(
        
        n_years = n(),
        
        .groups = "drop"
      ) %>%
      
      group_by(
        station_id
      ) %>%
      
      slice_max(
        n_years,
        n = 1,
        with_ties = FALSE
      ) %>%
      
      ungroup() %>%
      
      # --------------------------------------------------------
    # Convert Years dominant into four classes
    # --------------------------------------------------------
    
    mutate(
      
      years_class = case_when(
        
        n_years <= 2 ~
          "1-2",
        
        n_years <= 4 ~
          "3-4",
        
        n_years <= 8 ~
          "5-8",
        
        n_years > 8 ~
          ">8"
      ),
      
      years_class = factor(
        
        years_class,
        
        levels = c(
          "1-2",
          "3-4",
          "5-8",
          ">8"
        )
      )
    )
    
    
    # ----------------------------------------------------------
    # PIE CHART
    #
    # Counts dominant variables across ALL station-years.
    #
    # Keep top 5 variables.
    # Everything else becomes "Others".
    # ----------------------------------------------------------
    
    pie_df <- dominant_year %>%
      
      count(
        variable,
        name = "n_years_total",
        sort = TRUE
      ) %>%
      
      mutate(
        rank = row_number()
      ) %>%
      
      mutate(
        
        variable =
          if_else(
            rank <= 5,
            variable,
            "Others"
          )
      ) %>%
      
      group_by(
        variable
      ) %>%
      
      summarise(
        
        n_years_total =
          sum(n_years_total),
        
        .groups = "drop"
      ) %>%
      
      arrange(
        desc(n_years_total)
      ) %>%
      
      mutate(
        
        prop =
          n_years_total /
          sum(n_years_total)
      )
    
    
    # ----------------------------------------------------------
    # Return
    # ----------------------------------------------------------
    
    list(
      
      station_map =
        station_map,
      
      pie_df =
        pie_df,
      
      dominant_year =
        dominant_year
    )
  }
  
  
  # ============================================================
  # 6. CREATE DOMINANT-DRIVER OBJECTS
  # ============================================================
  
  
  # ------------------------------------------------------------
  # Japanese cedar
  # ------------------------------------------------------------
  
  out_sugi_start <-
    get_dominant_outputs(
      shap_values_sugi_start,
      df_sugi_start
    )
  
  
  out_sugi_peak <-
    get_dominant_outputs(
      shap_values_sugi_peak,
      df_sugi_peak
    )
  
  
  out_sugi_amt <-
    get_dominant_outputs(
      shap_values_sugi_amt,
      df_sugi_amt
    )
  
  
  out_sugi_total <-
    get_dominant_outputs(
      shap_values_sugi_total,
      df_sugi_total
    )
  
  
  # ------------------------------------------------------------
  # Japanese cypress
  # ------------------------------------------------------------
  
  out_hinoki_start <-
    get_dominant_outputs(
      shap_values_hinoki_start,
      df_hinoki_start
    )
  
  
  out_hinoki_peak <-
    get_dominant_outputs(
      shap_values_hinoki_peak,
      df_hinoki_peak
    )
  
  
  out_hinoki_amt <-
    get_dominant_outputs(
      shap_values_hinoki_amt,
      df_hinoki_amt
    )
  
  
  out_hinoki_total <-
    get_dominant_outputs(
      shap_values_hinoki_total,
      df_hinoki_total
    )
  
  
  # ============================================================
  # 7. PIE CHART FUNCTION
  #
  # Default:
  # display percentages >= 12%
  #
  # force_label allows us to force ALAN to appear even
  # when its percentage is below 12%.
  # ============================================================
  
  make_pie_chart <- function(
    pie_df,
    force_label = NULL
  ) {
    
    pie_df2 <- pie_df %>%
      
      arrange(
        desc(variable)
      ) %>%
      
      mutate(
        
        pct =
          round(
            prop * 100
          ),
        
        label =
          ifelse(
            
            pct >= 12 |
              variable %in%
              force_label,
            
            paste0(
              pct,
              "%"
            ),
            
            ""
          ),
        
        ypos =
          cumsum(
            n_years_total
          ) -
          0.5 *
          n_years_total
      )
    
    
    ggplot(
      pie_df2,
      aes(
        x = "",
        y = n_years_total,
        fill = variable
      )
    ) +
      
      geom_col(
        
        width = 1,
        
        color = "white",
        
        linewidth = 0.3
      ) +
      
      geom_text(
        
        aes(
          y = ypos,
          label = label
        ),
        
        color = "black",
        
        size = 3,
        
        fontface = "bold"
      ) +
      
      coord_polar(
        theta = "y"
      ) +
      
      scale_fill_manual(
        
        values =
          shap_colors,
        
        drop = FALSE
      ) +
      
      theme_void() +
      
      theme(
        
        legend.position =
          "none",
        
        plot.background =
          element_blank(),
        
        panel.background =
          element_rect(
            fill = NA,
            color = NA
          ),
        
        plot.margin =
          margin(
            2,
            2,
            2,
            2
          )
      )
  }
  
  
  # ============================================================
  # 8. DOMINANT DRIVER MAP FUNCTION
  # ============================================================
  
  make_dom_map <- function(
    dom_df,
    pie_df,
    panel_label = NULL,
    force_pie_label = NULL
  ) {
    
    # ----------------------------------------------------------
    # Main map
    # ----------------------------------------------------------
    
    base_map <- ggplot() +
      
      geom_polygon(
        
        data = japan_map,
        
        aes(
          x = long,
          y = lat,
          group = group
        ),
        
        fill = "gray92",
        
        color = "gray50",
        
        linewidth = 0.25
      ) +
      
      geom_point(
        
        data = dom_df,
        
        aes(
          x = longitude,
          y = latitude,
          color = variable,
          size = years_class
        ),
        
        alpha = 0.9
      ) +
      
      # --------------------------------------------------------
    # Manual size classes
    # --------------------------------------------------------
    
    scale_size_manual(
      
      values = c(
        
        "1-2" = 1.6,
        
        "3-4" = 2.3,
        
        "5-8" = 3.1,
        
        ">8" = 4.0
      ),
      
      drop = FALSE
    ) +
      
      scale_color_manual(
        
        values =
          shap_colors,
        
        drop = FALSE
      ) +
      
      coord_fixed(
        
        xlim = c(
          128,
          146
        ),
        
        ylim = c(
          30,
          46
        )
      ) +
      
      theme_classic(
        base_size = 12
      ) +
      
      labs(
        
        x = "Longitude",
        
        y = "Latitude",
        
        tag = panel_label
      ) +
      
      theme(
        
        plot.title =
          element_blank(),
        
        legend.position =
          "none",
        
        panel.border =
          element_blank(),
        
        plot.tag =
          element_text(
            face = "bold",
            size = 14
          ),
        
        plot.tag.position =
          c(
            0.02,
            0.98
          ),
        
        plot.margin =
          margin(
            1,
            2,
            1,
            2
          )
      )
    
    
    # ----------------------------------------------------------
    # Pie chart
    # ----------------------------------------------------------
    
    pie_plot <-
      make_pie_chart(
        
        pie_df,
        
        force_label =
          force_pie_label
      )
    
    
    # ----------------------------------------------------------
    # Add pie as inset
    # ----------------------------------------------------------
    
    base_map +
      
      inset_element(
        
        pie_plot,
        
        left = 0.00,
        
        bottom = 0.55,
        
        right = 0.45,
        
        top = 1.00,
        
        align_to = "panel"
      )
  }
  
  
  # ============================================================
  # 9. CREATE ALL 8 MAP PANELS
  #
  # Layout:
  #
  # (a) Japanese cedar start
  # (b) Japanese cypress start
  #
  # (c) Japanese cedar peak
  # (d) Japanese cypress peak
  #
  # (e) Japanese cedar maximum daily pollen
  # (f) Japanese cypress maximum daily pollen
  #
  # (g) Japanese cedar total pollen concentration
  # (h) Japanese cypress total pollen concentration
  # ============================================================
  
  
  # ------------------------------------------------------------
  # Start
  # ------------------------------------------------------------
  
  p_map1 <-
    make_dom_map(
      out_sugi_start$station_map,
      out_sugi_start$pie_df,
      "(a)"
    )
  
  
  p_map5 <-
    make_dom_map(
      out_hinoki_start$station_map,
      out_hinoki_start$pie_df,
      "(b)"
    )
  
  
  # ------------------------------------------------------------
  # Peak
  # ------------------------------------------------------------
  
  # Force ALAN percentage label in panel (c)
  # because ALAN is <12% but important to the manuscript.
  
  p_map2 <-
    make_dom_map(
      out_sugi_peak$station_map,
      out_sugi_peak$pie_df,
      "(c)",
      force_pie_label = "ALAN"
    )
  
  
  p_map6 <-
    make_dom_map(
      out_hinoki_peak$station_map,
      out_hinoki_peak$pie_df,
      "(d)"
    )
  
  
  # ------------------------------------------------------------
  # Maximum daily pollen
  # ------------------------------------------------------------
  
  p_map3 <-
    make_dom_map(
      out_sugi_amt$station_map,
      out_sugi_amt$pie_df,
      "(e)"
    )
  
  
  p_map7 <-
    make_dom_map(
      out_hinoki_amt$station_map,
      out_hinoki_amt$pie_df,
      "(f)"
    )
  
  
  # ------------------------------------------------------------
  # Total pollen concentration
  # ------------------------------------------------------------
  
  p_map4 <-
    make_dom_map(
      out_sugi_total$station_map,
      out_sugi_total$pie_df,
      "(g)"
    )
  
  
  p_map8 <-
    make_dom_map(
      out_hinoki_total$station_map,
      out_hinoki_total$pie_df,
      "(h)"
    )
  
  
  # ============================================================
  # 10. COLUMN LABEL FUNCTION
  #
  # More spacing underneath Japanese cedar / cypress
  # ============================================================
  
  make_col_label <- function(label) {
    
    ggplot() +
      
      annotate(
        
        "text",
        
        x = 0.5,
        
        y = 0.68,
        
        label = label,
        
        fontface = "bold",
        
        size = 6.5
      ) +
      
      coord_cartesian(
        
        xlim = c(
          0,
          1
        ),
        
        ylim = c(
          0,
          1
        ),
        
        clip = "off"
      ) +
      
      theme_void() +
      
      theme(
        
        plot.margin =
          margin(
            t = 0,
            r = 0,
            b = 14,
            l = 0
          )
      )
  }
  
  
  # ============================================================
  # 11. ROW LABEL FUNCTION
  # ============================================================
  
  make_row_label <- function(label) {
    
    ggplot() +
      
      annotate(
        
        "text",
        
        x = 1,
        
        y = 0.5,
        
        label = label,
        
        fontface = "bold",
        
        size = 6.5,
        
        angle = 90
      ) +
      
      coord_cartesian(
        
        xlim = c(
          0,
          1
        ),
        
        ylim = c(
          0,
          1
        ),
        
        clip = "off"
      ) +
      
      theme_void() +
      
      theme(
        
        plot.margin =
          margin(
            0,
            -12,
            0,
            0
          )
      )
  }
  
  
  # ============================================================
  # 12. COLUMN + ROW LABELS
  # ============================================================
  
  blank_label <-
    ggplot() +
    theme_void()
  
  
  col_cedar <-
    make_col_label(
      "Japanese cedar"
    )
  
  
  col_cypress <-
    make_col_label(
      "Japanese cypress"
    )
  
  
  row_start <-
    make_row_label(
      "Start of Pollen Season"
    )
  
  
  row_peak <-
    make_row_label(
      "Peak of Pollen Season"
    )
  
  
  row_amt <-
    make_row_label(
      "Maximum Daily Pollen"
    )
  
  
  row_total <-
    make_row_label(
      "Total Pollen Concentration"
    )
  
  
  # ============================================================
  # 13. COLOR LEGEND
  # ============================================================
  
  color_legend_df <- tibble(
    
    variable = c(
      
      "ALAN",
      
      "ISA",
      
      "Human settlement",
      
      "Land cover",
      
      "Tmax",
      
      "Tmin",
      
      "Precipitation",
      
      "Others"
    )
  ) %>%
    
    mutate(
      
      y =
        rev(
          seq_along(
            variable
          )
        )
    )
  
  
  color_legend_plot <-
    
    ggplot(
      color_legend_df
    ) +
    
    geom_point(
      
      aes(
        x = 1,
        y = y,
        color = variable
      ),
      
      size = 5
    ) +
    
    geom_text(
      
      aes(
        x = 1.18,
        y = y,
        label = variable,
        color = variable
      ),
      
      hjust = 0,
      
      size = 4
    ) +
    
    scale_color_manual(
      
      values =
        shap_colors,
      
      guide = "none"
    ) +
    
    coord_cartesian(
      
      xlim = c(
        1,
        2.8
      ),
      
      clip = "off"
    ) +
    
    theme_void() +
    
    theme(
      
      plot.margin =
        margin(
          t = 12,
          r = 20,
          b = 10,
          l = 10
        ),
      
      plot.background =
        element_rect(
          fill = "white",
          color = NA
        ),
      
      panel.background =
        element_rect(
          fill = "white",
          color = NA
        )
    )
  
  
  # ============================================================
  # 14. YEARS DOMINANT LEGEND
  #
  # Categorical rather than continuous
  # ============================================================
  
  size_legend_df <- tibble(
    
    years_class = factor(
      
      c(
        "1-2",
        "3-4",
        "5-8",
        ">8"
      ),
      
      levels = c(
        "1-2",
        "3-4",
        "5-8",
        ">8"
      )
    ),
    
    y = c(
      4,
      3,
      2,
      1
    )
  )
  
  
  size_legend_plot <-
    
    ggplot(
      size_legend_df
    ) +
    
    geom_point(
      
      aes(
        x = 1,
        y = y,
        size = years_class
      ),
      
      color = "black"
    ) +
    
    geom_text(
      
      aes(
        x = 1.20,
        y = y,
        label = years_class
      ),
      
      hjust = 0,
      
      size = 4
    ) +
    
    scale_size_manual(
      
      values = c(
        
        "1-2" = 1.6,
        
        "3-4" = 2.3,
        
        "5-8" = 3.1,
        
        ">8" = 4.0
      ),
      
      guide = "none",
      
      drop = FALSE
    ) +
    
    coord_cartesian(
      
      xlim = c(
        1,
        2.5
      ),
      
      ylim = c(
        0.5,
        5.0
      ),
      
      clip = "off"
    ) +
    
    labs(
      title =
        "Years dominant"
    ) +
    
    theme_void() +
    
    theme(
      
      plot.title =
        element_text(
          
          face = "bold",
          
          size = 12,
          
          hjust = 0,
          
          margin =
            margin(
              b = 12
            )
        ),
      
      plot.margin =
        margin(
          t = 10,
          r = 20,
          b = 10,
          l = 10
        ),
      
      plot.background =
        element_rect(
          fill = "white",
          color = NA
        ),
      
      panel.background =
        element_rect(
          fill = "white",
          color = NA
        )
    )
  
  
  # ============================================================
  # 15. COMBINE LEGENDS
  # ============================================================
  
  legend_plot <-
    
    color_legend_plot /
    
    size_legend_plot +
    
    plot_layout(
      
      heights = c(
        1.45,
        1
      )
    )
  
  
  legend_box <-
    wrap_elements(
      full = legend_plot
    )
  
  
  # ============================================================
  # 16. MAIN 8-PANEL MAP TABLE
  # ============================================================
  
  combined_maps_table <-
    
    (
      blank_label |
        col_cedar |
        col_cypress
    ) /
    
    (
      row_start |
        p_map1 |
        p_map5
    ) /
    
    (
      row_peak |
        p_map2 |
        p_map6
    ) /
    
    (
      row_amt |
        p_map3 |
        p_map7
    ) /
    
    (
      row_total |
        p_map4 |
        p_map8
    ) +
    
    plot_layout(
      
      widths = c(
        0.08,
        1,
        1
      ),
      
      heights = c(
        0.13,
        1,
        1,
        1,
        1
      )
    )
  
  
  # ============================================================
  # 17. RIGHT-SIDE LEGEND AREA
  #
  # Blank spacer ABOVE legend prevents clipping.
  # Other spacers keep legend concentrated near the top.
  # ============================================================
  
  right_side <-
    
    plot_spacer() /
    
    legend_box /
    
    plot_spacer() /
    
    plot_spacer() /
    
    plot_spacer() +
    
    plot_layout(
      
      heights = c(
        
        0.16,
        
        1.30,
        
        0.95,
        
        0.95,
        
        0.75
      )
    )
  
  
  # ============================================================
  # 18. FINAL FIGURE 4
  # ============================================================
  
  Figure4 <-
    
    (
      combined_maps_table |
        right_side
    ) +
    
    plot_layout(
      
      widths = c(
        4,
        1.15
      )
    )
  
  
  # ============================================================
  # 19. DISPLAY
  # ============================================================
  
  Figure4
  
  
  # ============================================================
  # 20. SAVE
  # ============================================================
  
  ggsave(
    
    "Figure4_Dominant_SHAP_Driver_Maps.pdf",
    
    Figure4,
    
    width = 15,
    
    height = 14,
    
    dpi = 300
  )
  
  
  ggsave(
    
    "Figure4_Dominant_SHAP_Driver_Maps.png",
    
    Figure4,
    
    width = 15,
    
    height = 14,
    
    dpi = 300
  )
  
})
local({
  
  
  # ====================================================================
  # PART 4 — SUPPLEMENTARY FIGURE S2
  # ====================================================================
  # 2. LOAD + CLEAN MASTER DATA
  # ============================================================
  
  dat <- read_excel(
    "Merged_files_annual.xlsx"
  ) %>%
    clean_names()
  
  
  dat <- dat %>%
    select(
      -any_of(
        c(
          "x22",
          "...22"
        )
      )
    )
  
  
  # Clean character missing values
  dat2 <- dat %>%
    
    mutate(
      across(
        where(is.character),
        ~ na_if(.x, "NA")
      )
    ) %>%
    
    mutate(
      across(
        where(is.character),
        ~ na_if(.x, "")
      )
    ) %>%
    
    mutate(
      across(
        where(is.character),
        ~ na_if(.x, "NaN")
      )
    )
  
  
  # ============================================================
  # 3. CONVERT NUMERIC VARIABLES
  # ============================================================
  
  num_cols <- c(
    
    "alan",
    "isa",
    "isa_year",
    
    "latitude",
    "longitude",
    
    "start_mean_tmax",
    "start_mean_tmin",
    "start_total_precip",
    
    "peak_mean_tmax",
    "peak_mean_tmin",
    "peak_total_precip",
    
    "end_mean_tmax",
    "end_mean_tmin",
    "end_total_precip",
    
    "annual_mean_tmax_c",
    "annual_mean_tmin",
    "annual_mean_monthly_precip_mm",
    
    "max_sugi",
    "max_hinoki",
    
    "sum_sugi",
    "sum_hinoki"
  )
  
  
  dat2 <- dat2 %>%
    
    mutate(
      across(
        any_of(num_cols),
        ~ parse_number(
          as.character(.x)
        )
      )
    ) %>%
    
    mutate(
      
      year = as.integer(year),
      
      station_id = as.factor(
        station_id
      ),
      
      landcover = as.factor(
        landcover
      ),
      
      humansettlement = as.factor(
        humansettlement
      )
    )
  
  
  # ============================================================
  # 4. REBUILD CYPRESS / HINOKI DOY IF NECESSARY
  # ============================================================
  
  if (!is.numeric(dat2$dis_hinoki_doy)) {
    
    dat2 <- dat2 %>%
      mutate(
        
        dis_hinoki_doy =
          yday(
            as.Date(
              as.numeric(dis_hinoki),
              origin = "1899-12-30"
            )
          )
      )
  }
  
  
  if (!is.numeric(dat2$maxdis_hinoki_doy)) {
    
    dat2 <- dat2 %>%
      mutate(
        
        maxdis_hinoki_doy =
          yday(
            as.Date(
              as.numeric(maxdis_hinoki),
              origin = "1899-12-30"
            )
          )
      )
  }
  
  
  # Validate Cypress DOY
  dat2 <- dat2 %>%
    
    mutate(
      
      dis_hinoki_doy = ifelse(
        dis_hinoki_doy >= 1 &
          dis_hinoki_doy <= 366,
        dis_hinoki_doy,
        NA
      ),
      
      maxdis_hinoki_doy = ifelse(
        maxdis_hinoki_doy >= 1 &
          maxdis_hinoki_doy <= 366,
        maxdis_hinoki_doy,
        NA
      )
    )
  
  
  # ============================================================
  # 5. BUILD 8 ANALYSIS DATASETS
  # ============================================================
  
  
  # ============================================================
  # 5.1 JAPANESE CEDAR - START OF POLLEN SEASON
  # ============================================================
  
  df_sugi_start <- dat2 %>%
    
    transmute(
      
      station_id,
      year,
      
      y = dis_sugi_doy,
      
      latitude,
      longitude,
      
      alan,
      isa,
      humansettlement,
      landcover,
      
      start_mean_tmax,
      start_mean_tmin,
      start_total_precip
    ) %>%
    
    filter(
      
      !is.na(y),
      y >= 1,
      y <= 366,
      
      !is.na(alan),
      
      !is.na(start_mean_tmax),
      !is.na(start_mean_tmin),
      !is.na(start_total_precip)
    )
  
  
  # ============================================================
  # 5.2 JAPANESE CEDAR - PEAK OF POLLEN SEASON
  # ============================================================
  
  df_sugi_peak <- dat2 %>%
    
    transmute(
      
      station_id,
      year,
      
      y = maxdis_sugi_doy,
      
      latitude,
      longitude,
      
      alan,
      isa,
      humansettlement,
      landcover,
      
      peak_mean_tmax,
      peak_mean_tmin,
      peak_total_precip
    ) %>%
    
    filter(
      
      !is.na(y),
      y >= 1,
      y <= 366,
      
      !is.na(alan),
      
      !is.na(peak_mean_tmax),
      !is.na(peak_mean_tmin),
      !is.na(peak_total_precip)
    )
  
  
  # ============================================================
  # 5.3 JAPANESE CEDAR - MAXIMUM DAILY POLLEN
  # ============================================================
  
  df_sugi_amt <- dat2 %>%
    
    transmute(
      
      station_id,
      year,
      
      peak_amt = max_sugi,
      
      latitude,
      longitude,
      
      alan,
      isa,
      humansettlement,
      landcover,
      
      peak_mean_tmax,
      peak_mean_tmin,
      peak_total_precip
    ) %>%
    
    filter(
      
      !is.na(peak_amt),
      peak_amt > 0,
      
      !is.na(alan),
      
      !is.na(peak_mean_tmax),
      !is.na(peak_mean_tmin),
      !is.na(peak_total_precip)
    ) %>%
    
    mutate(
      y = log1p(peak_amt)
    ) %>%
    
    select(
      -peak_amt
    )
  
  
  # ============================================================
  # 5.4 JAPANESE CEDAR - TOTAL POLLEN CONCENTRATION
  #
  # Uses annual meteorological variables
  # ============================================================
  
  df_sugi_total <- dat2 %>%
    
    transmute(
      
      station_id,
      year,
      
      total_amt = sum_sugi,
      
      latitude,
      longitude,
      
      alan,
      isa,
      humansettlement,
      landcover,
      
      annual_mean_tmax_c,
      annual_mean_tmin,
      annual_mean_monthly_precip_mm
    ) %>%
    
    filter(
      
      !is.na(total_amt),
      total_amt > 0,
      
      !is.na(alan),
      
      !is.na(annual_mean_tmax_c),
      !is.na(annual_mean_tmin),
      !is.na(annual_mean_monthly_precip_mm)
    ) %>%
    
    mutate(
      y = log1p(total_amt)
    ) %>%
    
    select(
      -total_amt
    )
  
  
  # ============================================================
  # 5.5 JAPANESE CYPRESS - START OF POLLEN SEASON
  # ============================================================
  
  df_hinoki_start <- dat2 %>%
    
    transmute(
      
      station_id,
      year,
      
      y = dis_hinoki_doy,
      
      latitude,
      longitude,
      
      alan,
      isa,
      humansettlement,
      landcover,
      
      start_mean_tmax,
      start_mean_tmin,
      start_total_precip
    ) %>%
    
    filter(
      
      !is.na(y),
      y >= 1,
      y <= 366,
      
      !is.na(alan),
      
      !is.na(start_mean_tmax),
      !is.na(start_mean_tmin),
      !is.na(start_total_precip)
    )
  
  
  # ============================================================
  # 5.6 JAPANESE CYPRESS - PEAK OF POLLEN SEASON
  # ============================================================
  
  df_hinoki_peak <- dat2 %>%
    
    transmute(
      
      station_id,
      year,
      
      y = maxdis_hinoki_doy,
      
      latitude,
      longitude,
      
      alan,
      isa,
      humansettlement,
      landcover,
      
      peak_mean_tmax,
      peak_mean_tmin,
      peak_total_precip
    ) %>%
    
    filter(
      
      !is.na(y),
      y >= 1,
      y <= 366,
      
      !is.na(alan),
      
      !is.na(peak_mean_tmax),
      !is.na(peak_mean_tmin),
      !is.na(peak_total_precip)
    )
  
  
  # ============================================================
  # 5.7 JAPANESE CYPRESS - MAXIMUM DAILY POLLEN
  # ============================================================
  
  df_hinoki_amt <- dat2 %>%
    
    transmute(
      
      station_id,
      year,
      
      peak_amt = max_hinoki,
      
      latitude,
      longitude,
      
      alan,
      isa,
      humansettlement,
      landcover,
      
      peak_mean_tmax,
      peak_mean_tmin,
      peak_total_precip
    ) %>%
    
    filter(
      
      !is.na(peak_amt),
      peak_amt > 0,
      
      !is.na(alan),
      
      !is.na(peak_mean_tmax),
      !is.na(peak_mean_tmin),
      !is.na(peak_total_precip)
    ) %>%
    
    mutate(
      y = log1p(peak_amt)
    ) %>%
    
    select(
      -peak_amt
    )
  
  
  # ============================================================
  # 5.8 JAPANESE CYPRESS - TOTAL POLLEN CONCENTRATION
  #
  # Uses annual meteorological variables
  # ============================================================
  
  df_hinoki_total <- dat2 %>%
    
    transmute(
      
      station_id,
      year,
      
      total_amt = sum_hinoki,
      
      latitude,
      longitude,
      
      alan,
      isa,
      humansettlement,
      landcover,
      
      annual_mean_tmax_c,
      annual_mean_tmin,
      annual_mean_monthly_precip_mm
    ) %>%
    
    filter(
      
      !is.na(total_amt),
      total_amt > 0,
      
      !is.na(alan),
      
      !is.na(annual_mean_tmax_c),
      !is.na(annual_mean_tmin),
      !is.na(annual_mean_monthly_precip_mm)
    ) %>%
    
    mutate(
      y = log1p(total_amt)
    ) %>%
    
    select(
      -total_amt
    )
  
  
  # ============================================================
  # 6. XGBOOST MODEL
  # ============================================================
  
  xgb_model <- boost_tree(
    
    trees = 800,
    
    tree_depth = 6,
    
    learn_rate = 0.05,
    
    loss_reduction = 0,
    
    min_n = 10
    
  ) %>%
    
    set_engine(
      "xgboost"
    ) %>%
    
    set_mode(
      "regression"
    )
  
  
  # ============================================================
  # 7. GROUPED 10-FOLD CROSS-VALIDATION FUNCTION
  #
  # station_id is used as the grouping variable so observations
  # from the same monitoring station remain in the same fold.
  # ============================================================
  
  fit_xgb_pipeline <- function(df) {
    
    rec <- recipe(
      y ~ .,
      data = df
    ) %>%
      
      update_role(
        station_id,
        year,
        latitude,
        longitude,
        new_role = "id"
      ) %>%
      
      step_unknown(
        all_nominal_predictors()
      ) %>%
      
      step_dummy(
        all_nominal_predictors()
      ) %>%
      
      step_zv(
        all_predictors()
      ) %>%
      
      step_normalize(
        all_numeric_predictors()
      )
    
    
    wf <- workflow() %>%
      
      add_recipe(
        rec
      ) %>%
      
      add_model(
        xgb_model
      )
    
    
    folds <- group_vfold_cv(
      df,
      group = station_id,
      v = 10
    )
    
    
    cv_results <- fit_resamples(
      
      wf,
      
      resamples = folds,
      
      metrics = yardstick::metric_set(
        yardstick::rmse,
        yardstick::rsq,
        yardstick::mae
      ),
      
      control = control_resamples(
        save_pred = TRUE
      )
    )
    
    
    list(
      recipe = rec,
      workflow = wf,
      folds = folds,
      cv_results = cv_results
    )
  }
  
  
  # ============================================================
  # 8. RUN CROSS-VALIDATION FOR ALL 8 MODELS
  # ============================================================
  
  set.seed(123)
  
  
  mod_sugi_start <- fit_xgb_pipeline(
    df_sugi_start
  )
  
  mod_sugi_peak <- fit_xgb_pipeline(
    df_sugi_peak
  )
  
  mod_sugi_amt <- fit_xgb_pipeline(
    df_sugi_amt
  )
  
  mod_sugi_total <- fit_xgb_pipeline(
    df_sugi_total
  )
  
  
  mod_hinoki_start <- fit_xgb_pipeline(
    df_hinoki_start
  )
  
  mod_hinoki_peak <- fit_xgb_pipeline(
    df_hinoki_peak
  )
  
  mod_hinoki_amt <- fit_xgb_pipeline(
    df_hinoki_amt
  )
  
  mod_hinoki_total <- fit_xgb_pipeline(
    df_hinoki_total
  )
  
  
  # ============================================================
  # 9. EXTRACT CROSS-VALIDATED PREDICTIONS
  # ============================================================
  
  preds_sugi_start <-
    collect_predictions(
      mod_sugi_start$cv_results
    )
  
  preds_sugi_peak <-
    collect_predictions(
      mod_sugi_peak$cv_results
    )
  
  preds_sugi_amt <-
    collect_predictions(
      mod_sugi_amt$cv_results
    )
  
  preds_sugi_total <-
    collect_predictions(
      mod_sugi_total$cv_results
    )
  
  
  preds_hinoki_start <-
    collect_predictions(
      mod_hinoki_start$cv_results
    )
  
  preds_hinoki_peak <-
    collect_predictions(
      mod_hinoki_peak$cv_results
    )
  
  preds_hinoki_amt <-
    collect_predictions(
      mod_hinoki_amt$cv_results
    )
  
  preds_hinoki_total <-
    collect_predictions(
      mod_hinoki_total$cv_results
    )
  
  
  # ============================================================
  # 10. EXTRACT CROSS-VALIDATION METRICS
  # ============================================================
  
  metrics_sugi_start <-
    collect_metrics(
      mod_sugi_start$cv_results
    )
  
  metrics_sugi_peak <-
    collect_metrics(
      mod_sugi_peak$cv_results
    )
  
  metrics_sugi_amt <-
    collect_metrics(
      mod_sugi_amt$cv_results
    )
  
  metrics_sugi_total <-
    collect_metrics(
      mod_sugi_total$cv_results
    )
  
  
  metrics_hinoki_start <-
    collect_metrics(
      mod_hinoki_start$cv_results
    )
  
  metrics_hinoki_peak <-
    collect_metrics(
      mod_hinoki_peak$cv_results
    )
  
  metrics_hinoki_amt <-
    collect_metrics(
      mod_hinoki_amt$cv_results
    )
  
  metrics_hinoki_total <-
    collect_metrics(
      mod_hinoki_total$cv_results
    )
  
  
  # ============================================================
  # 11. PANEL STATISTICS
  #
  # RMSE / R² / MAE come from grouped cross-validation.
  # Slope is calculated from pooled cross-validated predictions.
  # ============================================================
  
  get_panel_stats <- function(
    preds_df,
    metrics_df
  ) {
    
    rmse_val <- metrics_df %>%
      filter(
        .metric == "rmse"
      ) %>%
      pull(mean)
    
    
    rsq_val <- metrics_df %>%
      filter(
        .metric == "rsq"
      ) %>%
      pull(mean)
    
    
    mae_val <- metrics_df %>%
      filter(
        .metric == "mae"
      ) %>%
      pull(mean)
    
    
    slope_val <- coef(
      lm(
        .pred ~ y,
        data = preds_df
      )
    )[2]
    
    
    n_val <- nrow(
      preds_df
    )
    
    
    list(
      rmse = rmse_val,
      rsq = rsq_val,
      mae = mae_val,
      slope = slope_val,
      n = n_val
    )
  }
  
  
  # ============================================================
  # 12. SHARED X/Y LIMITS
  #
  # Cedar and cypress use the same limits within each metric.
  # ============================================================
  
  get_shared_limits <- function(...) {
    
    vals <- c(...)
    
    vals <- vals[
      is.finite(vals)
    ]
    
    
    min_val <- min(
      vals,
      na.rm = TRUE
    )
    
    
    max_val <- max(
      vals,
      na.rm = TRUE
    )
    
    
    range_val <-
      max_val -
      min_val
    
    
    # 8% padding
    lower <-
      min_val -
      0.08 *
      range_val
    
    
    upper <-
      max_val +
      0.08 *
      range_val
    
    
    c(
      lower,
      upper
    )
  }
  
  
  # Start
  start_lims <- get_shared_limits(
    
    preds_sugi_start$y,
    preds_sugi_start$.pred,
    
    preds_hinoki_start$y,
    preds_hinoki_start$.pred
  )
  
  
  # Peak
  peak_lims <- get_shared_limits(
    
    preds_sugi_peak$y,
    preds_sugi_peak$.pred,
    
    preds_hinoki_peak$y,
    preds_hinoki_peak$.pred
  )
  
  
  # Maximum daily pollen
  amt_lims <- get_shared_limits(
    
    preds_sugi_amt$y,
    preds_sugi_amt$.pred,
    
    preds_hinoki_amt$y,
    preds_hinoki_amt$.pred
  )
  
  
  # Total pollen concentration
  total_lims <- get_shared_limits(
    
    preds_sugi_total$y,
    preds_sugi_total$.pred,
    
    preds_hinoki_total$y,
    preds_hinoki_total$.pred
  )
  
  
  # ============================================================
  # 13. OBSERVED VS. PREDICTED PANEL FUNCTION
  #
  # Smaller + more transparent points reduce overplotting.
  # ============================================================
  
  make_cv_plot <- function(
    preds_df,
    metrics_df,
    lims,
    xlab,
    ylab
  ) {
    
    stats <- get_panel_stats(
      preds_df,
      metrics_df
    )
    
    
    ann_text <- paste0(
      
      "RMSE = ",
      round(
        stats$rmse,
        2
      ),
      "\n",
      
      "R² = ",
      round(
        stats$rsq,
        2
      ),
      "\n",
      
      "Slope = ",
      round(
        stats$slope,
        2
      ),
      "\n",
      
      "n = ",
      stats$n,
      "\n",
      
      "MAE = ",
      round(
        stats$mae,
        2
      )
    )
    
    
    range_val <- diff(
      lims
    )
    
    
    ggplot(
      preds_df,
      aes(
        x = y,
        y = .pred
      )
    ) +
      
      # --------------------------------------------------------
    # UPDATED FOR OVERPLOTTING
    # --------------------------------------------------------
    
    geom_point(
      size = 0.4,
      alpha = 0.20,
      color = "gray25"
    ) +
      
      
      # 1:1 reference line
      geom_abline(
        slope = 1,
        intercept = 0,
        color = "black",
        linewidth = 0.8
      ) +
      
      
      # Statistics
      annotate(
        
        "text",
        
        x =
          lims[2] -
          0.05 *
          range_val,
        
        y =
          lims[1] +
          0.08 *
          range_val,
        
        label = ann_text,
        
        hjust = 1,
        
        vjust = 0,
        
        size = 3.2
      ) +
      
      
      coord_fixed(
        
        ratio = 1,
        
        xlim = lims,
        
        ylim = lims
      ) +
      
      
      theme_classic(
        base_size = 12
      ) +
      
      
      labs(
        x = xlab,
        y = ylab
      ) +
      
      
      theme(
        
        plot.title =
          element_blank(),
        
        panel.grid.major =
          element_blank(),
        
        panel.grid.minor =
          element_blank(),
        
        axis.line =
          element_line(
            color = "black",
            linewidth = 0.8
          )
      )
  }
  
  
  # ============================================================
  # 14. CREATE 8 PANELS
  # ============================================================
  
  
  # ------------------------------------------------------------
  # (a) Japanese cedar - Start
  # ------------------------------------------------------------
  
  p_cv1 <- make_cv_plot(
    
    preds_sugi_start,
    metrics_sugi_start,
    
    start_lims,
    
    "Observed DOY",
    
    "Predicted DOY"
  )
  
  
  # ------------------------------------------------------------
  # (b) Japanese cypress - Start
  # ------------------------------------------------------------
  
  p_cv5 <- make_cv_plot(
    
    preds_hinoki_start,
    metrics_hinoki_start,
    
    start_lims,
    
    "Observed DOY",
    
    "Predicted DOY"
  )
  
  
  # ------------------------------------------------------------
  # (c) Japanese cedar - Peak
  # ------------------------------------------------------------
  
  p_cv2 <- make_cv_plot(
    
    preds_sugi_peak,
    metrics_sugi_peak,
    
    peak_lims,
    
    "Observed DOY",
    
    "Predicted DOY"
  )
  
  
  # ------------------------------------------------------------
  # (d) Japanese cypress - Peak
  # ------------------------------------------------------------
  
  p_cv6 <- make_cv_plot(
    
    preds_hinoki_peak,
    metrics_hinoki_peak,
    
    peak_lims,
    
    "Observed DOY",
    
    "Predicted DOY"
  )
  
  
  # ------------------------------------------------------------
  # (e) Japanese cedar - Maximum Daily Pollen
  # ------------------------------------------------------------
  
  p_cv3 <- make_cv_plot(
    
    preds_sugi_amt,
    metrics_sugi_amt,
    
    amt_lims,
    
    "Observed log maximum daily pollen",
    
    "Predicted log maximum daily pollen"
  )
  
  
  # ------------------------------------------------------------
  # (f) Japanese cypress - Maximum Daily Pollen
  # ------------------------------------------------------------
  
  p_cv7 <- make_cv_plot(
    
    preds_hinoki_amt,
    metrics_hinoki_amt,
    
    amt_lims,
    
    "Observed log maximum daily pollen",
    
    "Predicted log maximum daily pollen"
  )
  
  
  # ------------------------------------------------------------
  # (g) Japanese cedar - Total Pollen Concentration
  # ------------------------------------------------------------
  
  p_cv4 <- make_cv_plot(
    
    preds_sugi_total,
    metrics_sugi_total,
    
    total_lims,
    
    "Observed log total pollen",
    
    "Predicted log total pollen"
  )
  
  
  # ------------------------------------------------------------
  # (h) Japanese cypress - Total Pollen Concentration
  # ------------------------------------------------------------
  
  p_cv8 <- make_cv_plot(
    
    preds_hinoki_total,
    metrics_hinoki_total,
    
    total_lims,
    
    "Observed log total pollen",
    
    "Predicted log total pollen"
  )
  
  
  # ============================================================
  # 15. COLUMN LABEL FUNCTION
  # ============================================================
  
  make_col_label <- function(label) {
    
    ggplot() +
      
      annotate(
        
        "text",
        
        x = 0.5,
        y = 0.5,
        
        label = label,
        
        fontface = "bold",
        
        size = 6.5
      ) +
      
      theme_void()
  }
  
  
  # ============================================================
  # 16. ROW LABEL FUNCTION
  # ============================================================
  
  make_row_label <- function(label) {
    
    ggplot() +
      
      annotate(
        
        "text",
        
        x = 1,
        y = 0.5,
        
        label = label,
        
        fontface = "bold",
        
        size = 6.5,
        
        angle = 90
      ) +
      
      
      coord_cartesian(
        
        xlim = c(
          0,
          1
        ),
        
        ylim = c(
          0,
          1
        ),
        
        clip = "off"
      ) +
      
      
      theme_void() +
      
      
      theme(
        
        plot.margin =
          margin(
            0,
            -12,
            0,
            0
          )
      )
  }
  
  
  # ============================================================
  # 17. CREATE HEADERS
  # ============================================================
  
  blank_label <-
    ggplot() +
    theme_void()
  
  
  col_cedar <-
    make_col_label(
      "Japanese cedar"
    )
  
  
  col_cypress <-
    make_col_label(
      "Japanese cypress"
    )
  
  
  row_start <-
    make_row_label(
      "Start of Pollen Season"
    )
  
  
  row_peak <-
    make_row_label(
      "Peak of Pollen Season"
    )
  
  
  row_amt <-
    make_row_label(
      "Maximum Daily Pollen"
    )
  
  
  row_total <-
    make_row_label(
      "Total Pollen Concentration"
    )
  
  
  # ============================================================
  # 18. COMBINE FIGURE S2
  # ============================================================
  
  FigureS2 <-
    
    (
      blank_label |
        col_cedar |
        col_cypress
    ) /
    
    (
      row_start |
        p_cv1 |
        p_cv5
    ) /
    
    (
      row_peak |
        p_cv2 |
        p_cv6
    ) /
    
    (
      row_amt |
        p_cv3 |
        p_cv7
    ) /
    
    (
      row_total |
        p_cv4 |
        p_cv8
    ) +
    
    plot_layout(
      
      widths = c(
        0.08,
        1,
        1
      ),
      
      heights = c(
        0.10,
        1,
        1,
        1,
        1
      )
    )
  
  
  # ============================================================
  # 19. DISPLAY
  # ============================================================
  
  FigureS2
  
  
  # ============================================================
  # 20. SAVE FIGURE S2
  # ============================================================
  
  ggsave(
    
    "FigureS2_CrossValidated_Predictions.pdf",
    
    FigureS2,
    
    width = 12,
    
    height = 18,
    
    dpi = 300
  )
  
  
  ggsave(
    
    "FigureS2_CrossValidated_Predictions.png",
    
    FigureS2,
    
    width = 12,
    
    height = 18,
    
    dpi = 300
  )
  
})
