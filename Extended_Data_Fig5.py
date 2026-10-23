# ============================================================
# TRUE SHAP LAYERED VIOLIN — TOP 5 BY MEAN ABSOLUTE SHAP
# 8 PANELS
#
# Japanese cedar + Japanese cypress
#
# TOP 5:
# Calculated separately for each model using
# mean absolute SHAP value.
#
# Layered violin:
# low feature values  = blue
# high feature values = red
#
# ============================================================


# ============================================================
# 1. PACKAGES
# ============================================================

import os
import numpy as np
import pandas as pd
import matplotlib.pyplot as plt

from scipy.stats import gaussian_kde


# ============================================================
# 2. WORKING DIRECTORY
# ============================================================

os.chdir(
    "/Users/arisaokamoto/Library/CloudStorage/OneDrive-Vanderbilt/"
    "Meng, Lin's files - 1_JapanPollen/2_CleanData/Merged"
)

print("Working directory:")
print(os.getcwd())


# ============================================================
# 3. FILES
# ============================================================

files = [
    "SHAP_Cedar_Start.csv",
    "SHAP_Cypress_Start.csv",
    "SHAP_Cedar_Peak.csv",
    "SHAP_Cypress_Peak.csv",
    "SHAP_Cedar_Max.csv",
    "SHAP_Cypress_Max.csv",
    "SHAP_Cedar_Total.csv",
    "SHAP_Cypress_Total.csv"
]


# ============================================================
# 4. CHECK FILES
# ============================================================

for f in files:

    if not os.path.exists(f):

        raise FileNotFoundError(
            f"Missing file: {f}"
        )

    print("FOUND:", f)


# ============================================================
# 5. LOAD DATA
# ============================================================

df = pd.concat(
    [pd.read_csv(f) for f in files],
    ignore_index=True
)


df["SHAP_value"] = pd.to_numeric(
    df["SHAP_value"],
    errors="coerce"
)


print("\nRows:", len(df))

print("\nColumns:")
print(df.columns.tolist())


# ============================================================
# 6. CLEAN FEATURE NAMES
# ============================================================

feature_names = {

    "alan":
        "ALAN",

    "isa":
        "ISA",

    "humansettlement":
        "Human settlement",

    "landcover":
        "Land cover",


    # Start-season climate
    "start_mean_tmax":
        "Tmax",

    "start_mean_tmin":
        "Tmin",

    "start_total_precip":
        "Precipitation",


    # Peak-season climate
    "peak_mean_tmax":
        "Tmax",

    "peak_mean_tmin":
        "Tmin",

    "peak_total_precip":
        "Precipitation",


    # Annual climate
    "annual_mean_tmax_c":
        "Tmax",

    "annual_mean_tmin":
        "Tmin",

    "annual_mean_monthly_precip_mm":
        "Precipitation"
}


df["Feature_label"] = (
    df["Feature"]
    .replace(feature_names)
)


# ============================================================
# 7. CONVERT FEATURE VALUES FOR COLORING
# ============================================================

def convert_feature_values(values):

    # --------------------------------------------------------
    # Try numeric first
    # --------------------------------------------------------

    numeric = pd.to_numeric(
        values,
        errors="coerce"
    )


    if numeric.notna().mean() >= 0.8:

        return numeric.to_numpy(
            dtype=float
        )


    # --------------------------------------------------------
    # Otherwise treat as categorical
    # --------------------------------------------------------

    text = values.astype(str)


    levels = sorted(
        text.dropna().unique()
    )


    mapping = {
        level: i
        for i, level in enumerate(levels)
    }


    return (
        text
        .map(mapping)
        .to_numpy(dtype=float)
    )


# ============================================================
# 8. FUNCTION:
# GET TOP 5 FEATURES BY MEAN ABSOLUTE SHAP
# ============================================================

def get_top_features(
    panel_data,
    n_features=5
):

    importance = (

        panel_data

        .groupby(
            "Feature_label"
        )["SHAP_value"]

        .apply(
            lambda x:
            np.nanmean(
                np.abs(x)
            )
        )

        .sort_values(
            ascending=False
        )

    )


    top_features = (
        importance
        .head(n_features)
        .index
        .tolist()
    )


    return top_features, importance


# ============================================================
# 9. TRUE LAYERED VIOLIN FUNCTION
# ============================================================

def layered_violin(
    ax,
    panel_data,
    feature_order,
    title,
    panel_letter,
    n_bins=20
):

    panel_data = panel_data.copy()


    # --------------------------------------------------------
    # SAMPLE SIZE
    # --------------------------------------------------------

    n = (
        panel_data["row_id"]
        .nunique()
    )


    # --------------------------------------------------------
    # FULL PANEL SHAP RANGE
    #
    # Uses the full model range to preserve the appearance
    # of the previous figure.
    # --------------------------------------------------------

    shap_all = (
        panel_data["SHAP_value"]
        .dropna()
        .to_numpy(dtype=float)
    )


    max_abs = np.max(
        np.abs(shap_all)
    )


    max_abs *= 1.04


    xmin = -max_abs
    xmax = max_abs


    # --------------------------------------------------------
    # SHARED X POSITIONS
    # --------------------------------------------------------

    x_points = np.linspace(
        xmin,
        xmax,
        300
    )


    # --------------------------------------------------------
    # Y POSITIONS
    #
    # Most important variable appears on top.
    # --------------------------------------------------------

    features_top_to_bottom = (
        feature_order
    )


    y_positions = np.arange(
        len(features_top_to_bottom)
    )[::-1]


    # --------------------------------------------------------
    # BLUE -> RED COLOR PALETTE
    # --------------------------------------------------------

    cmap = plt.get_cmap(
        "coolwarm"
    )


    # ========================================================
    # LOOP THROUGH FEATURES
    # ========================================================

    for y_pos, feature in zip(
        y_positions,
        features_top_to_bottom
    ):


        sub = panel_data[
            panel_data["Feature_label"]
            ==
            feature
        ].copy()


        if len(sub) == 0:
            continue


        sub = sub.dropna(
            subset=[
                "SHAP_value"
            ]
        )


        shap_values = (
            sub["SHAP_value"]
            .to_numpy(dtype=float)
        )


        feature_values = (
            convert_feature_values(
                sub["feature_value"]
            )
        )


        valid = (
            np.isfinite(shap_values)
            &
            np.isfinite(feature_values)
        )


        shap_values = (
            shap_values[valid]
        )


        feature_values = (
            feature_values[valid]
        )


        if len(shap_values) < 5:
            continue


        # ----------------------------------------------------
        # HORIZONTAL BASELINE
        # ----------------------------------------------------

        ax.hlines(
            y=y_pos,
            xmin=xmin,
            xmax=xmax,
            color="#4056d9",
            linewidth=0.7,
            alpha=0.9,
            zorder=0
        )


        # ====================================================
        # SORT BY FEATURE VALUE
        # ====================================================

        order = np.argsort(
            feature_values
        )


        feature_sorted = (
            feature_values[order]
        )


        shap_sorted = (
            shap_values[order]
        )


        # ====================================================
        # DEFINE FEATURE-VALUE BINS
        # ====================================================

        unique_values, counts = np.unique(
            feature_sorted,
            return_counts=True
        )


        # ----------------------------------------------------
        # Categorical / few unique values
        # ----------------------------------------------------

        if len(unique_values) <= n_bins:

            bin_edges = np.cumsum(
                counts
            )


            bin_edges = np.insert(
                bin_edges,
                0,
                0
            )


        # ----------------------------------------------------
        # Continuous variable
        # ----------------------------------------------------

        else:

            bin_edges = (

                np.linspace(
                    0,
                    len(feature_sorted),
                    n_bins + 1
                )

                .round()

                .astype(int)
            )


        nbins = (
            len(bin_edges) - 1
        )


        # ====================================================
        # KDE FOR EACH FEATURE-VALUE BIN
        # ====================================================

        densities = np.zeros(
            (
                nbins,
                len(x_points)
            )
        )


        for i in range(nbins):


            start = bin_edges[i]

            end = bin_edges[i + 1]


            shaps_bin = (
                shap_sorted[
                    start:end
                ]
            )


            # ------------------------------------------------
            # Too few observations
            # ------------------------------------------------

            if len(shaps_bin) < 2:

                if i > 0:

                    densities[i, :] = (
                        densities[
                            i - 1,
                            :
                        ]
                    )

                continue


            # ------------------------------------------------
            # Tiny jitter prevents singular KDE
            # ------------------------------------------------

            jitter = np.random.default_rng(
                123 + i
            ).normal(
                0,
                0.001,
                len(shaps_bin)
            )


            try:

                kde = gaussian_kde(
                    shaps_bin
                    +
                    jitter
                )


                density = kde(
                    x_points
                )


            except Exception:

                continue


            # ------------------------------------------------
            # Scale by number of observations
            # ------------------------------------------------

            size = (
                end - start
            )


            expected_size = (
                len(feature_sorted)
                /
                nbins
            )


            relative_size = (
                size
                /
                expected_size
            )


            density *= (
                relative_size
            )


            densities[
                i,
                :
            ] = density


        # ====================================================
        # CUMULATIVE DENSITY
        #
        # This creates the layered SHAP violin.
        # ====================================================

        densities = np.cumsum(
            densities,
            axis=0
        )


        if densities.max() <= 0:
            continue


        # ----------------------------------------------------
        # VIOLIN WIDTH
        # ----------------------------------------------------

        violin_width = 0.64


        scale = (
            densities.max()
            *
            2
            /
            violin_width
        )


        # ====================================================
        # DRAW LAYERS
        #
        # High feature values = red
        # Low feature values  = blue
        # ====================================================

        for i in range(
            nbins - 1,
            -1,
            -1
        ):


            y_width = (
                densities[i, :]
                /
                scale
            )


            if nbins == 1:

                color_fraction = 0.5


            else:

                color_fraction = (
                    i
                    /
                    (nbins - 1)
                )


            color = cmap(
                color_fraction
            )


            ax.fill_between(

                x_points,

                y_pos - y_width,

                y_pos + y_width,

                facecolor=color,

                edgecolor="none",

                zorder=2
            )


    # ========================================================
    # ZERO SHAP LINE
    # ========================================================

    ax.axvline(
        0,
        color="0.55",
        linewidth=0.8,
        zorder=5
    )


    # ========================================================
    # X LIMITS
    # ========================================================

    ax.set_xlim(
        xmin,
        xmax
    )


    # ========================================================
    # Y LIMITS
    # ========================================================

    ax.set_ylim(
        -0.65,
        len(feature_order) - 0.35
    )


    # ========================================================
    # Y-TICKS
    # ========================================================

    ax.set_yticks(
        y_positions
    )


    # ========================================================
    # LARGE FEATURE LABELS
    # ========================================================

    ax.set_yticklabels(
        features_top_to_bottom,
        fontsize=11
    )


    ax.tick_params(
        axis="y",
        length=0
    )


    # ========================================================
    # X-TICK SIZE
    # ========================================================

    ax.tick_params(
        axis="x",
        labelsize=8
    )


    # ========================================================
    # REMOVE EXTRA SPINES
    # ========================================================

    ax.spines[
        "top"
    ].set_visible(
        False
    )


    ax.spines[
        "right"
    ].set_visible(
        False
    )


    ax.spines[
        "left"
    ].set_visible(
        False
    )


    # ========================================================
    # PANEL TITLE
    # ========================================================

    ax.set_title(

        f"({panel_letter}) {title}  (n = {n})",

        fontsize=11,

        fontweight="bold",

        pad=8
    )


# ============================================================
# 10. PANEL INFORMATION
# ============================================================

panels = [

    (
        "Japanese cedar",
        "Start of Pollen Season",
        "Cedar — Start DOY",
        "a"
    ),

    (
        "Japanese cypress",
        "Start of Pollen Season",
        "Cypress — Start DOY",
        "b"
    ),

    (
        "Japanese cedar",
        "Peak of Pollen Season",
        "Cedar — Peak DOY",
        "c"
    ),

    (
        "Japanese cypress",
        "Peak of Pollen Season",
        "Cypress — Peak DOY",
        "d"
    ),

    (
        "Japanese cedar",
        "Maximum Daily Pollen",
        "Cedar — Maximum Daily Pollen",
        "e"
    ),

    (
        "Japanese cypress",
        "Maximum Daily Pollen",
        "Cypress — Maximum Daily Pollen",
        "f"
    ),

    (
        "Japanese cedar",
        "Total Pollen Dispersal",
        "Cedar — Total Dispersal",
        "g"
    ),

    (
        "Japanese cypress",
        "Total Pollen Dispersal",
        "Cypress — Total Dispersal",
        "h"
    )
]


# ============================================================
# 11. CREATE FIGURE
# ============================================================

fig, axes = plt.subplots(

    4,
    2,

    figsize=(
        12,
        13
    )
)


# ============================================================
# 12. CREATE ALL PANELS
#
# TOP 5 ARE CALCULATED AUTOMATICALLY
# FROM MEAN ABSOLUTE SHAP.
# ============================================================

for ax, (
    species,
    outcome,
    title,
    letter
) in zip(
    axes.flat,
    panels
):


    # --------------------------------------------------------
    # Select model
    # --------------------------------------------------------

    panel_data = df[

        (
            df["Species"]
            ==
            species
        )

        &

        (
            df["Outcome"]
            ==
            outcome
        )

    ].copy()


    # --------------------------------------------------------
    # Calculate top 5 using mean absolute SHAP
    # --------------------------------------------------------

    top5, importance = (
        get_top_features(
            panel_data,
            n_features=5
        )
    )


    # --------------------------------------------------------
    # Print ranking to Jupyter
    # --------------------------------------------------------

    print(
        "\n========================================"
    )

    print(
        f"({letter}) {title}"
    )

    print(
        "Top 5 by mean absolute SHAP:"
    )


    for rank, feature in enumerate(
        top5,
        start=1
    ):

        print(
            f"{rank}. {feature}: "
            f"{importance.loc[feature]:.4f}"
        )


    # --------------------------------------------------------
    # Make violin
    # --------------------------------------------------------

    layered_violin(

        ax=ax,

        panel_data=panel_data,

        feature_order=top5,

        title=title,

        panel_letter=letter,

        n_bins=20
    )


# ============================================================
# 13. X-AXIS LABELS
# ============================================================

for r in range(3):

    axes[r, 0].set_xlabel("")

    axes[r, 1].set_xlabel("")


axes[3, 0].set_xlabel(
    "SHAP value",
    fontsize=10
)


axes[3, 1].set_xlabel(
    "SHAP value",
    fontsize=10
)


# ============================================================
# 14. SPACING
# ============================================================

plt.subplots_adjust(

    left=0.16,

    right=0.98,

    bottom=0.055,

    top=0.97,

    wspace=0.38,

    hspace=0.28
)


# ============================================================
# 15. SAVE PNG
# ============================================================

plt.savefig(

    "Ex_Fig5_SHAP_layered_violin_8panel_TOP5_meanAbsSHAP.png",

    dpi=600,

    bbox_inches="tight",

    facecolor="white"
)


# ============================================================
# 16. SAVE PDF
# ============================================================

plt.savefig(

    "Ex_Fig5_SHAP_layered_violin_8panel_TOP5_meanAbsSHAP.pdf",

    bbox_inches="tight",

    facecolor="white"
)


# ============================================================
# 17. SHOW FIGURE
# ============================================================

plt.show()