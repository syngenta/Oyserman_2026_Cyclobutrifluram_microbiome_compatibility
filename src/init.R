#!/usr/bin/env Rscript

suppressPackageStartupMessages({
	library(tidyverse)
	library(ggpubr)
})

## INIT SCRIPT
# Contains the setup code for the figure generation scripts


# constants -----------------------------------------------------------------------------------
INPUT_DATA_DIR <- "./data_files"
RESULTS_DIR <- "./results"

METADATA_FILE <- file.path(INPUT_DATA_DIR, "supplemental_tables.xlsx")
MP_COORDINATES_FILE <- file.path(INPUT_DATA_DIR, "miniplot_coordinates.tsv")
FUNGICIDAL_ACTIVITY_FILE <- file.path(INPUT_DATA_DIR, "fungicidal_activity.tsv")

ALPHA_CUTOFF <- 0.05


# functions -----------------------------------------------------------------------------------
pooled.sd <- function(data, value_col, group_cols = c()) {
	#' @title Pooled standard deviation
	#' @description
	#' Calculates the pooled standard deviation for a given grouped data set.
	#' It is generalised for \eqn{k} groups as:
	#' \deqn{ s_{pooled} = \sqrt{\frac{\sum^k_i (n_i - 1)s^2_i}{\sum^k_i (n_i) - k}} }
	#' @param data Tibble to use for calculation.
	#' @param value_col Column name containing the values to use for SD calculation.
	#' @param group_cols The grouping column names.
	#' @returns The pooled SD, as a numeric scalar.
	data |>
		group_by(across(all_of(group_cols))) |>
		summarise(
			n = n(),
			sd.weighted = (n-1)*sd(.data[[value_col]])^2,
			.groups = "drop"
		) |>
		summarise(pooled = sqrt(sum(sd.weighted)/(sum(n)-n()))) |>
		pull(pooled)
}


std.err <- function(v) {
	#' @title Standard error of the mean (SEM)
	#' @description
	#' Calculates the SEM of a vector.
	#' Any \code{NA} values will be silently dropped.
	#'
	#' SEM is calculated as:
	#' \eqn{ SEM = \frac{\sigma}{\sqrt{n}} }
	#'
	#' @param v A numeric vector.
	#' @return A numeric scalar.
	#' @examples
	#' v <- c(1,2,3,4,5)
	#' std.err(v)
	v <- v[!is.na(v)]
	v.sd <- sd(v)
	v.se <- v.sd / sqrt(length(v))
	return(v.se)
}


axis_symmetry <- function(v) {
	max(abs(v)) * c(-1, 1)
}


geom_crosshair <- function(x = 0, y = 0, linetype = "dotted", color = "#00000099", ...) {
	list(
		geom_hline(yintercept = y, linetype = linetype, color = color, ...),
		geom_vline(xintercept = x, linetype = linetype, color = color, ...)
	)
}


gg_pt2mm <- function(size.pt) {
	#' @title Point to millimeter conversion
	#' @description
	#' Convert points (pt; \eqn{1/72} In) to millimeters (mm).
	#' \code{ggpubr} uses mm for their font sizes, but pt are usually used by \code{ggplot2}
	#' functions.
	#' @param size.pt Size in pt to convert to mm.
	#' @returns A numeric scalar.

	# 1/72 inch to mm
	size.mm <- size.pt / ggplot2::.pt
	return(size.mm)
}


ggsave_springer <- function(
	plot = last_plot(),
	filename = "ggsave.eps",
	double_cols = FALSE,
	width = NULL,
	height = 220
) {
	# set dimensions based on Springer guidelines
	if(is.null(width))
		width <- ifelse(double_cols, 174, 84)
	height <- min(height, 234)

	ggsave(
		plot = plot,
		filename = filename,
		path = file.path(RESULTS_DIR, "eps"),
		create.dir = TRUE,
		device = ifelse(grepl("macos", osVersion, ignore.case = TRUE), "pdf", cairo_ps),
		width = width,
		height = height,
		units = "mm",
		dpi = 1200,  # minimum 1200 DPI for raster images
		scale = 1,
		bg = "white"
	)
}

# ggplot theming
# font: Arial or Helvetica, 8-12 pt
mytheme <- function(
	brewer_pal = "Set3",
	legend_pos = "none",
	legend_discrete = TRUE,
	border = FALSE,
	grid_axis = NULL
) {
	# init theme
	thm <- theme_pubr(
		border = border,
		legend = legend_pos,
		base_family = "Arial",
		base_size = 8
	) %+replace%
		theme(
			legend.frame = element_rect(color="black", linetype = 1),
			# (sub)titles not allowed
			plot.title = element_blank(),
			plot.subtitle = element_blank(),
			strip.text = element_text(hjust = 0),
			strip.background = element_rect(fill = "transparent", linetype = 0),
			panel.spacing = unit(0, "mm"),
			complete = TRUE
		)

	# init graphics object
	gg <- list(theme=thm)

	if(legend_discrete)
		gg[["guides"]] <- guides(
			fill = guide_legend(
				override.aes = list(
					color = "black", shape = 22, size = gg_pt2mm(7)
				)
			)
		)


	if(is.null(brewer_pal)) {
		gg[["fill"]] <- scale_fill_grey(start = 0.3, end = 1)
		gg[["color"]] <- scale_color_grey(start = 0.3, end = 1)
	} else {
		gg[["fill"]] <- scale_fill_brewer(palette = brewer_pal)
		gg[["color"]] <- scale_color_brewer(palette = brewer_pal)
	}

	if(!is.null(grid_axis))
		gg[["grids"]] <- grids(axis = grid_axis, linetype = "solid", color = "#00000022")

	return(gg)
}


load_phylo <- function(target = "") {
	stopifnot("'target' must be one of '16S' or 'ITS'!"= target %in% c("16S", "ITS"))

	fname <- file.path(INPUT_DATA_DIR, sprintf("phyloseq_%s.rds", target))
	ps <- readRDS(fname)


	# add SampleID column from row names
	ps_metadata <- data.frame(phyloseq::sample_data(ps)) |>
		mutate(
			across(contains("Treatment"), ~ sub("TYM", "Cyc", .x, fixed = TRUE)),
			Treatment_Soil = paste(Treatment, Soil, sep = "_"),
			Treatment_Crop = paste(Treatment, Crop, sep = "_"),
			Crop_Soil = paste(Crop, Soil, sep = "_"),
			Crop_Treatment_Soil = paste(Crop, Treatment, Soil, sep = "_"),
		)

	phyloseq::sample_data(ps) <- phyloseq::sample_data(ps_metadata)


	return(list(
		phyloseq=ps,
		metadata=as_tibble(ps_metadata),
		target=target,
		input_file=fname
	))
}


load_sig_taxa <- function(target = "") {
	stopifnot("'target' must be one of '16S' or 'ITS'!"= target %in% c("16S", "ITS"))

	fname <- file.path(INPUT_DATA_DIR, sprintf("significant_taxa_%s.xlsx", target))

	sig_only_df <- readxl::read_excel(fname, sheet = 1, col_names = TRUE) |>
		select(-1)

	return(list(
		df=sig_only_df,
		target=target,
		input_file=fname
	))
}


# data setup ----------------------------------------------------------------------------------
# Greenhouse data
greenhouse_trial_df <- readxl::read_excel(
	path = METADATA_FILE,
	sheet = "Table 2 - GH Plant & AMF",
	na = c("-")
) |>
	filter(!is.na(Arbuscules)) |>
	mutate(
		sample_id = `Sample ID`,
		crop = fct_inorder(Crop),
		CYC = TYM == "yes",  # cyclobutrifluram present?
		labels = fct_inorder(Labels),
		soil = fct_inorder(`Active Soil`),
		arbuscules = Arbuscules,
		.keep = "none"
	)


# Miniplot coordinates
mp_locations_df <- read_tsv(
	file = MP_COORDINATES_FILE,
	col_types = "iiiicciic"
) |>
	mutate(
		inner_row = (subplot_row-1) %% 2 + 1,
		inner_col = (subplot_col-1) %% 2 + 1,
		across(where(is.numeric), as.character),
		across(!all_of("sample_id"), fct_inorder)
	)


# Miniplot data
miniplot_trial_df <- readxl::read_excel(
	path = METADATA_FILE,
	sheet = "Table 3 - MP Plant & AMF"
) |>
	mutate(
		sample_id = as.character(`Sample_ID`),
		CYC = TYM == "yes",  # cyclobutrifluram present?
		AMF = AMF == "yes",  # AMF present?
		color_code = fct_inorder(Color_code),
		soil = fct_inorder(Soil),
		arbuscules = Arbuscules,
		mean_dsw = `mean_dsw (g)`,
		treatment = sprintf("%sAMF\n%scyc", if_else(AMF,"+","-"), if_else(CYC,"+","-")),
		.keep = "none"
	) |>
	full_join(mp_locations_df, by = "sample_id")


# mycorrhizal response assay
mycorrhizal_response_df <- readxl::read_excel(
	path = METADATA_FILE,
	sheet = "Table 5 - Myc Response Assay",
	range = "A1:O49",
	na = c("-")
) |>
	mutate(
		experiment = fct_inorder(Experiment),
		pot = fct_inseq(as.character(`TOTAL POTS`)),
		CYC = Cyclobutrifluram == "yes",
		AMF = AMF == "yes",
		amf_pretty = as.factor(if_else(AMF, "With AMF", "No AMF")),
		cyc_pretty = as.factor(if_else(CYC, "With cyc", "No cyc")),
		treatment = sprintf("%sAMF\n%scyc", if_else(AMF,"+","-"), if_else(CYC,"+","-")),
		arbuscules,
		FSW = `FSW (g)`,
		FRW = `FRW (g)`,
		DSW = `DSW (g)`,
		DRW = `DRW (g)`,
		.keep = "none"
	)


# spore germination assay
spore_germination_df <- readxl::read_excel(
	path = METADATA_FILE,
	sheet = "Table 6 - AMF Spore Germ Assay"
) |>
	mutate(
		treatments = fct_relevel(treatments, "Control"),
		technology = str_split_i(treatments, "_", 1),
		name = fct_relevel(name, "control"),
		name_format = sub(" mg/kg", "", name, fixed = TRUE),
		rate = sprintf("%.2f", `rate (mg/kg)`),
		percent_germinated,
		.keep = "none"
	)


# in-vitro testing of fungi
fungicidal_activity_df <- read_tsv(
	file = FUNGICIDAL_ACTIVITY_FILE,
	col_types = "ccci"
) |>
	mutate(across(where(is.character), ~ fct_inorder(.x)))


# clean intermediates
rm(mp_locations_df)
