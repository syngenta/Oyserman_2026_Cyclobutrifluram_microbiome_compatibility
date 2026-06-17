#!/usr/bin/env Rscript

suppressPackageStartupMessages({
	library(tidyverse)
	library(ggpubr)
})

source("./src/init.R", echo = FALSE, local = TRUE)


# data processing GH --------------------------------------------------------------------------
p_gh <- greenhouse_trial_df |>
	mutate(soil_crop = paste(soil, crop, sep = "\n")) |>
	ggbarplot(
		x = "CYC",
		y = "arbuscules",
		fill = "CYC",
		facet.by = c("soil_crop"),
		add = "mean_se",
		error.plot = "linerange",
		width = 1,
		key_glyph = "point"
	) +
	facet_grid(cols = vars(soil_crop), switch = "x") +
	scale_x_discrete(labels = NULL, expand = c(0.4, 0.4)) +
	scale_y_continuous(labels = scales::label_percent(scale = 1), expand = c(0,0,0.05,0)) +
	stat_pwc(
		method = "t_test",
		method.args = list(paired = FALSE, alternative = "two.sided", var.equal = TRUE),
		label = "p={p.format}",
		hide.ns = TRUE,
		remove.bracket = TRUE,
		p.adjust.method = "none",
		y.position = 80,
		label.size = gg_pt2mm(6),  # in mm
	) +
	mytheme(brewer_pal = NULL, legend_pos = "bottom") +
	theme(
		strip.text.x = element_text(angle = 90, hjust = 1),
		strip.placement = "outside",
		axis.ticks.x = element_blank()
	) +
	labs(
		title = "Degree of mycorrhization (greenhouse)",
		subtitle = "Mean mycorrhization + SEM + Two-sample Student's t-test",
		x = NULL,
		y = "Mycorrhization",
		fill = "Cyclobrutrifluram"
	) +
	coord_cartesian(ylim = c(0,100)); p_gh


ggsave_springer(
	plot = p_gh,
	filename = "Fig1A.eps",
	height = 80
)
