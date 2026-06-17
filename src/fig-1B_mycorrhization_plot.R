#!/usr/bin/env Rscript

suppressPackageStartupMessages({
	library(tidyverse)
	library(ggpubr)
})

source("./src/init.R", echo = FALSE, local = TRUE)


# data processing MP --------------------------------------------------------------------------
# Tukey HSD
print(shapiro.test(miniplot_trial_df$arbuscules))

treat_aov <- aov(arbuscules ~ color_code : soil, data = miniplot_trial_df)
print(summary(treat_aov))

thsd <- TukeyHSD(treat_aov)
multcomp_groups <- multcompView::multcompLetters4(treat_aov, thsd, Letters = LETTERS)

signif_groups <- data.frame(
	signif_group = multcomp_groups[[length(multcomp_groups)]]$Letters
) |>
	rownames_to_column("fit_interaction")


# plot
p_mp <- miniplot_trial_df |>
	mutate(fit_interaction = paste(color_code, soil, sep = ":")) |>
	left_join(signif_groups, by = "fit_interaction") |>
	ggplot(aes(x = treatment, y = arbuscules, fill = treatment)) +
	stat_mean(geom = "col", width = 1, color = "black") +
	stat_mean(  # bit of a hack; draws only once per group instead of per record
		geom = "label",
		aes(y = 100, label = signif_group),
		show.legend = FALSE,
		vjust = 1,
		hjust = 0.5,
		fontface = "bold",
		size = 6,
		size.unit = "pt",
		color = "black",
		fill = "#FFFFFFC0"
	) +
	stat_summary(
		geom = "linerange",
		fun.min = \(x) mean(x)-std.err(x),
		fun.max = \(x) mean(x)+std.err(x)
	) +
	facet_grid(cols = vars(soil), scales = "free_x", switch = "x") +
	scale_x_discrete(labels = NULL) +
	scale_y_continuous(
		limits = c(0, 100),
		labels = scales::label_percent(scale = 1, accuracy = 1),
		expand = c(0, 0, 0.05, 0)
	) +
	mytheme(brewer_pal = NULL, legend_pos = "bottom") +
	theme(
		panel.spacing = unit(2, "mm"),
		strip.placement = "outside",
		axis.ticks.x = element_blank()
	) +
	labs(
		title = "Arbusculation (miniplot)",
		subtitle = "Mean AMF arbusculation per treatment",
		fill = "Treatment",
		x = NULL,
		y = "Mycorrhization"
	); p_mp


# export
ggsave_springer(
	plot = p_mp,
	filename = "Fig1B.eps",
	height = 60,
)
