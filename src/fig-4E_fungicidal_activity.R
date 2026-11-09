#!/usr/bin/env Rscript

suppressPackageStartupMessages({
	library(tidyverse)
	library(ggpubr)
})

source("./src/init.R", echo = FALSE, local = TRUE)

ALPHA_CUTOFF <- 0.05


# stats ---------------------------------------------------------------------------------------
fungicidal_activity_stats_df <- fungicidal_activity_df |>
	group_by(organism) |>
	rstatix::t_test(
		formula = diameter_mm ~ compound,
		paired = FALSE,
		alternative = "two.sided",
		var.equal = TRUE
	) |>
	rstatix::add_significance("p") |>
	rstatix::adjust_pvalue(method = "bonferroni") |>
	rstatix::add_significance("p.adj")


# barplot -------------------------------------------------------------------------------------
p_fa <- fungicidal_activity_df |>
	mutate(cyc = compound == "Cyclobutrifluram") |>
	ggplot(aes(x = organism, y = diameter_mm, fill = cyc)) +
	stat_summary(
		geom = "col",
		position = "dodge",
		width = 0.8,
		color = "black",
		fun = \(x) mean(x)
	) +
	stat_summary(
		geom = "linerange",
		position = position_dodge(width = 0.8),
		color = "black",
		fun.min = \(x) mean(x)-std.err(x),
		fun.max = \(x) mean(x)+std.err(x)
	) +
	stat_pwc(
		method = "t_test",
		method.args = list(paired = FALSE, alternative = "two.sided", var.equal = TRUE),
		label = sprintf(
			"{ifelse(p.adj >= %f, 'ns', paste0('italic(P)=', p.adj.format))}",
			ALPHA_CUTOFF
		),
		p.format.style = "nejm",
		remove.bracket = TRUE,
		p.adjust.method = "bonferroni",
		p.min.threshold = 0.001,
		y.position = 75,
		label.size = gg_pt2mm(6),  # in mm
  ) +
	mytheme(brewer_pal = NULL, legend_pos = "bottom") +
	theme(
		panel.spacing = unit(2, "mm"),
		axis.text.x = element_text(face = "italic"),
		axis.text.y = element_text(vjust = 0),
		plot.margin = margin(t=18,r=18,b=4,l=4),
		axis.ticks.x = element_blank(),
	) +
	scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
	scale_y_continuous(
		limits = c(0,NA),
		expand = c(0,0,0.1,0),
		labels = scales::number_format(accuracy = 1, suffix = " mm")
	) +
	labs(
		x = NULL,
		y = "Colony diamater",
		fill = "Treatment"
	); p_fa


ggsave_springer(
	plot = p_fa,
	filename = "Fig4E.eps",
	height = 70
)
