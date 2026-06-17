#!/usr/bin/env Rscript

# setup ---------------------------------------------------------------------------------------
suppressPackageStartupMessages({
	library(tidyverse)
	library(ggpubr)
})

source("./src/init.R", echo = FALSE, local = TRUE)


# data ingestion ------------------------------------------------------------------------------
sig_taxa <- load_sig_taxa("16S")


# clade prevalence plot -----------------------------------------------------------------------
alpha <- 0.05


p_sig_clade <- sig_taxa$df |>
	mutate(is_signif = padj <= alpha) |>
	summarise(
		n_asv = n(),
		.by = c(Phylum, is_signif, increase_abun)
	) |>
	mutate(group_total = sum(n_asv), .by = c(Phylum)) |>
	pivot_wider(names_from = is_signif, names_prefix = "signif_", values_from = n_asv, values_fill = 0) |>
	filter(sum(signif_TRUE) > 0, .by = c(Phylum)) |>
	ggplot(aes(x = signif_TRUE, y = increase_abun, fill = increase_abun)) +
	geom_col(color = "black", width = 1, key_glyph = "point") +
	geom_text(
		aes(label = sprintf("%.0f%%", 100*signif_TRUE/(group_total))),
		hjust = -0.25,
		size.unit = "pt",
		size = 8
	) +
	facet_grid(
		rows = vars(sprintf("%s\n(%d ASV's)", Phylum, group_total)),
		switch = "y"
	) +
	scale_x_continuous(expand = c(0,0,0.1,0), labels = scales::label_number(accuracy = 1)) +
	scale_y_discrete(labels = NULL, expand = c(0,.7)) +
	mytheme(brewer_pal = NULL) +
	theme(
		strip.text.y.left = element_text(angle = 0, hjust = 1, margin = margin(r=1), face = "italic"),
		strip.placement = "outside",
		axis.ticks.y = element_blank(),
		legend.position = "inside",
		legend.justification = c(1, 0.05),
		legend.box.margin = margin(b = 2, r = 2),
		legend.background = element_rect(colour = "black", fill = "white")
	) +
	labs(
		x = "# ASV's",
		y = NULL,
		fill = latex2exp::TeX(r"($\sgn(\log_2 FC)$)")
	); p_sig_clade


ggsave_springer(
	plot = p_sig_clade,
	filename = sprintf("Fig3CD_%s.eps", sig_taxa$target),
	height = 120
)
