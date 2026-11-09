#!/usr/bin/env Rscript

# setup ---------------------------------------------------------------------------------------
suppressPackageStartupMessages({
	library(tidyverse)
	library(ggpubr)
})

source("./src/init.R", echo = FALSE, local = TRUE)


# data ingestion ------------------------------------------------------------------------------
sig_taxa <- load_sig_taxa("16S")


# LFC sign plot -------------------------------------------------------------------------------
p_pos_v_neg <- sig_taxa$df |>
	filter(padj <= ALPHA_CUTOFF) |>
	summarise(observations = n(), .by = c(increase_abun)) |>
	mutate(proportion = observations / nrow(sig_taxa$df)) |>
	ggplot(aes(x = increase_abun, fill = increase_abun)) +
	geom_col(aes(y = proportion), position = "dodge", color = "black", width = 0.8) +
	geom_label(
		aes(label = sprintf("(%d) %.0f%%", observations, round(proportion*100)), y = 0),
		vjust = -0.5,
		fill = "#FFFFFFA0",
		label.size = 0,
		label.padding = unit(5, "pt"),
		size.unit = "pt",
		size = 8
	) +
	scale_y_continuous(expand = c(0,0,0.05,0), labels = scales::label_percent()) +
	labs(
		title = latex2exp::TeX(sprintf(r"(%s: Proportion of $\sgn(\log_2 FC)$)", sig_taxa$target)),
		subtitle = latex2exp::TeX(sprintf(r"(Significance threshold: $p_{adj} \leq %s$)", ALPHA_CUTOFF)),
		x = NULL,
		y = "Proportion of total observed"
	) +
	mytheme(brewer_pal = NULL); p_pos_v_neg


ggsave_springer(
	plot = p_pos_v_neg,
	filename = sprintf("Fig3AB_%s.eps", sig_taxa$target),
	height = 60
)
