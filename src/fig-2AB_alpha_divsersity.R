#!/usr/bin/env Rscript

# setup ---------------------------------------------------------------------------------------
suppressPackageStartupMessages({
	library(tidyverse)
	library(ggpubr)
	library(phyloseq)
})

source("./src/init.R", echo = FALSE, local = TRUE)


# data loading --------------------------------------------------------------------------------
ps <- load_phylo("16S")

alpha_stat_df <- estimate_richness(ps$phyloseq) |>
	rownames_to_column("SampleID") |>
	inner_join(ps$metadata, by = "SampleID") |>
	mutate(across(where(is.character), as.factor))


# diversity -----------------------------------------------------------------------------------
print(shapiro.test(alpha_stat_df$Shannon))

comparisons <- Map(
	c,
	str_replace(levels(alpha_stat_df$Crop_Soil), fixed("_"), "_Blank_"),
	str_replace(levels(alpha_stat_df$Crop_Soil), fixed("_"), "_Cyc_")
)
comparisons <- comparisons[!grepl("Bulk_", names(comparisons))]


stat_test_df <- rstatix::t_test(
	data = alpha_stat_df,
	formula = Shannon ~ Crop_Treatment_Soil,
	comparisons = comparisons,
	alternative = "two.sided",
	paired = FALSE,
	var.equal = TRUE,
) |>
	mutate(
		Crop_Soil = str_remove(group1, "_Blank"),
		statistic, p, p.adj, p.adj.signif,
		.keep = "none"
	)


p_alpha <- alpha_stat_df |>
	left_join(stat_test_df, by = "Crop_Soil") |>
	ggplot(aes(x = Crop_Soil, y = Shannon, fill = Treatment)) +
	geom_point(
		size = 1,
		alpha = 0.3,
		shape = 21,
		color = "black",
		position = position_dodge(width = 0.4),
		show.legend = FALSE
	) +
	stat_summary(
		geom = "linerange",
		color = "black",
		position = position_dodge(width = 0.4),
		fun.min = \(x) mean(x)-std.err(x),
		fun.max = \(x) mean(x)+std.err(x),
		show.legend = FALSE
	) +
	stat_summary(
		geom = "point",
		color = "black",
		shape = 21,
		size = 2,
		position = position_dodge(width = 0.4),
		fun = \(x) mean(x)
	) +
	stat_mean(
		geom = "text",
		aes(label = p.adj.signif ,y = max(alpha_stat_df$Shannon)*1.03),
		position = "identity",
		color = "black",
		show.legend = FALSE
	) +
	facet_grid(cols = vars(Soil), scales = "free_x", space = "free_x") +
	scale_x_discrete(
		guide = guide_axis(n.dodge = 2),
		labels = \(x) deframe(distinct(select(alpha_stat_df, Crop_Soil, Crop)))[x]
	) +
	scale_y_continuous(expand = c(0,0.1,0,0.2)) +
	mytheme(brewer_pal = NULL, legend_pos = "bottom") +
	theme(panel.spacing = unit(2, "mm")) +
	labs(
		title = sprintf("%s alpha diversity", ps$target),
		subtitle = "Shannon diversity index",
		x = NULL,
		y = "Sample diversity (Shannon)",
		fill = "Treatment",
	); p_alpha


# write figure
ggsave_springer(
	plot = p_alpha,
	filename = sprintf("Fig2AB_%s.eps", ps$target),
	width = 100,
	height = 70
)
