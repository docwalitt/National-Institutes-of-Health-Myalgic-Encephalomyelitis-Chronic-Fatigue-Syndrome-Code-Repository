## Code used for shotgun sequencing analysis of microbiome samples of the Walitt et al (2023) paper

For shotgun sequencing analysis of the microbiome samples, the [JAMS package](https://github.com/johnmcculloch/JAMS_BW) was used for both analysis *within* and *between* samples. Although the JAMS package is a *single* software package written mostly in R, the two phases of shotgun sequencing analyis, namely, analysis **within** a sample and comparison **between** samples is run independently in JAMS.

Briefly, fastqs for each sample are first put through a pipeline called [JAMSalpha](https://github.com/johnmcculloch/JAMS_BW/wiki/JAMSalpha), which yields a _single_ file with extension _.jams_ (called a jamsfile).

These jamsfiles (one for each sample) are then used, together with metadata, to obtain features-by-sample matrices through the [JAMSbeta](https://github.com/johnmcculloch/JAMS_BW/wiki/JAMSbeta) pipeline.


### Pre-analysis of samples starting from fastqs.
In order to run fastqs for a single sample through JAMSalpha, use the JAMSalpha command on bash, with one line per sample, as in the example below:
```bash
JAMSalpha -f /path/to/forward/Sample_R1.fastq -r /path/to/reverse/Sample_R2.fastq -d /path/to/JAMSdb/JAMSdbApr2020_96Gbk2db -A metagenome -p SamplePrefix
```

The database used for this paper was built in January 2022 and can be found [here](https://hpc.nih.gov/~mccullochja/JAMSdb202201.tar.gz). Please note that other more recent JAMS-compatible kraken2 databases are now available.

### Comparison between samples
#### Building a feature table using the outputs from JAMSalpha
The JAMSbeta pipeline will look for sample prefixes present within a column named "Sample" of the metadata file supplied and select the corresponding jamsfiles present within the folder specified following the `-y` argument:

```bash
JAMSbeta -p Walitt2023 -t metadata_file.tsv -y /path/to/folder/with/jamsfiles
```

This will create an R-session image (.RData) containing [SummarizedExperiment](https://bioconductor.org/packages/release/bioc/html/SummarizedExperiment.html) objects which can be used with the JAMS package plotting functions.

## Analysis of fecal samples obtained from humans

The taxonomic SummarizedExperiment object is contained within an object of class list called _expvec_.
As a supplementary resource for the reader, on this current repository, we make available the taxonomic SummarizedExperiment object for the relevant samples in the Walitt et al (2023) paper as an R object in .rds form called _SEobj_CFSvsHV_phase1_, which can be downloaded from here: [SEobj_CFSvsHV_phase1](../data/SEobj_CFSvsHV_phase1.rds).

Now that we have this SummarizedExperiment object which is used as input for all main JAMS functions, we can proceed to actual plotting.




### Code for Supplementary Figure 19 panels A and B

```R
# Code for Supplementary Figure 19 panels A and B

pdf("Supplementary_Fig_19_AB.pdf", paper = "a4r")

    alphaout <- plot_alpha_diversity(ExpObj = SEobj_CFSvsHV_phase1, measures = c("Observed", "InvSimpson"), stratify_by_kingdoms = TRUE, glomby = NULL, samplesToKeep = NULL, featuresToKeep = NULL, subsetby = NULL, compareby = "Group", colourby = "Group", shapeby = NULL, facetby = NULL, wrap_facet = FALSE, overlay_boxplot = FALSE, applyfilters = "light", featcutoff = NULL, GenomeCompletenessCutoff = NULL, PctFromCtgscutoff = NULL, PPM_normalize_to_bases_sequenced = FALSE, cdict = NULL, addtit = "Only samples from visit 1", signiflabel = "p.format", max_pairwise_cats = 4, ignoreunclassified = TRUE, class_to_ignore = "N_A", returnstats = TRUE)

    isdf <- sapply(1:length(alphaout), function(x) { "data.frame" %in% (sapply(alphaout, class)[[x]]) } )

    print(alphaout[!isdf])

    write.table(alphaout[isdf], file = "Supplementary_Fig_19_AB_Alpha_diversity_stats.tsv", quote = FALSE, sep = "\t", row.names = FALSE)

    #Or alternatively,
    #write.xlsx(alphaout[isdf], file = "Supplementary_Fig_19_AB_Alpha_diversity_stats.xlsx", colnames = TRUE, rownames = FALSE, colWidths = "auto", borders = "all")

dev.off()

```

[Supplementary Figure 19 A and B pdf output](../pdfs/Supplementary_Fig_19_AB.pdf)

[Supplementary Figure 19 A and B Alpha diversity statistics spreadsheet](../data/Supplementary_Fig_19_AB_Alpha_diversity_stats.tsv)




### Code for Supplementary Figure 19 panel C

```R
# Code for Supplementary Figure 19 panel C

pdf("Supplementary_Fig_19_C.pdf", paper = "a4r")

Supplementary_Fig_19_C_plot_list <- plot_Ordination(ExpObj = SEobj_CFSvsHV_phase1, glomby = NULL, subsetby = NULL, samplesToKeep = NULL, samplesToHighlight = NULL, featuresToKeep = NULL, ignoreunclassified = TRUE, applyfilters = NULL, featcutoff = NULL, GenomeCompletenessCutoff = NULL, PctFromCtgscutoff = NULL, PPM_normalize_to_bases_sequenced = FALSE, algorithm = "PCoA", distmethod = "bray", compareby = "Group", colourby = "Group", shapeby = "Visit", ellipseby = "Group", sizeby = NULL, pairby = NULL, textby = NULL, dotsize = 2, dotborder = NULL, log2tran = TRUE, transp = TRUE, perplx = NULL, max_neighbors = 15, permanova = TRUE, plotcentroids = plotcentroids, highlight_centroids = FALSE, show_centroid_distances = FALSE, addtit = paste("Only samples from visit 1"), cdict = NULL, grid = FALSE, forceaspectratio = 1, threads = 8, class_to_ignore = "N_A", return_coordinates_matrix = TRUE)

print(Supplementary_Fig_19_C_plot_list[[2]])

dev.off()

#Export x, y coordinates of ordination plot to spreadsheet format if so wished
write.table(Supplementary_Fig_19_C_plot_list[[1]], file = "Supplementary_Fig_19_C_ordination_plot_xy_coordinates.tsv", quote = FALSE, sep = "\t", row.names = TRUE)

#Or alternatively,
#write.xlsx(Supplementary_Fig_19_C_plot_list[[1]], file = "Supplementary_Fig_19_C_ordination_plot_xy_coordinates.xlsx", colNames = TRUE, rowNames = TRUE, colWidths = "auto", borders = "all")

```

[Supplementary Figure 19 C pdf output](../pdfs/Supplementary_Fig_19_C.pdf)

[Supplementary Figure 19 C xy coordinate output](../data/Supplementary_Fig_19_C_ordination_plot_xy_coordinates.tsv)




### Code for Supplementary Figure 19 panel D

```R
pdf("Supplementary_Fig_19_D.pdf", paper = "a4r")

    heatmapstats <- plot_relabund_heatmap(ExpObj = SEobj_CFSvsHV_phase1, glomby = NULL, hmtype = "comparative",
    samplesToKeep = NULL, featuresToKeep = NULL,
    applyfilters = "moderate", featcutoff = NULL, GenomeCompletenessCutoff = NULL, PPM_normalize_to_bases_sequenced = FALSE,
    scaled = TRUE, subsetby = NULL, compareby = "Group", invertbinaryorder = TRUE, showonlypbelow = 0.05, adj_pval_for_threshold = FALSE, minl2fc = 1, ntop = NULL,
    colcategories = c("Group", "Visit"),
    splitcolsby = NULL, ordercolsby = NULL, textby = NULL,
    cluster_samples_per_heatmap = TRUE, cluster_features_per_heatmap = TRUE,
    label_samples = FALSE, cluster_rows = TRUE, max_rows_in_heatmap = 50, no_underscores = TRUE, showGram = TRUE, show_GenomeCompleteness = TRUE, addtit = NULL, returnstats = TRUE)

    #Save p-values and stats for all features
    heatmapstats <- heatmapstats[[1]]
    #Rearrange order for clarity. Features plotted on the heatmap first.
    heatmapstats_plotted <- subset(heatmapstats, pval <= 0.05)
    heatmapstats_plotted <- subset(heatmapstats_plotted, absl2fc >= 1)
    heatmapstats_rump <- heatmapstats[!(rownames(heatmapstats) %in% rownames(heatmapstats_plotted)), ]
    heatmapstats_reordered <- rbind(heatmapstats_plotted, heatmapstats_rump)

dev.off()

#Write stats to table
write.table(heatmapstats_reordered, file = "Supplementary_Fig_19_D_feature_stats.tsv", quote = FALSE, sep = "\t", row.names = TRUE)

#Or alternatively,
#write.xlsx(heatmapstats_reordered, file = "Supplementary_Fig_19_D_feature_stats.xlsx", colNames = TRUE, rowNames = TRUE, colWidths = "auto", borders = "all")

```

[Supplementary Figure 19 D pdf output](../pdfs/Supplementary_Fig_19_D.pdf)

[Supplementary Figure 19 D feature stats](../data/Supplementary_Fig_19_D_feature_stats.tsv)


### Export relative abundace table for convenience

```R
# Export relative abundances and metadata to spreadsheet

expvec_CFSvsHV_phase1 <- list()
expvec_CFSvsHV_phase1$LKT <- SEobj_CFSvsHV_phase1

export_expvec_to_XL(expvec = expvec_CFSvsHV_phase1, usefulexp = NULL, filename = generate_filename(paste("Walitt2023_Microbiome", "metadata_and_PPM_relative_abundance", sep = "_"), suffix = "xlsx"), asPPM = TRUE, PPM_normalize_to_bases_sequenced = TRUE, applyfilters = "light")

```
[Metadata and relative abundance tables](../data/Walitt2023_Microbiome_metadata_and_PPM_relative_abundance.xlsx)
