library('tidyverse')
library('SummarizedExperiment')
library('DESeq2')
library('biomaRt')
library('testthat')
library('fgsea')

#' Function to generate a SummarizedExperiment object with counts and coldata
#' to use in DESeq2
#'
#' @param csv_path (str): path to the file verse_counts.tsv
#' @param metafile (str): path to the metadata sample_metadata.csv
#' @param subset(list): list of sample timepoints to use
#' 
#'   
#' @return SummarizedExperiment object with subsetted counts matrix
#'   and sample data
#' @export
#'
#' @examples se <- make_se('verse_counts.tsv', 'sample_metadata.csv', c('vP0', 'vAd'))
make_se <- function(counts_csv, metafile_csv, subset) {
  
  metadata <- read_csv(metafile_csv) %>%
    mutate(timepoint = factor(timepoint, levels=subset))
  
  filtered <- dplyr::select(metadata, samplename, timepoint) %>%
    filter(timepoint %in% subset)
  
  filtered$timepoint <- as.factor(filtered$timepoint)
  
  raw_counts <- read.delim('verse_counts.tsv')
  
  counts <- dplyr::select(raw_counts, filtered$samplename)
  counts['gene'] <- raw_counts['gene']
  counts <- relocate(counts, gene)
  row.names(counts) <- counts$gene
  counts <- dplyr::select(counts, -1)
  counts <- data.matrix(counts)
  
  
  
  sumex <- SummarizedExperiment(assays = list(counts = counts), colData = filtered)
  
  
  
  metadata(sumex)$model <- counts ~ timepoint
  
  return(sumex)
}

#' Function that runs DESeq2 and returns a named list containing the DESeq2
#' results as a dataframe and the dds object returned by DESeq2
#'
#' @param se (obj): SummarizedExperiment object containing counts matrix and
#' coldata
#' @param design: the design formula to be used in DESeq2
#'
#' @return list with DESeqDataSet object after running DESeq2 and results from
#'   DESeq2 as a dataframe
#' @export
#'
#' @examples results <- return_deseq_res(se, ~ timepoint)
return_deseq_res <- function(se, design) {
  dds <- DESeqDataSet(se, design = design) #reads SummarizedExperiment obj
  dds <- DESeq(dds)
  
  res <- results(dds) %>% as.data.frame()
  
  output_l <- list('res' = res, 'dds' = dds)
  
  return(output_l)
}

#' Function that takes the DESeq2 results dataframe, converts it to a tibble and
#' adds a column to denote plotting status in volcano plot. Column should denote
#' whether gene is either 1. Significant at padj < .10 and has a positive log
#' fold change, 2. Significant at padj < .10 and has a negative log fold change,
#' 3. Not significant at padj < .10. Have the values for these labels be UP,
#' DOWN, NS, respectively. The column should be named `volc_plot_status`.
#'
#' @param deseq2_res (df): results from DESeq2 
#' @param padj_threshold (float): threshold for considering significance (padj)
#'
#' @return Tibble with all columns from DESeq2 results and one additional column
#'   labeling genes by significant and up-regulated, significant and
#'   downregulated, and not significant at padj < .10.
#'   
#' @export
#'
#' @examples labeled_results <- label_res(res, .10)
label_res <- function(deseq2_res, padj_threshold) {
  volc_res <- as_tibble(deseq2_res, rownames = 'genes') %>%
    mutate(volc_plot_status = dplyr::case_when((log2FoldChange > 0) & (padj < padj_threshold) ~ 'UP',
                                               (log2FoldChange < 0) & (padj < padj_threshold) ~ 'DOWN',
                                               TRUE                                           ~ 'NS'))
  volc_res <- relocate(volc_res, c(genes, volc_plot_status, log2FoldChange, padj))
  
  volc_res <- arrange(volc_res, padj)
  return(volc_res)
}

#' Function to plot the unadjusted p-values as a histogram
#'
#' @param labeled_results (tibble): Tibble with DESeq2 results and one additional
#' column denoting status in volcano plot
#'
#' @return ggplot: a histogram of the raw p-values from the DESeq2 results
#' @export
#'
#' @examples pval_plot <- plot_pvals(labeled_results)
plot_pvals <- function(labeled_results) {
  p <- labeled_results %>% 
    ggplot(aes(x=pvalue)) +
    geom_histogram(color='grey40',fill='lightsalmon1') +
    theme_linedraw() +
    labs(title='Histogram of raw pvalues obtained from DE analysis (vP0 vs. vAd)' )
  
  
  return(p)
}

#' Function to plot the log2foldchange from DESeq2 results in a histogram
#'
#' @param labeled_results (tibble): Tibble with DESeq2 results and one additional
#' column denoting status in volcano plot
#' @param padj_threshold (float): threshold for considering significance (padj)
#'
#' @return ggplot: a histogram of log2FC values from genes significant at padj 
#' threshold of 0.1
#' @export
#'
#' @examples log2fc_plot <- plot_log2fc(labeled_results, .10)
plot_log2fc <- function(labeled_results, padj_threshold) {
  filtered = filter(labeled_results, padj < padj_threshold)
  
  p <- filtered %>% 
    ggplot(aes(x=log2FoldChange)) +
    geom_histogram(color='grey40',fill='lightsalmon1', bins=85) +
    theme_linedraw() +
    labs(title='Histogram of Log2FoldChanges for DE Genes (vP0 vs. vAd)')
  
  
  return(p)
}

#' Function to make scatter plot of normalized counts for top ten genes ranked
#' by ascending padj
#'
#' @param labeled_results (tibble): Tibble with DESeq2 results and one
#'   additional column denoting status in volcano plot
#' @param dds_obj (obj): The object returned by running DESeq (dds) containing
#' the updated DESeqDataSet object with test results
#' @param num_genes (int): Number of genes to plot
#'
#' @return ggplot: a scatter plot with the normalized counts for each sample for
#' each of the top ten genes ranked by ascending padj
#' @export
#'
#' @examples norm_counts_plot <- scatter_norm_counts(labeled_results, dds, 10)
scatter_norm_counts <- function(labeled_results, dds_obj, num_genes){
  #normalize
  dds <- estimateSizeFactors(dds_obj)
  
  normalized_counts <- counts(dds, normalized=TRUE)
  
  normalized_counts <- as_tibble(normalized_counts, rownames = 'genes')
  
  #get top (number specified) amount of genes and prepare for plot
  top_num <- slice_min(labeled_results, padj, n=num_genes)
  
  top_num_genes <- dplyr::select(top_num, genes) %>%
    left_join(normalized_counts, by='genes') %>%
    pivot_longer(!genes)
  
  
  # plot
  p <- top_num_genes %>%
    ggplot() +
    geom_point(aes(x=genes, y=log10(value), color=name), position=position_jitter(w=0.13,h=0)) +
    theme_linedraw() +
    labs(title='Plot of Log10(normalized counts) for top ten DE genes', x='', y='log10(norm_counts)') +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.7, hjust=1))
  
  return(p)
}

#' Function to generate volcano plot from DESeq2 results
#'
#' @param labeled_results (tibble): Tibble with DESeq2 results and one
#'   additional column denoting status in volcano plot
#'
#' @return ggplot: a scatterplot (volcano plot) that displays log2foldchange vs
#'   -log10(padj) and labeled by status
#' @export
#'
#' @examples volcano_plot <- plot_volcano(labeled_results)
#' 
plot_volcano <- function(labeled_results) {
  p <- labeled_results %>%
    ggplot() +
    geom_point(mapping=aes(x = log2FoldChange, y = -log10(padj), color = volc_plot_status)) +
    geom_hline(yintercept=-log10(0.05), col="black") +
    theme_linedraw() +
    labs(title='Volcano plot of DESeq2 differential expression results (vP0 vs. vAd)')
  
  return(p)
}

#' Function to run fgsea on DESeq2 results
#'
#' @param labeled_results (tibble): the labeled results from DESeq2
#' @param gmt (str): the path to the GMT file
#' @param min_size: the threshold for minimum size of the gene set
#' @param max_size: the threshold for maximum size of the gene set
#'
#' @return tibble containing the results from running fgsea using descending
#' log2foldchange as a ranking metric
#' @export
#'
#' @examples fgsea_results <- run_gsea(labeled_results, 'c2.cp.v7.5.1.symbols.gmt', 15, 500)
run_gsea <- function(labeled_results, gmt, min_size, max_size) {
  
  labeled_results$genes <- gsub("\\..*","",labeled_results$genes) #remove decimal from geneid
  
  genes <- labeled_results$genes
  
  human_data <- useMart('ensembl','hsapiens_gene_ensembl', host="useast.ensembl.org")
  mouse_data <- useMart('ensembl','mmusculus_gene_ensembl', host="useast.ensembl.org")
  
  linked_data <- getLDS(attributes = c('ensembl_gene_id'),
                        filters = 'ensembl_gene_id',
                        values = genes,
                        mart = mouse_data,
                        attributesL = c('hgnc_symbol'),
                        martL = human_data,
                        uniqueRows = TRUE,
                        bmHeader = TRUE)
  
  gene_id_results <- left_join(labeled_results, linked_data, c('genes' = 'Gene.stable.ID'))
  
  #spent 4+ hours on ranks input alone, could not get this to format correctly for run_gsea (biomart also not cooperating); using solution for 
  # for this only 
  ranks <- gene_id_results %>%
    drop_na(HGNC.symbol, log2FoldChange) %>%
    distinct(HGNC.symbol, log2FoldChange, .keep_all=TRUE) %>%
    arrange(desc(log2FoldChange)) %>%
    dplyr::select(HGNC.symbol, log2FoldChange) %>%
    deframe()
  
  fgsea_output <- fgsea(pathways = gmtPathways(gmt),
                        stats = ranks,
                        minSize = min_size,
                        maxSize = max_size)
  
  
  return(as_tibble(fgsea_output))
}

#' Function to plot top ten positive NES and top ten negative NES pathways
#' in a barchart
#'
#' @param fgsea_results (tibble): the fgsea results in tibble format returned by
#'   the previous function
#' @param num_paths (int): the number of pathways for each direction (top or
#'   down) to include in the plot. Set this at 10.
#'
#' @return ggplot with a barchart showing the top twenty pathways ranked by positive
#' and negative NES
#' @export
#'
#' @examples fgsea_plot <- top_pathways(fgsea_results, 10)
top_pathways <- function(fgsea_results, num_paths){
  
  positive_num <- slice_max(fgsea_results, NES, n = num_paths) %>%
    dplyr::select(pathway)
  
  negative_num <- slice_min(fgsea_results, NES, n = num_paths) %>%
    dplyr::select(pathway)
  
  pos_neg <- bind_rows(positive_num, negative_num)
  
  filtered <- fgsea_results %>% 
    filter(pathway %in% pos_neg$pathway)
  
  factors <- factor(filtered$pathway)
  
  filtered$names <- factors
  
  filtered$names <- fct_reorder(filtered$names, filtered$NES, max)
  
  p <- filtered %>%
    ggplot() +
    geom_bar(aes(x=names, y=NES, fill = NES > 0), stat='identity') +
    theme_linedraw() +
    theme(legend.position="none") +
    scale_fill_manual(values=c("#f56262", "#6276f5")) +
    labs(title='fgsea results for Hallmark MSigDB gene sets', x='', y='Normalized Enrichment Score (NES)') +
    coord_flip()
  
  return(p)
}
