#Load Packages
library(DESeq2)
library(tidyverse)

#' Load a tsv located at specific location `filename` into a tibble
#'
#'
#' @param filename (str): the path to a specific file (ie 'file/path/to/file.tsv')
#'
#' @return tibble: a (g x m) tibble with a 'gene' column followed by
#' sample names as column names. 
#' 
#' @note Column 'gene' should be first and the only column to contain strings.
#' Data in sample_name columns CANNOT be strings
#' 
#' @example `verse_counts <- read_data('verse_counts.tsv')`

read_data <- function(filename){
  return(read_tsv(filename))
}


#' Filter out genes with zero variance
#'
#'
#' @param verse_counts tibble: a (g x m) tibble of raw read counts
#'
#' @return tibble: a (n x m) tibble of raw reads with genes that have 
#' zero variance across samples removed
#' 
#' @note (g >= n)
#' 
#' @example `filtered_counts <- filter_zero_var_genes(verse_counts)`

filter_zero_var_genes <- function(verse_counts) {
  verse_counts['var'] <- apply(verse_counts[,-1], 1, var)
  
  verse_counts <- filter(verse_counts, var > 0) %>%
    select(-var)
  return(verse_counts)
}


#' Extract time point information from sample name
#'
#'
#' @param str string: sample name from count data.
#'
#' @return string: string character representing sample time point 
#'
#' @example `timepoint_from_sample("vAd_1")`
#' output:`"Ad"`

timepoint_from_sample <- function(str) {
  return(str_sub(str, 2, 3))
}


#' Grab sample replicate number from sample name
#'
#' @param str  string: sample name from count data.
#'
#' @return string: string character represent sample replicate number
#' 
#' @note you may choose to return numeric values instead of strings here
#'
#' @example `sample_replicate("vAd_1")`
#' output: `"1"`

sample_replicate <- function(str) {
  return(str_sub(str, 5, 5))
}


#' Generate sample-level metadata from sample names. 
#' 
#' Will include columns named "sample", "timepoint", and "replicate" that store 
#' sample names, sample time points, and sample replicate, respectively.
#'
#'
#' @param sample_names vector: character vector of length (_S_) consisting of sample
#' names from count data.
#'
#' @return tibble: a (_S_ x 3) tibble with column names "sample", 
#' "timepoint", and "replicate". "sample"holds sample_names; "timepoint" 
#' stores sample time points; and "replicate" stores sample replicate
#' 
#' @note _S_ < m
#'
#' @example `meta <- meta_info_from_labels(colnames(count_data)[colnames(count_data)!='gene'])`

meta_info_from_labels <- function(sample_names) {
  
  sample <- sample_names
  replicate <- str_sub(sample_names, 5, 5)
  timepoint <- str_sub(sample_names, 2, 3)
  
  return_tib <- tibble(sample = sample, timepoint = timepoint, replicate = replicate)
  return(return_tib)
}

#' Calculate total read counts for each sample in a count data.
#'
#'
#' @param count_data tibble: a (n x m) tibble of raw read counts.
#'
#' @return named vector: numeric vector of read totals from each sample
#'
#' @examples `get_library_size(count_data)`

get_library_size <- function(count_data) {
  count_data %>%
    summarise(across(where(is.numeric), sum)) %>%
    return()
}


#' Normalize raw count data to counts per million WITH pseudocounts using the 
#' following formula:
#'     (count + 1) / ((sample_library_size/10^6) + 1)
#'
#'
#' @param count_data tibble: a (n x m) matrix of raw read counts.
#'
#' @return tibble: a (n x m) matrix with read count normalized to counts
#' per million
#'
#' @examples
#' `normalize_by_cpm(count_data)`

normalize_by_cpm <- function(count_data) {
  count_data_n <- count_data[c(-1)]
  size <- get_library_size(count_data_n)*(10^-6)
  
  r_tib <- as_tibble(count_data_n / size[col(count_data_n)])
  
  
  return(r_tib)
  
}


#' Normalize raw count data using DESeq2
#'
#'
#' @param count_data tibble: a (n x m) matrix of raw reads
#' @param meta_data tibble: sample-level information tibble containing time point,
#' sample, and replicate information.
#' @param design_formula formula: formula of comparision of interest
#'
#' @return tibble: a (n x m) tibble of DESeq2 normalized count data.
#'
#' @example ' `deseq_normalize(count_data, meta_data, ~ timepoint)`

deseq_normalize <- function(count_data, meta_data) {
  dds <- DESeqDataSetFromMatrix(
    countData=count_data[c(-1)],
    colData=meta_data,
    design=~1)
  
  dds <- estimateSizeFactors(dds)
  
  dds_normalized <- as_tibble(counts(dds, normalized=TRUE))
  
  dds_normalized['gene'] <- count_data[c(1)]
  
  returned_df <- dds_normalized %>%
    select(gene, everything())
  
  return(returned_df)
  
}


#' Perform and plot PCA using processed data.
#' 
#' PCA is performed over genes, and samples should be colored by time point.
#' Both `y` and `x` axis should have percent of explained variance included.
#'
#'
#' @param data tibble: a (n x _S_) data set
#' @param meta tibble: sample-level meta information (_S_ x 3)
#' @param title string: title for plot
#'
#' @return ggplot: scatter plot showing each sample in the first two PCs. 
#'
#' @examples
#' `plot_pca(data, meta, "Raw Count PCA")`

plot_pca <- function(data, meta, title="") {
  pca <- prcomp(t(data))
  
  meta_dat <- meta
  
  meta_dat['PC1'] <- pca$x[,1]
  meta_dat['PC2'] <- pca$x[,2]
  
  v_exp <- summary(pca)
  
  PC1_var_e <- toString(round(v_exp$importance[2,1],2) * 100)
  
  PC2_var_e <- toString(round(v_exp$importance[2,2],2) * 100)
  
  p <- ggplot(meta_dat, aes(x=PC1, y=PC2, col=timepoint)) +
    geom_point() +
    labs(x=str_glue("PC1: {PC1_var_e} % variance"),y=str_glue("PC2: {PC2_var_e} % variance"), title=title)
  
  return(p)
}


#' Plot gene count distributions for each sample using boxplots.
#' 
#'
#' @param data tibble: a (n x _S_) data set
#' @param scale_y_axis boolean: whether to scale the `y` axis to log10 values.
#' Default is FALSE, and y-axis will not be transformed.
#' @param title string: title to give the chart.
#'
#' @return ggplot: boxplot show gene count distributions for each sample
#'
#' @example `plot_sample_distributions(data, scale_y_axis=TRUE, title='Raw Count Distributions')`

plot_sample_distributions <- function(data, scale_y_axis=FALSE, title="") {
  counts <- pivot_longer(data, cols = everything())
  
  counts$name <- fct_inorder(counts$name)
  
  counts_n0 <- filter(counts,value > 0)
  
  p_true <- ggplot(counts_n0, aes(x=name, y=value, col=name)) +
    geom_boxplot() +
    scale_y_log10() +
    labs(title = title, x = "sample", y = "counts")
  
  p_false <- ggplot(counts, aes(x=name, y=value, col=name)) +
    geom_boxplot() +
    labs(title = title, x = "sample", y = "counts")
  
  if (scale_y_axis) {
    return_p <- p_true
  } else {
    return_p <- p_false
  }
  
  return(return_p)
}


#' Plot relationship between mean read counts and variability over all genes.
#'
#'
#' @param data tibble: a (n x _S_) data set
#' @param scale_y_axis boolean: whether to scale to y-axis to log10 values. Default
#' is false, and the y-axis will not be transformed.
#' @param title string: title to give the chart.
#'
#' @return ggplot: A scatter plot where the x-axis is the rank of gene ordered by mean
#' count over all samples, and the y-axis is the observed variance of the
#' given gene. Each dot should have their transparency increased. The scatter
#' plot should also be accompanied by a line representing the average mean and
#' variance values.
#'
#' @example `plot_variance_vs_mean(data, scale_y_axis=TRUE, title='variance vs mean (raw counts)')`

plot_variance_vs_mean <- function(data, scale_y_axis=FALSE, title="") {
  mean <- apply(data[c(-1)], 1, mean) 
  
  var <- apply(data[c(-1)], 1, var)
  
  plot_tib <- tibble(mean=mean, variance=var)
  
  plot_tib['ranks'] <- rank(plot_tib['mean'])
  
  p2_false <- ggplot(plot_tib, aes(x=ranks, y=var)) +
    geom_point() +
    geom_smooth() +
    labs(x="Rank(Mean)", y="Variance", title=title)
  
  p2_true <- ggplot(plot_tib, aes(x=ranks, y=var)) +
    geom_point() +
    geom_smooth() +
    labs(x="Rank(Mean)", y="Variance", title=title)+
    scale_y_log10()
  
  if (scale_y_axis) {
    return2_p <- p2_true
  } else {
    return2_p <- p2_false
  }
  
  return(return2_p)
}
