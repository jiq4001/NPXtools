% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot_npx_norm_qc.R
\name{plot_npx_norm_qc}
\alias{plot_npx_norm_qc}
\title{plot_npx_norm_qc (Generate QC plots pre and post normalization )}
\usage{
plot_npx_norm_qc(
  normed_se,
  bridge_pattern = "HD Urine Pool",
  fields = "Sample",
  example = NULL,
  Randox = F,
  randox_info_f = NULL
)
}
\arguments{
\item{normed_se}{a summarizedexperiment, normaly generated by cmb_npx_sc() after normalization}

\item{bridge_pattern}{regex to identify bridging samples}

\item{fields}{colData name from which to identify bridging samples uing the bridge_pattern}

\item{example}{(optional)string of a analyt name, defalult is NULL}

\item{Randox}{(optional) if to track Randox among plates, defalult is F}

\item{randox_info_f}{(optional) if to plot Randox's target value. required to generate a csv file from
the product.info sheet manually.
Analyt    Randox    Target    SD
IL2    Randox_1    49.81      8.72
IL2    Randox_2    191.77    33.56
IL2    Randox_3    399.19    69.87
IL4    Randox_1    18.8      3.29
IL4    Randox_2    67.69      11.85
IL4    Randox_3    241.12    42.2
IL6    Randox_1    16.36      2.69}
}
\description{
normalization QC plots, generate
1) analyt/pre vs post scatter plot, colored by plates
2) optional to select a analyt of choice plot for
pre vs post npx by plates scatter plots
3) optional plot to track Randox behavior pre and post normalization
if randox_info is given plot the targeted value
}
