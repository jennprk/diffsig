# Diffsig: Associating Risk Factors with Mutational Signatures

## Introduction and Motivation

Somatic mutations accumulate over an individual’s lifetime due to mutagenic processes such as UV exposure, smoking, etc. These processes leave distinct patterns of mutations in the genome, known as mutational signatures which are used to predict therapy response and to understand the molecular mechanisms driving cancer.

Studies focused on detecting novel signatures based on non-matrix Factorization (NMF) algorithms, but linking underlying biological causes were challenging due to uncertainty in signature contributions (low counts, sequencing coverage, tumor purity, etc.) [3,4,5]

To address the limitations, we present **Diffsig**, a hierarchical Bayesian Dirichlet-Multinomial model designed to estimate association between mutational signatures and risk factors.

## Key Highlights of **Diffsig**

-   **Diffsig** allows to use preferred set of mutational signatures either detected by preferred classification method, or selected from large databases like COSMIC.\
-   **Diffsig** accounts for sample-specific uncertainty.\
-   **Diffsig** allows to test multiple risk factors simultaneously, enabling robust and unbiased inference.\
-   **Diffsig** allows not only binary, but also categorical and continuous risk factors, allowing a wide selection of clinical variables.

## Schematic of **Diffsig** Model

<p align="center">
<img src="https://github.com/jennprk/diffsig/blob/main/vignettes/figures/schematic/schematic.001.png" width="600"/>
</p>

## Installation in R

The **Diffsig** R package can be installed using devtools. **Diffsig** requires RStan which utilizes compilation of C++ code.

```         
library(devtools) 
install_github("jennprk/diffsig")
```

## Citation

``` tex
@article{
 Diffsig, 
 author = {Park, Ji-Eun and Smith, Markia A. and Van Alsten, Sarah C. and Walens, Andrea and Wu, Di and Hoadley, Katherine A. and Troester, Melissa A. and Love, Michael I.}, 
 title = {Diffsig: Associating Risk Factors with Mutational Signatures}, 
 journal = {Cancer Epidemiology, Biomarkers & Prevention}, 
 volume = {33}, 
 number = {5}, 
 pages = {721-730}, 
 year = {2024}, 
 issn = {1055-9965}, 
 doi = {10.1158/1055-9965.EPI-23-0728}
 }
```
