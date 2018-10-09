imptree
=======

[![CRAN_Status_Badge](https://img.shields.io/cran/v/imptree.svg)](https://CRAN.R-project.org/package=imptree)


This R package creates imprecise classification trees. They rely on probability estimation within each node by means of either the imprecise Dirichlet model or the nonparametric predictive inference approach. 

It implements the methods, as described in 
> Fink, P. & Crossman, R. (2013). Entropy Based Decision Trees. In Fabio Cozman, Therry Denoeux, Sebastien Destercke and Teddy Seidfenfeld, editors, *ISIPTA '13, Proceedings of the Eighth International Symposium on Imprecise Probability: Theories and Applications*, pages 139–147. SIPTA [https://www.sipta.org/isipta13/index.php?id=paper&paper=014.html](https://www.sipta.org/isipta13/index.php?id=paper&paper=014.html)

> Abellan, J. & Moral, S. (2003). Building classification trees using the total uncertainty criterion.  *International Journal of Intelligent Systems*, **18**, [doi:10.1002/int.10143](https://dx.doi.org/10.1002/int.10143)

### Installation

To install the CRAN release version: 
```R
install.packages('imptree')
```

To install the latest development release (requires `devtools` installation):
```R
devtools::install_github('paul-fink/imptree')
```


### Citation

If you're using this package, please cite the companion paper in the ISIPTA proceedings

> Fink, P. & Crossman, R. (2013). Entropy Based Decision Trees. In Fabio Cozman, Therry Denoeux, Sebastien Destercke and Teddy Seidfenfeld, editors, *ISIPTA '13, Proceedings of the Eighth International Symposium on Imprecise Probability: Theories and Applications*, pages 139–147. SIPTA [https://www.sipta.org/isipta13/index.php?id=paper&paper=014.html](https://www.sipta.org/isipta13/index.php?id=paper&paper=014.html)

