# Robust Mixture Regression



<!-- badges: start -->

[![License](http://img.shields.io/badge/license-GPL%20v3-orange.svg?style=flat)](http://www.gnu.org/licenses/gpl-3.0.en.html)
![Download](https://cranlogs.r-pkg.org/badges/RobMixReg)
[![CRAN_Status_Badge](https://www.r-pkg.org/badges/version-ago/RobMixReg)](https://CRAN.R-project.org/package=RobMixReg)
[![cran checks](https://cranchecks.info/badges/worst/RobMixReg)](https://CRAN.R-project.org/web/checks/check_results_RobMixReg.html)


[![documentation](https://github.com/laresbernardo/lares/workflows/documentation/badge.svg)](https://CRAN.R-project.org/package=RobMixReg/RobMixReg.pdf) 
[![travis](https://travis-ci.com/laresbernardo/lares.svg?branch=master)](https://travis-ci.org/github/changwn/RobMixReg)
[![Hi](https://img.shields.io/badge/say-hi-blue.svg)](https://changwn.github.io/)

<!-- badges: end -->

[r package link](https://CRAN.R-project.org/package=RobMixReg) [https://CRAN.R-project.org/package=RobMixReg]

[manual document](https://CRAN.R-project.org/package=RobMixReg/RobMixReg.pdf) [https://CRAN.R-project.org/package=RobMixReg/RobMixReg.pdf]

[Paper download here:](https://arxiv.org/abs/2005.11599) [<Wennan Chang et al. , A New Algorithm using Component-wise Adaptive Trimming For Robust Mixture Regression, arxiv, 2020>](https://arxiv.org/abs/2005.11599)

[Wennan Chang](https://changwn.github.io/), [Chi Zhang\* ](https://zcslab.github.io/), [Sha Cao\* ](https://medicine.iu.edu/faculty/38873/cao-sha)

[Biomedical Data Research Lab (BDR)](https://zcslab.github.io/) 

[For other software, please refer to the BDR github page](https://github.com/zcslab)


### Robust Mixture Regression Plot (with outliers)
![[line1]](pic1.png)


### Add Regresseion Line
![[line2]](pic2.png)

# News
2020-02-25 

* version 0.1.0 released

2020-05-10 

* version 0.2.0 released

* Update CTLE function to CTLERob function.
This new function have one more parameter 'rlr_method' which let user choose the robust regression method in 'lmRob','lmrob','ltsReg'.

* Update class definition of RobMixReg.
The new class add one slot which return the posterior probability of the mixture regression.

2020-05-26

* version 0.2.1 released

* Our paper published on arxiv, please cite us. For more detail of the proposed method, please refer to DESCRIPTION file.

# Install from CRAN
```
install.packages("RobMixReg)
library("RobMixReg")
```

# Install from github for most updated package. 
#### Please report the bug as the description in the Question&Problem.
```
library("devtools")
devtools::install_github("changwn/RobMixReg")
```

# Example
```
library(RobMixReg)
#library(robust)
library(flexmix)
library(robustbase)
library(MASS)
library(gtools)

# gaussData
x=(gaussData$x);y=as.numeric(gaussData$y);
formula01=as.formula("y~x")
example_data01=data.frame(x,y)

res_rmr = rmr(lr.method='flexmix', formula=formula01, data=example_data01)
res_rmr = rmr(lr.method='TLE', formula=formula01, data=example_data01)
res_rmr = rmr(lr.method='CTLERob', formula=formula01, data=example_data01)
res_rmr = rmr(lr.method='mixbi', formula=formula01, data=example_data01)
res_rmr = rmr(lr.method='mixLp', formula=formula01, data=example_data01)

# simuData
example_data02 <- simuData[,1:3]
formula02=as.formula("y~X1+X2")

res_rmr = rmr(lr.method='flexmix', formula=formula01, data=example_data01, nc=3)
res_rmr = rmr(lr.method='TLE', formula=formula01, data=example_data01, nc=3,tRatio=0.05)
res_rmr = rmr(lr.method='CTLERob', formula=formula01, data=example_data01, nc=3)
res_rmr = rmr(lr.method='mixbi', formula=formula01, data=example_data01, nc=3)
res_rmr = rmr(lr.method='mixLp', formula=formula01, data=example_data01, nc=3)

```

## Citations
If you find the code helpful in your resarch or work, please cite us.
```BibTex
@article{wennan2020cat,
  title={A New Algorithm using Component-wise Adaptive Trimming For Robust Mixture Regression},
  author={Chang, Wennan and Wan, Changlin and Zhou, Xinyu and Zhang, Chi and Cao, Sha},
  journal={arXiv preprint arXiv:2005.11599},
  year={2020}
}
```

# Questions & Problems

If you have any questions or problems, please feel free to open a new issue [here](https://github.com/changwn/RMR/issues). We will fix the new issue ASAP.  You can also email the maintainers and authors below.

- [Wennan Chang](https://changwn.github.io/)
(wnchang@iu.edu)

PhD candidate at BDR group, Indiana University School of Medicine

- [Sha Cao](https://medicine.iu.edu/faculty/38873/cao-sha/)
(shacao@iu.edu)

Assistant Professor

Department of Biostatistics, Indiana University School of Medicine
