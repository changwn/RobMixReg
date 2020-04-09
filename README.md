# Robust Mixture Regression

[r package link](https://cran.r-project.org/web/packages/RobMixReg/index.html) [https://cran.r-project.org/web/packages/RobMixReg/index.html]

[manual document](https://cran.r-project.org/web/packages/RobMixReg/RobMixReg.pdf) [https://cran.r-project.org/web/packages/RobMixReg/RobMixReg.pdf]

### Robust Mixture Regression Plot (with outliers)
![[line1]](pic1.png)


### Add Regresseion Line
![[line2]](pic2.png)

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
res_rmr = rmr(lr.method='CTLE', formula=formula01, data=example_data01)
res_rmr = rmr(lr.method='mixbi', formula=formula01, data=example_data01)
res_rmr = rmr(lr.method='mixLp', formula=formula01, data=example_data01)

# simuData
example_data02 <- simuData[,1:3]
formula02=as.formula("y~X1+X2")

res_rmr = rmr(lr.method='flexmix', formula=formula01, data=example_data01, nc=3)
res_rmr = rmr(lr.method='TLE', formula=formula01, data=example_data01, nc=3,tRatio=0.05)
res_rmr = rmr(lr.method='CTLE', formula=formula01, data=example_data01, nc=3)
res_rmr = rmr(lr.method='mixbi', formula=formula01, data=example_data01, nc=3)
res_rmr = rmr(lr.method='mixLp', formula=formula01, data=example_data01, nc=3)

```
# Questions & Problems

If you have any questions or problems, please feel free to open a new issue [here](https://github.com/changwn/RMR/issues). We will fix the new issue ASAP.  You can also email the maintainers and authors below.

- [Wennan Chang](https://zcslab.github.io/people/wennan/)
(wnchang@iu.edu)

PhD candidate at BDR group, Indiana University School of Medicine

- [Sha Cao](https://medicine.iu.edu/faculty/38873/cao-sha/)
(shacao@iu.edu)

Assistant Professor

Department of Biostatistics, Indiana University School of Medicine
