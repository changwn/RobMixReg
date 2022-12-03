# RobMixReg 0.1.0

* Initial version on CRAN.

# RobMixReg 0.2.0

*  Update CTLE function to CTLERob function.
This new function have one more parameter 'rlr_method' which let user choose the robust regression method in 'lmRob','lmrob','ltsReg'.

*  Update class definition of RobMixReg.
The new class add one slot which return the posterior probability of the mixture regression.

# RobMixReg 0.2.1

* Our paper published on arxiv, please cite us. For more detail of the proposed method, please refer to DESCRIPTION file.


# RobMixReg 1.0.0

* Add method mixtureReg which let user run the mixture regression model flexibility by fixing the specific coefficient (slpoe value).

* Add method Regression Based Suspace Learning (RBSL) that enable to fitting the mixture model for high dimension variable. The implementation is 'CSMR' function.

* Add new wrapper function to deal with following four scenarios: 

1) robust regression: one regression line and outliers.

2) flexible mixture regression: two regression lines without outliers. The flexible means that the coefficient of the regression line can be declare by the user.

3) robust mixture regression: two regression lines with outliers. The algorithm to solve this challenge is Component-wise Adaptive Trimming (CAT). the implementation is 'CTLERob' function. The details of the algorithm please refer our paper.

4) supervised subspace clustering: clustering heterogeneity objects into subgroup and selecting contributed attributes simultaneously. The algorithm to solve this challenge is RBSL. The details of the algorithm please refer our paper.

* Add plot module for the above four scenarios. 

* Add two real dataset: Breast cancer multi-omic data and Two genes cytokine response data. The detail and example refer to package manual and vignette.

# RobMixReg 1.1.0

* add several wrapper function

* add model selection method

* update vignette

