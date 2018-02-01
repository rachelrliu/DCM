## Overview
This project applied discrete choice model on 2 years of Nielsen scanner data to model customer behavior and estimate customer demand.

## The Model
Discrete Choice Model is a model developed by Daniel McFadden, who won the Nobel prize in 2000 for his pioneering work in developing the theoretical basis for this model. It is a model that describes, explains, and predicts choices between two or more discrete alternatives. 

All transactions involve choice. By looking at choices people make, we can understand how they trade off elements of the marketing mix: Price, Features, Brand, Advertising, Channels etc. The model decomposes customer choices into systematic (observable and predictable) and non-systematic (unobservable or random) components, then models the systematic components. In essence, we are modeling the probabilities that consumers buy or don't buy products with certain configurations in a competitive marketplace.

Related Papers:
- Berry, S. 1994. "Estimating Discrete-Choice Models of Product Differentiation," The RAND Journal of Economics (25:2), Sum, pp. pp. 242-262.[[pdf](https://github.com/rliiu/DCM/blob/master/Ref/Estimating%20Discrete-Choice%20Models%20of%20Product%20Differentiation.pdf)]
- Berry, S., Levinsohn, J., and Pakes, A. 1995. "Automobile Prices in Market Equilibrium," Econometrica (63:4), Jul, pp. 841-890.[[pdf](https://github.com/rliiu/DCM/blob/master/Ref/Automobile%20Prices%20in%20Market%20Equilibrium.pdf)]
- Nevo, Aviv. "A Practitioner's Guide to Estimation of Random‐Coefficients Logit Models of Demand," Journal of Economics & Management Strategy 9.4 (2000): 513-548.[[pdf](https://github.com/rliiu/DCM/blob/master/Ref/A%20Practitioner's%20Guide%20to%20Estimation%20of%20Random%E2%80%90Coefficients%20Logit%20Models%20of%20Demand.pdf)]
- Stock, J.H., and Yogo, M. 2005. "Testing for Weak Instruments in Linear Iv Regression," Identification and inference for econometric models: Essays in honor of Thomas Rothenberg (For Instruments Variables Testing) [[pdf](https://github.com/rliiu/DCM/blob/master/Ref/Testing%20for%20Weak%20Instruments%20in%20Linear%20Iv%20Regression.pdf)]
- Hansen, L.P. 1982. "Large Sample Properties of Generalized Method of Moments Estimators," Econometrica: Journal of the Econometric Society, pp. 1029-1054.(For Instruments Variables Testing)[[pdf](https://github.com/rliiu/DCM/blob/master/Ref/Large%20Sample%20Properties%20of%20Generalized%20Method%20of%20Moments%20Estimators.pdf)]

## Modeling Process
[[R code](https://github.com/rliiu/DCM/tree/master/Modeling)]

prepare_data_frame.R intergrates data from multiple sources, then segregates and defines competitive market, and format dataframe for multilevel fixed effect estimation.

The data then feed into run_DCM for parameter estimation. If no endogeneity problem arises, the estimated parameters are passed on to calculate_elastisities.R to calculate elasticties for Tableau dashboard building.



 ## Visualization and Presentation 
 
The estiamted price elasticities were visualized by a Tableau dashboard, and a presentation was given to senior management.
