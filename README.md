# DECART: Data-Efficient Classification And Regression Trees

This work provides an approach to data-efficient performance learning for configurable systems. 

Data-efficient learning is gaining momentum in academia and industry. The key idea is to efficiently reuse the available data and make learning for many small-data problems where acquiring data is expensive, such as personalized healthcare, robot reinforcement learning, sentiment analysis, and community detection (ref: ICML 2016 Workshop on Data-Efficient Machine Learning, https://sites.google.com/site/dataefficientml/).

We bring the idea of data-efficient learning to performance prediction of configurable systems, proposing Data-Efficient Classification And Regression Trees (DECART), a performance prediction method that suffices with a small sample of measured configurations of a software system and that effectively determines a reasonably accurate prediction model therefrom. DECART works automatically and progressively with random samples, such that one can use it to produce predictions, starting with a small random sample, and subsequently extend it when further measurements are available. 

DECART combines our previous approach based on plain CART (ref: https://github.com/jmguo/cpm/) with automated resampling and parameter tuning. Using resampling, DECART learns a prediction model and determines the model's accuracy based on a given sample of measured configurations. Using parameter tuning, DECART ensures that the prediction model has been learned using optimal parameter settings of CART based on the currently available sample. DECART incorporates three well-established resampling methods (hold-out, 10-fold cross-validation, and bootstrapping) and three parameter-tuning techniques (random search, grid search, and Bayesian optimization).

We implemented DECART using GNU R, Version 3.4. In particular, we used the R package rpart to implement the underlying CART and to train the performance models. Moreover, we used the R package rBayesianOptimization to implement Bayesian optimization.

The source code contains the following files:
- main.R : the main entrance to the program. Starting from here, the entire process is automated.
- init.R : the initialization of the program, including setting the bounds of key parameters.
- analysis.R : the core implementation of DECART, including resampling, parameter tuning and progressive sampling.
- regmodels.R : building the prediction models using CART.
- data.R : the analysis of the results, including the calculation of sample quality metrics.
- monitor.R : printing the debugging messages.
