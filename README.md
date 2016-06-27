# DECART: Data-Efficient Classification And Regression Trees
This work provides a data-efficient performance learning approach for configurable systems. 

Data-efficient learning is gaining momentum in academia and industry. The key idea is to efficiently reuse the available data and make learning for many small-data problems where acquiring data is expensive, such as personalized healthcare, robot reinforcement learning, sentiment analysis, and community detection (ref: ICML 2016 Workshop on Data-Efficient Machine Learning, https://sites.google.com/site/dataefficientml/).

DECART combines Classification And Regression Trees (CART) with bootstrapping, grid search, and progressive sampling. A previous approach based on plain CART can be found here: https://github.com/jmguo/cpm.

The implementation language is R version 3.2.4-revised.
