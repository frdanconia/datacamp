# Dimensionality_Reduction
Dimensionality Reduction techniques in R and Python

What ever the original number of independent variables we can end up with two independent variables by applying 
an appropriate dimensionality reduction technique. 

There are two types of Dimensionality reduction techniques:

Feature Selection
	Backward Elimination
	Forward Selection
	Bidirectional Elimination
	Score Comparison

Feature Extraction
	Princible Component Analysis (PCA)
	Linear Discriminat Analysis (LDA)
	Kernel PCA
	Quadratic Discriminant Analysis

DATA SET + BUSINESS PROBLEM DESCRIPTION
These data are the results of a chemical analysis of wines grown in the same region in Italy 
but derived from three different cultivars. The analysis determined the quantities of 13 constituents found serving each 
to the three types of customer segments. 

The attributes are;
1) Alcohol 
2) Malic acid 
3) Ash 
4) Alcalinity of ash 
5) Magnesium 
6) Total phenols 
7) Flavanoids 
8) Nonflavanoid phenols 
9) Proanthocyanins 
10)Color intensity 
11)Hue 
12)OD280/OD315 of diluted wines 
13)Proline

Based on previously validated customer segments, we aim to validate the prediction region and prediction boundary.
But, based on the number of independent variables it is impossible to visualize as a graph. 
Therefore we used dimentionality reduction techniques to find the two new independent variables that explain the most 
variance of the dataset. Then we will be able to see the prediction regions and prediction boundary.  

PCA APPLIED OVER SVM MODEL
From the m independent variables of the dataset, PCA extracts p<=m new independent variables that explain the most 
variance of the dataset regardless of the dependent variable. The fact that the dependent variable is not considered 
makes PCA an unsupervised model.

LDA (LINEAR DISCRIMINANT ANALYSIS) APPLIED OVER SVM MODEL
From the n independent variables of the dataset, LDA extracts p <= n new independent variables that separate the most 
classes of the depedent variable. Since it considers the classes of dependent variable to procced with feature extraction technique, 
this makes LDA a supervised model. Which means, number of linear discriminants will be related to the information on dependent variable.
That information is the number of classes in the dependent variable. 

KERNEL PCA APPLIED OVER A LINEAR CLASSIFIER LOGISTIC REGRESSION
Previous feature extraction techniques works on linear data and requires a linearly separable data. 
For a non-linearly separable data we need Kernel PCA to reduce dimensionality. 
Kernel PCA is a kernalised version of the PCA where we map the data to a higher dimension using Kernel trick and 
then we extract new principle components. 

# we use a non-linear dataset. And already tested non-linear classifiers showed a better perfornce on this dataset. 
Below codes aim to test how Kernel PCA handles non-linearity. 


