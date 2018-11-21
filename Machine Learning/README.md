# Practical Machine Learning

<p>This repository is a collection of all homework assignments completed for Northwestern University MSDS program in 'Practical Machine Learning' (MSDS 422). Each assignment includes a PDF summary of the analysis as well as a link to the code executed in Google Colabratory.</p>

<h4>Assignment 1 - Program Objectives</h4>
<p>This project is based on a collection of data from the ‘MSPA Software Survey’, in which the university gathered responses from students and faculty in relation to their own preferences and their perception of industry trends. The analysis provides code for data review in Python as well as initial data visualization tecqhniques and analysis of correlation.</p>

<h4>Assignment 2 - Term Deposit Participation</h4>
<p>An exercise in modelling consumer behaviors with a binary response variable - is customer likely to purchase a service or not? The example is taken from: Miller, T. W. 2015. 'Marketing Data Science: Modeling Techniques in Predictive Analytics with R and Python'. Model techniques include SciKit Learn 'KNeighborsClassifier', 'Naive Bayes' & 'Logistic Regression' as well as cross validation methods such as ROC curve.</p>

<h4>Assignment 3 - Housing Sales Results, part 1</h4>
<p>An exercise using SciKit Learn Machine Learning methods to determine the best fit for a linear model. The exercise uses the Boston housing data from: David A. Belsley, Edwin Kuh, and Roy E.Welsch. 'Regression Diagnostics: Identifying Influential Data and Sources of Collinearity.' I begin with data evaluation and visualization, and then evaluate modeling techniques using SciKit Learn Linear, Lasso and Ridge Regression models. I also experiement with data transformation methods to normalize the data, and the measure the changes in model performance.</p>

<h4>Assignment 4 - Housing Sales Results, part 2</h4>
<p>This exercise builds on the linear regression models we developed for assignment 3 and uses the same data set. We drop the variable 'neighborhood' and use the remaining continuous data variables to build multiple models using SciKit Learn: Ridge, DecisionTreeRegressor, RandomForestRegressor, & GradientBoostingRegressor. By experimenting with bootstrapping and data transformation techniques (Log transform), we are able to build models that perform better than the regression models in assignment 3.</p>

<h4>Assignment 5 - Optical Recognition</h4>
<p>This exercise uses a random forest learning method for multiclass prediction of handwritten digits in the MNIST dataset. (https://en.wikipedia.org/wiki/MNIST_database) The challenge is to build a model that successfully assigns a predicted digit value that is equal to the handwritten one, while keeping the number of variables to a minimum to reduce processing times. The model techniques use RandomForestClassifier, and compares compute times when using all potential variables or a reduced data set using a PCA model that accounts for 95% of the variation in the data. For cross validation, we consider average F1 and precision scores as well as a confusion matrix to visually represent the multi-class output accuracy.</p>

<h4>Assignment 6 - Model Development</h4>
<p>Assignment 6 builds on the analysis of assignment #5, and requires the application of an artificial neural network (ANN) for prediction of handwritten digits in the MNIST dataset. In this assignment, we apply the TensorFlow DNNClassifier instead of SciKit Learn’s Random Forest Classifer. The analysis looks at how changing the number of hidden layers and neurons within each layer impacts the amount of time it takes to train the model relative to the accuracy of the estimated output.</p>

<h4>Assignment 7 - User-Content Identification</h4>
<p>Assignment 7 builds on the models used for assignment #6 but with a different data set. The objective of the analysis is to build a neural network that accurately classifies images of cats and dogs. The data set is a sub-set of the data used in the kaggle competition. It contains 12,500 cat pictures and 12,500 dog pictures. The example goes through the steps to prepare the data and assign values to Numpy arrays to concatenate, shape and scale the data easily. We then apply the layered DNN models developed for the previous assignment.</p>
