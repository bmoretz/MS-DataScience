# Practical Machine Learning

<p>This repository is a collection of all homework assignments completed for Northwestern University MSDS program in 'Practical Machine Learning' (MSDS 422). Each assignment includes a PDF summary of the analysis as well as a link to the code executed in Google Colabratory.</p>

<h3>1 - Program Objectives</h3>

<blockquote>
  
  <h4><a href="https://github.com/bmoretz/MS-DataScience/raw/master/Machine%20Learning/Program%20Objectives.pdf">Management Problem</a></h4>
  
  <p>This project is based on a collection of data from the ‘MSPA Software Survey’, in which the university gathered responses from students and faculty in relation to their own preferences and their perception of industry trends. The analysis provides code for data review in Python as well as initial data visualization tecqhniques and analysis of correlation.</p>
  
  <ul>
  <li>Learn about current student software preferences</li>
  <li>Learn about student interest in potential new courses</li>
  <li>Guide software and systems planning for current and future courses</li>
  <li>Guide data science curriculum planning</li>
  </ul>
  
  <h4><a href="https://colab.research.google.com/github/bmoretz/MSDS-MachineLearning/blob/master/Assignment_01/Assignment_01.ipynb" target="_blank">Solution Overview</a></h4>
  
  <p>This project is based on a collection of data from the ‘MSPA Software Survey’, in which the university gathered responses from students and faculty in relation to their own preferences and their perception of industry trends. The analysis provides code for data review in Python as well as initial data visualization tecqhniques and analysis of correlation.</p>

</blockquote>

<h3>2 - Term Deposit Participation</h3>

<blockquote>
  
  <h4><a href="https://github.com/bmoretz/MS-DataScience/raw/master/Machine%20Learning/Term%20Deposit%20Participation.pdf">Management Problem</a></h4>
  
  <p>Imagine that you are advising the bank about machine learning methods to guide telephone marketing campaigns.</p>
  
  <p><strong>Management Questions</strong></p>
  
  <ul>
  <li>Which of the two modeling methods would you recommend, and why?</li>
  <li>And, given the results of your research, which group of banking clients appears to be the best target for direct marketing efforts (similar to those used with previous telephone campaigns)? </li>
  </ul>
  <ul>
  
  <h4><a href="https://colab.research.google.com/github/bmoretz/MSDS-MachineLearning/blob/master/Assignment_02/Assignment_02.ipynb" target="_blank">Solution Overview</a></h4>
  
  <p>An exercise in modelling consumer behaviors with a binary response variable - is customer likely to purchase a service or not? Model techniques include SciKit Learn 'KNeighborsClassifier', 'Naive Bayes' & 'Logistic Regression' as well as cross validation methods such as ROC curve.</p>

</blockquote>

<h3>3 - Housing Sales Results, part 1</h3>

<blockquote>
  
  <h4><a href="https://github.com/bmoretz/MS-DataScience/raw/master/Machine%20Learning/Housing%20Sales%20Results_01.pdf">Management Problem</a></h4>
  
  <p>Imagine that you are advising a real estate brokerage firm in its attempt to employ machine learning methods.</p>
  
  <p><strong>Management Questions</strong></p>
  
  <ul>
  <li>The firm wants to use machine learning to complement conventional methods for assessing the market value of residential real estate.</li>
  <li>Of the modeling methods examined in your study, which would you recommend to management, and why?</li>
  </ul>
  
  <h4><a href="https://colab.research.google.com/github/bmoretz/MSDS-MachineLearning/blob/master/Assignment_03/Assignment_03.ipynb" target="_blank">Solution Overview</a></h4>
  
  <p>An exercise in modelling consumer behaviors with a binary response variable - is customer likely to purchase a service or not? Model techniques include SciKit Learn 'KNeighborsClassifier', 'Naive Bayes' & 'Logistic Regression' as well as cross validation methods such as ROC curve.</p>

</blockquote>	
 
<h3>4 - Housing Sales Results, part 2</h3>

<blockquote>
  
  <h4><a href="https://github.com/bmoretz/MS-DataScience/raw/master/Machine%20Learning/Housing%20Sales%20Results_02.pdf">Management Problem</a></h4>
  <p>Imagine that you are advising a real estate brokerage firm in its attempt to employ machine learning methods.</p>
      
  <p><strong>Management Questions</strong></p>
  
  <ul>
  <li>The firm wants to use machine learning to complement conventional methods for assessing the market value of residential real estate.</li>
  <li>Of the modeling methods examined in your study, which would you recommend to management, and why?</li>
  <li>Reviewing the results of the random forests, which explanatory variables are most important in predicting home prices?</li>
  </ul>
  
  <h4><a href="https://colab.research.google.com/github/bmoretz/MSDS-MachineLearning/blob/master/Assignment_04/Assignment_04.ipynb" target="_blank">Solution Overview</a></h4>
  
  <p>This exercise builds on the linear regression models we developed for assignment 3 and uses the same data set. We drop the variable 'neighborhood' and use the remaining continuous data variables to build multiple models using SciKit Learn: Ridge, DecisionTreeRegressor, RandomForestRegressor, & GradientBoostingRegressor. By experimenting with bootstrapping and data transformation techniques (Log transform), we are able to build models that perform better than the regression models in assignment 3.</p>

</blockquote>

<h3>5 - Model Development</h3>

<blockquote>
  
  <h4><a href="https://github.com/bmoretz/MS-DataScience/raw/master/Machine%20Learning/Model%20Development.pdf">Management Problem</a></h4>
  
  <p>Suppose you are a financial institution evaluating machine learning technologies for optical character recognition. Initial testing is on the MNIST digits.</p>

  <p><strong>Management Questions</strong></p>
      
  <ul>
  <li>What can you conclude from your benchmark study?</li>
  <li>Which neural network typology and hyperparameter settings would you recommend as being the most trustworthy?</li>
  </ul>
  
  <h4><a href="https://colab.research.google.com/github/bmoretz/MSDS-MachineLearning/blob/master/Assignment_05/Assignment_05.ipynb" target="_blank">Solution Overview</a></h4>
  
  <p>The Benchmark Experiment. Tested neural network structures should be explored within a benchmark experiment, a factorial design with at least two levels on each of two experimental factors (at least a 2x2 completely crossed design). But due to the time required to fit each neural network, we will observe only one trial for each cell in the design. Also, we will be using a simple training-and-test split with the split having been performed previously. That is, we use the training and test sets that come with <a href="https://en.wikipedia.org/wiki/MNIST_database">MNIST</a>.</p>

<p>An example experiment could include two values for the number of nodes per inner layer and two values for the number of inner layers. Various machine learning hyperparameter settings may be used.</p>
	
</blockquote>

<h3>6 - Optical Recognition</h3>

<blockquote>
  
  <h4><a href="https://github.com/bmoretz/MS-DataScience/raw/master/Machine%20Learning/Optical%20Recognition.pdf">Management Problem</a></h4>
  
  <p>From a management perspective, the predictive accuracy of models must be weighed against the costs of model development and implementation.</p>

  <p><strong>Management Questions</strong></p>
      
  <ul>
  <li>Suppose you were the manager of a data science team responsible for implementing models for computer vision (classification of images analogous to the MINST problem)</li>
  <li>Would you recommend using PCA as a preliminary to machine learning classification? Explain your thinking?</li>
  </ul>
  
  <h4><a href="https://colab.research.google.com/github/bmoretz/MSDS-MachineLearning/blob/master/Assignment_06/Assignment_06.ipynb" target="_blank">Solution Overview</a></h4>
  
  <p>This exercise uses a random forest learning method for multiclass prediction of handwritten digits in the MNIST dataset. <a href="https://en.wikipedia.org/wiki/MNIST_database">(https://en.wikipedia.org/wiki/MNIST_database)</a> The challenge is to build a model that successfully assigns a predicted digit value that is equal to the handwritten one, while keeping the number of variables to a minimum to reduce processing times. The model techniques use Random Forest Classifier, and compares compute times when using all potential variables or a reduced data set using a PCA model that accounts for 95% of the variation in the data. For cross validation, we consider average F1 and precision scores as well as a confusion matrix to visually represent the multi-class output accuracy.</p>
	
</blockquote>

<h3>7 - User-Content Identification</h3>

<blockquote>
  
  <h4><a href="https://github.com/bmoretz/MS-DataScience/raw/master/Machine%20Learning/User-Content%20Identification.pdf">Management Problem</a></h4>
  
  <p>Assume that we are providing advice to a website provider who is looking for tools to automatically label images provided by endusers. As we look across the factors in the study, making recommendations to management about image classification, we are most concerned about achieving the highest possible accuracy in image classification.</p>

  <p><strong>Management Questions</strong></p>
      
  <ul>
  <li>That is, we should be willing to sacrifice training time for model accuracy. What type of machine learning model works best?</li>
  <li>If it is a Convolutional Neural Network, what type of network should we use?</li>
  <li>Part of this recommendation may concern information about the initial images themselves (input data for the classification task). What types of images work best?</li>
  </ul>
  
  <h4><a href="https://colab.research.google.com/github/bmoretz/MSDS-MachineLearning/blob/master/Assignment_07/Assignment_07.ipynb" target="_blank">Solution Overview</a></h4>
  
<p>The objective of the analysis is to build a neural network that accurately classifies images of cats and dogs. The data set is a sub-set of the data used in the kaggle competition. It contains 12,500 cat pictures and 12,500 dog pictures. The example goes through the steps to prepare the data and assign values to Numpy arrays to concatenate, shape and scale the data easily. We then apply the layered DNN models developed for the previous assignment.</p>		

</blockquote>
  
<h3>8 - Natural Language Classification</h3>

<blockquote>
  
  <h4><a href="https://github.com/bmoretz/MS-DataScience/raw/master/Machine%20Learning/Natural%20Language%20Classification.pdf">Management Problem</a></h4>
  
  <p>Suppose management is thinking about using a language model to classify written customer reviews, call, and complaint logs. If the most critical customer messages can be identified, then customer support personnel can be assigned to contact those customers.</p>
  
<ul>
  <li>How would you advise senior management? What kinds of systems and methods would be most relevant to the customer services function?</li>
  <li>Considering the results of this assignment, what is needed to make an automated customer support system that can identify negative customer feelings?</li>
  <li>What can data scientists do to make language models more useful in customer service function?</li>
  </ul>
 
  <h4><a href="https://colab.research.google.com/github/bmoretz/MSDS-MachineLearning/blob/master/Assignment_08/Assignment_08.ipynb" target="_blank">Solution Overview</a></h4>
  
<p>This assignment involves working with language models developed with pre-trained word vectors. We used sentences (sequences of words) to train language models for predicting movie review sentiment (thumbs-up versus thumbs-down). We study effects of word vector size, vocabulary size, and neural network structure (hyperparameters) on classification performance. We build on resources for recurrent neural networks (RNNs) as implemented in TensorFlow. RNNs are well suited to the analysis of sequences, as needed for natural language processing (NLP).</p>

</blockquote>
