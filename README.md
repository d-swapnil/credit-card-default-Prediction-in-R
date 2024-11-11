# Credit Card Default Prediction in R

This project aims to predict credit card defaults using various machine learning techniques implemented in R. By analyzing customer data, we can identify patterns that indicate a higher likelihood of default, enabling financial institutions to make informed decisions.

## Table of Contents

- [Introduction](#introduction)
- [Dataset](#dataset)
- [Data Preprocessing](#data-preprocessing)
- [Modeling Techniques](#modeling-techniques)
- [Model Evaluation](#model-evaluation)
- [Results](#results)
- [Conclusion](#conclusion)
- [References](#references)

## Introduction

Credit card default prediction is crucial for financial institutions to manage risk and maintain profitability. This project explores the application of machine learning models to predict the likelihood of a customer defaulting on their credit card payment.

## Dataset

The dataset used in this project is sourced from the UCI Machine Learning Repository and contains information on default payments, demographic factors, credit data, payment history, and bill statements of credit card clients in Taiwan.

- **Number of Instances**: 30,000
- **Number of Attributes**: 24

Key attributes include:

- `LIMIT_BAL`: Amount of given credit (NT dollar)
- `SEX`: Gender (1 = male; 2 = female)
- `EDUCATION`: Education level (1 = graduate school; 2 = university; 3 = high school; 4 = others)
- `MARRIAGE`: Marital status (1 = married; 2 = single; 3 = others)
- `AGE`: Age in years
- `PAY_0` to `PAY_6`: Past monthly payment records
- `BILL_AMT1` to `BILL_AMT6`: Bill statement amounts
- `PAY_AMT1` to `PAY_AMT6`: Previous payments
- `default.payment.next.month`: Default payment (1 = yes; 0 = no)

## Data Preprocessing

Data preprocessing steps included:

1. **Handling Missing Values**: Identified and imputed missing values appropriately.
2. **Feature Engineering**: Created new features to capture additional information.
3. **Normalization**: Scaled numerical features to ensure uniformity.
4. **Encoding Categorical Variables**: Converted categorical variables into numerical format using one-hot encoding.

## Modeling Techniques

Several machine learning models were implemented and evaluated:

- **Logistic Regression**: A baseline model to assess linear relationships.
- **Decision Trees**: Captured non-linear patterns in the data.
- **Random Forest**: An ensemble method to improve prediction accuracy.
- **Support Vector Machines (SVM)**: Classified data by finding the optimal hyperplane.
- **Neural Networks**: Modeled complex relationships through multiple layers.

## Model Evaluation

Models were evaluated using the following metrics:

- **Accuracy**: Proportion of correctly predicted instances.
- **Precision**: Proportion of true positive predictions among all positive predictions.
- **Recall**: Proportion of true positive predictions among all actual positives.
- **F1-Score**: Harmonic mean of precision and recall.
- **ROC-AUC**: Area under the Receiver Operating Characteristic curve.

## Results

The Random Forest model achieved the highest performance with:

- **Accuracy**: 81%
- **Precision**: 75%
- **Recall**: 70%
- **F1-Score**: 72%
- **ROC-AUC**: 0.85

These results indicate that the Random Forest model effectively balances precision and recall, making it suitable for predicting credit card defaults.

## Conclusion

Predicting credit card defaults is vital for financial risk management. This project demonstrates that machine learning models, particularly Random Forests, can effectively predict defaults by analyzing customer data. Future work may involve exploring additional features, tuning hyperparameters, and implementing more advanced models to further enhance prediction accuracy.

## References

- UCI Machine Learning Repository: [Default of Credit Card Clients Dataset](https://archive.ics.uci.edu/ml/datasets/default+of+credit+card+clients)
- Swapnil Deshpande, "Credit Card Default Prediction in R," (Group_Project.pdf)
- Swapnil Deshpande, "Credit Card Default Prediction Presentation," (Group 2_PPT.pptx)
