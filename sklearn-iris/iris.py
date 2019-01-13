#!/usr/bin/env python3

import numpy as np
from   sklearn import datasets
from   sklearn.linear_model import LogisticRegression, Perceptron
from   sklearn.metrics import accuracy_score
from   sklearn.model_selection import train_test_split
from   sklearn.preprocessing import StandardScaler
from   sklearn.svm import SVC
#   sklearn.tree.DecisionTreeClassifier
#   sklearn.neighbors.KNeighborsClassifier

iris = datasets.load_iris()
x = iris.data
y = iris.target

# 30% = test, 70% = train
x_train, x_test, y_train, y_test = train_test_split(
    x, y, test_size=0.3, random_state=0,
)

scaler = StandardScaler()
scaler.fit(x_train)
x_train_std = scaler.transform(x_train)
x_test_std  = scaler.transform(x_test)
# see also sklearn.preprocessing.normalize

models = {
    LogisticRegression(
        max_iter=100, multi_class='auto', solver='liblinear'
    ),
    Perceptron(eta0=0.001, max_iter=100, tol=0.001),
    SVC(),
}

for model in models:
    print(type(model).__name__)
    model.fit(x_train_std, y_train)

    y_pred = model.predict(x_test_std)
    deviations = sum(y_pred != y_test)
    # print(
    #     '\tdeviations in prediction:', deviations, '/', len(y_test), '=',
    #     deviations / len(y_test),
    # )
    print('\taccuracy:', accuracy_score(y_test, y_pred))
