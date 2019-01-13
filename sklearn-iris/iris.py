#!/usr/bin/env python3

import numpy as np
from   sklearn import datasets
from   sklearn.linear_model import LogisticRegression, Perceptron
from   sklearn.metrics import accuracy_score
from   sklearn.model_selection import train_test_split
from   sklearn.preprocessing import StandardScaler
from   sklearn.svm import SVC
from   sklearn.tree import DecisionTreeClassifier
from   sklearn.neighbors import KNeighborsClassifier

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

models = [
    Perceptron(eta0=0.001, max_iter=100, tol=0.001),
    LogisticRegression(multi_class='auto', solver='liblinear'),
    SVC(),
    DecisionTreeClassifier(),
    KNeighborsClassifier(),
]

for model in models:
    print(type(model).__name__ + ':')
    model.fit(x_train_std, y_train)
    y_pred = model.predict(x_test_std)
    print('\taccuracy:', accuracy_score(y_test, y_pred))
