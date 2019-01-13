#!/usr/bin/env python3

import numpy as np
from   sklearn import datasets
from   sklearn.linear_model import LogisticRegression, Perceptron
from   sklearn.metrics import accuracy_score
from   sklearn.model_selection import train_test_split
from   sklearn.preprocessing import StandardScaler

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

# instead of Perceptron use:
#   LogisticRegression
#   sklearn.svm.SVC
#   sklearn.tree.DecisionTreeClassifier
#   sklearn.neighbors.KNeighborsClassifier

models = {
    # 'LogisticRegression': LogisticRegression(),
    'Perceptron': Perceptron(
        eta0=0.001,  # less = better
        max_iter=40,  # greater = better
        random_state=0,
        tol=0.001,
    ),
}

for model_name, model in models.items():
    print(model_name)
    model.fit(x_train_std, y_train)

    y_pred = model.predict(x_test_std)
    deviations = sum(y_pred != y_test)
    # print(
    #     '\tdeviations in prediction:', deviations, '/', len(y_test), '=',
    #     deviations / len(y_test),
    # )
    print('\taccuracy_score:', accuracy_score(y_test, y_pred))
