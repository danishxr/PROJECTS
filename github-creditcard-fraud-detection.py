import numpy as np
import matplotlib.pyplot as plt  
import pandas as pd  
 
#load the data set we are going to work on it is a csv file  
data=pd.read_csv('creditcard.csv')
#lets look at the top five observations
data.head()
#counting the different class and making a bar chart to visualize  to know the #representation of each classes  and plot it  
count_classes=pd.value_counts(data['Class'], sort=True)
count_classes.plot(kind='bar')
plt.title("Fraud class histogram")
plt.xlabel("Class")
plt.ylabel("Frequency")

data=data.drop(['Time','Amount'],axis=1)
#now we need to specify our dependent and independent variables  
 
X = data.ix[:, data.columns != 'Class'].copy()
y = data.ix[:, data.columns == 'Class'].copy()


#lets start the procedure.
# Number of data points in the minority class and getting there indices  
number_records_fraud = len(data[data.Class == 1])
fraud_indices = np.array(data[data.Class == 1].index)
 
# Picking the indices of the normal classes
normal_indices = data[data.Class == 0].index
 
#lets get the random sample and make it the size of our fraud sample  
random_normal_indices=np.random.choice(normal_indices,number_records_fraud,replace=False)
random_normal_indices=np.array(random_normal_indices)
 
# Appending the 2 indices
under_sample_indices = np.concatenate([ fraud_indices,random_normal_indices])
 
#now getting the data together(so we have a dataset which has representation of both the classes in equal proportion )
under_sample_data=data.iloc[under_sample_indices,:]
 
#preparing the undersample X and Y variables i.e loading the dependent and independent variable  
 
X_undersample = under_sample_data.ix[:, under_sample_data.columns != 'Class']
y_undersample = under_sample_data.ix[:, under_sample_data.columns == 'Class']

#after this lets split the data set into training and testing set

from sklearn.cross_validation import train_test_split
 
# Whole dataset(splitting into training and test data)
X_train, X_test, y_train, y_test = train_test_split(X,y,test_size = 0.3, random_state = 0)
 
# Undersampled dataset(splitting into training and test data of undersampled data) 
X_train_undersample, X_test_undersample, y_train_undersample, y_test_undersample = train_test_split(X_undersample,y_undersample,test_size = 0.3,random_state = 0)

'''class sklearn.ensemble.RandomForestClassifier(n_estimators=10, criterion=’gini’, max_depth=None, 
min_samples_split=2, min_samples_leaf=1, min_weight_fraction_leaf=0.0, max_features=’auto’, 
max_leaf_nodes=None, min_impurity_decrease=0.0, min_impurity_split=None, bootstrap=True, 
oob_score=False, n_jobs=1, random_state=None, verbose=0, warm_start=False, class_weight=None)'''

#tunning the parameter

#now we apply K-fold cross validation here we will apply 10 fold cross validation.

from sklearn.cross_validation import KFold  
from sklearn.ensemble import RandomForestClassifier
from sklearn.metrics import   confusion_matrix,precision_recall_curve,auc,roc_auc_score,roc_curve,recall_score
from sklearn.model_selection import cross_val_score

#tunning the forest  
 
j_shout=range(1,300)
j_acc=[]
for j in j_shout:  
   lr = RandomForestClassifier(n_estimators = j, criterion = 'entropy', random_state = 0)
 score=cross_val_score(lr,X_undersample.values,y_undersample.values.ravel(),cv=10,scoring='accuracy')
   j_acc.append(score.mean())


lets save the result in a pickle, its really a good technique to save a trained data or processed data in pickle, and call it whenever you want in the program without running again the same process. For me, i am working on a i3 Intel 2nd generation processor ,having 4 GB RAM , to execute the above loop it took me about 36 minutes. Suppose i want to redo this program for some reason, it will again consume me the same time so a better and efficient way to handle that problem is saving this into a pickle.

import pickle
Randomforesttunner=open('tunned forest.pickle','wb')
pickle.dump(j_acc,Randomforesttunner)
Randomforesttunner.close()

#finding for which number of tree we get the maximum accuracy
f=j_shout[j_acc.index(max(j_acc))]
 
#plotting the graph  
plt.plot(j_shout,j_acc)
plt.title("tunning the forest")
plt.xlabel("number of trees")
plt.ylabel("cross val testing accuracy score ")
plt.axhline(max(j_acc),color='r',linestyle='--')
plt.axvline(x=f,color='y',linestyle='-')
# so its 190 trees



#after selecting the optimum parameter which gives us the best result we now use random forest with that to predict   
# then plot the confusion matrix  to show the levels and all
 
from sklearn.ensemble import RandomForestClassifier
#our Random forest model,calling the classifier
lr = RandomForestClassifier(n_estimators=190,criterion='entropy', random_state = 0)
#fitting this on our undersampled data
lr.fit(X_train_undersample,y_train_undersample.values.ravel())
#predicting this on the undersample test data
y_pred_undersample=lr.predict(X_test_undersample)
#getting the confusion matrix
cm=confusion_matrix(y_test_undersample,y_pred_undersample)
#specifying classes
classes=[0,1]


import itertools
plt.imshow(cm, interpolation='nearest', cmap=plt.cm.Reds)
plt.colorbar()
 
thresh = cm.max() / 2.
for i, q in itertools.product(range(cm.shape[0]), range(cm.shape[1])):
        plt.text(q, i, cm[i, q],
                 horizontalalignment="center",
                 color="white" if cm[i, q] > thresh else "black")
 
plt.tight_layout()
tick_marks=np.arange(len(classes))
plt.xticks(tick_marks,classes,rotation=0)
plt.yticks(tick_marks,classes)
 
plt.ylabel('True value')
plt.xlabel('predicted value')
plt.title('Confusion Matrix')


#Accuracy: Overall, how often is the classifier correct?
print('OVERALL ACCURACY:-',((cm[0,0]+cm[1,1])/(cm[0,0]+cm[0,1]+cm[1,0]+cm[1,1])))

#Misclassification Rate: Overall, how often is it wrong?
print('MISCLASSIFICATION RATE:-',((cm[0,1]+cm[1,0])/(cm[0,0]+cm[0,1]+cm[1,0]+cm[1,1])))

#True Positive Rate: When it's actually yes, how often does it predict yes?
#sensitivity or recall or True positive rate
print('RECALL:-',(cm[1,1]/(cm[1,1]+cm[1,0])))


#Precision: When it predicts yes, how often is it correct?.

print('PRECISION:-',(cm[1,1]/(cm[0,1]+cm[1,1])))


#Lets plots the ROC curve and find out the Area under curve.

from sklearn import metrics
from sklearn.metrics import roc_curve,auc
 
fpr,tpr,thresholds=metrics.roc_curve(y_test_undersample,y_pred_undersample,pos_label=1)
 plt.figure()
lw = 1
roc_auc=auc(fpr,tpr)
plt.plot(fpr, tpr, color='darkorange',lw=1,label='ROC curve (area = %0.2f)' % (roc_auc))
plt.plot([0, 1], [0, 1], color='navy', linestyle='--')
plt.xlim([0.0, 1.0])
plt.ylim([0.0, 1.05])
plt.xlabel('False Positive Rate')
plt.ylabel('True Positive Rate')
plt.title('Receiver operating characteristic')
plt.legend(loc="lower right")
plt.show()
















