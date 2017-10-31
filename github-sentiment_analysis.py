import nltk
import random
from nltk.classify.scikitlearn import SklearnClassifier
import pickle
from sklearn import naive_bayes
from nltk.classify import ClassifierI
from statistics import mode
from nltk.tokenize import word_tokenize


#creating a class VoteClassifier which inherits the classifier.
class VoteClassifier(ClassifierI):
    def __init__(self, *classifiers):
          self._classifiers = classifiers
    def classify(self, features):
          votes = []
          for c in self._classifiers:
             v = c.classify(features)
             votes.append(v)
           return mode(votes)
#we have two text data of reviews containing positive reviews and negative reviews
#we are loading them into our environment    
short_pos = open("good_reviews.txt","r",encoding='latin-1').read()
short_neg = open("bad_reviews.txt","r",encoding='latin-1').read()

# move this up here
all_words = []
documents = []

#  j is adjective, r is adverb, and v is verb
#allowed_word_types = ["J","R","V"]
allowed_word_types = ["J"]
#we are loading the string into for loop and splitting it by sentence
for p in short_pos.split('\n'):
    #we are creating a tuple adding a category positive here
    documents.append( (p, "pos") )
    #getting the words as tokens
    words = word_tokenize(p)
    #finding the parts of speech
    pos = nltk.pos_tag(words)
    for w in pos:
        #the first word always starts with an article so we skip w[0]
        if w[1][0] in allowed_word_types:
            all_words.append(w[0].lower())

#similarly we get all the words from the negative.txt to for the all_words list     
for p in short_neg.split('\n'):
    documents.append( (p, "neg") )
    words = word_tokenize(p)
    pos = nltk.pos_tag(words)
    for w in pos:
        if w[1][0] in allowed_word_types:
            all_words.append(w[0].lower())

now all_words contains list of  all the words which is taken from both positive reviews and negative reviews
#saving this using pickle so that we can load it later ,when we import it as a module
#it saves us time as we don't have to redo all this  
save_documents = open("pickles/documents.pickle","wb")
pickle.dump(documents, save_documents)
save_documents.close()

#taking the frequency distribution of words and occurrences
all_words = nltk.FreqDist(all_words)
#getting the keys as moving it into variable ,basically the words is the key number of time occurrences are the values
word_features = list(all_words.keys())[:4311]
#saving this as a pickle ,so as to load it later
save_word_features = open("pickles/word_features5k.pickle","wb")
pickle.dump(word_features, save_word_features)
save_word_features.close()

#we parse documents i.e is a list containing tuples into this user defined function
def find_features(document):
    #getting the words as tokens
    words = word_tokenize(document)
    #declaring a dictionary
    features = {}
    for w in word_features: 
     features[w] = (w in words)
#basically creating a dictionary(features) containing boolean values of True or false for words present in documents into features
    return features
#storing everything in the featuresets
featuresets = [(find_features(rev), category) for (rev, category) in documents]

#saving this also as pickle
featuresets_f=open("pickles/featuresets.pickle", "wb")
pickle.dump(featuresets,featuresets_f)
featuresets_f.close()
#shuffling it
random.shuffle(featuresets)

#splitting the dataset into  training and testing sets to be trained on various classifiers
testing_set = featuresets[4799:]
training_set = featuresets[:4799]

#we use various classifiers,which are also present in scikit learn
#we need to save each one of them as pickle so that we don't need to train them again

classifier = nltk.NaiveBayesClassifier.train(training_set)
print("Original Naive Bayes Algo accuracy percent:", (nltk.classify.accuracy(classifier, testing_set))*100)

#Pickle and saving it
save_classifier = open("pickles/originalnaivebayes5k.pickle","wb")
pickle.dump(classifier, save_classifier)
save_classifier.close()

voted_classifier = VoteClassifier(classifier)
                                  



#user defined function for getting the word to classify it as pos(positive) or neg(negative ).
def sentiment(text):
    feats = find_features(text)
    return voted_classifier.classify(feats)

    ----------------------------------------------------------------------------------------------------------------------------
    import requests
import time
import pickle
import random
#here i have removed my token value you will get the token value from your developers page
token='xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx'



#function to just pass the arguments of the page with token to authenticate
def request (req):
    r=requests.get('https://graph.facebook.com/v2.10/'+req,{'access_token':token})
    return r


#calling the above function and passing the parameters
j=request("880442985351926?fields=posts")

results=j.json()


#looking at the json file which we have our data and we will extract from it

#getting the data and storing it in a list
data=[]
i=0
while True:
    try:
        #time.sleep() is to sent request in with a delay,so that the graph api smoothly gives us the value without terminating the connection
        time.sleep(random.randint(2,5))
        data.extend(results['posts']['data'])
        r=requests.get(results['posts']['paging']['next'])
        results=r.jason()
        i +=1
    
    
    except:
        print ("done")
        break

#pickled the data and saved it so that we can use it for later purposes
pickle.dump(data,open("pickles/stream.pkl","wb"))
loadpickledata=pickle.load(open("pickles/stream.pkl","rb"))
data=loadpickledata

from nltk.tokenize import sent_tokenize
#data is a list containing the dictionary having the post as strings i need to extract each sentence as a strings and count the number of posts
#we pass each post into a dictionary
dict={}
for k in range(0,20):
    mm=data[k]['message']
    pp=sent_tokenize(mm)
    dict[k]=pp



#now lets run our sentiment analysis script on the posts which we gathered
#lets load our data in the dictionary to the sentiment engine

import sentiment_module as s
#taking the posts which are stored in the dictionary as lists
#passing them as strings to the sentiment module
a=list(dict.values())
c=[]
for a in a:
    x=s.sentiment(str(a))
    c.append(x)
#creating a counter to find the general sentiment of the posts of the facebook page(postive->pv,negative->ng).
  
pv=c.count('pos')
ng=c.count('neg')
if pv > ng:
    print("The page BEEF JANATA PARTY contains mostly positive posts")
else:
    print("The page BEEF JANATA PARTY contains mostly negative posts")