# Letter_Recognition
Build a model that uses attributes of images of four letters A,B,P,R to predict which letter a particular image corresponds to

In this problem, I worked on letter recognition. I built a simple model that uses attributes of images of four letters in the Roman alphabet: A, B, P and R to predict which letter a particular image corresponds to.
Because I have four possible labels of each observation, this is a multi-class classification problem.
The data set is composed of 3116 observations each of which corresponds to a certain image of the four letters.

I started by predicting whether or not the letter is a "B". I built a logistic regression model, a CART tree and a Random Forest model. 

Then, I moved to the original problem of interest: which letter the image corresponds to. 

I built a LDA model, CART model, Bagging,  a Random Forest model and I finally applied Boosting. 
