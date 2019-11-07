# DannyD
Fatal Rate of Police Shooting Final Report

Cleaning the Data and Outside Predictors

The data given contained many missing, unknown and empty, NA, values. 
Different approaches to cleaning the data were taken to maximize the usefulness of the given observations. 
The first approach was to attempt making the predictors that would typically have a numeric value including NumberOfShots, 
SubjectAge, NumberOfOfficers and NumberOfSubjects into numeric variables. 
Furthermore, OfficerRace’s observations typically included many white officers, 
so a predictor was added that represented the ratio of non-white officers at the crime. 
When this approach proved ineffective, each of the predictors were turned into factors of multiple levels. 
For instance, SubjectAge was turned into a factor with levels such as “20-29” to indicate the decade of the age of the subject. 
The Notes and FullNarrative predictors were then examined using text analysis 
to find key words and phrases that would indicate a certain fatality or a definitive non-fatal instance. 
The columns created from this search were NoteFword, NarFword, NoteNFword, NarNFword. Using these columns of Booleans, 
a final predictor, cert_f, was initialized that provided highly accurate predictions over whether the police shooting fatal. 
The last column created was cert_city where the cities with the highest percentages of fatal shootings 
and the lowest percentages were respectively labeled TRUE or FALSE with the rest as Unknown. 
In terms of outside data, a dataset about national elections and census data about cities was at first combined to the data. 
The hypothesis was that lower percentages of fatal shootings could be the result of public policy and that 
indicators such as city density would also play an important role in these outcomes. 
However, this data proved ineffective in most of the models attempted, and likewise it was not included in either of the final models. 

Description of the Best Models

The best model that was used for our final Kaggle score was a pruned tree using the predictors cert_f, Year, 
SubjectAge, SubjectArmed, shots and cert_city.  Before this model was attained, 
there were many attempts at different types of models such as logistic regression, random forest 
and even manually calculating a tree using clear separation points in the data. 
The original tree generated for the final model using the tree package in R was initialized with the predictors SubjectArmed, 
SubjectAge, Year, shots, cert_f and cert_city for classifying Fatal. 
The order of importance of the predictors were cert_f, year being categorized as “Unknown,” SubjectAge, SubjectArmed, 
shots and cert_city.
To avoid overfitting the maximum number of nodes of the pruned model was 11, 
and it was observed that an increase in maximum nodes had a minimal effect on the miclassification rate on the training data.   

Best Misclassification Error from this model: 0.1875
The Shortcomings and Successes of the Model

There were some important shortcomings and successes with the final model’s predictive capabilities. 
The simplicity of this tree has allowed for a decrease in the bias of the predictions, 
but perhaps oversimplification due to this significant increase in testing misclassification 
after the rest of the testing data was applied. This change could be due to this model being too rigid. 
As a team we were concerned with the leaderboard score, and this focus led to a model more tailored for the testing dataset on hand.
