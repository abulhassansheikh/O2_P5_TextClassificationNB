
NBModel= function(LWMatrix){

###Create basic PT NB prob table from TrainingPTMat 
#Compute probability between word and label
NBmodel = data.frame(Label = "DUMB", LabelPresent = "DUMB", Word = "DUMB", WordPresent = "DUMB", Prob = 0.4, stringsAsFactors = FALSE)

#Identify the text used to construct model
colnames(LWMatrix)[1] = "Model_Label"
AllWordText = names(LWMatrix)[-1]
AllLabelText = LWMatrix[!duplicated(LWMatrix[,1]),][,1]

#Add-One Smooting function
nWords = length(AllWordText)
nLabels = length(AllLabelText)
Total = nrow(LWMatrix)

###Compliment Naive Bayes data
LabelDF = data.frame(Pro_Label = AllLabelText, stringsAsFactors = FALSE)
Pro_StringWordn = data.frame(nWord = rowSums(LWMatrix[2:ncol(LWMatrix)], na.rm = FALSE, dims = 1))
Pro_StringWordn = cbind(Pro_Label = LWMatrix[,1],Pro_StringWordn ) 
for(i in 1:nrow(LabelDF)){LabelDF$LabelnWord[i] = sum(subset(Pro_StringWordn, as.character(Pro_StringWordn$Pro_Label) == as.character(LabelDF$Pro_Label[i]), select = nWord)[,1])}
Pro_Wordn = data.frame(nWord = colSums(LWMatrix[2:ncol(LWMatrix)], na.rm = FALSE, dims = 1))
Pro_Wordn = cbind(Pro_Word= rownames(Pro_Wordn), Pro_Wordn, row.names = NULL)

#Total numb of words occuring in classes other than the selected classRowMatchCount 
nWordTotal = sum(Pro_StringWordn$nWord)
LabelDF$LabelnWord_Not = nWordTotal - LabelDF$LabelnWord + 1 

#Total numb of selected word occuring in string in classes other than the selected class
WordnotLabelDF = data.frame(Pro_Label = "DUMB", Pro_Word = "DUMB", WordLabelContrast = 0, LabelContrast = 0, stringsAsFactors = FALSE)

if(length(AllLabelText)>1){pb = txtProgressBar(min = 1, max = length(AllLabelText), initial = 0, char = "*", width = 100, style = 3)}
for(L in 1:length(AllLabelText)){
	
	LabelCol = data.frame(LabelValue = LWMatrix[,1], stringsAsFactors = FALSE)
	LabelText = AllLabelText[L]
	LabelConstrastValue = subset(LabelDF, LabelText == LabelDF$Pro_Label, select = LabelnWord_Not)[1,1]
	
for(W in 2:ncol(LWMatrix)){

	Word = data.frame(Word = LWMatrix[,W])
	WordText = names(LWMatrix)[W]

	Contrastdf = cbind(LabelCol, Word)
	Contrastdf = subset(Contrastdf, LabelText != LabelCol$LabelValue)
	
	Output = c(as.character(LabelText), as.character(WordText) ,sum(Contrastdf$Word)+nWords, LabelConstrastValue)
	WordnotLabelDF = rbind(WordnotLabelDF , Output)

}
if(length(AllLabelText)>1){setTxtProgressBar(pb,L)}
}

WordnotLabelDF$prior = as.numeric(WordnotLabelDF$WordLabelContrast)/ as.numeric(WordnotLabelDF$LabelContrast)
WordnotLabelDF$log_prior = log(WordnotLabelDF$prior)
WordnotLabelDF$abs_log_prior = abs(WordnotLabelDF$log_prior)

for(i in 1:nrow(LabelDF)){
	LabelPriorSum = sum(subset(WordnotLabelDF, WordnotLabelDF$Pro_Label == LabelDF$Pro_Label[i], select = abs_log_prior))
	WordnotLabelDF$priorSum[WordnotLabelDF$Pro_Label == LabelDF$Pro_Label[i]] <- LabelPriorSum
}

WordnotLabelDF$WeightNormalization = WordnotLabelDF$log_prior/WordnotLabelDF$priorSum

NBmodel = subset(WordnotLabelDF, Pro_Label != "DUMB" & Pro_Word!= "DUMB", select = c(Pro_Label, Pro_Word,WeightNormalization ))

return(NBmodel)
}
################
###Troubleshooting

#TEMP(NBmodel)

#TrainingSetModel = NBmodel 

#test = NBModel(LWMatrix=testMat )
#TEMP(test)
























