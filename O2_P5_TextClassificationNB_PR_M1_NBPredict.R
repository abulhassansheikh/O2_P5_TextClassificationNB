###Predict the part type of a set of product disctriptions using the mainsheet data
#######################################
NBPredict = function(PredictionDF, source){

###Subset PredictionDF into test and training set
PredictionDF= subset(PredictionDF, !is.na(PredictionDF$Pro_String))
PredictionDF$string = trimws(PredictionDF$Pro_String)
PredictionDF$label = trimws(PredictionDF$Pro_Label)
PredictionDF$STATUS[!is.na(PredictionDF$Pro_String) & !is.na(PredictionDF$Pro_Label)] <- 0
PredictionDF$STATUS[!is.na(PredictionDF$Pro_String) & is.na(PredictionDF$Pro_Label)] <- 1

###Format the string in PredictionDF
#Format the string to get rid of numbers and symboles
Processed_string = FormatStringcolumn(TextCol = PredictionDF$Pro_String, ColName = "Pro_String", WordColName = "Pro_String", noDup1all0 = 1)
string_stw = data.frame(Processed_string[1], stringsAsFactors = FALSE)

#Get list of jobber words
string_words = data.frame(Processed_string[2], stringsAsFactors = FALSE)
ExtraCol_s = c(Processed_string[4][[1]][-1], "DUMB")

#Remove words such as blank, idk if less than 5% total occurance should be also removed, will see
string_words_Ref = subset(string_words, string_words[,1]!="")

#Build string word matrix and merge with jobber_Ref
WordMatrix = data.frame(DUMB = matrix(0, 1, nrow = nrow(string_stw)))
for(i in 1:nrow(string_words_Ref)){
	WordArray = RowMatch(Word = string_words_Ref[i,1], InputDF=string_stw)
	WordMatrix = cbind(WordMatrix, WordArray)
}

#combin the word matrix with the string
string_wrdMat = cbind(string_stw, WordMatrix)
string_wrdMat= string_wrdMat[,!(names(string_wrdMat) %in% ExtraCol_s)]

#Join everything with the PredictionDF
PredictionDF = merge(PredictionDF, string_wrdMat, by= "Pro_String", all=TRUE)

###Create NB Model
#Make training, evaluation and test Set
TrainingSet = subset(PredictionDF, PredictionDF$STATUS==0)
TrainingSet = TrainingSet[,!names(TrainingSet) %in% c("Numb_Sku", "STATUS", "Pro_String")]
EvaluationSet = subset(PredictionDF, PredictionDF$STATUS==0)
EvaluationSet = EvaluationSet[,!names(EvaluationSet) %in% c("STATUS", "Pro_String")]
TestSet = subset(PredictionDF, PredictionDF$STATUS==1)
TestSet= TestSet[,!names(TestSet) %in% c("label", "STATUS", "Pro_String")]


###Make Training Set NB Model
message("\n")
message("\nMachine Learning...")
TrainingSetModel = NBModel(LWMatrix=TrainingSet )


###Ceate output file and perform PT prediction on taugh and testing skus
#Create output file
OutputFile = data.frame(Numb_Sku= "DUMB", ActualLabel= "DUMB", PredictedLabel= "DUMB", Percent= 0.4, Difference= 0.4, Pro_String= "DUMB",  stringsAsFactors = FALSE)

#Perform evaluation of TrainingSetModel 
message("\nTesting Machine...")
pb = txtProgressBar(min = 1, max = nrow(EvaluationSet), initial = 0, char = "*", width = 100, style = 3)
for(i in 1:nrow(EvaluationSet)){

	NumbString = data.frame(t(EvaluationSet[i,3:ncol(EvaluationSet)]), stringsAsFactors =FALSE)
	WordMatch = data.frame(Words = rownames(subset(NumbString, NumbString[,1] == 1)), stringsAsFactors =FALSE)
	sku = as.character(EvaluationSet[i,1])
	String = as.character(subset(PredictionDF, PredictionDF$Numb_Sku== EvaluationSet[i,1], select = string))
	actL = as.character(EvaluationSet[i,2])

	EvaluationResult = NB_Prob(ProbDF = TrainingSetModel, WordValueDF = WordMatch )
	Count = sum(EvaluationResult$Match)

	if(Count == 1){
		match = subset(EvaluationResult, EvaluationResult$Match ==1, select = c(Labels, PerLgWProb, Difference))
		estL = match[1,1]
		PercL = round(match[1,2], 3)
		DiffL = round(match[1,3], 3)


	} else if(Count == 0){
		match= subset(EvaluationResult, EvaluationResult$PerLgWProb== max(EvaluationResult$PerLgWProb), select = c(Labels, PerLgWProb, Difference))
		estL = match[1,1]
		PercL = round(match[1,2], 3)
		DiffL = round(match[1,3], 3)


	} else{ 
		matchAll = subset(testResult, testResult$Match ==1, select = c(Labels, PerLgWProb, Difference))
		match= subset(matchAll, matchAll$PerLgWProb== max(matchAll$PerLgWProb))
		estL = match[1,1]
		PercL = round(match[1,2], 3)
		DiffL = round(match[1,3], 3)

	}

	OutputString = c(sku, actL , estL , PercL , DiffL , String  )
	OutputFile = rbind(OutputFile, OutputString)
	setTxtProgressBar(pb,i)
}
 
OutputFile = subset(OutputFile , Numb_Sku!= "DUMB" & ActualLabel != "DUMB" & PredictedLabel != "DUMB")
OutputFile$Check[OutputFile$ActualLabel != OutputFile$PredictedLabel] <- 0
OutputFile$Check[OutputFile$ActualLabel == OutputFile$PredictedLabel] <- 1
CorrectPercent = round((sum(as.numeric(OutputFile$Check))/nrow(OutputFile))*100, 2)
message("\n***Machine ", CorrectPercent , "% Accurate.***")

#Perform label prediction for the test set
message("\nPredicting New Skus...")
pb = txtProgressBar(min = 0, max = nrow(TestSet), initial = 0, char = ">", width = 100, style = 3)
for(i in 1:nrow(TestSet)){

	NumbString = data.frame(t(TestSet[i,]), stringsAsFactors =FALSE)
	WordMatch = data.frame(Words = rownames(subset(NumbString, NumbString[,1] == 1)), stringsAsFactors =FALSE)
	sku = as.character(TestSet[i,1])
	String = as.character(subset(PredictionDF, PredictionDF$Numb_Sku== TestSet[i,1], select = Pro_String))
	actL = "New_Sku"

	testResult = NB_Prob(ProbDF = TrainingSetModel, WordValueDF = WordMatch)

	Count = sum(testResult$Match)

	if(Count == 1){
		match = subset(testResult, testResult$Match ==1, select = c(Labels, PerLgWProb, Difference))
		estL = match[1,1]
		PercL = round(match[1,2], 3)
		DiffL = round(match[1,3], 3)


	} else if(Count == 0){
		match= subset(testResult, testResult$PerLgWProb== max(testResult$PerLgWProb), select = c(Labels, PerLgWProb, Difference))
		estL = match[1,1]
		PercL = round(match[1,2], 3)
		DiffL = round(match[1,3], 3)


	} else{ 
		matchAll = subset(testResult, testResult$Match ==1, select = c(Labels, PerLgWProb, Difference))
		match= subset(matchAll, matchAll$PerLgWProb== max(matchAll$PerLgWProb))
		estL = match[1,1]
		PercL = round(match[1,2], 3)
		DiffL = round(match[1,3], 3)

	}

	OutputString = c(sku, actL , estL , PercL , DiffL , String  )
	OutputFile = rbind(OutputFile, OutputString)

setTxtProgressBar(pb,i)
}
OutputFile$Check[OutputFile$ActualLabel == "New_Sku"] <- 0

WrongRef = as.numeric(subset(OutputFile , OutputFile$Check == 0 & OutputFile$ActualLabel != "New_Sku", select = Difference)[,1])
WorMax = max(as.numeric(WrongRef))

RightRef = as.numeric(subset(OutputFile , OutputFile$Check == 1 & OutputFile$ActualLabel != "New_Sku", select = Difference)[,1])
CorAvg = mean(as.numeric(RightRef))

#NewRef = as.numeric(subset(OutputFile , OutputFile$ActualLabel == "New_Sku", select = Difference)[,1])
#WorAvg = mean(as.numeric(WrongRef))
#CorMax = max(as.numeric(RightRef))
#hist(RightRef , col=rgb(0,1,0,0.5))
#hist(NewRef , col=rgb(0,1,0.5), add=T)
#hist(WrongRef , col=rgb(1,0,0,0.5), add=T)
#abline(h = 0, v = WorMax  , col = "red", lwd = 5)
#abline(h = 0 , v = WorAvg  , col = "blue", lwd = 5)
#abline(h = 0, v = CorMax  , col = "green", lwd = 5)
#abline(h = 0 , v = CorAvg  , col = "purple", lwd = 5)

OutputFile$EasyConfidence[as.numeric(OutputFile$Difference) <= CorAvg] = "Low"
OutputFile$EasyConfidence[as.numeric(OutputFile$Difference) > CorAvg& as.numeric(OutputFile$Difference) <= WorMax] = "Medium"
OutputFile$EasyConfidence[as.numeric(OutputFile$Difference) > WorMax] = "High"

FinalOutputFile = subset(OutputFile, select = c("Numb_Sku", "ActualLabel", "PredictedLabel", "Percent", "EasyConfidence", "Pro_String", "Difference"))
names(FinalOutputFile) = c("Numb_Sku", 
				   paste("ActualLabel", source, sep= "_"),
				   paste("PredictedLabel", source, sep= "_"),
				   paste("Percent", source, sep= "_"),
				   paste("EasyConfidence", source, sep= "_"),
				   paste("Pro_String", source, sep= "_"),
				   paste("Difference", source, sep= "_"))




message("\nPrediction Complete.\n")
return(FinalOutputFile )

###
#End function

}


##########################
#Troubleshoot

#test = JobberPTPredict(BrandName = "Fass_Fuel")
#TEMP(test )

###########EXTRA CODE

###Create Parttype classificaion matrix using TrainingSet data set order
#TrainingSet_PTlist = data.frame(part_type_filter = TrainingSet$part_type_filter, stringsAsFactors =FALSE)
#TrainingPTWordMat = merge(TrainingSet_PTlist, PT_wrdMat, by = "part_type_filter")
#oldnames = c("sku", "part_type_filter")
#TrainingSet = TrainingSet[,!(names(TrainingSet) %in% oldnames )]

###Stack up the 0-1 vector of descriptions in training set
#Create seed dataframe to attach all the concol values too
#TrainingSet_concol = subset(TrainingSet, TrainingSet[,length(PTcolNames)]=="DUMB")[1, length(PTcolNames):ncol(TrainingSet)  ]
#colnames(TrainingSet_concol)[1] = "PT_Words"

#for(p in 1:length(PTcolNames)){
#
#	ExcludeCol = 1:length(PTcolNames)
#	ExcludeCol = ExcludeCol[-p]
#	TrainingSetSS = TrainingSet[-ExcludeCol]
#	colnames(TrainingSetSS)[1] = "PT_Words"
#	TrainingSet_concol = rbind(TrainingSet_concol, TrainingSetSS)
#}

#TrainingSet_ConFin = subset(TrainingSet_concol, TrainingSet_concol$PT_Words != "DUMB")


###Creat NB Models
#TrainingSetNBmod = NBModel(LWMatrix=TrainingSet_ConFin )
#TrainingPTNBmod = NBModel(LWMatrix=TrainingPTWordMat )


###Use the test set to predict the PT words
#for(i in 1:nrow(TestSet)){

#	NumbString = data.frame(t(TestSet[i,]), stringsAsFactors =FALSE)
#	WordMatch = data.frame(Words = rownames(subset(NumbString, NumbString[,1] == 1)), stringsAsFactors =FALSE)
#	String = as.character(subset(jobber, jobber$sku == TestSet[i,1], select = SkuString))
#	#print(WordMatch)

#	PTwordMatch = NB_Prob(ProbDF = TrainingSetNBmod, WordValueDF = WordMatch)

#	WordMatch = subset(PTwordMatch , PTwordMatch$Match ==1, select = Labels )

#	if(nrow(WordMatch )>0){message("MATCH:: ", WordMatch , " :: ", String )}	

#	#print(PTwordMatch)
	#PTwordMatch = subset(PTwordMatch, PTwordMatch$Match==1, select = Labels)
	#names(PTwordMatch) = "Words"

	#testResult = print(NB_Prob(ProbDF = TrainingPTNBmod , WordValueDF = PTwordMatch ))
#}

###Create PT label matrix
#TrainingPTLabel = data.frame(part_type_filter = TrainingPTWordMat$part_type_filter, stringsAsFactors = FALSE)

#Get list of PT words and colum names
#PT_List = data.frame(PT_stw$part_type_filter, stringsAsFactors = FALSE)

#Build PT word matrix and merge with PT_Ref
#PTClassMatrix = data.frame(DUMB = matrix(0, 1, nrow = nrow(PT_stw)))
#for(i in 1:nrow(PT_List)){
#	PTlistWordArray = RowMatch(Word = PT_List[i,1], InputDF=PT_stw)
#	PTClassMatrix = cbind(PTClassMatrix, PTlistWordArray )
#}

#PT_ListMat = cbind(PT_stw, PTClassMatrix )
#PT_ListMat = PT_ListMat[,!(names(PT_ListMat) %in% c(PTcolNames, "DUMB") )]

#TrainingPTLabel = merge(TrainingPTLabel , PT_ListMat, by= "part_type_filter")











