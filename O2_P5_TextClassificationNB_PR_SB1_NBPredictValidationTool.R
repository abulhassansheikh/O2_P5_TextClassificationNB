###Predict the part type of a set of product disctriptions using the mainsheet data
#######################################
NBPredictValidationTool = function(PredictionDFOG){

###Subset PredictionDF into test and training set
PredictionDF_Base= subset(PredictionDFOG, !is.na(PredictionDFOG$Pro_String))
PredictionDF_Base$Pro_String = trimws(PredictionDF_Base$Pro_String)
PredictionDF_Base$Pro_Label = trimws(PredictionDF_Base$Pro_Label)


###Format the string in PredictionDF
#Format the string to get rid of numbers and symboles
Processed_string = FormatStringcolumn(TextCol = PredictionDF_Base$Pro_String, ColName = "Pro_String", WordColName = "Pro_String", noDup1all0 = 1)
string_stw = data.frame(Processed_string[1], stringsAsFactors = FALSE)

#Get list of jobber words
string_words = data.frame(Processed_string[2], stringsAsFactors = FALSE)
ExtraCol_s = c(Processed_string[4][[1]][-1], "DUMB")

#Remove words such as blank, idk if less than 5% total occurance should be also removed, will see
string_words_Ref = subset(string_words, string_words[,1]!="")

#Build string word matrix and merge with jobber_Ref
WordMatrix_raw = data.frame(DUMB = matrix(0, 1, nrow = nrow(string_stw)))
for(i in 1:nrow(string_words_Ref)){
	WordArray = RowMatchCount(Word = string_words_Ref[i,1], InputDF=string_stw) ####
	WordMatrix_raw = cbind(WordMatrix_raw, WordArray)
}

###Normilzie String Length
NumString = length(PredictionDF_Base$Pro_String)
WordFreq = data.frame(Pro_Words = names(WordMatrix_raw), sum = colSums((WordMatrix_raw )^2, na.rm = FALSE, dims = 1))
WordFreq$InverseFreq = (NumString/WordFreq$sum)
WordFreq$SqrFreq = sqrt(NumString/WordFreq$sum)
WordMatrix =  WordMatrix_raw #No transformation
for(i in 1:nrow(WordFreq)){WordMatrix[,i] = WordMatrix[,i]*WordFreq[i,3]} #Inverse Frequency 


#Create Train predictionDF
PredictionDF_train = PredictionDF_Base

###Run Validation
FinalOutputFile = data.frame() #output file for results

#combin the word matrix with the string
string_wrdMat = cbind(string_stw, WordMatrix)
string_wrdMat= string_wrdMat[,!(names(string_wrdMat) %in% ExtraCol_s)]
string_wrdMat_raw = cbind(string_stw, WordMatrix_raw)
string_wrdMat_raw= string_wrdMat_raw[,!(names(string_wrdMat_raw) %in% ExtraCol_s)]

#Join everything with the PredictionDF
PredictionDF_use = merge(PredictionDF_train, string_wrdMat, by= "Pro_String", all=TRUE)
#TEMP(PredictionDF_use)

###Create NB Model
#Make training, evaluation and test Set
TrainingSet = PredictionDF_use
TrainingSet = TrainingSet[,!names(TrainingSet) %in% c("Numb_Sku", "STATUS", "Pro_String")]
TEMP(TrainingSet)

EvaluationSet = PredictionDF_use
EvaluationSet = EvaluationSet[,!names(EvaluationSet) %in% c("STATUS", "Pro_Label", "Numb_Sku")]
#EvaluationSet = unique(EvaluationSet )
TEMP(EvaluationSet)


###Make Training Set NB Model
message("\n")
message("\nMachine Learning...")
TrainingSetModel = NBModel(LWMatrix=TrainingSet )

#Create output file
OutputFile = data.frame( Pro_String= "DUMB",  PredictedLabel= "DUMB", Difference= 0.4, stringsAsFactors = FALSE)
LabelList = data.frame(Pro_Label = unique(TrainingSetModel$Pro_Label), MatchValue = 0,stringsAsFactors= FALSE)

#Perform label prediction for the test set
message("\nPredicting New Skus...")
pb = txtProgressBar(min = 0, max = nrow(EvaluationSet ), initial = 0, char = ">", width = 100, style = 3)
for(i in 1:nrow(EvaluationSet )){

	NumbString = data.frame(t(EvaluationSet[i,3:ncol(EvaluationSet)]), stringsAsFactors =FALSE)
	WordMatch  = subset(NumbString, NumbString[,1] > 0)
	WordMatch$Words = rownames(WordMatch)
	names(WordMatch) = c("Freq", "Words")
	String = as.character(EvaluationSet$Pro_String[i])

	for(j in 1:nrow(WordMatch)){
		word = WordMatch[j,2]
		freq = as.numeric(WordMatch[j,1])
	
		for(k in 1:nrow(LabelList)){
			PriorValue = subset(TrainingSetModel, TrainingSetModel$Pro_Label == LabelList[k,1] & TrainingSetModel$Pro_Word == word, select = WeightNormalization )[1,1]
			LabelList[k,2]= LabelList[k,2] + PriorValue 
		}
	}

	MatchOutput = cbind(String, subset(LabelList, LabelList$MatchValue == min(LabelList$MatchValue)))
	names(MatchOutput ) = c("Pro_String", "PredictedLabel", "Difference")
	OutputFile = rbind(OutputFile, MatchOutput )

	setTxtProgressBar(pb,i)
}
OutputFile = subset(OutputFile , PredictedLabel != "DUMB")

message("\nPrediction Complete.\n")
return(OutputFile )

###
#End function
}


##########################
#Testing Code

ValidationFile = read.csv("//192.168.2.32/Group/Data Team/Abul/1. Development/2. RnD/R_RnD/5. Test_Location/MS_DCI_PT_Prediction_ref.csv", header = TRUE, row.names=NULL, stringsAsFactors=FALSE)
ValidationFile = subset(ValidationFile, select = c("Numb_Sku", "dciptdescr", "part_type_filter"))
names(ValidationFile ) = c("Numb_Sku", "Pro_Label", "Pro_String")

PredictionDF_use = read.csv("//192.168.2.32/Group/Data Team/Abul/1. Development/2. RnD/R_RnD/5. Test_Location/PredictionDF_use.PT_Prediction.csv", header = TRUE, row.names=NULL, stringsAsFactors=FALSE)
TrainingSet = read.csv("//192.168.2.32/Group/Data Team/Abul/1. Development/2. RnD/R_RnD/5. Test_Location/TrainingSet.PT_Prediction.csv", header = TRUE, row.names=NULL, stringsAsFactors=FALSE)


PTValidationFile_ForJustin = NBPredictValidationTool(PredictionDFOG= ValidationFile )
FINAL(PTValidationFile_ForJustin)



memory.size()










