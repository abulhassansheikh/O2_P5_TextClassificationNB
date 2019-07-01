###Predict the part type of a set of product disctriptions using the mainsheet data
#######################################
NBPredictDiagTool = function(PredictionDFOG, source, TestSkus){

###Subset PredictionDF into test and training set
PredictionDF_Base= subset(PredictionDFOG, !is.na(PredictionDF$Pro_String))
PredictionDF_Base$Pro_String = trimws(PredictionDF_Base$Pro_String)
PredictionDF_Base$Pro_Label = trimws(PredictionDF_Base$Pro_Label)
PredictionDF_Base$STATUS[!is.na(PredictionDF_Base$Pro_String) & !is.na(PredictionDF_Base$Pro_Label)] <- 0
PredictionDF_Base$STATUS[!is.na(PredictionDF_Base$Pro_String) & is.na(PredictionDF_Base$Pro_Label)] <- 1

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
#LenNormSqrSumDenom = (data.frame(SqrSum = rowSums((WordMatrix_raw )^2, na.rm = FALSE, dims = 1)))^0.5
#for(i in 1:nrow(LenNormSqrSumDenom)){WordMatrix[i,] = WordMatrix[i,]/LenNormSqrSumDenom[i,]}
NumString = length(PredictionDF_Base$Pro_String)
WordFreq = data.frame(Pro_Words = names(WordMatrix_raw), sum = colSums((WordMatrix_raw )^2, na.rm = FALSE, dims = 1))
WordFreq$InverseFreq = (NumString/WordFreq$sum)
WordFreq$SqrFreq = sqrt(NumString/WordFreq$sum)

#Create Train predictionDF
PredictionDF_train = PredictionDF_Base

###Run Diagnosis
FinalOutputFile = data.frame() #output file for results
TypeTest = c("FrequencyOnly", "PowerLaw", "InverseFrequency", "SQRTInverseFrequency")
	TypeOne = TypeTest[1]
	TypeTwo = TypeTest[2]
	TypeThree = TypeTest[3]
	TypeFour = TypeTest[4]

###Test all types
for(d in 1:4){
if(d ==1){
	WordMatrix =  WordMatrix_raw #No transformation
	Type = TypeTest[d]
} else if(d == 2){
	WordMatrix =  WordMatrix_raw #No transformation
	WordMatrix =  log(WordMatrix_raw+ 1) # Simple Power Law Transofrmation, d =1
	Type = TypeTest[d]
} else if(d == 10){
	WordMatrix =  WordMatrix_raw #No transformation
	WordMatrix =  log(WordMatrix_raw + PowerLawD ) # Optimized Power Law Transofrmation, d =1
} else if(d == 3){
	WordMatrix =  WordMatrix_raw #No transformation
	for(i in 1:nrow(WordFreq)){WordMatrix[,i] = WordMatrix[,i]*WordFreq[i,3]} #Inverse Frequency 
	Type = TypeTest[d]
} else if(d == 4){
	WordMatrix =  WordMatrix_raw #No transformation	
	for(i in 1:nrow(WordFreq)){WordMatrix[,i] = WordMatrix[,i]*WordFreq[i,4]} #SQRT Inverse Frequency 
	Type = TypeTest[d]
}
message(Type)


#combin the word matrix with the string
string_wrdMat = cbind(string_stw, WordMatrix)
string_wrdMat= string_wrdMat[,!(names(string_wrdMat) %in% ExtraCol_s)]
string_wrdMat_raw = cbind(string_stw, WordMatrix_raw)
string_wrdMat_raw= string_wrdMat_raw[,!(names(string_wrdMat_raw) %in% ExtraCol_s)]

#Join everything with the PredictionDF
PredictionDF_use = merge(PredictionDF_train, string_wrdMat, by= "Pro_String", all=TRUE)

###Create NB Model
#Make training, evaluation and test Set
TrainingSet = subset(PredictionDF_use, PredictionDF_use$STATUS==0)
TrainingSet = TrainingSet[,!names(TrainingSet) %in% c("Numb_Sku", "STATUS", "Pro_String")]
EvaluationSet = subset(PredictionDF_use, PredictionDF_use$STATUS==0)
EvaluationSet = EvaluationSet[,!names(EvaluationSet) %in% c("STATUS", "Pro_String")]


###Make Training Set NB Model
message("\n")
message("\nMachine Learning...")
TrainingSetModel = NBModel(LWMatrix=TrainingSet )

#Create output file
OutputFile = data.frame(Numb_Sku= "DUMB", ActualLabel= "DUMB", PredictedLabel= "DUMB", Difference= 0.4, Pro_String= "DUMB",  stringsAsFactors = FALSE)
LabelList = data.frame(Pro_Label = unique(TrainingSetModel$Pro_Label), MatchValue = 0,stringsAsFactors= FALSE)

#Perform label prediction for the test set
message("\nPredicting New Skus...")
pb = txtProgressBar(min = 0, max = nrow(EvaluationSet ), initial = 0, char = ">", width = 100, style = 3)
for(i in 1:nrow(EvaluationSet )){

	NumbString = data.frame(t(EvaluationSet[i,3:ncol(EvaluationSet)]), stringsAsFactors =FALSE)
	WordMatch  = subset(NumbString, NumbString[,1] > 0)
	WordMatch$Words = rownames(WordMatch)
	names(WordMatch) = c("Freq", "Words")
	sku = as.character(EvaluationSet [i,1])
	String = as.character(subset(PredictionDF_use, PredictionDF_use$Numb_Sku== sku , select = Pro_String))
	actL = as.character(EvaluationSet[i,2])

	LabelList$MatchValue = 0

	for(j in 1:nrow(WordMatch)){
		word = WordMatch[j,2]
		freq = as.numeric(WordMatch[j,1])
	
		for(k in 1:nrow(LabelList)){
			PriorValue = subset(TrainingSetModel, TrainingSetModel$Pro_Label == LabelList[k,1] & TrainingSetModel$Pro_Word == word, select = WeightNormalization )[1,1]
			LabelList[k,2]= LabelList[k,2] + PriorValue 
		}
	}

	MatchOutput = cbind(sku, actL, subset(LabelList, LabelList$MatchValue == min(LabelList$MatchValue)), String )
	names(MatchOutput ) = c("Numb_Sku", "ActualLabel", "PredictedLabel", "Difference", "Pro_String")
	OutputFile = rbind(OutputFile, MatchOutput )

	setTxtProgressBar(pb,i)
}
OutputFile = subset(OutputFile , Numb_Sku!= "DUMB" & ActualLabel != "DUMB" & PredictedLabel != "DUMB")
NoMatch = subset(OutputFile, as.character(OutputFile$ActualLabel) != as.character(OutputFile$PredictedLabel))
if(nrow(NoMatch) !=0){NoMatch$Type = Type; FinalOutputFile = rbind(FinalOutputFile , NoMatch)}

#End Output for single transformation 
}

Summary = data.frame(sort(table(FinalOutputFile$Numb_Sku)))
names(Summary) = c("Numb_Sku", "Error-Freq")
#TypeDF = data.frame(matrix(vector(), nrow(Summary), length(TypeTest),dimnames=list(c(), TypeTest)),stringsAsFactors=F)
#Summary = cbind(Summary,TypeDF )


for(s in 1:nrow(Summary)){

	SkuData = subset(FinalOutputFile, Summary$Numb_Sku[s] == FinalOutputFile$Numb_Sku)

	TypeOneData = subset(SkuData, SkuData$Type == TypeOne)
	TypeTwoData = subset(SkuData, SkuData$Type ==  TypeTwo)
	TypeThreeData = subset(SkuData, SkuData$Type ==  TypeThree)
	TypeFourData = subset(SkuData, SkuData$Type ==  TypeFour)

	Summary$TypeOne[s] = TypeOneData$PredictedLabel[1]
	Summary$TypeTwo[s] = TypeTwoData$PredictedLabel[1]
	Summary$TypeThree[s] = TypeThreeData$PredictedLabel[1]
	Summary$TypeFour[s] = TypeFourData$PredictedLabel[1]

	Summary$ActualLabel[s] = SkuData$ActualLabel[1]
	Summary$Pro_String[s] = SkuData$Pro_String[1]
}

message("\nPrediction Complete.\n")
return(Summary)

###
#End function
}


##########################
#Troubleshoot

OutputJobberPT = NBPredictDiagTool(PredictionDF = PredictPT_jobber, source = "jobber", TestSkus = REF.NewSkuList)
TEMP(OutputJobberPT )
OutputIFPT = NBPredictDiagTool(PredictionDF = PredictPT_IF, source = "jobber", TestSkus = REF.NewSkuList)
TEMP(OutputIFPT )
OutputDCIPT = NBPredictDiagTool(PredictionDF = PredictPT_DCI, source = "jobber", TestSkus = REF.NewSkuList)
TEMP(OutputDCIPT )

OutputJobberSE = NBPredictDiagTool(PredictionDF = PredictSE_jobber, source = "jobber", TestSkus = REF.NewSkuList)
TEMP(OutputJobberSE )
OutputIFSE = NBPredictDiagTool(PredictionDF = PredictSE_IF, source = "jobber", TestSkus = REF.NewSkuList)
TEMP(OutputIFSE )
OutputDCISE = NBPredictDiagTool(PredictionDF = PredictSE_DCI, source = "jobber", TestSkus = REF.NewSkuList)
TEMP(OutputDCISE )


#test = JobberPTPredict(BrandName = "Fass_Fuel")
TEMP(test )

#TestSkus = REF.NewSkuList
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











