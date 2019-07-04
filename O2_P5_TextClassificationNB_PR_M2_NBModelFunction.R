
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

pbL = txtProgressBar(min = 0, max = length(AllLabelText), initial = 0, char = ">", width = 100, style = 3)
#pbW = txtProgressBar(min = 2, max = ncol(LWMatrix), initial = 0, char = "*", width = 100, style = 3)
for(L in 1:length(AllLabelText)){

	LabelCol = data.frame(LabelValue = LWMatrix[,1], Label = 0, stringsAsFactors = FALSE)
	LabelText = AllLabelText[L]
	LabelCol$Label[LabelCol$LabelValue == LabelText] <- 1 
	Label = data.frame(Label = LabelCol[,2])
	
	#message("---", LabelText, " ", sum(Label))

	#All Label present and Absent Values
	LabelAbsent= c(as.character(LabelText), "No", "-", "All", log((nrow(Label) - sum(Label)+1)/(nrow(Label)+nLabels) ) )
	NBmodel= rbind(NBmodel, LabelAbsent)
	LabelPresent= c(as.character(LabelText), "Yes", "-", "All", log((sum(Label)+1)/(nrow(Label)+nLabels)))
	NBmodel= rbind(NBmodel, LabelPresent)

for(W in 2:ncol(LWMatrix)){

	Word = data.frame(Word = LWMatrix[,W])
	WordText = names(LWMatrix)[W]
	#message(WordText , " ", sum(Word ))

	Contrastdf = cbind(Label, Word)

	#Common Values
	Contrastdf$Common = as.integer((Contrastdf$Label== 1) & (Contrastdf$Word== 1))
	CommonValue = c(as.character(LabelText), "Yes", as.character(WordText), "Yes", log((sum(Contrastdf$Common)+1)/ (nrow(Contrastdf)+ nWords)) )
	NBmodel= rbind(NBmodel, CommonValue )

	#Absent Values
	Contrastdf$Absent = as.integer((Contrastdf$Label== 0) & (Contrastdf$Word== 0))
	AbsentValue = c(as.character(LabelText), "No", as.character(WordText), "No", log((sum(Contrastdf$Absent)+1)/ (nrow(Contrastdf)+ nWords) ) )
	NBmodel= rbind(NBmodel, AbsentValue)

	#Label Only Value
	Contrastdf$LabelOnly =  as.integer((Contrastdf$Label== 1) & (Contrastdf$Common == 0) & (Contrastdf$Absent == 0))
	LabelOnlyValue = c(as.character(LabelText), "Yes", as.character(WordText), "No", log((sum(Contrastdf$LabelOnly)+1)/(nrow(Contrastdf)+ nWords)) )
	NBmodel= rbind(NBmodel, LabelOnlyValue)

	Contrastdf$WordOnly =  as.integer((Contrastdf$Word== 1) & (Contrastdf$Common == 0) & (Contrastdf$Absent == 0))
	WordOnlyValue = c(as.character(LabelText), "No", as.character(WordText), "Yes", log((sum(Contrastdf$WordOnly)+1)/(nrow(Contrastdf)+ nWords)) )
	NBmodel= rbind(NBmodel, WordOnlyValue)

	#message(L, " ", W, " ", sum(Contrastdf$Common), " ", sum(Contrastdf$Absent ), " ", sum(Contrastdf$LabelOnly), " ", sum(Contrastdf$WordOnly))
	#setTxtProgressBar(pbW,W)

}	
	setTxtProgressBar(pbL,L)
}
NBmodel = subset(NBmodel, Label != "DUMB" & LabelPresent != "DUMB" & Word != "DUMB" & WordPresent != "DUMB")

return(NBmodel)
}
################
###Troubleshooting

#TEMP(NBmodel)

#TrainingSetModel = NBmodel 

#test = NBModel(LWMatrix=testMat )
#TEMP(test)
























