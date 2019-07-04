#Calculate Naive Bayes probability using input probability df

NB_Prob = function(ProbDF, WordValueDF){

if(nrow(WordValueDF)>0){

#Construct Output DF
NBPorbOutput = data.frame(Labels = ProbDF[!duplicated(trimws(ProbDF$Label)),1], stringsAsFactors = FALSE)
NBPorbOutput = subset(NBPorbOutput , Labels != "Label" & Labels != "-")
NBPorbOutput$LProb = 0
NBPorbOutput$nLProb= 0
NBPorbOutput$WgL_ind= 0
NBPorbOutput$WgnL_ind= 0
NBPorbOutput$WgLProb= 0
NBPorbOutput$WgnLProb= 0 
NBPorbOutput$LgWProb= 0
NBPorbOutput$nLgWProb= 0
NBPorbOutput$Match = 0

A = -1

for(L in 1:nrow(NBPorbOutput)){

	#Set Label Value
	LabelValue = NBPorbOutput$Labels[L]
	A = A + 1

	#Prob(Label)
	LProb = as.numeric(subset(ProbDF, 
			ProbDF$Label == LabelValue & 
			ProbDF$LabelPresent == "Yes" &
			ProbDF$Word == "-" & 
			ProbDF$WordPresent == "All", 
			select = Prob)[1])
	NBPorbOutput$LProb[L] = LProb 

	#Prob(!Label)
	nLProb = as.numeric(subset(ProbDF, 
			ProbDF$Label == LabelValue & 
			ProbDF$LabelPresent == "No" &
			ProbDF$Word == "-" & 
			ProbDF$WordPresent == "All", 
			select = Prob)[1])
	NBPorbOutput$nLProb[L] = nLProb 
	
	NBPorbOutput$LProb [L] = LProb 
	NBPorbOutput$nLProb [L] = nLProb 
	WgLProb = 0
	WgnLProb = 0

	for(W in 1:nrow(WordValueDF)){

		WordValue = WordValueDF$Words[W]
		
		#Logic for WordValue absent in ProbDF$Word
		WordValue %in% ProbDF$Word 


		#message("### ", LabelValue, " ", WordValue)
		#Prob(Word|Label)
		WgL_ind = as.numeric(subset(ProbDF, 
				ProbDF$Label == LabelValue & 
				ProbDF$LabelPresent == "Yes" &
				ProbDF$Word == WordValue & 
				ProbDF$WordPresent == "Yes", 
				select = Prob)[1])
		#message("WgL_ind ", WgL_ind)
		NBPorbOutput$WgL_ind[L] = WgL_ind

		#Prob(Word|!Label)
		WgnL_ind = as.numeric(subset(ProbDF, 
				ProbDF$Label == LabelValue & 
				ProbDF$LabelPresent == "No" &
				ProbDF$Word == WordValue & 
				ProbDF$WordPresent == "Yes", 
				select = Prob)[1])
		#message("WgnL_ind ", WgnL_ind)
		NBPorbOutput$WgnL_ind[L]= WgnL_ind      

		#message("WgLProb ", WgLProb)
		#message("WgnLProb ", WgnLProb )

		WgLProb = WgLProb + WgL_ind 
		WgnLProb = WgnLProb + WgnL_ind 

	}

	#message("WgLProb for ",LabelValue ," ", WgLProb)
	#message("WgnLProb for ",LabelValue ," ", WgnLProb )

	NBPorbOutput$WgLProb[L]= WgLProb 
	NBPorbOutput$WgnLProb[L]= WgnLProb 

	LgWProb = WgLProb + LProb 
	nLgWProb = WgnLProb + nLProb 

	NBPorbOutput$LgWProb[L]= LgWProb 
	NBPorbOutput$nLgWProb[L]= nLgWProb 

	NBPorbOutput$Larger[L]= LgWProb > nLgWProb
	NBPorbOutput$Difference[L]= LgWProb - nLgWProb
	NBPorbOutput$expLgWProb = exp(NBPorbOutput$LgWProb)
	NBPorbOutput$expnLgWProb = exp(NBPorbOutput$nLgWProb)
	NBPorbOutput$sum = NBPorbOutput$expLgWProb + NBPorbOutput$expnLgWProb
	NBPorbOutput$PerLgWProb = round((NBPorbOutput$expLgWProb/NBPorbOutput$sum)*100, 3)
	NBPorbOutput$PernLgWProb = round((NBPorbOutput$expnLgWProb/NBPorbOutput$sum)*100, 3)

	if(LgWProb > nLgWProb){NBPorbOutput$Match[L] = 1} #else{NBPorbOutput$Match[i]= 0}

	}

	return(NBPorbOutput )
} else{return("No Match")}

}
	

###########################
#Trouble Shoot
#NB_Prob(ProbDF = TrainingPTNBmod , WordValueDF = PTwordMatch)

#Standardize Text in original Column
#LabelText = "System"
#Textdf_char = trimws(as.character(LabelText ))#Turn into character
#Textdf_lower = tolower(Textdf_char)#lower case
#Textdf_ac = gsub("A/C", "AC", Textdf_lower)
#Textdf_punc = gsub("[[:punct:]]", " ",Textdf_ac)#To remove punctuations
#Textdf_tripleSpace = gsub("   ", " ", Textdf_punc)
#Textdf_doubleSpace = gsub("  ", " ", Textdf_tripleSpace)
#Textdf_doubleSpace = gsub("  ", " ", Textdf_doubleSpace)
#LabelWords = strsplit(Textdf_doubleSpace, " ")[[1]]
#StWString = data.frame(Words = (LabelWords[!duplicated(LabelWords)]))
#NB_Prob(ProbDF = PTLabelProb , WordValueDF = StWString )

#ProbDiagnosis(df=TrainingSetNBmod)

#for(W in 2:ncol(TrainingPTWordMat)){

#	WordText = names(TrainingPTWordMat)[W]
#	message(names(TrainingPTWordMat)[W])

#	test = NB_Prob(ProbDF = PTLabelProb , WordValue = WordText )
#	print(test)
#}

#ProbValue = function(ProbDF, LabelText , LabelPresent1  , WordText , WordPresent1 ){
#
#		Value = 	subset(ProbDF, 
#				ProbDF$Label == LabelText & 
#				ProbDF$LabelPresent == LabelPresent1  &
#				ProbDF$Word == WordText & 
#				ProbDF$WordPresent == WordPresent1 )
#	return(Value)
#}
#ProbValue(ProbDF = PTLabelProb, LabelText = "Fuel System Pump Components" , LabelPresent1  = "Yes", WordText = "components", WordPresent1 = "Yes")









