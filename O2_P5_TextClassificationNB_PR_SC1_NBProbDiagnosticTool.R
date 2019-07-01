###For all given labels, identify which word combinations result in a match

ProbDiagnosis = function(df){
#Input prob df
RefProbdf = PTLabelProb 
DiagProbDF = RefProbdf 
DiagProbDF = subset(DiagProbDF , DiagProbDF$Label != "Label" & DiagProbDF$Word!= "Word" & DiagProbDF$Word!= "-")

AllDiagLabels = DiagProbDF[!duplicated(DiagProbDF[,1]),][,1]
AllDiagWords = DiagProbDF[!duplicated(DiagProbDF[,3]),][,3]



for(L in 1:length(AllDiagLabels)){

	LabelText = (AllDiagLabels[L])

	message("---------",LabelText)	

	#Prob(Label)
	LProb = as.numeric(subset(RefProbdf, 
			RefProbdf$Label == LabelText & 
			RefProbdf$LabelPresent == "Yes" &
			RefProbdf$Word == "-" & 
			RefProbdf$WordPresent == "All", 
			select = Prob)[1])

	#Prob(!Label)
	nLProb = as.numeric(subset(RefProbdf, 
			RefProbdf$Label == LabelText& 
			RefProbdf$LabelPresent == "No" &
			RefProbdf$Word == "-" & 
			RefProbdf$WordPresent == "All", 
			select = Prob)[1])

	#Standardize Text in original Column
	Textdf_char = trimws(as.character(LabelText ))#Turn into character
	Textdf_lower = tolower(Textdf_char)#lower case
	Textdf_ac = gsub("A/C", "AC", Textdf_lower)
	Textdf_punc = gsub("[[:punct:]]", " ",Textdf_ac)#To remove punctuations
	Textdf_tripleSpace = gsub("   ", " ", Textdf_punc)
	Textdf_doubleSpace = gsub("  ", " ", Textdf_tripleSpace)
	LabelWords = strsplit(Textdf_doubleSpace, " ")[[1]]
	LabelWords = LabelWords[!duplicated(LabelWords)]



	for(n in 1:length(LabelWords)){
		Worddf = data.frame(WORD = t(combn(LabelWords, n)))
		outputWorddf = Worddf 

		for(r in 1:nrow(Worddf)){

			WgLProb = 0
			WgnLProb = 0

		for(c in 1:ncol(Worddf)){

		WordValue = Worddf[r,c]
				
		#Prob(Word|Label)
		WgL_ind = as.numeric(subset(RefProbdf, 
				RefProbdf$Label == LabelText& 
				RefProbdf$LabelPresent == "Yes" &
				RefProbdf$Word == WordValue & 
				RefProbdf$WordPresent == "Yes", 
				select = Prob)[1])
		#message(WgL_ind)

		#Prob(Word|!Label)
		WgnL_ind = as.numeric(subset(RefProbdf, 
				RefProbdf$Label == LabelText& 
				RefProbdf$LabelPresent == "No" &
				RefProbdf$Word == WordValue & 
				RefProbdf$WordPresent == "Yes", 
				select = Prob)[1])
		#message(WgnL_ind)


		WgLProb = WgLProb + WgL_ind 
		WgnLProb = WgnLProb + WgnL_ind 

		}

		LgWProb = WgLProb + LProb 
		nLgWProb = WgnLProb + nLProb 
		#message(LgWProb )
		#message(nLgWProb )

		outputWorddf$LgWProb[r]= LgWProb
		outputWorddf$nLgWProb[r]= nLgWProb
		outputWorddf$Larger[r]= LgWProb > nLgWProb
		}
		message("")
		print(outputWorddf)

	}
		message("")

} 

}

#ProbDiagnosis (df= PTLabelProb)


