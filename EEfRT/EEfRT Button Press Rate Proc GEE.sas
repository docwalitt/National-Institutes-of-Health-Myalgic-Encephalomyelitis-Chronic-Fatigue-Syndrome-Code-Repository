data work.Import;
set work.Import;
by ID;
Trial_Number = Trial -1;
CTrial_Number = Trial_Number;
Status = Status_MECFS_is_1;
Sex = Sex_Male_is_1;
Difficulty_Choice = Trial_Difficulty_Hard_is_1;
Successful_Completion = Successful_Completion_Yes_is_1;
Reward_Value = Value_of_Reward;
Reward_Probability = Probability_of_Reward;
Expected_Value = Reward_Value*Reward_Probability;
run;

proc gee data=work.Import descending;
where Adjudicated_is_1 = 1 AND Valid_is_1 = 1 AND Trial_Number >= 0 AND Choice_Time ^= 5;
class ID CTrial_Number Status Sex Difficulty_Choice Successful_Completion;
model Button_Press_Rate = Status|Difficulty_Choice|Trial_Number Reward_Value Reward_Probability Expected_Value Sex;
repeated subject=ID /type = exch within=CTrial_Number;
estimate 'Trials slope, HC Difficulty_Choice = Easy' Trial_Number 1 Trial_Number*Status 1 0 Trial_Number*Difficulty_Choice 1 0 Trial_Number*Status*Difficulty_Choice 1 0 0 0,
	     'Trials slope, HC Difficulty_Choice = Hard' Trial_Number 1 Trial_Number*Status 1 0 Trial_Number*Difficulty_Choice 0 1 Trial_Number*Status*Difficulty_Choice 0 1 0 0,
		 'Trials slope, ME/CFS Difficulty_Choice = Easy' Trial_Number 1 Trial_Number*Status 0 1 Trial_Number*Difficulty_Choice 1 0 Trial_Number*Status*Difficulty_Choice 0 0 1 0,
	     'Trials slope, ME/CFS Difficulty_Choice = Hard' Trial_Number 1 Trial_Number*Status 0 1 Trial_Number*Difficulty_Choice 0 1 Trial_Number*Status*Difficulty_Choice 0 0 0 1 /e adj=bon;
estimate 'diff Trials slope, HC-ME/CFS Difficulty_Choice=0' Trial_Number*Status 1 -1 Trial_Number*Status*Difficulty_Choice 1 0 -1 0,
		 'diff Trials slope, HC-ME/CFS Difficulty_Choice=1' Trial_Number*Status 1 -1 Trial_Number*Status*Difficulty_Choice 0 1 0 -1 / e adj = bon;
estimate 'diff diff Trials slope, HC-ME/CFS Difficulty_Choice=0-Difficulty_Choice=1' Trial_Number*Status*Difficulty_Choice 1 -1 -1 1 / e;
effectplot slicefit (x = Trial_Number sliceby = Status plotby = Difficulty_Choice) /clm AT(Sex(CODED)=0.6 0.4);
run;
