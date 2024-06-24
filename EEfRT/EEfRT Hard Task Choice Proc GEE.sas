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
where Adjudicated_is_1 AND Valid_is_1 = 1 AND Trial_Number >= 0 AND Choice_Time ^= 5;
class ID CTrial_Number Status Sex Difficulty_Choice Successful_Completion;
model Difficulty_Choice = Status Trial_Number Reward_Value Reward_Probability Expected_Value Sex /dist = binary link = logit;
repeated subject=ID /type = exch covb corrw within=CTrial_Number;
lsmeans Status /oddsratio CL diff;
ods graphics on / width=1000px height=750px;
effectplot slicefit (x = Trial_Number sliceby = Status) /clm AT(Sex(CODED)=0.6 0.4);
effectplot slicefit (x = Reward_Value sliceby = Status) /clm AT(Sex(CODED)=0.6 0.4);
effectplot slicefit (x = Reward_Probability sliceby = Status) /clm AT(Sex(CODED)=0.6 0.4);
store gee;
run;
