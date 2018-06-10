# Analysis of the clean finisher-only data from the pilot study.

# Load the clean pilot data, changing the column names for ease of use.
namesPilot=c('run','progVersion','timeStarted','minSpent','positionCode','points','age','yearBirth','education','eduScore','gender','genScore','income','incScore','incEstimate','country','device','startAt','trID','posture','postureExp','seconds','timePhrase','timeCode','postureStartTime','timeClick','timeClick2','randomizeqp2k05a','goalsText','qScore','goals','anxiousBlank','anxious','confidentBlank','confident','goodBlank','good','powerfulBlank','powerful','happyBlank','happy','depressedBlank','depressed','mood','timeFinish','randomizem8u82ed','goalsPre','powerfulBlankPre','powerfulPre','anxiousBlankPre','anxiousPre','goodTextPre','goodPre','happyTextPre','happyPre','depressedTextPre','depressedPre','confidentBlankPre','confidentPre','moodPre','randomizeuw6cn40','goodText','anxiousText','happyText','confidentText','depressedText','goalsText','powerfulText','moodChg','powerfulChg','confidentChg','goodChg','goalsChg','happyChg','anxiousChg','depressedChg')
classesPilot=c('numeric','numeric','character','numeric','factor','numeric','numeric','numeric','character','integer','factor','numeric','character','integer','numeric','character','factor','character','character','factor','character','numeric','character','character','character','character','character','character','character','integer','integer','character','integer','character','integer','character','integer','character','integer','character','integer','character','integer','integer','character','character','integer','character','integer','character','integer','character','integer','character','integer','character','integer','character','integer','integer','character','character','character','character','character','character','character','character','integer','integer','integer','integer','integer','integer','integer','integer')
pilot=read.csv("Power pose replication study - clean - only finishers - anonymized.csv",col.names=namesPilot,colClasses=classesPilot,stringsAsFactors = FALSE)

# Check that the measures of mood correlate with each other and it makes sense to combine them into a single measure of mood.
print("Mood measures before posing")
print(cor(pilot[,c(47,49,51,53,55,57,59)])) # Pre posing variables
print("Mood measures after posing")
print(cor(pilot[,c(31,33,35,37,39,41,43)])) # After posing variables

# Encode how uncomfortable / powerful each position is.
pilot=cbind(pilot,discomfort=factor(posture,levels=c("akimbo","armsBehindHead","armsStraightUp","crumpled","neutralArmsAtSides","noPosing","strongMan")),power=factor(posture,levels=c("akimbo","armsBehindHead","armsStraightUp","crumpled","neutralArmsAtSides","noPosing","strongMan")))
levels(pilot$discomfort) = c(0.5,1,1.5,1,0,0,1) # Based on personal guess at how uncomfortable each posture is.
pilot$discomfort=as.numeric(as.character(pilot$discomfort))
levels(pilot$power) = c(1,1,1,-1,0,0,1) # Crumpled is powerless, all power poses are assumed equal.
pilot$power=as.numeric(as.character(pilot$power))

# Fit three linear models of effects on mood change: power only, power + regression to mean, power + reg to mean + discomfort
model1=lm(moodChg~power,pilot)
model2=lm(moodChg~power+moodPre,pilot)
model3=lm(moodChg~power+moodPre+discomfort,pilot)
print("ANOVA of three linear models")
print(anova(model1,model2,model3)) # Discomfort doesn't add anything to the model and we'll ignore it from now on.
print("Coefficients for power and regression to mean")
print(summary(model2)) 
# The R-squared is below 0.1, the vast majority of mood change is unaccounted for. 
# Interestingly, the intercept of 1.9 is highly significant. Mood improves over time regardless of anything!

# Add a vector of actual time passed while posing, see if it affects mood change
pilot=cbind(pilot,time=as.numeric(sub(" seconds","",pilot$timeClick)))
model4=lm(moodChg~power+moodPre+time,pilot)
print("Model with time spent")
print(summary(model4)) # Nope.

# Does model 2 do a good job?
library(ggplot2)
plotModelFit=ggplot()+geom_jitter(aes(x=pilot$moodChg,y=model2$fitted.values))+labs(x="Observed mood change",y="Predicted mood change") 
# We can see how weak the relationship is at R-squared = 0.09. At least it looks unbiased.

# Exploratory: let's see if any variables affect power sensitivity.
pilot=cbind(pilot,moodAdj=pilot$moodChg-model2$coefficients[3]*pilot$moodChg)
pilot=cbind(pilot,sensitivity=numeric(length=length(pilot$run)))
pilot$sensitivity[power!=0]= pilot$moodAdj[power!=0]*pilot$power[power!=0] 
# It's basically just a measure of mood change because of the tiny model effects, but this correction is better than nothing.
print("Do any demographic variables affect mood change?")
print(summary(lm(moodChg~age+eduScore+genScore+incEstimate,pilot))) # Nope, none.
print("Do any demographic variables affect sensitivity to power?")
print(summary(lm(sensitivity~age+eduScore+genScore+incEstimate,pilot))) # Perhaps it drops a bit with age.
