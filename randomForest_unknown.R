library(randomForest)
set.seed(1012)

training.rf<-randomForest(class~., data=zcurve_4472500)

unknown.pred<-predict(training.rf, unknown_zcurve_4472500)

vote<-predict(training.rf, unknown_zcurve_4472500, norm.votes=TRUE, type="vote")

reads<-as.character(unknown_zcurve_4472500$seq_name)
aaa<-cbind(unknown.pred, vote, reads)
write.table(aaa, file="/apple/unknown_vote_4472500")
