lm_gm_TL<-lm(log(gonads_g)~TL_mm, data)

summary(lm_gm_TL)




nd1<-data.frame(TL_mm=100:450, gonads_g="")
 
nd1$gonads_g<-exp(predict(lm_gm_TL, newdata=nd1))

plot(log(gonads_g)~TL_mm, data)
abline(lm_gm_TL)

ggplot(aes(y=gonads_g, x=TL_mm), data=data)+
 geom_point()+
 geom_line(inherit.aes=F, aes(y=gonads_g, x=TL_mm), data=nd1)

as.data.frame(p1)


plot(pred.eggs~gonads_g, data1)
