library(xgboost)
library(pROC)
library(gtools)
library(tidyverse)

CLR_all <- roc(all.test.response, all.test.predictor, auc=TRUE, ci=TRUE)
UniRaw <- roc(all.test.response, all.test.predictor, auc=TRUE, ci=TRUE)
Raw <- roc(all.test.response, all.test.predictor, auc=TRUE, ci=TRUE)

plot(c(1,0),c(0,1),
     type='l',
     lty=3,
     xlim=c(1.01,0), ylim=c(-0.01,1.01),
     xaxs='i', yaxs='i',
     ylab='', xlab='')

plot(CLR_all,
     col='black',
     lwd=2,
     add=T,
     lty=1)
auc.ci <- ci.auc(CLR_all)
ci.sp.obj1 <- ci.sp(CLR_all, sensitivities = seq(0,1,.01), boot.n=100)
plot(ci.sp.obj1, type = "shape", col = adjustcolor("grey", alpha = 0.5))
legend(x=0.7,y=0.2,
       legend=(sprintf('Test - AUC: %.3g, CI: %.3g', CLR_all$auc, (auc.ci[3]-auc.ci[2]))),
       bty='n',
       xjust=0,
       lty=c(1,1),
       col='black',
       text.col='black')


plot(UniRaw,
     col='darkblue',
     lwd=2,
     add=T,
     lty=1)
auc.ci <- ci.auc(UniRaw)
ci.sp.obj2 <- ci.sp(UniRaw, sensitivities = seq(0,1,.01), boot.n=100)
plot(ci.sp.obj2, type = "shape", col = adjustcolor("darkblue", alpha = 0.4))
legend(x=0.7,y=0.25,
       legend=(sprintf('Test - AUC: %.3g, CI: %.3g', UniRaw$auc, (auc.ci[3]-auc.ci[2]))),
       bty='n',
       xjust=0,
       lty=c(1,1),
       col='darkblue',
       text.col='darkblue')


plot(Raw,
     col='darkred',
     lwd=2,
     add=T,
     lty=1)
auc.ci <- ci.auc(Raw)
ci.sp.obj3 <- ci.sp(Raw, sensitivities = seq(0,1,.01), boot.n=100)
plot(ci.sp.obj3, type = "shape", col = adjustcolor("darkred", alpha = 0.4))
legend(x=0.7,y=0.15,
       legend=(sprintf('Test - AUC: %.3g, CI: %.3g', Raw$auc, (auc.ci[3]-auc.ci[2]))),
       bty='n',
       xjust=0,
       lty=c(1,1),
       col='darkred',
       text.col='darkred')

# Label the axes
mtext(side=2,
      text="Sensitivity",
      line=2.5,
      cex=1)
mtext(side=1,
      text="Specificity",
      line=2.5,
      cex=1)
