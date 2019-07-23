# ##sy: simulate draws from spline model
if(length(ranefs)>0){
  means<-bs_model@beta
}else{
  means<-bs_model$coefficients
}
cov_mat<-vcov(bs_model)
beta_draws<-rmvnorm(n=1000, mean=means, sigma=as.matrix(cov_mat))


##sy: data. Design matrix is combo of fixefs and basis effects
X_pred<-make_fixef_matrix(pred_data, fixefs=fixefs, add_intercept = T)

##sy: add basis effects
B_pred<-matrix(ncol=0, nrow=nrow(pred_data))
for(d_var in delta_vars){
  if(length(ranefs)>0){
    specs<-bs_model@frame[[paste0("bs(", d_var, ")")]] #`bs(age_start)`
  }else{
    specs<-bs_model$model[[paste0("bs(", d_var, ")")]] #`bs(age_start)`
    
  }
  knots<-attr(specs, "knots")
  bounds<-attr(specs, "Boundary.knots")
  
  temp_b_matrix<-bs(pred_data[[d_var]], knots=knots, Boundary.knots = bounds)
  colnames(temp_b_matrix)<-paste0(d_var, 1:ncol(temp_b_matrix))
  B_pred<-cbind(B_pred, temp_b_matrix)
}

X_pred<-cbind(X_pred, B_pred)


##sy: predict
draw_list<-list(betas=beta_draws)
data_list<-list(X=X_pred)
pred_math<-"exp(X %*% betas)"
system.time(
  spline_preds<-predict_draws(prediction_math=pred_math, draw_list=draw_list, data_list=data_list,
                              return_draws=F, upper_lower=T)
)
setnames(spline_preds, c("pred", "lower", "upper"), c("sp_pred", "sp_lower", "sp_upper"))


preds<-cbind(preds, spline_preds)


