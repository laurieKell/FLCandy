#define TMB_LIB_INIT R_init_FLCandy

#include <TMB.hpp>

// Space time
template<class Type>

// objective function
Type objective_function<Type>::operator() () {

  // Data
  DATA_VECTOR( rec );
  DATA_VECTOR( ssb );
  DATA_VECTOR( prior_s ); // Prior vector for s, [logit(mean), stdev in logit, useflag]
  DATA_VECTOR( spr0 );
  DATA_INTEGER(nyears);
  DATA_INTEGER(Rmodel); // Recruitment model

  // Parameters
  PARAMETER(log_r0);
  PARAMETER(log_sigR);
  PARAMETER(logit_s);
  PARAMETER(logit_d);
  
  // Derived quantities
  Type r0 = exp(log_r0);
  Type sigR = exp(log_sigR);
  Type s = 0.2001 + (0.9999-0.2001)/(1+exp(-logit_s));//falls between 0.2001 & 0.9999
  Type d = 0.5 + (2.0 - 0.5) / (1 + exp(-logit_d));//falls between 0.5 & 2.0
  vector<Type> log_rec_hat(nyears);
  
  Type a = 0;
  Type b = 0;
  Type d = 0;
  
  // Objective function
  Type ans=0;
  vector<Type> v(nyears);

  if(Rmodel==0){ // bevholtSV()
   for( int t=0; t< nyears; t++){
     v(t)=r0*spr0(t);
     log_rec_hat(t) = log(4.0 * s * r0 *ssb(t) / (v(t)*(1.0-s)+ssb(t)*(5.0*s-1.0)));//-pow(sigR,2)/2.0;
     }}

   if(Rmodel==1){ // rickerSV()
     for( int t=0; t< nyears; t++){
       v(t)=r0*spr0(t);
       b = log(5.0*s)/(0.8*v(t));
       a = exp(b*v(t))/spr0(t);
       log_rec_hat(t) = log(a*ssb(t)*exp(-b*ssb(t)));
       //log_rec_hat(t) = log(r0 * ssb(t) / v * exp(s*(1.0-ssb(t)/v)));
     }}
   
   if(Rmodel==2){ // segreg() aka Hockey Stick
     for( int t=0; t< nyears; t++){
       v(t)=r0*spr0(t);
       log_rec_hat(t) = log(r0)+log(2.5*s/v(t)*(ssb(t)+0.2*v(t)/s-pow(pow(ssb(t)-0.2*v(t)/s,2.0),0.5)));//-pow(sigR,2)/2.0;
     }}


   // Depensatory Beverton and Holt
   // r=a/(1+(b/S)^d)
   // V=a*spr0
   // h=((1+b/V)^d)/((1+b/V*0.4)^d)
   //if(Rmodel==3){ 
   //   for( int t=0; t< nyears; t++){
   //    v(t)=r0*spr0(t);
   //    a(t)=v(t)/spr0(t);
   //     b(t)=0.2*((0.2*a(t)*spr0(t))/v(t)-1.0)^(1.0/d);
   //    
   //     log_rec_hat(t) = log(a(t)/(1+(b(t)/ssb(t))^d));
   //    }
   }

   vector<Type> rec_hat = exp(log_rec_hat);
 
   // LL
   for( int t=0; t<nyears; t++){
     ans -= dnorm(log(rec(t)), log_rec_hat(t), sigR, true);
   }
 
  //ans=loglAR1(log(rec(t)), log_rec_hat(t), rho) 

   //prior s
   ans -= dnorm(logit_s, prior_s(0), prior_s(1), 1); // Prior for logn
   
   
   
//   
//   if(Rmodel==0){
//   a = Type(4)*v*s/(spr0*(Type(5)*s-Type(1)));
//   b = v*(Type(1)-s)/(Type(5)*s-Type(1));
//   }
//   
//   if(Rmodel==1){
//     //b = log(5.0*s)/(0.8*v);
//     //a = exp(b*v)/spr0;
//   }
//   
//   if(Rmodel==2){
//     b = 0.2*v/s;
//     a = r0/b;
//   }
//   
//   // prior s
//   
//   
   // Reporting
   REPORT( rec_hat );
   REPORT( nyears );
   REPORT( sigR );
   REPORT( r0 );
   REPORT( v );
//   REPORT( a );
//   REPORT( b );
   REPORT( s );
   ADREPORT(r0);
   ADREPORT(v);
// 
   return ans;}



//logit_d=seq(0.5,2,0.01);y=0.5 + (2.0 - 0.5) / (1 + exp(-logit_d))
//plot(y,logit_d)

  
