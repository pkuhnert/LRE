#include <stdlib.h>
#include <math.h>



extern void MatrixCalc(double ** W, double ** SS, double * LhatC,
                int n, int len, double rho, double sig2, double *var);


void calcSE(double *XdesignV, double *LhatC, double *alpha1,
         double *alpha2, double *beta1, double *rho, double *WV, double *sig2,
         int *len, int *n, double *SEload){



  int i, j, k;
  double var, me, meS;
  double se, seS, Ssum;



  /* Allocating memory */
  double** Xdesign = (double**) calloc(*n, sizeof(double *));
  for(i=0; i<*n; i++){
      Xdesign[i] = (double*) calloc(*len, sizeof(double));
  }
  double** SS = (double**) calloc(*len, sizeof(double *));
  double** W = (double**) calloc(*len, sizeof(double *));
  for(i=0; i<*len; i++){
      SS[i] = (double*) calloc(*len, sizeof(double));
      W[i] = (double*) calloc(*len, sizeof(double));
  }
  double* S = (double*) calloc(*len, sizeof(double));





  /* Coercing relevant Vectors into Matrices */
  /* Xdesign Matrix: */
  k = 0;
  for(j=0; j<*len; j++){
      for(i=0; i<*n; i++){
         Xdesign[i][j] = XdesignV[k];
         k += 1;
      }
  }



  /* W matrix: */
  k = 0;
  for(j=0; j<*len; j++){
      for(i=0; i<*len; i++){
          W[i][j] = WV[k];
         k += 1;
      }
  }



  /* Main part of program */
  Ssum = 0.0;
  for(j=0; j<*len; j++){
      for(i=0; i<*n; i++)
          Ssum += Xdesign[i][j] * LhatC[i];
      S[j] = Ssum;
      Ssum = 0.0;
  }
  for(i=0; i<*len; i++){
      for(j=0; j<*len; j++)
          SS[i][j] = S[i] * S[j];
  }


  /* Is autocorrelation present? */
  MatrixCalc(W, SS, LhatC, *n, *len, *rho, *sig2, &var);

  /* Measurment Error */
  if(*alpha1 == 0.0)
     me = 0.0;
  else{
     meS = 0;
     for(i=0; i<*n; i++){
         meS += pow(LhatC[i], 2) * pow(1 + *beta1,2);
     }
     me = pow(*alpha1, 2) * meS;
   }
   /* cross-sectional error */
  if(*alpha2 == 0.0)
     se = 0.0;
  else{
     seS = 0;
     for(i=0; i<*n; i++){
         seS += LhatC[i] * (1 + *beta1);
     }
     se = pow(*alpha2, 2) * pow(seS, 2);
   }
   *SEload = sqrt(var + me + se);


   /* Freeing memory */
  for(i=0; i<*n; i++){
      free(Xdesign[i]);
  }
  free(Xdesign);
  for(i=0; i<*len; i++){
      free(SS[i]);
      free(W[i]);
  }
  free(SS);
  free(W);
  free(S);





}





