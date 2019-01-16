#include <stdlib.h>
#include <math.h>

void MatrixCalc(double ** W, double ** SS, double * LhatC,
                int n, int len, double rho, double sig2, double *var){

   int i, j, k;
   double Dsum, LhatCsum, Lsum, Wsum;
   
   /* Allocating memory */
   double** WSS = (double**) calloc(len, sizeof(double *));
   for(i=0; i<len; i++)
       WSS[i] = (double*) calloc(len, sizeof(double));
   double* D = (double*) calloc(len, sizeof(double));



   /* Matrix multiplication */
   for(k=0; k<len; k++){
       for(i=0; i<len; i++){
          Wsum = 0.0;
          for(j=0; j<len; j++)
              Wsum += W[i][j] * SS[j][i];
          WSS[i][k] = Wsum;
       }
   }

   /* Calculate first term */
   for(i=0; i<len; i++)
       D[i] = WSS[i][i];
   Dsum = 0.0;
   for(i=0; i<len; i++)
         Dsum += D[i];

  /* Calculate second term */
  if(rho == 0){
     LhatCsum = 0.0;
     for(j=0; j<n; j++)
            LhatCsum += pow(LhatC[j], 2);
  }
  else{
    Lsum = 0.0;
    LhatCsum = 0.0;
    for(k=0; k<n; k++){
        for(j=0; j<n; j++)
            Lsum += LhatC[j] * pow(rho, abs(j-k));
        LhatCsum += Lsum * LhatC[k];
        Lsum = 0.0;
    }
  }
         
         
  *var = Dsum + sig2 * LhatCsum;

   /* Freeing memory */
   for(i=0; i<len; i++)
       free(WSS[i]);
   free(WSS);
   free(D);


}
