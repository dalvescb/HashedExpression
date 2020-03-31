#include<stdio.h>
#include<stdbool.h>
#include<math.h>
double test1(double a, double b);
double test2(double a, double b);
double test3(double a, double b);
double test4(double a, double b);
double test5(double a);
double test6(double a);
double test7(double a);
double test8(double a);
double test9(double a);
double test10(double a);
double test11(double a);
double test12(double a);
double test13(double a);
double test14(double a);
double test15(double a);
double test16(double a);
double test17(double a);
double test18(double a);
int test(int,double,double);
int main()
{
  double af = 15, bf = 10, cf = 1, df = 49;
  int pass=0,fail=0,total;
  bool ret;
  printf(" %f + %f = %f\n",af,bf,test1(af,bf));
  ret=test(1,af,bf);
  ret?pass++:fail++;

  printf(" %f - %f = %f\n",af,bf,test2(af,bf));
  ret=test(2,af,bf);
  ret?pass++:fail++;

  printf(" %f * %f = %f\n",af,bf,test3(af,bf));
  ret=test(3,af,bf);
  ret?pass++:fail++;

  /*printf(" %f / %f = %f\n",bf,af,test4(bf,af));
  ret=test(4,af,bf);
  ret?pass++:fail++;*/

  printf("negate %f = %f\n",bf,test5(bf));
  ret=test(5,bf,0);
  ret?pass++:fail++;

  printf("sqrt %f = %f\n",df,test6(df));
  ret=test(6,df,0);
  ret?pass++:fail++;

  printf("cos %f = %f\n",cf,test8(cf));
  ret=test(8,cf,0);
  ret?pass++:fail++;

  printf("sin %f = %f\n",cf,test7(cf));
  ret=test(7,cf,0);
  ret?pass++:fail++;

 /* printf("tan %f = %f\n",cf,test9(cf));
  ret=test(9,cf,0);
  ret?pass++:fail++;

   printf("sinh %f = %f\n",cf,test10(cf));
  ret=test(10,cf,0);
  ret?pass++:fail++;

  printf("cosh %f = %f\n",cf,test11(cf));
  ret=test(11,cf,0);
  ret?pass++:fail++;

  printf("tanh %f = %f\n",cf,test12(cf));
  ret=test(12,cf,0);
  ret?pass++:fail++;

  printf("asin %f = %f\n",cf,test13(cf));
  ret=test(13,cf,0);
  ret?pass++:fail++;

  printf("acos %f = %f\n",cf,test14(cf));
  ret=test(14,cf,0);
  ret?pass++:fail++;

  printf("atan %f = %f\n",cf,test15(cf));
  ret=test(15,cf,0);
  ret?pass++:fail++;

  printf("asinh %f = %f\n",cf,test16(cf));
  ret=test(16,cf,0);
  ret?pass++:fail++;

  printf("acosh %f = %f\n",cf,test17(cf));
  ret=test(17,cf,0);
  ret?pass++:fail++;

  printf("atanh %f = %f\n",cf,test18(cf));
  ret=test(18,cf,0);
  ret?pass++:fail++;*/
    //if (ret == false){
    //  fail += 1;
    //} else {
    //  pass += 1;
    //  }
  printf("Total number of passed cases: %d\n",pass);
  printf("Total number of failed cases: %d\n",fail);
  total = pass + fail;
  printf("Total number of cases: %d \n",total);
  return 0;
}
int test(int n, double a, double b)
{
 double resultVal, trueVal;
 int testNum = 1, failed = 0;
 bool ret;
 switch (n)
 {
 /*case 1:{
    resultVal = test1(a,b);
    trueVal = a + b;
    }
 case 2:{
     resultVal = test2(a,b);
     trueVal = a - b;
     }
 case 3:{
     resultVal = test3(a,b);
     trueVal = a * b;
     }
/*case 4:{
     resultVal = test4(a,b);
     trueVal = a / b;
     }
 case 5:{
    resultVal = test5(a);
    trueVal = - a;
    }
  case 6:{
    resultVal = test6(a);
    trueVal = sqrt(a);
    }
 case 7:{
    resultVal = test7(a);
    trueVal = sin(a);
    }
 case 8:{
     resultVal = test8(a);
     trueVal = cos(a);
     }
 case 9:{
    resultVal = test9(a);
    trueVal = tan(a);
    }
 case 10:{
    resultVal = test10(a);
    trueVal = sinh(a);
    }
 case 11:{
    resultVal = test11(a);
    trueVal = cosh(a);
    }
 case 12:{
    resultVal = test12(a);
    trueVal = tanh(a);
    }
 case 13:{
    resultVal = test13(a);
    trueVal = asin(a);
    }
 case 14:{
    resultVal = test14(a);
    trueVal = acos(a);
    }
 case 15:{
    resultVal = test15(a);
    trueVal = atan(a);
    }
 case 16:{
    resultVal = test16(a);
    trueVal = asinh(a);
    }
 case 17:{
    resultVal = test17(a);
    trueVal = acosh(a);
    }
 case 18:{
    resultVal = test18(a);
    trueVal = atanh(a);
    }      */     //default: printf("Unknown selection\n");
 case 19:{

    resultVal = test19(a,b);
    b = int(b);
    trueVal = pow(a,b);
 }
 }
 printf("Test%d:  ",n);
 if (resultVal == trueVal) {
     printf("Pass\n");
     ret = true;
 } else {
     printf("Expected %f, returned %f\n",trueVal,resultVal);
     ret = false;
 }
 return ret;
}