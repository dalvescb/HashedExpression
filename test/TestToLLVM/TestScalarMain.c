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
double test19(double a);
double test20(double a);
double test21(double a, double b);
double test22(double a, double b, double c);
double test23(double a, double b, double c);
double test24(double a, double b, double c);
double test25(double a, double b, double c);
double test26(double a, double b, double c);
double test27(double a, double b, double c);
double test28(double a, double b, double c);
double test29(double a, double b);
double test30(double a, double b);
double test31(double a, double b, double c);
double test32(double a);
double test33(double a, double b);
int test(int,double,double,double);
int main()
{
  double af = 10, bf = 2, cf = 1, df = 49;
  int pass=0,fail=0,total;
  bool ret;
  printf(" %f + %f = %f\n",af,bf,test1(af,bf));
  ret=test(1,af,bf,0);
  ret?pass++:fail++;

  printf(" %f - %f = %f\n",af,bf,test2(af,bf));
  ret=test(2,af,bf,0);
  ret?pass++:fail++;

  printf(" %f * %f = %f\n",af,bf,test3(af,bf));
  ret=test(3,af,bf,0);
  ret?pass++:fail++;

  printf(" %f / %f = %f\n",af,bf,test4(af,bf));
  ret=test(4,af,bf,0);
  ret?pass++:fail++;

  printf("negate %f = %f\n",bf,test5(bf));
  ret=test(5,bf,0,0);
  ret?pass++:fail++;

  printf("sqrt %f = %f\n",df,test6(df));
  ret=test(6,df,0,0);
  ret?pass++:fail++;

  printf("sin %f = %f\n",cf,test7(cf));
  ret=test(7,cf,0,0);
  ret?pass++:fail++;

  printf("cos %f = %f\n",cf,test8(cf));
  ret=test(8,cf,0,0);
  ret?pass++:fail++;

  printf("tan %f = %f\n",cf,test9(cf));
  ret=test(9,cf,0,0);
  ret?pass++:fail++;

  printf("sinh %f = %f\n",cf,test10(cf));
  ret=test(10,cf,0,0);
  ret?pass++:fail++;

  printf("cosh %f = %f\n",cf,test11(cf));
  ret=test(11,cf,0,0);
  ret?pass++:fail++;

  printf("tanh %f = %f\n",cf,test12(cf));
  ret=test(12,cf,0,0);
  ret?pass++:fail++;

  printf("asin %f = %f\n",cf,test13(cf));
  ret=test(13,cf,0,0);
  ret?pass++:fail++;

  printf("acos %f = %f\n",cf,test14(cf));
  ret=test(14,cf,0,0);
  ret?pass++:fail++;

  printf("atan %f = %f\n",cf,test15(cf));
  ret=test(15,cf,0,0);
  ret?pass++:fail++;

  printf("asinh %f = %f\n",cf,test16(cf));
  ret=test(16,cf,0,0);
  ret?pass++:fail++;

  printf("acosh %f = %f\n",cf,test17(cf));
  ret=test(17,cf,0,0);
  ret?pass++:fail++;

  printf("atanh %f = %f\n",cf,test18(cf));
  ret=test(18,cf,0,0);
  ret?pass++:fail++;

  printf("exp %f = %f\n",cf,test19(cf));
  ret=test(19,cf,0,0);
  ret?pass++:fail++;

  printf("log %f = %f\n",cf,test20(cf));
  ret=test(20,cf,0,0);
  ret?pass++:fail++;

  printf(" %f ^ %f = %f\n",af,bf,test21(af,bf));
  ret=test(21,af,bf,0);
  ret?pass++:fail++;

  printf("%f + %f + %f = %f\n",af,bf,cf,test22(af,bf,cf));
  ret=test(22,af,bf,cf);
  ret?pass++:fail++;

  printf("%f + %f * %f = %f\n",af,bf,cf,test23(af,bf,cf));
  ret=test(23,af,bf,cf);
  ret?pass++:fail++;

  printf("%f * %f + %f = %f\n",af,bf,cf,test24(af,bf,cf));
  ret=test(24,af,bf,cf);
  ret?pass++:fail++;

  printf("%f + %f / %f = %f\n",af,bf,cf,test25(af,bf,cf));
  ret=test(25,af,bf,cf);
  ret?pass++:fail++;

  printf("%f * %f / %f = %f\n",af,bf,cf,test26(af,bf,cf));
  ret=test(26,af,bf,cf);
  ret?pass++:fail++;

  printf("%f * %f * %f = %f\n",af,bf,cf,test27(af,bf,cf));
  ret=test(27,af,bf,cf);
  ret?pass++:fail++;

  printf("%f / %f / %f = %f\n",af,bf,cf,test28(af,bf,cf));
  ret=test(28,af,bf,cf);
  ret?pass++:fail++;

  printf("%f + sin %f = %f\n",af,cf,test29(af,cf));
  ret=test(29,af,cf,0);
  ret?pass++:fail++;

  printf("sin %f - cos %f = %f\n",cf,cf,test30(cf,cf));
  ret=test(30,af,cf,0);
  ret?pass++:fail++;

  printf("%f + %f ^ %f = %f\n",af,cf,bf,test31(af,cf,bf));
  ret=test(31,af,cf,bf);
  ret?pass++:fail++;

  printf("neg(neg %f) = %f\n",cf,test32(cf));
  ret=test(32,cf,0,0);
  ret?pass++:fail++;

  printf("%f + neg %f = %f\n",af,bf,test33(af,bf));
  ret=test(33,af,bf,0);
  ret?pass++:fail++;

  printf("Total number of passed cases: %d\n",pass);
  printf("Total number of failed cases: %d\n",fail);
  total = pass + fail;
  printf("Total number of cases: %d \n",total);
  return 0;
}
int test(int n, double a, double b, double c)
{
 double resultVal, trueVal;
 int testNum = 1, failed = 0;
 bool ret = true;
 for (int loop = 0; loop < 10; loop++) {
 switch (n)
 {
 case 1:{
    resultVal = test1(a,b);
    trueVal = a + b;
    a++;
    b++;
    break;
    }
 case 2:{
     resultVal = test2(a,b);
     trueVal = a - b;
     a++;
     b++;
     break;
     }
 case 3:{
     resultVal = test3(a,b);
     trueVal = a * b;
     a++;
     b++;
     break;
     }
 case 4:{
     resultVal = test4(a,b);
     trueVal = a / b;
     a++;
     b++;
     break;
     }
 case 5:{
    resultVal = test5(a);
    trueVal = (-a);
    a++;
    break;
    }
  case 6:{
    resultVal = test6(a);
    trueVal = sqrt(a);
    a++;
    break;
   }
 case 7:{
    resultVal = test7(a);
    trueVal = sin(a);
    a+=0.1;
    break;
    }
 case 8:{
     resultVal = test8(a);
     trueVal = cos(a);
     a+=0.1;
     break;
     }
 case 9:{
    resultVal = test9(a);
    trueVal = tan(a);
    a+=0.1;
    break;
    }
 case 10:{
    resultVal = test10(a);
    trueVal = sinh(a);
    a+=0.1;
    break;
    }
 case 11:{
    resultVal = test11(a);
    trueVal = cosh(a);
    a+=0.1;
    break;
    }
 case 12:{
    resultVal = test12(a);
    trueVal = tanh(a);
    a+=0.1;
    break;
    }
 case 13:{
    resultVal = test13(a);
    trueVal = asin(a);
    a-=0.1;
    break;
    }
 case 14:{
    resultVal = test14(a);
    trueVal = acos(a);
    a-=0.1;
    break;
    }
 case 15:{
    resultVal = test15(a);
    trueVal = atan(a);
    a+=0.1;
    break;
    }
 case 16:{
    resultVal = test16(a);
    trueVal = asinh(a);
    a+=0.1;
    break;
    }
 case 17:{
    resultVal = test17(a);
    trueVal = acosh(a);
    a+=0.1;
    break;
    }
 case 18:{
    a-=0.1;
    resultVal = test18(a);
    trueVal = atanh(a);
    break;
    }          //default: printf("Unknown selection\n");
 case 19:{
    resultVal = test19(a);
    trueVal = exp(a);
    a++;
    break;
 }
 case 20:{
     resultVal = test20(a);
     trueVal = log(a);
     a++;
     break;
 }
 case 21:{
    resultVal = test21(a,b);
    trueVal = pow(a,b);
    a++;
    break;
 }
 case 22:{
    resultVal = test22(a,b,c);
    trueVal = a + b + c;
    a++;
    b++;
    c++;
    break;
 }
  case 23:{
     resultVal = test23(a,b,c);
     trueVal = a + b * c;
     a++;
     b++;
     c++;
     break;
  }
  case 24:{
     resultVal = test24(a,b,c);
     trueVal = a * b + c;
     a++;
     b++;
     c++;
     break;
  }case 25:{
     resultVal = test25(a,b,c);
     trueVal = a + b / c;
     a++;
     b++;
     c++;
     break;
  }case 26:{
     resultVal = test26(a,b,c);
     trueVal = a * b / c;
     a++;
     b++;
     c++;
     break;
  }case 27:{
     resultVal = test27(a,b,c);
     trueVal = a * b * c;
     a++;
     b++;
     c++;
     break;
  }case 28:{
     resultVal = test28(a,b,c);
     trueVal = a / b / c;
     //printf("R:%f T:%f\n",resultVal,trueVal);
     /*if (fabs(resultVal-trueVal)<1e-10)
     {printf("Pass\n");
     }
     else{ printf("Fail:%f\n",resultVal-trueVal);
     }
     */
     a++;
     b++;
     c++;
     break;
  }case 29:{
     resultVal = test29(a,b);
     trueVal = a + sin(b);
     a++;
     b+=0.1;
     break;
  }case 30:{
     resultVal = test30(a,b);
     trueVal = sin(a) - cos(b);
     a+=0.1;
     b+=0.1;
     break;
  }case 31:{
     resultVal = test31(a,b,c);
     //double d = a+b;
     trueVal = pow((a+b),c);
     a++;
     b++;
     break;
  }case 32:{
     resultVal = test32(a);
     trueVal = (-(-a));
     a++;
     break;
  }case 33:{
     resultVal = test33(a,b);
     trueVal = a+(-b);
     a++;
     b++;
     break;
  }
 }
 if (fabs(resultVal-trueVal)<1e-10) {
 } else {
   printf("Failed Test%d: (%f,%f,%f) Expected: %f, Returned: %f \n",n,a,b,c,trueVal,resultVal);
     ret = false;
 }
 }
 return ret;
}