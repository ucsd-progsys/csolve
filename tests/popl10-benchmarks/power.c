/* Generated by CIL v. 1.3.7 */
/* print_CIL_Input is true */

#include <stdlib.h>
#include <math.h>
#include <time.h>
#include <csolve.h>

struct demand {
   double P ;
   double Q ;
};
typedef struct demand Demand;
struct lateral;
struct root {
   Demand D ;
   double theta_R ;
   double theta_I ;
   Demand last ;
   double last_theta_R ;
   double last_theta_I ;
   struct lateral *(SHAPE_IGNORE_BOUND feeders)[10] ;
};
typedef struct root *Root;
struct branch;
struct lateral {
   Demand D ;
   double alpha ;
   double beta ;
   double R ;
   double X ;
   struct lateral *next_lateral ;
   struct branch *branch ;
};
typedef struct lateral *Lateral;
struct leaf;
struct branch {
   Demand D ;
   double alpha ;
   double beta ;
   double R ;
   double X ;
   struct branch *next_branch ;
   struct leaf *(SHAPE_IGNORE_BOUND leaves)[10] ;
};
typedef struct branch *Branch;
struct leaf {
   Demand D ;
   double pi_R ;
   double pi_I ;
};
typedef struct leaf *Leaf;
typedef long clock_t;
#pragma merger(0,"/tmp/cil-elT7V5Z3.i","")
void Compute_Tree(Root r ) ;
Demand *Compute_Lateral(Lateral l , double theta_R , double theta_I , double pi_R ,
                        double pi_I ) ;
Demand *Compute_Branch(Branch br , double theta_R , double theta_I , double pi_R ,
                       double pi_I ) ;
Demand *Compute_Leaf(Leaf l , double pi_R , double pi_I ) ;
void optimize_node(double pi_R , double pi_I ) ;
double find_g(void) ;
double find_h(void) ;
double find_gradient_f(double pi_R , double pi_I , double * ARRAY gradient ) ;
double find_gradient_g(double * ARRAY gradient ) ;
double find_gradient_h(double * ARRAY gradient ) ;
void find_dd_grad_f(double pi_R , double pi_I , double * ARRAY dd_grad ) ;
double make_orthogonal(double *  ARRAY v_mod , double *v_static ) ;
void Compute_Tree(Root r ) 
{ register int i ;
  Lateral l ;
  Demand *a ;
  Demand tmp ;
  double theta_R ;
  double theta_I ;

  {
  tmp.P = 0.0;
  tmp.Q = 0.0;
  i = 0;
  while (i < 10) {
    validptr(r);
    validptr(&r->feeders[i]);
    l = r->feeders[i];
    if (l == (Lateral)0) { DIVERGE: goto DIVERGE; } // pmr ASSUME
    validptr(&r->theta_R);
    theta_R = r->theta_R;
    validptr(&r->theta_I);
    theta_I = r->theta_I;
    a = Compute_Lateral(l, theta_R, theta_I, theta_R, theta_I);
    // pmr: a used to be on stack; don't check ptr validity
    tmp.P += a->P;
    tmp.Q += a->Q;
    i ++;
  }
  validptr(&r->D.P);
  r->D.P = tmp.P;
  validptr(&r->D.Q);
  r->D.Q = tmp.Q;
  return;
}
}
Demand *Compute_Lateral(Lateral l , double theta_R , double theta_I , double pi_R ,
                        double pi_I ) 
{ Demand *a1 ;
  Demand *a2 ;
  double new_pi_R ;
  double new_pi_I ;
  double a ;
  double b ;
  double c ;
  double root ;
  Lateral next ;
  Branch br ;
  double tmp ;
  Demand *d ;
  void *tmp___0 ;

  {
  validptr(l);
  validptr(&l->alpha);
  validptr(&l->beta);
  validptr(&l->X);
  validptr(&l->R);
  new_pi_R = pi_R + l->alpha * (theta_R + (theta_I * l->X) / l->R);
  new_pi_I = pi_I + l->beta * (theta_I + (theta_R * l->R) / l->X);
  validptr(&l->next_lateral);
  next = l->next_lateral;
  if ((unsigned int )next != (unsigned int )((Lateral )0)) {
    a1 = Compute_Lateral(next, theta_R, theta_I, new_pi_R, new_pi_I);
  }
  validptr(&l->branch);
  br = l->branch;
  a2 = Compute_Branch(br, theta_R, theta_I, new_pi_R, new_pi_I);
  validptr(&l->D.P);
  validptr(&l->D.Q);
  if ((unsigned int )next != (unsigned int )((Lateral )0)) {
    l->D.P = a1->P + a2->P;
    l->D.Q = a1->Q + a2->Q;
  } else {
    l->D.P = a2->P;
    l->D.Q = a2->Q;
  }
  a = l->R * l->R + l->X * l->X;
  b = ((((double )2.0 * l->R) * l->X) * l->D.Q - (((double )2.0 * l->X) * l->X) * l->D.P) - l->R;
  c = l->R * l->D.Q - l->X * l->D.P;
  c = c * c + l->R * l->D.P;
  tmp = sqrt(b * b - ((double )4.0 * a) * c);
  root = (- b - tmp) / ((double )2.0 * a);
  l->D.Q += ((root - l->D.P) * l->X) / l->R;
  l->D.P = root;
  a = ((double )2.0 * l->R) * l->D.P;
  b = ((double )2.0 * l->X) * l->D.Q;
  l->alpha = a / (((double )1.0 - a) - b);
  l->beta = b / (((double )1.0 - a) - b);
  tmp___0 = malloc((int )sizeof(Demand ));
  d = (Demand *)tmp___0;
  d->P = l->D.P;
  d->Q = l->D.Q;
  return (d);
}
}
Demand *Compute_Branch(Branch br , double theta_R , double theta_I , double pi_R ,
                       double pi_I ) 
{ Demand *a2 ;
  Demand tmp ;
  double new_pi_R ;
  double new_pi_I ;
  double a ;
  double b ;
  double c ;
  double root ;
  Leaf l ;
  Branch next ;
  int i ;
  Demand *a1 ;
  double tmp___0 ;
  Demand *d ;
  void *tmp___1 ;
  Leaf *leaves;

  {
  validptr(&br->alpha);
  validptr(&br->beta);
  validptr(&br->X);
  validptr(&br->R);
  validptr(&br->next_branch);
  new_pi_R = pi_R + br->alpha * (theta_R + (theta_I * br->X) / br->R);
  new_pi_I = pi_I + br->beta * (theta_I + (theta_R * br->R) / br->X);
  next = br->next_branch;
  if ((unsigned int )next != (unsigned int )((Branch )0)) {
    a1 = Compute_Branch(next, theta_R, theta_I, new_pi_R, new_pi_I);
  }
  tmp.P = 0.0;
  tmp.Q = 0.0;
  br = br;
  i = 0;
  while (i < 10) {
    validptr(&br->leaves[i]);
    l = br->leaves[i];
    if (l == (Leaf)0) { DIVERGE: goto DIVERGE; } // pmr assume
    // pmr: Null ptr deref of a2 is ok here (was originally on the stack)
    a2 = Compute_Leaf(l, new_pi_R, new_pi_I);
    tmp.P += a2->P;
    tmp.Q += a2->Q;
    i ++;
  }
  validptr(&br->D.P);
  validptr(&br->D.Q);
  if ((unsigned int )next != (unsigned int )((Branch )0)) {
    // pmr: Null ptr deref of a1 is ok here (was originally on the stack)
    br->D.P = a1->P + tmp.P;
    br->D.Q = a1->Q + tmp.Q;
  } else {
    br->D.P = tmp.P;
    br->D.Q = tmp.Q;
  }
  a = br->R * br->R + br->X * br->X;
  b = ((((double )2.0 * br->R) * br->X) * br->D.Q - (((double )2.0 * br->X) * br->X) * br->D.P) - br->R;
  c = br->R * br->D.Q - br->X * br->D.P;
  c = c * c + br->R * br->D.P;
  tmp___0 = sqrt(b * b - ((double )4.0 * a) * c);
  root = (- b - tmp___0) / ((double )2.0 * a);
  br->D.Q += ((root - br->D.P) * br->X) / br->R;
  br->D.P = root;
  a = ((double )2.0 * br->R) * br->D.P;
  b = ((double )2.0 * br->X) * br->D.Q;
  br->alpha = a / (((double )1.0 - a) - b);
  br->beta = b / (((double )1.0 - a) - b);
  tmp___1 = malloc((int )sizeof(Demand ));
  d = (Demand *)tmp___1;
  d->P = br->D.P;
  d->Q = br->D.Q;
  return (d);
}
}
Demand *Compute_Leaf(Leaf l , double pi_R , double pi_I ) 
{ double P ;
  double Q ;
  Demand *d ;
  void *tmp ;

  {
  validptr(&l->D.P);
  validptr(&l->D.Q);
  P = l->D.P;
  Q = l->D.Q;
  optimize_node(pi_R, pi_I);
  if (P < 0.0) {
    P = 0.0;
    Q = 0.0;
  }
  l->D.P = P;
  l->D.Q = Q;
  tmp = malloc((int )sizeof(Demand ));
  d = (Demand *)tmp;
  d->P = l->D.P;
  d->Q = l->D.Q;
  return (d);
}
}
void optimize_node(double pi_R , double pi_I ) 
{ double g ;
  double h ;
  double grad_f[2] ;
  double grad_g[2] ;
  double grad_h[2] ;
  double dd_grad_f[2] ;
  double magnitude ;
  int i ;
  double total ;
  double max_dist ;
  double P ;
  double Q ;
  double tmp ;
  double tmp___0 ;
  double tmp___1 ;
  double tmp___2 ;
  double tmp___3 ;
  double tmp___4 ;
  double tmp___5 ;
  double tmp___6 ;

  {
  while (1) {
    h = find_h();
    tmp = fabs(h);
    if (tmp > 0.000001) {
      magnitude = find_gradient_h(grad_h);
      total = h / magnitude;
      validptr(&grad_h[0]);
      P -= total * grad_h[0];
      validptr(&grad_h[1]);
      Q -= total * grad_h[1];
    }
    g = find_g();
    if (g > 0.000001) {
      magnitude = find_gradient_g(grad_g);
      find_gradient_h(grad_h);
      tmp___0 = make_orthogonal(grad_g, grad_h);
      magnitude *= tmp___0;
      total = g / magnitude;
      validptr(&grad_g[0]);
      P -= total * grad_g[0];
      validptr(&grad_g[1]);
      Q -= total * grad_g[1];
    }
    magnitude = find_gradient_f(pi_R, pi_I, grad_f);
    find_dd_grad_f(pi_R, pi_I, dd_grad_f);
    total = 0.0;
    i = 0;
    while (i < 2) {
      validptr(&grad_f[i]);
      validptr(&dd_grad_f[i]);
      total += grad_f[i] * dd_grad_f[i];
      i ++;
    }
    tmp___1 = fabs(total);
    magnitude /= tmp___1;
    find_gradient_h(grad_h);
    tmp___2 = make_orthogonal(grad_f, grad_h);
    magnitude *= tmp___2;
    find_gradient_g(grad_g);
    total = 0.0;
    i = 0;
    while (i < 2) {
      validptr(&grad_f[i]);
      validptr(&grad_g[i]);
      total += grad_f[i] * grad_g[i];
      i ++;
    }
    if (total > (double )0.0) {
      tmp___3 = find_g();
      max_dist = - tmp___3 / total;
      if (magnitude > max_dist) {
        magnitude = max_dist;
      }
    }
    validptr(&grad_f[0]);
    P += magnitude * grad_f[0];
    validptr(&grad_f[1]);
    Q += magnitude * grad_f[1];
    h = find_h();
    g = find_g();
    find_gradient_f(pi_R, pi_I, grad_f);
    find_gradient_h(grad_h);
    tmp___4 = fabs(h);
    if ((tmp___4 > 0.000001) != 0) {
      if ((g > 0.000001) != 0) {
        tmp___5 = fabs(g);
        if (tmp___5 > 0.000001) {
          validptr(&grad_f[0]);
          validptr(&grad_h[1]);
          validptr(&grad_f[1]);
          validptr(&grad_h[0]);
          tmp___6 = fabs(grad_f[0] * grad_h[1] - grad_f[1] * grad_h[0]);
          if ((tmp___6 > 0.000001) != 0) {
            break;
          }
        } else {
          break;
        }
      }
    }
  }
  return;
}
}
double find_g(void) 
{ double P ;
  double Q ;

  {
  return ((P * P + Q * Q) - 0.8);
}
}
double find_h(void) 
{ double P ;
  double Q ;

  {
  return (P - (double )5.0 * Q);
}
}
double find_gradient_f(double pi_R , double pi_I , double *gradient ) 
{ int i ;
  double magnitude ;
  double P ;
  double Q ;

  {
  magnitude = 0.0;
  validptr(gradient);
  *(gradient + 0) = (double )1.0 / ((double )1.0 + P) - pi_R;
  validptr(gradient + 1);
  *(gradient + 1) = (double )1.0 / ((double )1.0 + Q) - pi_I;
  i = 0;
  while (i < 2) {
    validptr(gradient + i);
    magnitude += *(gradient + i) * *(gradient + i);
    i ++;
  }
  magnitude = sqrt(magnitude);
  i = 0;
  while (i < 2) {
    validptr(gradient + i);
    *(gradient + i) /= magnitude;
    i ++;
  }
  return (magnitude);
}
}
double find_gradient_g(double * ARRAY gradient ) 
{ int i ;
  double magnitude ;
  double P ;
  double Q ;

  {
  magnitude = 0.0;
  validptr(gradient + 0);
  *(gradient + 0) = (double )2.0 * P;
  validptr(gradient + 1);
  *(gradient + 1) = (double )2.0 * Q;
  i = 0;
  while (i < 2) {
    validptr(gradient + i);
    magnitude += *(gradient + i) * *(gradient + i);
    i ++;
  }
  magnitude = sqrt(magnitude);
  i = 0;
  while (i < 2) {
    validptr(gradient + i);
    *(gradient + i) /= magnitude;
    i ++;
  }
  return (magnitude);
}
}
double find_gradient_h(double *gradient ) 
{ int i ;
  double magnitude ;

  {
  magnitude = 0.0;
  validptr(gradient + 0);
  *(gradient + 0) = 1.0;
  validptr(gradient + 1);
  *(gradient + 1) = - 5.0;
  i = 0;
  while (i < 2) {
    validptr(gradient + i);
    magnitude += *(gradient + i) * *(gradient + i);
    i ++;
  }
  magnitude = sqrt(magnitude);
  i = 0;
  while (i < 2) {
    validptr(gradient + i);
    *(gradient + i) /= magnitude;
    i ++;
  }
  return (magnitude);
}
}
void find_dd_grad_f(double pi_R , double pi_I , double *dd_grad ) 
{ double P ;
  double Q ;
  double P_plus_1_inv ;
  double Q_plus_1_inv ;
  double P_grad_term ;
  double Q_grad_term ;
  double grad_mag ;

  {
  P_plus_1_inv = (double )1.0;
  Q_plus_1_inv = (double )1.0;
  P_grad_term = P_plus_1_inv - pi_R;
  Q_grad_term = Q_plus_1_inv - pi_I;
  grad_mag = sqrt(P_grad_term * P_grad_term + Q_grad_term * Q_grad_term);
  validptr(dd_grad + 0);
  *(dd_grad + 0) = ((- P_plus_1_inv * P_plus_1_inv) * P_grad_term) / grad_mag;
  validptr(dd_grad + 1);
  *(dd_grad + 1) = ((- Q_plus_1_inv * Q_plus_1_inv) * Q_grad_term) / grad_mag;
  return;
}
}
double make_orthogonal(double * ARRAY v_mod , double * ARRAY v_static ) 
{ int i ;
  double total ;
  double length ;
  double tmp ;

  {
  total = 0.0;
  length = 0.0;
  i = 0;
  while (i < 2) {
    validptr(v_mod + i);
    validptr(v_static + i);
    total += *(v_mod + i) * *(v_static + i);
    i ++;
  }
  i = 0;
  while (i < 2) {
    validptr(v_mod + i);
    validptr(v_static + i);
    *(v_mod + i) -= total * *(v_static + i);
    length += *(v_mod + i) * *(v_mod + i);
    i ++;
  }
  length = sqrt(length);
  i = 0;
  while (i < 2) {
    validptr(v_mod + i);
    *(v_mod + i) /= length;
    i ++;
  }
  if ((double )1.0 - total * total < (double )0.0) {
    return ((double )0.0);
  }
  tmp = sqrt((double )1.0 - total * total);
  return (tmp);
}
}
#pragma merger(0,"/tmp/cil-mKPzb7ty.i","")
Root build_tree(void) ;
Lateral build_lateral(int i , int num ) ;
Branch build_branch(int i , int j , int num ) ;
Leaf build_leaf(void) ;
Root build_tree(void) 
{ register int i ;
  register Root t ;
  register Lateral l ;
  void *tmp ;

  {
  tmp = malloc((int )sizeof(*t));
  t = (struct root *)tmp;
  i = 0;
  while (i < 10) {
    l = build_lateral(i * 20, 20);
    validptr(&t->feeders[i]);
    t->feeders[i] = l;
    i ++;
  }

  validptr(&t->theta_R);
  validptr(&t->theta_I);
  t->theta_R = 0.8;
  t->theta_I = 0.16;
  return (t);
}
}
Lateral build_lateral(int i , int num ) 
{ register Lateral l ;
  register Branch b ;
  register Lateral next ;
  void *tmp ;

  {
  if (num == 0) {
    return ((struct lateral *)0);
  }
  // pmr: Moved from below l = ... to work around location unfolding (next and tmp are same absloc!)
  next = build_lateral(i, num - 1);
  tmp = malloc((int )sizeof(*l));
  l = (struct lateral *)tmp;
  b = build_branch(i * 5, (num - 1) * 5, 5);
  l->next_lateral = next;
  l->branch = b;
  // pmr: investigate why the next line can't come before the assn to l->branch
  validptr(&l->branch);
  validptr(&l->R);
  validptr(&l->X);
  validptr(&l->alpha);
  validptr(&l->beta);
  l->R = (double )1.0 / 300000.0;
  l->X = 0.000001;
  l->alpha = 0.0;
  l->beta = 0.0;
  validptr(&l->next_lateral);
  return (l);
}
}
Branch build_branch(int i , int j , int num ) 
{ register Leaf l ;
  register Branch b ;
  void *tmp ;

  {
  if (num == 0) {
    return ((struct branch *)0);
  }
  tmp = malloc((int )sizeof(*b));
  b = (struct branch *)tmp;
  validptr(&b->next_branch);
  b->next_branch = build_branch(i, j, num - 1);
  i = 0;
  while (i < 10) {
    l = build_leaf();
    validptr(&b->leaves[i]);
    b->leaves[i] = l;
    i ++;
  }
  validptr(&b->R);
  validptr(&b->X);
  validptr(&b->alpha);
  validptr(&b->beta);
  b->R = 0.0001;
  b->X = 0.00002;
  b->alpha = 0.0;
  b->beta = 0.0;
  return (b);
}
}
Leaf build_leaf(void) 
{ register Leaf l ;
  void *tmp ;

  {
  tmp = malloc((int )sizeof(*l));
  l = (struct leaf *)tmp;
  validptr(&l->D.P);
  validptr(&l->D.Q);
  l->D.P = 1.0;
  l->D.Q = 1.0;
  return (l);
}
}
#pragma merger(0,"/tmp/cil-1NGF8NKb.i","")
int
main (int argc, char * ARRAY VALIDPTR * START NONNULL ARRAY SIZE(argc * 4) argv)
  CHECK_TYPE
{ double *map_P /* [36] */ ;
  double *map_Q /* [36] */ ;
  double wallclock___0 ;
  Root r ;
  int i ;
  int finished ;
  double d_theta_R ;
  double d_theta_I ;
  clock_t tmp ;
  double tmp___0 ;
  double tmp___1 ;
  clock_t tmp___2 ;

  {
  // pmr: HACK
  map_P = (double *)malloc(sizeof(double) * 36);
  map_Q = (double *)malloc(sizeof(double) * 36);
  for (int pmr = 0; pmr < 36; pmr++) {
      map_P[pmr] = 0.0;
      map_Q[pmr] = 0.0;
  }
  /*      
  map_P[0] = 8752.218091048;
  map_P[1] = 8446.106670416;
  map_P[2] = 8107.990680283;
  map_P[3] = 7776.191574285;
  map_P[4] = 7455.920518777;
  map_P[5] = 7146.602181352;
  map_P[6] = 6847.709026813;
  map_P[7] = 6558.734204024;
  map_P[8] = 6279.213382291;
  map_P[9] = 6008.702199986;
  map_P[10] = 5746.786181029;
  map_P[11] = 5493.078256495;
  map_P[12] = 5247.206333097;
  map_P[13] = 5008.828069358;
  map_P[14] = 4777.615815166;
  map_P[15] = 4553.258735900;
  map_P[16] = 4335.470002316;
  map_P[17] = 4123.971545694;
  map_P[18] = 3918.501939675;
  map_P[19] = 3718.817618538;
  map_P[20] = 3524.683625800;
  map_P[21] = 3335.876573044;
  map_P[22] = 3152.188635673;
  map_P[23] = 2973.421417103;
  map_P[24] = 2799.382330486;
  map_P[25] = 2629.892542617;
  map_P[26] = 2464.782829705;
  map_P[27] = 2303.889031418;
  map_P[28] = 2147.054385395;
  map_P[29] = 1994.132771399;
  map_P[30] = 1844.985347313;
  map_P[31] = 1699.475053321;
  map_P[32] = 1557.474019598;
  map_P[33] = 1418.860479043;
  map_P[34] = 1283.520126656;
  map_P[35] = 1151.338004216;
  map_Q[0] = 1768.846590190;
  map_Q[1] = 1706.229490046;
  map_Q[2] = 1637.253873079;
  map_Q[3] = 1569.637451623;
  map_Q[4] = 1504.419525242;
  map_Q[5] = 1441.477913810;
  map_Q[6] = 1380.700660446;
  map_Q[7] = 1321.980440476;
  map_Q[8] = 1265.218982201;
  map_Q[9] = 1210.322424636;
  map_Q[10] = 1157.203306183;
  map_Q[11] = 1105.780028163;
  map_Q[12] = 1055.974296746;
  map_Q[13] = 1007.714103979;
  map_Q[14] = 960.930643875;
  map_Q[15] = 915.558722782;
  map_Q[16] = 871.538200178;
  map_Q[17] = 828.810882006;
  map_Q[18] = 787.322098340;
  map_Q[19] = 747.020941334;
  map_Q[20] = 707.858376214;
  map_Q[21] = 669.787829741;
  map_Q[22] = 632.765987756;
  map_Q[23] = 596.751545633;
  map_Q[24] = 561.704466609;
  map_Q[25] = 527.587580585;
  map_Q[26] = 494.365739051;
  map_Q[27] = 462.004890691;
  map_Q[28] = 430.472546686;
  map_Q[29] = 399.738429196;
  map_Q[30] = 369.773787595;
  map_Q[31] = 340.550287137;
  map_Q[32] = 312.041496095;
  map_Q[33] = 284.222260660;
  map_Q[34] = 257.068973074;
  map_Q[35] = 230.557938283;
  */
  finished = 0;
  //  printf((char *)"Past initialization\n");
  tmp = clock();
  wallclock___0 = (double )tmp;
  r = build_tree();
  //  printf((char *)"Built tree\n");
  Compute_Tree(r);
  validptr(&r->last.P);
  validptr(&r->last.Q);
  validptr(&r->D.P);
  validptr(&r->D.Q);
  validptr(&r->last_theta_R);
  validptr(&r->last_theta_I);
  validptr(&r->theta_R);
  validptr(&r->theta_I);
  r->last.P = r->D.P;
  r->last.Q = r->D.Q;
  r->last_theta_R = r->theta_R;
  r->last_theta_I = r->theta_I;
  r->theta_R = 0.7;
  r->theta_I = 0.14;
  while (finished != 1) {
    Compute_Tree(r);
    /*    printf((char *)"TR=%13.9f, TI=%13.9f, P0=%13.9f, Q0=%13.9f\n", r->theta_R, r->theta_I,
          r->D.P, r->D.Q); */
    tmp___0 = fabs(r->D.P / 10000.0 - r->theta_R);
    if (tmp___0 < 0.00001) {
      tmp___1 = fabs(r->D.Q / 10000.0 - r->theta_I);
      if (tmp___1 < 0.00001) {
        finished = 1;
      } else {
        goto _L;
      }
    } else {
      _L: /* CIL Label */ 
      i = (int )((r->theta_R - 0.65) / 0.01);
      if (i < 0) {
        i = 0;
      }
      if (i > 35) {
        i = 35;
      }
      // validptr(&map_P[i + 1]); pmr: Original code accesses OOB here!
      validptr(&map_P[i]);
      d_theta_R = - (r->theta_R - r->D.P / 10000.0) / ((double )1.0 - (map_P[i + 1] - map_P[i]) / (0.01 * 10000.0));
      i = (int )((r->theta_I - 0.13) / 0.002);
      if (i < 0) {
        i = 0;
      }
      if (i > 35) {
        i = 35;
      }
      // validptr(&map_Q[i + 1]); pmr: Original code accesses OOB here!
      validptr(&map_Q[i]);
      d_theta_I = - (r->theta_I - r->D.Q / 10000.0) / ((double )1.0 - (map_Q[i + 1] - map_Q[i]) / (0.002 * 10000.0));
      //      printf((char *)"D TR-%13.9f, TI=%13.9f\n", d_theta_R, d_theta_I);
      r->last.P = r->D.P;
      r->last.Q = r->D.Q;
      r->last_theta_R = r->theta_R;
      r->last_theta_I = r->theta_I;
      r->theta_R += d_theta_R;
      r->theta_I += d_theta_I;
    }
  }
  tmp___2 = clock();
  //  wallclock___0 = (1000000.0 * ((double )tmp___2 - wallclock___0)) / (double )1000.0;
  //  printf((char *)"Elapsed time %f\n", wallclock___0 / 1000.0);
  return (0);
}
}
