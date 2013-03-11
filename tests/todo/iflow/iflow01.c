#include <csolve.h>
#include <stdlib.h>

void assert_same_scl_tags(int FINAL REF(TAGS([VVADDR]) = TAGS([y])) x,
                          int FINAL REF(TAGS([VVADDR]) = TAGS([x])) y);

void assert_same_ptr_tags(int FINAL * REF(TAGS([V]) = TAGS([y])) x,
                          int FINAL * REF(TAGS([V]) = TAGS([x])) y);

void test01(int *x) CHECK_TYPE
{
  int *y;
  y = x; //Tags of &y and &x should be equal 

  assert_same_scl_tags(x,y);
}

void test02(int *y, int *x) CHECK_TYPE
{
  *y = *x; //Tags of y and x should be equal

  assert_same_ptr_tags(x,y);
}

void test03(int * LOC(L) y, int * LOC(L) x) CHECK_TYPE
{
  *y = *x; //Tags of y and x should be equal?

  assert_same_ptr_tags(x,y);
}

void test03a(int * LOC(L) * y, int * LOC(L) * x) CHECK_TYPE
{
  *x = nondet_tainted(); //both y and x should reflect that they point to tainted data

  if (x == y) {
    assert_same_ptr_tags(x, y);
  }
}

/* int test03b(int * LOC(L) * x, int * LOC(L) * y, int v) CHECK_TYPE */
/* { */
/*   *x = v; */
/*   return *y; //If v was tainted, then this return value should be as well. */
/* } */

void test04(int g, int *x, int * START VALIDPTR ROOM_FOR(int)
                                 REF(?Set_emp([TAGSET([VVADDR])]) y) CHECK_TYPE
{
  if (g) {
    x = y; 
  }

  assert_same_scl_tags(g, x);

  //Tags of &x should be the union of &y's and &g's
}

void test04(int g, int *x, int *y) CHECK_TYPE
{
  if (g) {
    *x = *y; 
  }
  //Tags of x should be the union of y's and &g's
}

void test05(int *x) CHECK_TYPE
{
  int y;

  y = *x; //tags(&y) = tags(x)
}

int foo(int *x)
{
  int y = *x;
  return y;
}  
  


