/* 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;  File: convolve.h
;;;  Author: Simoncelli
;;;  Description: Header file for convolve.c
;;;  Creation Date:
;;;  ----------------------------------------------------------------
;;;    Object-Based Vision and Image Understanding System (OBVIUS),
;;;      Copyright 1988, Vision Science Group,  Media Laboratory,  
;;;              Massachusetts Institute of Technology.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
*/

#if !defined(DEPUTY) && ! defined(COUNT)
   #define NONNULL
   #define NTS
   #define COUNT(n)
#endif

#define abs(x)	  (x>=0 ? x : -(x))
#define ROOT2 1.4142135623730951
#define FILTER 0
#define EXPAND 1
#define IS    ==
#define ISNT  !=
#define AND &&
#define OR ||

typedef  void (*fptr)(float * COUNT(x_dim*y_dim) NONNULL filt,
                     int x_dim, int y_dim, int x_pos, int y_pos,
                     float * COUNT(x_dim*y_dim) NONNULL result, int f_or_e);

typedef struct 
  {
  char * NTS NONNULL name;
  fptr func;
  } EDGE_HANDLER;

fptr edge_function(char * NTS NONNULL edges);

int internal_filter(float * COUNT(x_dim*y_dim) NONNULL image,
                    int x_dim, int y_dim,
                    float *  COUNT(x_fdim*y_fdim) NONNULL filt,
                    float *  COUNT(x_fdim*y_fdim) NONNULL temp,
                    int x_fdim, int y_fdim,
                    int xgrid_start, int xgrid_step, int ygrid_start,
                    int ygrid_step,
                    float * COUNT(x_dim*y_dim/2) NONNULL result,
                    char * NTS NONNULL edges);

int internal_expand(float *  COUNT(x_dim*y_dim) NONNULL image,
                    float *  COUNT(x_fdim*y_fdim) NONNULL filt,
                    float *  COUNT(x_fdim*y_fdim) NONNULL temp,
                    int x_fdim, int y_fdim, int xgrid_start, int xgrid_step,
                    int ygrid_start, int ygrid_step,
                    float * COUNT(x_dim*y_dim) NONNULL result,
                    int x_dim, int y_dim, char * NTS NONNULL edges);


