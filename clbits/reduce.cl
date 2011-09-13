/* -----------------------------------------------------------------------------
 *
 * Kernel      : Reduce
 * Copyright   : [2008..2011] Manuel M T Chakravarty, Gabriele Keller, Sean Lee, Trevor L. McDonell
 * License     : BSD3
 *
 * Maintainer  : Trevor L. McDonell <tmcdonell@cse.unsw.edu.au>
 * Stability   : experimental
 *
 * ---------------------------------------------------------------------------*/

#ifndef __REDUCE__
#define __REDUCE__


/*
 * Cooperatively reduce a single warp's segment of an array to a single value
 */
inline TyOut
reduce_warp_n
(
    __local TyOut   *s_data,
    TyOut                sum,
    Ix                   n
)
{
  int warpSize = 32;
    const Ix tid  = get_local_id(0);
    const Ix lane = get_local_id(0) & (warpSize - 1);

    if (n > 16 && lane + 16 < n) { sum = apply(sum, getA_local(tid+16, s_data)); set_local(tid, sum, s_data); }
    if (n >  8 && lane +  8 < n) { sum = apply(sum, getA_local(tid+ 8, s_data)); set_local(tid, sum, s_data); }
    if (n >  4 && lane +  4 < n) { sum = apply(sum, getA_local(tid+ 4, s_data)); set_local(tid, sum, s_data); }
    if (n >  2 && lane +  2 < n) { sum = apply(sum, getA_local(tid+ 2, s_data)); set_local(tid, sum, s_data); }
    if (n >  1 && lane +  1 < n) { sum = apply(sum, getA_local(tid+ 1, s_data)); }

    return sum;
}

/*
 * Block reduction to a single value
 */
inline TyOut
reduce_block_n
(
    __local TyOut      *s_data,
    TyOut               sum,
    Ix                  n
)
{
    const Ix tid = get_local_id(0);

    if (n > 512) { if (tid < 512 && tid + 512 < n) { sum = apply(sum, getA_local(tid+512, s_data)); set_local(tid, sum, s_data); } }
    barrier(CLK_LOCAL_MEM_FENCE);
    if (n > 256) { if (tid < 256 && tid + 256 < n) { sum = apply(sum, getA_local(tid+256, s_data)); set_local(tid, sum, s_data); } }
    barrier(CLK_LOCAL_MEM_FENCE);
    if (n > 128) { if (tid < 128 && tid + 128 < n) { sum = apply(sum, getA_local(tid+128, s_data)); set_local(tid, sum, s_data); } }
    barrier(CLK_LOCAL_MEM_FENCE);
    if (n >  64) { if (tid <  64 && tid +  64 < n) { sum = apply(sum, getA_local(tid+ 64, s_data)); set_local(tid, sum, s_data); } }
    barrier(CLK_LOCAL_MEM_FENCE);
    if (n >  32) { if (tid <  32 && tid +  32 < n) { sum = apply(sum, getA_local(tid+ 32, s_data)); set_local(tid, sum, s_data); }}
    barrier(CLK_LOCAL_MEM_FENCE);
    if (n >  16) { if (tid <  16 && tid +  16 < n) { sum = apply(sum, getA_local(tid+ 16, s_data)); set_local(tid, sum, s_data); }}
    barrier(CLK_LOCAL_MEM_FENCE);
    if (n >  8) { if (tid <  8 && tid +     8 < n) { sum = apply(sum, getA_local(tid+  8, s_data)); set_local(tid, sum, s_data); }}
    barrier(CLK_LOCAL_MEM_FENCE);
    if (n >  4) { if (tid <  4 && tid +     4 < n) { sum = apply(sum, getA_local(tid+  4, s_data)); set_local(tid, sum, s_data); }}
    barrier(CLK_LOCAL_MEM_FENCE);
    if (n >  2) { if (tid <  2 && tid +     2 < n) { sum = apply(sum, getA_local(tid+  2, s_data)); set_local(tid, sum, s_data); }}
    barrier(CLK_LOCAL_MEM_FENCE);
    if (n >  1) { if (tid == 0 && tid +     1 < n) { sum = apply(sum, getA_local(tid+  1, s_data)); }}

    return sum;
}

#endif

