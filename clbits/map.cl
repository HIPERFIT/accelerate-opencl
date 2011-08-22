/* -----------------------------------------------------------------------------
 *
 * Kernel      : Map
 * Copyright   : [2011] Martin Dybdal
 * License     : BSD3
 *
 * Maintainer  : Martin Dybdal <dybber@dybber.dk>
 * Stability   : experimental
 *
 * Apply the function to each element of the array. Each thread processes
 * multiple elements, striding the array by the grid size.
 *
 * ---------------------------------------------------------------------------*/

__kernel void
map
(
     __global TyOut* d_out,
     __global const TyIn0* d_in0,
     const Ix shape
)
{
    Ix idx;
    const Ix gridSize = get_global_size(0);

    for(idx = get_global_id(0); idx < shape; idx += gridSize) {
      set(d_out, idx, apply(get0(d_in0, idx)));
    }
};
