#ifndef __ACCELERATE_OPENCL_SHAPE_H__
#define __ACCELERATE_OPENCL_SHAPE_H__

typedef int                                      Ix;
typedef void*                                     DIM0;
typedef Ix                                        DIM1;
typedef struct { Ix a1,a0; }                      DIM2;
typedef struct { Ix a2,a1,a0; }                   DIM3;

inline uint sizeDIM0(const DIM0 ix) {
  return 1;
}

inline uint sizeDIM1(const DIM1 ix) {
  return ix;
}

inline uint sizeDIM2(const DIM2 ix) {
  return ix.a0 * ix.a1;
}

inline uint sizeDIM3(const DIM3 ix) {
  return ix.a0 * ix.a1 * ix.a2;
}


inline uint dimDIM0(const DIM0 sh) {
  return 0;
}

inline uint dimDIM1(const DIM1 sh) {
  return 1;
}

inline uint dimDIM2(const DIM2 sh) {
  return 2;
}

inline uint dimDIM3(const DIM3 sh) {
  return 3;
}

inline DIM1 indexConsDIM0(const DIM0 sh, const Ix ix) {
  return ix;
}

inline DIM2 indexConsDIM1(const DIM1 sh, const Ix ix) {
  DIM2 v = { sh, ix };
  return v;
}

inline DIM3 indexConsDIM2(const DIM2 sh, const Ix ix) {
  DIM3 v = { sh.a1, sh.a0, ix };
  return v;
}

/* Head */
inline Ix indexHeadDIM0(const DIM0 ix) {
  return 0;
}

inline Ix indexHeadDIM1(const DIM1 ix) {
  return ix;
}

inline Ix indexHeadDIM2(const DIM2 ix) {
  return ix.a0;
}

inline Ix indexHeadDIM3(const DIM3 ix) {
  return ix.a0;
}

/* Tail */
inline DIM0 indexTailDIM1(const DIM1 ix) {
  return 0;
}

inline DIM1 indexTailDIM2(const DIM2 ix) {
  return ix.a1;
}

inline DIM2 indexTailDIM3(const DIM3 ix) {
  DIM2 sh = {ix.a2, ix.a1};
  return sh;
}

inline Ix toIndexDIM0(const DIM0 sh, const DIM0 ix) {
  return 0;
}

inline Ix toIndexDIM1(const DIM1 sh, const DIM1 ix) {
  return ix;
}

inline Ix toIndexDIM2(const DIM2 sh, const DIM2 ix) {
    return toIndexDIM1(indexTailDIM2(sh), indexTailDIM2(ix)) * indexHeadDIM2(sh)
             + indexHeadDIM2(ix);
}

inline Ix toIndexDIM3(const DIM3 sh, const DIM3 ix) {
    return toIndexDIM2(indexTailDIM3(sh), indexTailDIM3(ix)) * indexHeadDIM3(sh)
             + indexHeadDIM3(ix);
}

inline DIM0 fromIndexDIM0(const DIM0 sh, const Ix ix) {
    return 0;
}

inline DIM1 fromIndexDIM1(const DIM1 sh, const Ix ix) {
    return ix;
}

inline DIM2 fromIndexDIM2(const DIM2 sh, const Ix ix) {
    const Ix d = indexHeadDIM2(sh);
    return indexConsDIM1(fromIndexDIM1(indexTailDIM2(sh), ix / d), ix % d);
}


inline DIM3 fromIndexDIM3(const DIM3 sh, const Ix ix) {
    const Ix d = indexHeadDIM3(sh);
    return indexConsDIM2(fromIndexDIM2(indexTailDIM3(sh), ix / d), ix % d);
}

/*
 * Test for the magic index `ignore`
 */
inline int ignoreDIM0(const DIM0 ix) {
    return 1;
}

inline int ignoreDIM1(const DIM1 ix) {
    return indexHeadDIM1(ix) == -1 && ignoreDIM0(indexTailDIM1(ix));
}

inline int ignoreDIM2(const DIM2 ix) {
    return indexHeadDIM2(ix) == -1 && ignoreDIM1(indexTailDIM2(ix));
}

inline int ignoreDIM3(const DIM3 ix) {
    return indexHeadDIM3(ix) == -1 && ignoreDIM2(indexTailDIM3(ix));
}


#endif  // __ACCELERATE_OPENCL_SHAPE_H__
