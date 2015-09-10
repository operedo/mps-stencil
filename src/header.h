#ifndef MODE
#ifdef BASELINE
#define MODE 0
#elif defined(STENCIL) && (STENCIL == 0)
#define MODE 1
#elif defined(STENCIL) && (STENCIL == 125)
#define MODE 125
#ifndef REGSIZE
#if defined(REGISTER) && (REGISTER == 8)
#define REGSIZE 8
#elif defined(REGISTER) && (REGISTER == 16)
#define REGSIZE 16
#else
#define REGSIZE 32
#endif
#endif
#endif
#endif

#ifdef COMPRESSED
#define CHUNK 9
#endif
