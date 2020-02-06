#ifndef GCC_ARM_PRIM
#define GCC_ARM_PRIM

#ifdef __cplusplus
extern "C" {
#endif


static inline void bkpt() {
  __asm volatile ("bkpt");
}

static inline void dsb() {
  __asm volatile ("dsb");
}

static inline void isb() {
  __asm volatile ("isb");
}

static inline void sev() {
  __asm volatile ("sev");
}

static inline void wfi() {
  __asm volatile ("wfi");
}

static inline void wfe() {
  __asm volatile ("wfe");
}

#ifdef __cplusplus
}
#endif

#endif
