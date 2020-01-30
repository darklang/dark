#include <gmp.h>
#ifndef __GMP_H__
#error "No GMP header"
#endif
#if __GNU_MP_VERSION < 5
#error "GMP >= 5 is required to support mpz_powm_sec"
#endif

void test(void) {
	__gmp_init();
}
