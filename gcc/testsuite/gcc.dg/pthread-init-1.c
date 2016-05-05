/* Ensure we get clean warning results when using the pthread
 * initialization macros.
 *
 * Origin: Kaveh Ghazi (ghazi@caip.rutgers.edu) 9/27/2006.
 */

/* { dg-do compile } */
/* { dg-options "-Wextra -Wall" } */
/* { dg-skip-if "Incompatible system pthread.h" { sx*-nec-superux } { "*" } { "" } } */

#include "pthread-init-common.h"

