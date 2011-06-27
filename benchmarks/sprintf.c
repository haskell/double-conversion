#include <stdio.h>

void sprintf_exact(double x)
{
  char buf[64];
  snprintf(buf, 64, "%a", x);
}

void sprintf_exponential(int d, double x)
{
  char buf[64];
  snprintf(buf, 64, "%*e", d, x);
}

void sprintf_fixed(int d, double x)
{
  char buf[64];
  snprintf(buf, 64, "%*f", d, x);
}

void sprintf_generic(int d, double x)
{
  char buf[64];
  snprintf(buf, 64, "%*g", d, x);
}

void sprintf_generic_default(double x)
{
  char buf[64];
  snprintf(buf, 64, "%g", x);
}
