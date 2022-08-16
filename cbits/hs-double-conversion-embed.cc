#include "double-conversion.h"
#include "hs-double-conversion.h"
#include <stdio.h>

using namespace double_conversion;

static const int kToShortestLength = 26;

extern "C"
int _hs_ToShortestLength(void)
{
  return kToShortestLength;
}

static const int kToFixedLength =
  1 + DoubleToStringConverter::kMaxFixedDigitsBeforePoint +
  1 + DoubleToStringConverter::kMaxFixedDigitsAfterPoint;

extern "C"
int _hs_ToFixedLength(void)
{
  return kToFixedLength;
}

static const int kToExponentialLength =
  DoubleToStringConverter::kMaxExponentialDigits + 8;

extern "C"
int _hs_ToExponentialLength(void)
{
  return kToExponentialLength;
}

static const int kToPrecisionLength =
  DoubleToStringConverter::kMaxPrecisionDigits + 7;

extern "C"
int _hs_ToPrecisionLength(void)
{
  return kToPrecisionLength;
}

static int copy(uint16_t *buf, const StringBuilder& builder, const char *cbuf)
{
  const int pos = builder.position();
  for (int i = 0; i < pos; i++)
    buf[i] = cbuf[i];
  return pos;
}

static int copy(uint16_t *buf, const char *cbuf, const int len)
{
  for (int i = 0; i < len; i++)
    buf[i] = cbuf[i];
  return len;
}

static inline const DoubleToStringConverter& defaultConverter(void)
{
  const int flags = DoubleToStringConverter::UNIQUE_ZERO;
  static DoubleToStringConverter converter(flags,
                                           "Infinity",
                                           "NaN",
                                           'e',
                                           -6, 21,
                                           6, 0);
  return converter;
}

static inline const DoubleToStringConverter& floatConverter(void)
{
  const int flags = DoubleToStringConverter::UNIQUE_ZERO;
  static DoubleToStringConverter converter(flags,
                                           "Infinity",
                                           "NaN",
                                           'e',
                                           -6, 6,
                                           6, 0);
  return converter;
}

extern "C"
int _hs_ToShortest(double value, char *buf)
{
  StringBuilder builder(buf, kToShortestLength);
  return defaultConverter().ToShortest(value, &builder)
    ? builder.position() : -1;
}

extern "C"
int _hs_ToShortestFloat(float value, char *buf)
{
  StringBuilder builder(buf, kToShortestLength);
  return floatConverter().ToShortestSingle(value, &builder)
    ? builder.position() : -1;
}

extern "C"
int _hs_Text_ToShortest(double value, uint16_t *buf)
{
  char cbuf[kToShortestLength];
  return copy(buf, cbuf, _hs_ToShortest(value, cbuf));
}

extern "C"
int _hs_Text_ToShortestFloat(float value, uint16_t *buf)
{
  char cbuf[kToShortestLength];
  return copy(buf, cbuf, _hs_ToShortestFloat(value, cbuf));
}

extern "C"
int _hs_ToFixed(double value, char *buf, const int ndigits)
{
  StringBuilder builder(buf, kToFixedLength);
  return defaultConverter().ToFixed(value, ndigits, &builder)
    ? builder.position() : -1;
}

int _hs_ToFixedFloat(float value, char *buf, const int ndigits)
{
  StringBuilder builder(buf, kToFixedLength);
  return floatConverter().ToFixed(value, ndigits, &builder)
    ? builder.position() : -1;
}

extern "C"
int _hs_Text_ToFixed(double value, uint16_t *buf, const int ndigits)
{
  char cbuf[kToFixedLength];
  return copy(buf, cbuf, _hs_ToFixed(value, cbuf, ndigits));
}

extern "C"
int _hs_Text_ToFixedFloat(float value, uint16_t *buf, const int ndigits)
{
  char cbuf[kToFixedLength];
  return copy(buf, cbuf, _hs_ToFixedFloat(value, cbuf, ndigits));
}

extern "C"
int _hs_ToExponential(double value, char *buf, const int ndigits)
{
  StringBuilder builder(buf, kToExponentialLength);
  return defaultConverter().ToExponential(value, ndigits, &builder)
    ? builder.position() : -1;
}

extern "C"
int _hs_ToExponentialFloat(float value, char *buf, const int ndigits)
{
  StringBuilder builder(buf, kToExponentialLength);
  return floatConverter().ToExponential(value, ndigits, &builder)
    ? builder.position() : -1;
}

extern "C"
int _hs_Text_ToExponential(double value, uint16_t *buf, const int ndigits)
{
  char cbuf[kToExponentialLength];
  return copy(buf, cbuf, _hs_ToExponential(value, cbuf, ndigits));
}

extern "C"
int _hs_Text_ToExponentialFloat(float value, uint16_t *buf, const int ndigits)
{
  char cbuf[kToExponentialLength];
  return copy(buf, cbuf, _hs_ToExponentialFloat(value, cbuf, ndigits));
}

extern "C"
int _hs_ToPrecision(double value, char *buf, const int precision)
{
  StringBuilder builder(buf, kToPrecisionLength);
  return defaultConverter().ToPrecision(value, precision, &builder)
    ? builder.position() : -1;
}

extern "C"
int _hs_ToPrecisionFloat(float value, char *buf, const int precision)
{
  StringBuilder builder(buf, kToPrecisionLength);
  return floatConverter().ToPrecision(value, precision, &builder)
    ? builder.position() : -1;
}

extern "C"
int _hs_Text_ToPrecision(double value, uint16_t *buf, const int precision)
{
  char cbuf[kToPrecisionLength];
  return copy(buf, cbuf, _hs_ToPrecision(value, cbuf, precision));
}

extern "C"
int _hs_Text_ToPrecisionFloat(float value, uint16_t *buf, const int precision)
{
  char cbuf[kToPrecisionLength];
  return copy(buf, cbuf, _hs_ToPrecisionFloat(value, cbuf, precision));
}