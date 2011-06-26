#include "double-conversion.h"
#include "hs-double-conversion.h"
#include <stdio.h>

using namespace double_conversion;

static const int kToShortestLength = 24;

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

extern "C"
int _hs_ToShortest(double value, uint16_t *buf)
{
  const DoubleToStringConverter& converter =
    DoubleToStringConverter::EcmaScriptConverter();
  char cbuf[kToShortestLength];

  StringBuilder builder(cbuf, kToShortestLength);
  bool ok = converter.ToShortest(value, &builder);

  if (!ok)
    return -1;

  return copy(buf, builder, cbuf);
}

extern "C"
int _hs_ToFixed(double value, uint16_t *buf, const int ndigits)
{
  const DoubleToStringConverter& converter =
    DoubleToStringConverter::EcmaScriptConverter();
  char cbuf[kToFixedLength];

  StringBuilder builder(cbuf, kToFixedLength);
  bool ok = converter.ToFixed(value, ndigits, &builder);

  if (!ok)
    return -1;
  
  return copy(buf, builder, cbuf);
}

extern "C"
int _hs_ToExponential(double value, uint16_t *buf, const int ndigits)
{
  const DoubleToStringConverter& converter =
    DoubleToStringConverter::EcmaScriptConverter();
  char cbuf[kToExponentialLength];

  StringBuilder builder(cbuf, kToExponentialLength);
  bool ok = converter.ToExponential(value, ndigits, &builder);

  if (!ok)
    return -1;
  
  return copy(buf, builder, cbuf);
}

extern "C"
int _hs_ToPrecision(double value, uint16_t *buf, const int precision)
{
  const DoubleToStringConverter& converter =
    DoubleToStringConverter::EcmaScriptConverter();
  char cbuf[kToPrecisionLength];

  StringBuilder builder(cbuf, kToPrecisionLength);
  bool ok = converter.ToPrecision(value, precision, &builder);

  if (!ok)
    return -1;
  
  return copy(buf, builder, cbuf);
}
