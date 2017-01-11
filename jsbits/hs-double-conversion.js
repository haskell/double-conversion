
var kToFixedLength =
  1 + /* DoubleToStringConverter::kMaxFixedDigitsBeforePoint*/ 60 +
  1 + /* DoubleToStringConverter::kMaxFixedDigitsAfterPoint*/ 60;


var kToShortestLength = 26;
var kToExponentialLength =  /*DoubleToStringConverter::kMaxExponentialDigits*/ 120 + 8;
var kToPrecisionLength =  /*DoubleToStringConverter::kMaxPrecisionDigits*/ 120 + 7;

function h$_hs_ToShortestLength() {
  return kToShortestLength;
}

function h$_hs_ToFixedLength()
{
  return kToFixedLength;
}

function h$_hs_ToExponentialLength()
{
  return kToExponentialLength;
}

function h$_hs_ToPrecisionLength()
{
  return kToPrecisionLength;
}

//--------------------

function h$_hs_Text_ToShortest(value, buf)
{
 var conv = value.toString().replace("e+","e");
 Array.prototype.map.call(conv,function(c,i){ buf.u1[i]=c.charCodeAt(0)});
 return buf.len = conv.length;
}

function h$_hs_ToShortest(value, buf)
{
 var conv = value.toString().replace("e+","e");
 Array.prototype.map.call(conv,function(c,i){ buf.u8[i]=c.charCodeAt(0)});
 return buf.len = conv.length;
}

//--------------------

function h$_hs_Text_ToFixed(value, buf, ndigits)
{
 // negative digits differ for small values
  if (ndigits > 60) return -1;
  if (ndigits > 0)
  {
    var conv = value.toFixed(Math.min(20,ndigits))+ "0".repeat(Math.max(0,ndigits - 20));
    Array.prototype.map.call(conv,function(c,i){ buf.u1[i]=c.charCodeAt(0)});
    return buf.len = conv.length;
  }
  else
  {
    var conv = value.toFixed(0).slice(0,ndigits) + "0".repeat(-ndigits);
    Array.prototype.map.call(conv,function(c,i){ buf.u1[i]=c.charCodeAt(0)});
    return buf.len = conv.length;
  }
}

function h$_hs_ToFixed(value, buf, unused, ndigits)
{
 // negative digits differ for small values
  if (ndigits > 60) return -1;
  if (ndigits > 0)
  {
    var conv = value.toFixed(Math.min(20,ndigits))+ "0".repeat(Math.max(0,ndigits - 20));
    Array.prototype.map.call(conv,function(c,i){ buf.u8[i]=c.charCodeAt(0)});
    return buf.len = conv.length;
  }
  else
  {
    var conv = value.toFixed(0).slice(0,ndigits) + "0".repeat(-ndigits);
    Array.prototype.map.call(conv,function(c,i){ buf.u8[i]=c.charCodeAt(0)});
    return buf.len = conv.length;
  }
}

//--------------------

function h$_hs_Text_ToExponential(value, buf, ndigits)
{

 if (ndigits > 120 || ndigits < -1) return -1;
 var conv = (ndigits == -1 ? value.toExponential() : value.toExponential(Math.min(20,ndigits)).replace("e",("0".repeat(Math.max(0,ndigits - 20))+"e"))).replace("e+","e");
 Array.prototype.map.call(conv,function(c,i){ buf.u1[i]=c.charCodeAt(0)});
 return buf.len = conv.length;
}

function h$_hs_ToExponential(value, buf, unused, ndigits)
{
 if (ndigits > 120 || ndigits < -1) return -1;
 var conv = (ndigits == -1 ? value.toExponential() : value.toExponential(Math.min(20,ndigits)).replace("e",("0".repeat(Math.max(0,ndigits - 20))+"e"))).replace("e+","e");
 Array.prototype.map.call(conv,function(c,i){ buf.u8[i]=c.charCodeAt(0)});
 return buf.len = conv.length;
}

//--------------------

function h$_hs_Text_ToPrecision(value, buf, ndigits)
{
  if (ndigits > 120 || ndigits < 1) return -1;
  var conv = (value.toPrecision(Math.min(21,ndigits))+ "0".repeat(Math.max(0,ndigits - 21))).replace("e+","e");
  Array.prototype.map.call(conv,function(c,i){ buf.u1[i]=c.charCodeAt(0)});
  return buf.len = conv.length;
}

function h$_hs_ToPrecision(value, buf, unused, ndigits)
{
  if (ndigits > 120 || ndigits < 1) return -1;
  var conv = (value.toPrecision(Math.min(21,ndigits))+ "0".repeat(Math.max(0,ndigits - 21))).replace("e+","e");
  Array.prototype.map.call(conv,function(c,i){ buf.u8[i]=c.charCodeAt(0)});
  return buf.len = conv.length;
}
