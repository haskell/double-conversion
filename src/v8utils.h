// Copyright 2010 the V8 project authors. All rights reserved.
// Redistribution and use in source and binary forms, with or without
// modification, are permitted provided that the following conditions are
// met:
//
//     * Redistributions of source code must retain the above copyright
//       notice, this list of conditions and the following disclaimer.
//     * Redistributions in binary form must reproduce the above
//       copyright notice, this list of conditions and the following
//       disclaimer in the documentation and/or other materials provided
//       with the distribution.
//     * Neither the name of Google Inc. nor the names of its
//       contributors may be used to endorse or promote products derived
//       from this software without specific prior written permission.
//
// THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
// "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
// LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
// A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
// OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
// SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
// LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
// DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
// THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
// (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
// OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

#ifndef V8_V8UTILS_H_
#define V8_V8UTILS_H_

#include "utils.h"

namespace v8 {
namespace internal {

// Helper class for building result strings in a character buffer. The
// purpose of the class is to use safe operations that checks the
// buffer bounds on all operations in debug mode.
class StringBuilder {
 public:
  StringBuilder(char* buffer, int size)
      : buffer_(buffer, size), position_(0) { }

  ~StringBuilder() { if (!is_finalized()) Finalize(); }

  int size() const { return buffer_.length(); }

  // Get the current position in the builder.
  int position() const {
    ASSERT(!is_finalized());
    return position_;
  }

  // Reset the position.
  void Reset() { position_ = 0; }

  // Add a single character to the builder. It is not allowed to add
  // 0-characters; use the Finalize() method to terminate the string
  // instead.
  void AddCharacter(char c) {
    ASSERT(c != '\0');
    ASSERT(!is_finalized() && position_ < buffer_.length());
    buffer_[position_++] = c;
  }

  // Add an entire string to the builder. Uses strlen() internally to
  // compute the length of the input string.
  void AddString(const char* s);

  // Add the first 'n' characters of the given string 's' to the
  // builder. The input string must have enough characters.
  void AddSubstring(const char* s, int n);

  // Add character padding to the builder. If count is non-positive,
  // nothing is added to the builder.
  void AddPadding(char c, int count);

  // Finalize the string by 0-terminating it and returning the buffer.
  char* Finalize();

 private:
  Vector<char> buffer_;
  int position_;

  bool is_finalized() const { return position_ < 0; }

  DISALLOW_IMPLICIT_CONSTRUCTORS(StringBuilder);
};

} }  // namespace v8::internal

#endif  // V8_V8UTILS_H_
