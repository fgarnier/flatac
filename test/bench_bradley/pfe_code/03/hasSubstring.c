#include <assert.h>

// Write hasSubstring here.

int main() {
  assert(hasSubstring("Hello universe!", "lo"));
  assert(hasSubstring("Hello universe!", "verse"));
  assert(hasSubstring("Hello universe!", ""));
  assert(hasSubstring("", ""));
  assert(!hasSubstring("Hello universe!", "verses"));
  assert(!hasSubstring("Hello universe!", "loun"));
  assert(!hasSubstring("Hello universe!", "erse!!"));
  return 0;
}
