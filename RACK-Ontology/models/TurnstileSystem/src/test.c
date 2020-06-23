#include "test.h"
#include <stdbool.h>

int utc_1_1(void) {
  printf("UTC-1-1: PASS\n");
  return false;
}

int utc_1_2(void) {
  printf("UTC-1-2: PASS\n");
  return false;
}

int utc_1_3(void) {
  printf("UTC-1-3: PASS\n");
  return false;
}

int utc_1_4(void) {
  printf("UTC-1-4: FAIL\n");
  return true;
}

int main(void) {
  bool failed = false;
  failed |= utc_1_1();
  failed |= utc_1_2();
  failed |= utc_1_3();
  failed |= utc_1_4();
  return failed;
}
