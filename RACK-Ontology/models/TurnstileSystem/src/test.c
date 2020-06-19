#include "test.h"
#include <stdbool.h>

int tc_1_1(void) {
  printf("TC-1-1: PASS\n");
  return false;
}

int tc_1_2(void) {
  printf("TC-1-2: PASS\n");
  return false;
}

int tc_1_3(void) {
  printf("TC-1-3: PASS\n");
  return false;
}

int tc_1_4(void) {
  printf("TC-1-4: FAIL\n");
  return true;
}

int main(void) {
  bool failed = false;
  failed |= tc_1_1();
  failed |= tc_1_2();
  failed |= tc_1_3();
  failed |= tc_1_4();
  return failed;
}
