#include <iostream>

class Generator {
  int state = 1;

  int next() {
    state = state * 3 % 10;
    return state;
  }
};

int main(int argc, char *argv[]) {
  Generator g;
  std::cout << g.next() << g.next() << std::endl;
  return 0;
}