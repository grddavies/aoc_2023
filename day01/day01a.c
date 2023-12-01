#include <ctype.h>
#include <stddef.h>
#include <stdio.h>
#include <stdlib.h>

int combine(const char t, const char u) { return 10 * (t - '0') + (u - '0'); }

/**
 * Combine the first and last digits on the line
 */
int getNumber(const char *chars) {
  char fst;
  char lst;
  for (size_t i = 0; chars[i] != '\0'; i++) {
    if (isdigit(chars[i])) {
      if (!fst) {
        fst = chars[i];
      }
      lst = chars[i];
    }
  }
  return fst ? combine(fst, lst) : 0;
}

/**
 * Usage:
 *   cat input.txt | day01a
 */
int main() {
  char *buffer = NULL;
  size_t bufsize = 0;

  int sum = 0;
  while (getline(&buffer, &bufsize, stdin) != -1) {
    sum += getNumber(buffer);
  }

  printf("%d\n", sum);

  return EXIT_SUCCESS;
}
