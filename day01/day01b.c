#include <ctype.h>
#include <limits.h>
#include <stddef.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

typedef struct {
  int value;
  const char *string;
} Digit;

typedef struct {
  int first;
  int last;
} Positions;

Digit digits[] = {{0, "zero\0"},  {1, "one\0"},  {2, "two\0"}, {3, "three\0"},
                  {4, "four\0"},  {5, "five\0"}, {6, "six\0"}, {7, "seven\0"},
                  {8, "eight\0"}, {9, "nine\0"}};

int digit_combine(const Digit t, const Digit u) {
  return 10 * (t.value) + (u.value);
}

/**
 * Get position of first and last occurrance of a digit as a numeric eg '1'
 */
Positions digit_get_numeric_positions(const Digit d, const char *chars) {
  Positions pos = {.first = INT_MAX, .last = INT_MIN};
  char d_char = d.value + '0';
  int i = 0;

  while (chars[i] != '\0') {
    if (chars[i] == d_char) {

      if (i < pos.first) {
        pos.first = i;
      }
      if (i > pos.last) {
        pos.last = i;
      }
    }
    i++;
  }
  return pos;
}

/**
 * Get position of first and last occurrance of a digit as a word eg "one"
 */
Positions digit_get_word_positions(const Digit d, const char *chars) {
  Positions pos = {.first = INT_MAX, .last = INT_MIN};

  char *latest = NULL;
  char *found = strstr(chars, d.string);

  if (found == NULL) {
    return pos;
  }

  pos.first = found - chars;

  while (found != NULL) {
    latest = found;
    found = strstr(found + 1, d.string);
  }
  pos.last = latest - chars;
  return pos;
}

/**
 * Get the first and last digit in a string and combine
 */
int get_first_last_digit(const char *chars) {
  Digit *first = &digits[0];
  Digit *last = &digits[0];

  Positions pos = {.first = INT_MAX, .last = INT_MIN};

  for (size_t i = 1; i < sizeof(digits) / sizeof(digits[0]); i++) {
    Digit *d = &digits[i];
    Positions pos_d = digit_get_numeric_positions(*d, chars);
    Positions pos_w = digit_get_word_positions(*d, chars);

    if (pos_d.first < pos.first || pos_w.first < pos.first) {
      first = d;
      pos.first = pos_d.first < pos_w.first ? pos_d.first : pos_w.first;
    }

    if (pos_d.last > pos.last || pos_w.last > pos.last) {
      last = d;
      pos.last = pos_d.last > pos_w.last ? pos_d.last : pos_w.last;
    }
  }
  return digit_combine(*first, *last);
}

/**
 * Usage:
 *   cat input.txt | day01b
 */
int main() {
  char *buffer = NULL;
  size_t bufsize = 0;

  int sum = 0;
  while (getline(&buffer, &bufsize, stdin) != -1) {
    int d = get_first_last_digit(buffer);
    sum += d;
  }

  printf("%d\n", sum);

  return EXIT_SUCCESS;
}
