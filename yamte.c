#define _DEFAULT_SOURCE
#define _BSD_SOURCE
#define _GNU_SOURCE

#include <ncurses.h>
#include <stdlib.h>
#include <string.h>
#include <sys/types.h>
#include <stdio.h>

#define CTRL_KEY(k) ((k) & 0x1f)

void die(const char *s) {
  endwin();
  perror(s);
  exit(1);
}

/*** state ***/

typedef struct editorRow {
  int size;
  char *chars;
} editorRow;

struct editorState {
  int cursor_row, cursor_column;
  int row_count;
  editorRow *rows;
};
struct editorState state;

void initialiseState() {
  state.cursor_row = 0;
  state.cursor_column = 0;
  state.row_count = 0;
  state.rows = NULL;
}

/*** files ***/

void loadLine(char *line, ssize_t line_length) {
  state.rows = realloc(state.rows, sizeof(editorRow) * (state.row_count + 1));

  int row = state.row_count;
  state.row_count++;

  state.rows[row].size = line_length;
  state.rows[row].chars = malloc(line_length + 1);
  memcpy(state.rows[row].chars, line, line_length);
  state.rows[row].chars[line_length] = '\0';
}

void openFile(char *filename) {
  FILE *file = fopen(filename, "r");
  if (!file) die("fopen");

  char *line = NULL;
  size_t line_capacity = 0;
  ssize_t line_length;
  while ((line_length = getline(&line, &line_capacity, file)) != -1) {
    // Remove \r and/or \n from the end of the line
    while (line_length > 0 && (line[line_length - 1] == '\n'
                            || line[line_length - 1] == '\r'))
      line_length--;

    loadLine(line, line_length);
  }
  free(line);
  fclose(file);
}

/*** output ***/

void initialiseScreen() {
  initscr();
  raw(); // Disable line buffering
  noecho(); // Don't echo typed characters
  nonl(); // Disable translation of \r into \n
  intrflush(stdscr, FALSE);
  keypad(stdscr, TRUE); // Replace F1, F2, F3... with token values
}

void drawRows() {
  int row;
  for (row = 0; row < LINES; row++) {
    if (row >= state.row_count) {
      mvaddch(row, 0, '~');
    } else {
      int length = state.rows[row].size;
      if (length > COLS) length = COLS;
      mvaddnstr(row, 0, state.rows[row].chars, length);
    }
  }
}

void refreshScreen() {
  clear();
  drawRows();
  move(state.cursor_row, state.cursor_column);
  refresh();
}

/*** input ***/

void moveCursor(int key) {
  switch (key) {
    case 'w':
      if (state.cursor_row > 0) {
        state.cursor_row--;
      }
      break;
    case 's':
      if (state.cursor_row < LINES-1) {
        state.cursor_row++;
      }
      break;
    case 'a':
      if (state.cursor_column > 0) {
        state.cursor_column--;
      }
      break;
    case 'd':
      if (state.cursor_column < COLS-1) {
        state.cursor_column++;
      }
      break;
    case KEY_HOME:
      state.cursor_column = 0;
      break;
    case KEY_END:
      state.cursor_column = COLS-1;
      break;
  }
}

void processKey() {
  int key = getch();
  switch (key) {
    case CTRL_KEY('q'):
      endwin();
      exit(0);
      break;

    case 'w':
    case 's':
    case 'a':
    case 'd':
    case KEY_HOME:
    case KEY_END:
      moveCursor(key);
      break;
  }
}

/*** main ***/

int main(int argc, char *argv[]) {
  initialiseState();
  initialiseScreen();

  if (argc >= 2) {
    openFile(argv[1]);
  }

  while(1) {
    refreshScreen();
    processKey();
  }

  return 0;
}
