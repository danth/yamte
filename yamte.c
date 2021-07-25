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
  int row_offset, column_offset;
  int row_count;
  editorRow *rows;
};
struct editorState state;

void initialiseState() {
  state.cursor_row = 0;
  state.cursor_column = 0;
  state.row_offset = 0;
  state.column_offset = 0;
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

void clampScroll() {
  // Vertical
  if (state.cursor_row < state.row_offset) {
    state.row_offset = state.cursor_row;
  }
  if (state.cursor_row >= state.row_offset + LINES) {
    state.row_offset = state.cursor_row - LINES + 1;
  }

  // Horizontal
  if (state.cursor_column < state.column_offset) {
    state.column_offset = state.cursor_column;
  }
  if (state.cursor_column >= state.column_offset + COLS) {
    state.column_offset = state.cursor_column - COLS + 1;
  }
}

void drawRows() {
  int screen_row;
  for (screen_row = 0; screen_row < LINES; screen_row++) {
    int file_row = screen_row + state.row_offset;
    if (file_row >= state.row_count) {
      mvaddch(screen_row, 0, '~');
    } else {
      int length = state.rows[file_row].size - state.column_offset;
      if (length < 0) length = 0;
      if (length > COLS) length = COLS;
      mvaddnstr(
        screen_row, 0,
        &state.rows[file_row].chars[state.column_offset],
        length
      );
    }
  }
}

void refreshScreen() {
  clear();
  clampScroll();
  drawRows();
  move(
    state.cursor_row - state.row_offset,
    state.cursor_column - state.column_offset
  );
  refresh();
}

/*** input ***/

void moveCursor(int key) {
  editorRow *row = (state.cursor_row > state.row_count)
                 ? NULL
                 : &state.rows[state.cursor_row];

  switch (key) {
    case 'w':
      // Move up unless we are on the first line
      if (state.cursor_row > 0) {
        state.cursor_row--;
      }
      break;
    case 's':
      // Move down unless we are on the last line
      if (state.cursor_row < state.row_count) {
        state.cursor_row++;
      }
      break;
    case 'a':
      // Move left unless we are at the first column
      if (state.cursor_column > 0) {
        state.cursor_column--;
      }
      // Go to the last column of the previous line
      else if (state.cursor_row > 0) {
        state.cursor_row--;
        state.cursor_column = state.rows[state.cursor_row].size;
      }
      break;
    case 'd':
      // Move right unless we are at the last column
      if (row && state.cursor_column < row->size) {
        state.cursor_column++;
      }
      // Go to the first column of the next line
      else if (row && state.cursor_column == row->size) {
        state.cursor_row++;
        state.cursor_column = 0;
      }
      break;
    case KEY_HOME:
      // Move to the first column
      state.cursor_column = 0;
      break;
    case KEY_END:
      // Move to the right of the screen
      state.cursor_column = COLS-1;
      break;
  }

  row = (state.cursor_row > state.row_count)
      ? NULL
      : &state.rows[state.cursor_row];
  int row_length = row ? row->size : 0;
  if (state.cursor_column > row_length) {
    state.cursor_column = row_length;
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
