#define _DEFAULT_SOURCE
#define _BSD_SOURCE
#define _GNU_SOURCE

#include <ncurses.h>
#include <stdlib.h>
#include <string.h>
#include <sys/types.h>
#include <stdio.h>

#define TAB_STOP 8
#define CTRL_KEY(k) ((k) & 0x1f)

void die(const char *s) {
  endwin();
  perror(s);
  exit(1);
}

/*** state ***/

typedef struct editorRow {
  int size;
  char *characters;
  int rendered_size;
  char *rendered_characters;
} editorRow;

struct editorState {
  int editor_lines;
  int cursor_row, cursor_column, cursor_rendered_column;
  int row_offset, column_offset;
  int row_count;
  char *filename;
  editorRow *rows;
};
struct editorState state;

void initialiseState() {
  state.cursor_row = 0;
  state.cursor_column = 0;
  state.cursor_rendered_column = 0;
  state.row_offset = 0;
  state.column_offset = 0;
  state.row_count = 0;
  state.filename = NULL;
  state.rows = NULL;
}

/*** rows ***/

int renderedColumn(editorRow *row, int column) {
  int rendered_column = 0;
  int j;
  for (j = 0; j < column; j++) {
    if (row->characters[j] == '\t')
      rendered_column += (TAB_STOP - 1) - (rendered_column % TAB_STOP);
    rendered_column++;
  }
  return rendered_column;
}

void renderRow(editorRow *row) {
  int j;
  // Count how many tabs are in the row
  int tabs = 0;
  for (j = 0; j < row->size; j++)
    if (row->characters[j] == '\t') tabs++;

  free(row->rendered_characters);
  // Additional memory must be allocated for rendered tabs
  row->rendered_characters = malloc(row->size + tabs*(TAB_STOP-1) + 1);

  int idx = 0;
  for (j = 0; j < row->size; j++) {
    if (row->characters[j] == '\t') {
      // Replace tabs with spaces up to the next tab stop
      row->rendered_characters[idx++] = ' ';
      while (idx % TAB_STOP != 0) row->rendered_characters[idx++] = ' ';
    } else {
      row->rendered_characters[idx++] = row->characters[j];
    }
  }
  row->rendered_characters[idx] = '\0';
  row->rendered_size = idx;
}

void rowInsert(int at, char *text, size_t length) {
  if (at < 0 || at > state.row_count) return;

  state.rows = realloc(state.rows, sizeof(editorRow) * (state.row_count + 1));
  memmove(
    &state.rows[at + 1],
    &state.rows[at],
    sizeof(editorRow) * (state.row_count - at)
  );

  state.rows[at].size = length;
  state.rows[at].characters = malloc(length + 1);
  memcpy(state.rows[at].characters, text, length);
  state.rows[at].characters[length] = '\0';

  state.rows[at].rendered_size = 0;
  state.rows[at].rendered_characters = NULL;
  renderRow(&state.rows[at]);

  state.row_count++;
}

void rowFree(editorRow *row) {
  free(row->characters);
  free(row->rendered_characters);
}

void rowDelete(int at) {
  if (at < 0 || at > state.row_count) return;
  rowFree(&state.rows[at]);
  memmove(
    &state.rows[at],
    &state.rows[at + 1],
    sizeof(editorRow) * (state.row_count - at - 1)
  );
  state.row_count--;
}

void rowInsertCharacter(editorRow *row, int at, int character) {
  if (at < 0 || at > row->size) at = row->size;
  row->characters = realloc(row->characters, row->size + 2);
  memmove(&row->characters[at + 1], &row->characters[at], row->size - at + 1);
  row->size++;
  row->characters[at] = character;
  renderRow(row);
}

void rowDeleteCharacter(editorRow *row, int at) {
  if (at < 0 || at >= row->size) return;
  memmove(&row->characters[at], &row->characters[at + 1], row->size - at);
  row->size--;
  renderRow(row);
}

void rowAppendString(editorRow *row, char *text, size_t length) {
  row->characters = realloc(row->characters, row->size + length + 1);
  memcpy(&row->characters[row->size], text, length);
  row->size += length;
  row->characters[row->size] = '\0';
  renderRow(row);
}

/*** editor operations ***/

void insertCharacter(int character) {
  if (state.cursor_row == state.row_count) {
    rowInsert(state.row_count, "", 0);
  }
  rowInsertCharacter(&state.rows[state.cursor_row], state.cursor_column, character);
  state.cursor_column++;
}

void deleteCharacter() {
  if (state.cursor_row == state.row_count) return;
  if (state.cursor_column == 0 && state.cursor_row == 0) return;

  editorRow *row = &state.rows[state.cursor_row];
  if (state.cursor_column > 0) {
    rowDeleteCharacter(row, state.cursor_column - 1);
    state.cursor_column--;
  } else {
    state.cursor_column = state.rows[state.cursor_row - 1].size;
    rowAppendString(&state.rows[state.cursor_row - 1], row->characters, row->size);
    rowDelete(state.cursor_row);
    state.cursor_row--;
  }
}

void insertNewline() {
  if (state.cursor_column == 0) {
    rowInsert(state.cursor_row, "", 0);
  } else {
    editorRow *row = &state.rows[state.cursor_row];
    rowInsert(
      state.cursor_row + 1,
      &row->characters[state.cursor_column],
      row->size - state.cursor_column
    );
    row = &state.rows[state.cursor_row];
    row->size = state.cursor_column;
    row->characters[row->size] = '\0';
    renderRow(row);
  }
  state.cursor_row++;
  state.cursor_column = 0;
}

/*** files ***/

void openFile(char *filename) {
  free(state.filename);
  state.filename = strdup(filename);

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

    rowInsert(state.row_count, line, line_length);
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
  state.cursor_rendered_column = 0;
  if (state.cursor_row < state.row_count) {
    state.cursor_rendered_column = renderedColumn(
      &state.rows[state.cursor_row],
      state.cursor_column
    );
  }

  // Vertical
  if (state.cursor_row < state.row_offset) {
    state.row_offset = state.cursor_row;
  }
  if (state.cursor_row >= state.row_offset + state.editor_lines) {
    state.row_offset = state.cursor_row - state.editor_lines + 1;
  }

  // Horizontal
  if (state.cursor_rendered_column < state.column_offset) {
    state.column_offset = state.cursor_rendered_column;
  }
  if (state.cursor_rendered_column >= state.column_offset + COLS) {
    state.column_offset = state.cursor_rendered_column - COLS + 1;
  }
}

void drawRows() {
  int screen_row;
  for (screen_row = 0; screen_row < state.editor_lines; screen_row++) {
    int file_row = screen_row + state.row_offset;
    if (file_row >= state.row_count) {
      mvaddch(screen_row, 0, '~');
    } else {
      int length = state.rows[file_row].rendered_size - state.column_offset;
      if (length < 0) length = 0;
      if (length > COLS) length = COLS;
      mvaddnstr(
        screen_row, 0,
        &state.rows[file_row].rendered_characters[state.column_offset],
        length
      );
    }
  }
}

void drawStatus() {
  attron(A_STANDOUT);

  char status[80];
  int length = snprintf(status, sizeof(status), "%.40s - %d lines",
    state.filename ? state.filename : "[no name]", state.row_count);
  if (length > COLS) length = COLS;
  mvaddnstr(LINES-1, 0, status, length);

  while (length < COLS) {
    mvaddch(LINES-1, length, ' ');
    length++;
  }

  attroff(A_STANDOUT);
}

void refreshScreen() {
  clear();
  state.editor_lines = LINES - 1;
  clampScroll();
  drawRows();
  drawStatus();
  move(
    state.cursor_row - state.row_offset,
    state.cursor_rendered_column - state.column_offset
  );
  refresh();
}

/*** input ***/

void moveCursor(int key) {
  editorRow *row = (state.cursor_row > state.row_count)
                 ? NULL
                 : &state.rows[state.cursor_row];

  switch (key) {
    case KEY_UP:
      // Move up unless we are on the first line
      if (state.cursor_row > 0) {
        state.cursor_row--;
      }
      break;
    case KEY_DOWN:
      // Move down unless we are on the last line
      if (state.cursor_row < state.row_count) {
        state.cursor_row++;
      }
      break;
    case KEY_LEFT:
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
    case KEY_RIGHT:
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
      if (state.cursor_row < state.row_count) {
        state.cursor_column = state.rows[state.cursor_row].size;
      }
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

    case KEY_UP:
    case KEY_DOWN:
    case KEY_LEFT:
    case KEY_RIGHT:
    case KEY_HOME:
    case KEY_END:
      moveCursor(key);
      break;

    case '\r':
      insertNewline();
      break;

    case KEY_BACKSPACE:
    case KEY_DC:
    case CTRL_KEY('h'):
      if (key == KEY_DC) moveCursor(KEY_RIGHT);
      deleteCharacter();
      break;

    default:
      insertCharacter(key);
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
