#include <ncurses.h>
#include <stdlib.h>
#include <string.h>
#include <sys/types.h>
#include <stdio.h>
#include <vector>
#include <string>
#include <iostream>
#include <fstream>

#define TAB_STOP 8
#define CTRL_KEY(k) ((k) & 0x1f)

/*** state ***/

typedef struct editorRow {
  std::string characters;
  std::string rendered_characters;
} editorRow;

struct editorState {
  int editor_lines;
  int cursor_row, cursor_column, cursor_rendered_column;
  int row_offset, column_offset;
  std::string filename;
  std::vector <editorRow> rows;
};
struct editorState state;

void initialiseState() {
  state.cursor_row = 0;
  state.cursor_column = 0;
  state.cursor_rendered_column = 0;
  state.row_offset = 0;
  state.column_offset = 0;
  state.filename = "";
}

/*** rows ***/

int renderedColumn(editorRow row, int column) {
  int rendered_column = 0;
  int j;
  for (j = 0; j < column; j++) {
    if (row.characters[j] == '\t')
      rendered_column += (TAB_STOP - 1) - (rendered_column % TAB_STOP);
    rendered_column++;
  }
  return rendered_column;
}

void renderRow(editorRow *row) {
  row->rendered_characters = "";
  int j;
  for (j = 0; j < row->characters.size(); j++) {
    if (row->characters[j] == '\t') {
      // Replace tabs with spaces up to the next tab stop
      row->rendered_characters.push_back(' ');
      while (row->rendered_characters.size() % TAB_STOP != 0)
        row->rendered_characters.push_back(' ');
    } else {
      row->rendered_characters.push_back(row->characters[j]);
    }
  }
}

void rowInsert(int at, std::string text) {
  if (at < 0 || at > state.rows.size()) return;

  editorRow row;
  row.characters = text;
  renderRow(&row);

  state.rows.insert(state.rows.begin() + at, row);
}

void rowDelete(int at) {
  if (at < 0 || at > state.rows.size()) return;
  state.rows.erase(state.rows.begin() + at);
}

void rowInsertCharacter(editorRow *row, int at, int character) {
  if (at < 0 || at > row->characters.size()) at = row->characters.size();
  row->characters.insert(row->characters.begin() + at, character);
  renderRow(row);
}

void rowDeleteCharacter(editorRow *row, int at) {
  if (at < 0 || at >= row->characters.size()) return;
  row->characters.erase(row->characters.begin() + at);
  renderRow(row);
}

void rowAppendString(editorRow *row, std::string text) {
  row->characters.append(text);
  renderRow(row);
}

/*** editor operations ***/

void insertCharacter(int character) {
  if (state.cursor_row == state.rows.size()) {
    rowInsert(state.rows.size(), "");
  }
  rowInsertCharacter(&state.rows[state.cursor_row], state.cursor_column, character);
  state.cursor_column++;
}

void deleteCharacter() {
  if (state.cursor_row == state.rows.size()) return;
  if (state.cursor_column == 0 && state.cursor_row == 0) return;

  editorRow *row = &state.rows[state.cursor_row];
  if (state.cursor_column > 0) {
    rowDeleteCharacter(row, state.cursor_column - 1);
    state.cursor_column--;
  } else {
    state.cursor_column = state.rows[state.cursor_row - 1].characters.size();
    rowAppendString(&state.rows[state.cursor_row - 1], row->characters);
    rowDelete(state.cursor_row);
    state.cursor_row--;
  }
}

void insertNewline() {
  if (state.cursor_column == 0) {
    rowInsert(state.cursor_row, "");
  } else {
    editorRow *row = &state.rows[state.cursor_row];
    rowInsert(state.cursor_row + 1, row->characters.substr(state.cursor_column));
    row = &state.rows[state.cursor_row];
    row->characters.resize(state.cursor_column);
    renderRow(row);
  }
  state.cursor_row++;
  state.cursor_column = 0;
}

/*** files ***/

void openFile(std::string filename) {
  state.filename = filename;

  std::fstream file;
  file.open(filename, std::ios::in);
  if (file.is_open()) {
    std::string line;
    while(getline(file, line)) {
      rowInsert(state.rows.size(), line);
    }
    file.close();
  }
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
  if (state.cursor_row < state.rows.size()) {
    state.cursor_rendered_column = renderedColumn(
      state.rows[state.cursor_row],
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
    if (file_row >= state.rows.size()) {
      mvaddch(screen_row, 0, '~');
    } else {
      std::string rendered = state.rows[file_row].rendered_characters;
      std::string visible_rendered = rendered.substr(
          state.column_offset, state.column_offset + COLS);
      mvaddstr(screen_row, 0, visible_rendered.c_str());
    }
  }
}

void drawStatus() {
  attron(A_STANDOUT);

  // Fill with spaces to create background
  int j;
  for (j = 0; j < COLS; j++) mvaddch(LINES-1, j, ' ');

  // Overwrite some of the spaces with the status
  mvprintw(LINES-1, 0, "%.40s - %d lines",
    state.filename.size() ? state.filename.c_str() : "[no name]", state.rows.size());

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
  switch (key) {
    case KEY_UP:
      // Move up unless we are on the first line
      if (state.cursor_row > 0) {
        state.cursor_row--;
      }
      break;
    case KEY_DOWN:
      // Move down unless we are on the last line
      if (state.cursor_row < state.rows.size()) {
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
        state.cursor_column = state.rows[state.cursor_row].characters.size();
      }
      break;
    case KEY_RIGHT:
      if (state.cursor_row < state.rows.size()) {
        editorRow row = state.rows[state.cursor_row];
        // Move right unless we are at the last column
        if (state.cursor_column < row.characters.size()) {
          state.cursor_column++;
        }
        // Go to the first column of the next line
        else if (state.cursor_column == row.characters.size()) {
          state.cursor_row++;
          state.cursor_column = 0;
        }
      }
      break;
    case KEY_HOME:
      // Move to the first column
      state.cursor_column = 0;
      break;
    case KEY_END:
      // Move to the right of the screen
      if (state.cursor_row < state.rows.size()) {
        state.cursor_column = state.rows[state.cursor_row].characters.size();
      }
      break;
  }

  if (state.cursor_row > state.rows.size()) {
    state.cursor_column = 0;
  } else {
    int row_length = state.rows[state.cursor_row].characters.size();
    if (state.cursor_column > row_length) state.cursor_column = row_length;
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
