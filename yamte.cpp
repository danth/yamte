#include <ncurses.h>
#include <vector>
#include <string>
#include <iostream>
#include <fstream>

#define TAB_STOP 8
#define CTRL_KEY(k) ((k) & 0x1f)

/*** state ***/

class Row {
  private:
    std::string text;
    std::string rendered;

    void render() {
      rendered = "";
      int j;
      for (j = 0; j < text.size(); j++) {
        if (text[j] == '\t') {
          // Replace tabs with spaces up to the next tab stop
          rendered.push_back(' ');
          while (rendered.size() % TAB_STOP != 0) rendered.push_back(' ');
        } else {
          rendered.push_back(text[j]);
        }
      }
    }

  public:
    std::string getText() {
      return text;
    }

    std::string getRendered() {
      return rendered;
    }

    int size() {
      return text.size();
    }

    void setText(std::string t) {
      text = t;
      render();
    }

    void appendText(std::string t) {
      text.append(t);
      render();
    }

    void resizeText(int length) {
      text.resize(length);
      render();
    }

    void insertCharacter(int at, int character) {
      if (at < 0 || at > text.size()) at = text.size();
      text.insert(text.begin() + at, character);
      render();
    }

    void deleteCharacter(int at) {
      if (at < 0 || at >= text.size()) return;
      text.erase(text.begin() + at);
      render();
    }

    int renderedColumn(int column) {
      int rendered_column = 0;
      int j;
      for (j = 0; j < column; j++) {
        if (text[j] == '\t')
          rendered_column += (TAB_STOP - 1) - (rendered_column % TAB_STOP);
        rendered_column++;
      }
      return rendered_column;
    }
};

struct editorState {
  int editor_lines;
  int cursor_row, cursor_column, cursor_rendered_column;
  int row_offset, column_offset;
  std::string filename;
  std::vector <Row> rows;
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

void rowInsert(int at, std::string text) {
  if (at < 0 || at > state.rows.size()) return;
  Row row;
  row.setText(text);
  state.rows.insert(state.rows.begin() + at, row);
}

void rowDelete(int at) {
  if (at < 0 || at > state.rows.size()) return;
  state.rows.erase(state.rows.begin() + at);
}

/*** editor operations ***/

void insertCharacter(int character) {
  if (state.cursor_row == state.rows.size()) {
    rowInsert(state.rows.size(), "");
  }
  state.rows[state.cursor_row].insertCharacter(state.cursor_column, character);
  state.cursor_column++;
}

void deleteCharacter() {
  if (state.cursor_row == state.rows.size()) return;
  if (state.cursor_column == 0 && state.cursor_row == 0) return;

  Row *row = &state.rows[state.cursor_row];
  if (state.cursor_column > 0) {
    row->deleteCharacter(state.cursor_column - 1);
    state.cursor_column--;
  } else {
    Row *previous_row = &state.rows[state.cursor_row - 1];
    state.cursor_column = previous_row->size();
    previous_row->appendText(row->getText());
    rowDelete(state.cursor_row);
    state.cursor_row--;
  }
}

void insertNewline() {
  if (state.cursor_column == 0) {
    rowInsert(state.cursor_row, "");
  } else {
    Row *row = &state.rows[state.cursor_row];
    rowInsert(state.cursor_row + 1, row->getText().substr(state.cursor_column));
    row = &state.rows[state.cursor_row];
    row->resizeText(state.cursor_column);
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
    state.cursor_rendered_column =
      state.rows[state.cursor_row].renderedColumn(state.cursor_column);
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
      std::string rendered = state.rows[file_row].getRendered();
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
        state.cursor_column = state.rows[state.cursor_row].size();
      }
      break;
    case KEY_RIGHT:
      if (state.cursor_row < state.rows.size()) {
        Row row = state.rows[state.cursor_row];
        // Move right unless we are at the last column
        if (state.cursor_column < row.size()) {
          state.cursor_column++;
        }
        // Go to the first column of the next line
        else if (state.cursor_column == row.size()) {
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
        state.cursor_column = state.rows[state.cursor_row].size();
      }
      break;
  }

  if (state.cursor_row > state.rows.size()) {
    state.cursor_column = 0;
  } else {
    int row_length = state.rows[state.cursor_row].size();
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
