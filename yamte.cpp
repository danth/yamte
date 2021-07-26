#include <ncurses.h>
#include <vector>
#include <string>
#include <iostream>
#include <fstream>

#define TAB_STOP 8
#define CTRL_KEY(k) ((k) & 0x1f)

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

class Editor;

class Cursor {
  private:
    int cursor_row;
    int cursor_column;

  public:
    Cursor() {
      cursor_row = 0;
      cursor_column = 0;
    }

    int getRow() {
      return cursor_row;
    }

    void setRow(int row) {
      cursor_row = row;
    }

    void adjustRow(int by) {
      cursor_row += by;
    }

    int getColumn() {
      return cursor_column;
    }

    void setColumn(int column) {
      cursor_column = column;
    }

    void adjustColumn(int by) {
      cursor_column += by;
    }

    void move(int key, Editor *editor) {
      switch (key) {
        case KEY_UP:
          if (cursor_row > 0) cursor_row--;
          break;
        case KEY_DOWN:
          // Move down unless we are on the last line
          if (cursor_row < editor->countRows()) cursor_row++;
          break;
        case KEY_LEFT:
          // Move left unless we are at the first column
          if (cursor_column > 0) cursor_column--;
          // Go to the last column of the previous line
          else if (cursor_row > 0) {
            cursor_row--;
            cursor_column = editor->getRow(cursor_row).size();
          }
          break;
        case KEY_RIGHT:
          if (cursor_row < editor->countRows()) {
            Row row = editor->getRow(cursor_row);
            // Move right unless we are at the last column
            if (cursor_column < row.size()) cursor_column++;
            // Go to the first column of the next line
            else if (cursor_column == row.size()) {
              cursor_row++;
              cursor_column = 0;
            }
          }
          break;
        case KEY_HOME:
          // Move to the first column
          cursor_column = 0;
          break;
        case KEY_END:
          // Move to the right of the screen
          if (cursor_row < editor->countRows()) {
            cursor_column = editor->getRow(cursor_row).size();
          }
          break;
      }

      if (cursor_row > editor->countRows()) {
        cursor_column = 0;
      } else {
        int row_length = editor->getRow(cursor_row).size();
        if (cursor_column > row_length) cursor_column = row_length;
      }
    }
};

class Editor {
  private:
    Cursor cursor;
    std::string filename;
    std::vector<Row> rows;

  public:
    Editor() {
      filename = "";
    }

    std::string getFilename() {
      return filename;
    }

    bool isFileOpen() {
      return filename.size();
    }

    Cursor getCursor() {
      return cursor;
    }

    int countRows() {
      return rows.size();
    }

    Row* getRow(int at) {
      return &rows[at];
    }

    Row* getCurrentRow() {
      return getRow(cursor.getRow());
    }

    int getRenderedColumn() {
      return getCurrentRow().renderedColumn(cursor.getColumn());
    }

    void insertRow(int at, std::string text) {
      if (at < 0 || at > rows.size()) return;

      Row row;
      row.setText(text);
      rows.insert(rows.begin() + at, row);
    }

    void deleteRow(int at) {
      if (at < 0 || at > rows.size()) return;

      rows.erase(rows.begin() + at);
    }

    void insertCharacter(char character) {
      if (cursor.getRow() == rows.size()) insertRow(rows.size(), "");

      getCurrentRow().insertCharacter(cursor.getColumn(), character);
      cursor.adjustColumn(1);
    }

    void deleteCharacter() {
      if (cursor.getRow() == rows.size()) return;
      if (cursor.getColumn() == 0 && cursor.getRow() == 0) return;

      Row *row = &rows[cursor.getRow()];
      if (cursor.getColumn() > 0) {
        row->deleteCharacter(cursor.getColumn() - 1);
        cursor.adjustColumn(-1);
      } else {
        Row *previous_row = getRow(cursor.getRow() - 1);
        cursor.setColumn(previous_row->size());
        previous_row->appendText(row->getText());
        deleteRow(cursor.getRow());
        cursor.adjustRow(-1);
      }
    }

    void insertNewline() {
      if (cursor.getColumn() == 0) {
        insertRow(cursor.getRow(), "");
      } else {
        insertRow(cursor.getRow() + 1, getCurrentRow()->getText().substr(cursor.getColumn()));
        getCurrentRow()->resizeText(cursor.getColumn());
      }

      cursor.adjustRow(1);
      cursor.setColumn(0);
    }

    void openFile(std::string f) {
      filename = f;

      std::fstream file;
      file.open(filename, std::ios::in);
      if (file.is_open()) {
        std::string line;
        while(getline(file, line)) {
          insertRow(rows.size(), line);
        }
        file.close();
      }
    }

    void processKey(int key) {
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
          cursor.move(key, this);
          break;

        case '\r':
          insertNewline();
          break;

        case KEY_BACKSPACE:
        case KEY_DC:
        case CTRL_KEY('h'):
          if (key == KEY_DC) cursor.move(KEY_RIGHT, this);
          deleteCharacter();
          break;

        default:
          insertCharacter(key);
          break;
      }
    }
};

class Display {
  private:
    Editor editor;
    int lines;
    int row_offset;
    int column_offset;

  public:
    Display(Editor e) {
      editor = e;
      lines = LINES - 1;
      row_offset = 0;
      column_offset = 0;
    }

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
      if (editor.getCursor().getRow() < row_offset) {
        row_offset = editor.getCursor().getRow();
      }
      if (editor.getCursor().getRow() >= row_offset + lines) {
        row_offset = editor.getCursor().getRow() - lines + 1;
      }

      // Horizontal
      if (editor.getRenderedColumn() < column_offset) {
        column_offset = editor.getRenderedColumn();
      }
      if (editor.getRenderedColumn() >= column_offset + COLS) {
        column_offset = editor.getRenderedColumn() - COLS + 1;
      }
    }

    void drawRows() {
      int screen_row;
      for (screen_row = 0; screen_row < lines; screen_row++) {
        int file_row = screen_row + row_offset;

        if (file_row >= editor.countRows()) {
          mvaddch(screen_row, 0, '~');
        } else {
          std::string rendered = editor.getRow(file_row).getRendered();
          std::string visible_rendered = rendered.substr(
              column_offset, column_offset + COLS);
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
        editor.isFileOpen() ? editor.getFilename().c_str() : "[no name]",
        editor.countRows()
      );

      attroff(A_STANDOUT);
    }

    void refreshScreen() {
      clear();
      lines = LINES - 1;
      clampScroll();
      drawRows();
      drawStatus();
      move(
        editor.getCursor().getRow() - row_offset,
        editor.getRenderedColumn() - column_offset
      );
      refresh();
    }

    int getKey() {
      return getch();
    }
};

int main(int argc, char *argv[]) {
  Editor editor;
  Display display(editor);
  display.initialiseScreen();

  if (argc >= 2) {
    editor.openFile(argv[1]);
  }

  while(1) {
    display.refreshScreen();
    editor.processKey(display.getKey());
  }

  return 0;
}
