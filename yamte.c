#include <ncurses.h>
#include <stdlib.h>
#include <string.h>
#include <sys/types.h>

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
	editorRow row;
};
struct editorState state;

void initialiseState() {
	state.cursor_row = 0;
	state.cursor_column = 0;
	state.row_count = 0;
}

/*** files ***/

void openFile() {
	char *line = "Hello, world!";
	ssize_t line_length = 13;

	state.row.size = line_length;
	state.row.chars = malloc(line_length + 1);
	memcpy(state.row.chars, line, line_length);
	state.row.chars[line_length] = '\0';
	state.row_count = 1;
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
			int length = state.row.size;
			if (length > COLS) length = COLS;
			mvaddnstr(row, 0, state.row.chars, length);
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

int main() {
	initialiseState();
	initialiseScreen();
	openFile();

	while(1) {
		refreshScreen();
		processKey();
	}

	return 0;
}
