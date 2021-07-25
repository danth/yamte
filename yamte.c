#include <ncurses.h>
#include <stdlib.h>

#define CTRL_KEY(k) ((k) & 0x1f)

void die(const char *s) {
	endwin();
  perror(s);
  exit(1);
}

/*** state ***/

struct editorState {
	int cursor_row, cursor_column;
};
struct editorState state;

void initialiseState() {
	state.cursor_row = 0;
	state.cursor_column = 0;
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
		mvaddch(row, 0, '~');
	}
}

void refreshScreen() {
	clear();
	drawRows();
	move(state.cursor_row, state.cursor_column);
	refresh();
}

/*** input ***/

void moveCursor(char key) {
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
	}
}

void processKey() {
	char key = getch();
	switch (key) {
		case CTRL_KEY('q'):
			endwin();
			exit(0);
			break;

		case 'w':
		case 's':
		case 'a':
		case 'd':
			moveCursor(key);
			break;
	}
}

/*** main ***/

int main() {
	initialiseState();
	initialiseScreen();
	while(1) {
		refreshScreen();
		processKey();
	}
	return 0;
}
