#include <ncurses.h>
#include <stdlib.h>

#define CTRL_KEY(k) ((k) & 0x1f)

void die(const char *s) {
	endwin();
  perror(s);
  exit(1);
}

void drawRows() {
	int row;
	for (row = 0; row < LINES; row++) {
		mvaddch(row, 0, '~');
	}
}

void initialiseScreen() {
	initscr();
	raw(); // Disable line buffering
	noecho(); // Don't echo typed characters
	nonl(); // Disable translation of \r into \n
	intrflush(stdscr, FALSE);
	keypad(stdscr, TRUE); // Replace F1, F2, F3... with token values
}

void refreshScreen() {
	clear();
	drawRows();
	move(0, 0);
	refresh();
}

void processKey() {
	char key = getch();
	switch (key) {
		case CTRL_KEY('q'):
			endwin();
			exit(0);
			break;
	}
}

int main() {
	initialiseScreen();
	while(1) {
		refreshScreen();
		processKey();
	}
	return 0;
}
