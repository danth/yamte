#include <errno.h>
#include <stdlib.h>
#include <sys/ioctl.h>
#include <termios.h>
#include <unistd.h>

#define CTRL_KEY(k) ((k) & 0x1f)

struct editorConfig {
	int screen_rows;
	int screen_cols;
  struct termios original_termios;
};
struct editorConfig E;

void clearScreen() {
	// Clear the whole screen
	write(STDOUT_FILENO, "\x1b[2J", 4);
	// Move the cursor to top-left
	write(STDOUT_FILENO, "\x1b[H", 3);
}

void die(const char *s) {
	clearScreen();
  perror(s);
  exit(1);
}

void disableRawMode() {
	// Restore the original terminal attributes
	if (tcsetattr(STDIN_FILENO, TCSAFLUSH, &E.original_termios) == -1) die("tcsetattr");
}

void enableRawMode() {
	// Store a copy of the original terminal attributes
	if (tcgetattr(STDIN_FILENO, &E.original_termios) == -1) die("tcgetattr");
	// Restore them when the program quits
	atexit(disableRawMode);

	struct termios raw = E.original_termios;
	// Turning off ECHO stops input being displayed when it is typed
	// Turning off ICANON allows us to read bytewise rather than linewise
	// Turning off IEXTEN disables Ctrl-V (and Ctrl-O on macOS)
	// Turning off ISIG disables Ctrl-C and Ctrl-Z
	raw.c_lflag &= ~(ECHO | ICANON | IEXTEN | ISIG);
	// Turning off IXON disables Ctrl-S and Ctrl-Q
	// Turning off ICRNL stops \r input being translated to \n
	raw.c_iflag &= ~(IXON | ICRNL);
	// Turning off OPOST removes all output processing
	raw.c_oflag &= ~(OPOST);
	// Miscellaneous flags related to "raw mode"
	raw.c_iflag &= ~(BRKINT | INPCK | ISTRIP);
	raw.c_cflag |= (CS8);
	// read() can return as soon as there is any input
	raw.c_cc[VMIN] = 0;
	// read() times out after 1/10 of a second
  raw.c_cc[VTIME] = 1;

	if (tcsetattr(STDIN_FILENO, TCSAFLUSH, &raw) == -1) die("tcsetattr");
}

char readKey() {
	int nread;
	char key;
	while ((nread = read(STDIN_FILENO, &key, 1)) != 1) {
		if (nread == -1 && errno != EAGAIN) die("read");
	}
	return key;
}

int getWindowSize(int *rows, int *cols) {
  struct winsize ws;
  if (ioctl(STDOUT_FILENO, TIOCGWINSZ, &ws) == -1 || ws.ws_col == 0) {
    return -1;
  } else {
    *cols = ws.ws_col;
    *rows = ws.ws_row;
    return 0;
  }
}

void drawRows() {
	int y;
	for (y = 0; y < E.screen_rows; y++) {
		write(STDOUT_FILENO, "~", 1);
		if (y < E.screen_rows - 1) {
      write(STDOUT_FILENO, "\r\n", 2);
    }
	}
}

void refreshScreen() {
	clearScreen();
	drawRows();
	// Move the cursor to top-left
	write(STDOUT_FILENO, "\x1b[H", 3);
}

void initialiseEditor() {
  if (getWindowSize(&E.screen_rows, &E.screen_cols) == -1) die("getWindowSize");
}

void processKey() {
	char key = readKey();
	switch (key) {
		case CTRL_KEY('q'):
			clearScreen();
			exit(0);
			break;
	}
}

int main() {
	enableRawMode();
	initialiseEditor();
	while(1) {
		refreshScreen();
		processKey();
	}
	return 0;
}
