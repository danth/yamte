#include <errno.h>
#include <stdlib.h>
#include <termios.h>
#include <unistd.h>

#define CTRL_KEY(k) ((k) & 0x1f)

struct termios original_termios;

void die(const char *s) {
  perror(s);
  exit(1);
}

void disableRawMode() {
	// Restore the original terminal attributes
	if (tcsetattr(STDIN_FILENO, TCSAFLUSH, &original_termios) == -1) die("tcsetattr");
}

void enableRawMode() {
	// Store a copy of the original terminal attributes
	if (tcgetattr(STDIN_FILENO, &original_termios) == -1) die("tcgetattr");
	// Restore them when the program quits
	atexit(disableRawMode);

	struct termios raw = original_termios;
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

int main() {
	enableRawMode();

	while (1) {
		char c = '\0';
		if (read(STDIN_FILENO, &c, 1) == -1 && errno != EAGAIN) die("read");

		// Loop until 'q' is pressed
		if (c == CTRL_KEY('q')) break;
	};

	return 0;
}
