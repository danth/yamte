#include <stdlib.h>
#include <termios.h>
#include <unistd.h>

struct termios original_termios;

void disableRawMode() {
	// Restore the original terminal attributes
	tcsetattr(STDIN_FILENO, TCSAFLUSH, &original_termios);
}

void enableRawMode() {
	// Store a copy of the original terminal attributes
	tcgetattr(STDIN_FILENO, &original_termios);
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

	tcsetattr(STDIN_FILENO, TCSAFLUSH, &raw);
}

int main() {
	enableRawMode();

	while (1) {
		char c = '\0';
		read(STDIN_FILENO, &c, 1);

		// Loop until 'q' is pressed
		if (c == 'q') break;
	};

	return 0;
}
