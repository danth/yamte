#pragma once

#include "Buffer.h"
#include "Cursor.h"
#include "Display.h"
#include <string>


class Editor {
	private:
    Buffer buffer;
		Cursor cursor;
		Display display;
		std::string filename;
		bool input_mode;

		void drawStatus();
		void drawBuffer();
		void drawCursor();

		void insertCharacter(char character);
		void insertNewline();
		void deleteCharacter();

		void processKeyNormal(int key);
		void processKeyInsert(int key);
	
	public:
		Editor();

		void initialiseScreen();

		void openFile(std::string f);
		void saveFile();

		void processKey();
};
