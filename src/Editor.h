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

		void insertCharacter(wchar_t character);
		void insertNewline();
		void deleteCharacter();

		void processKeyNormal(wchar_t key);
		void processKeyInsert(wchar_t key);
	
	public:
		Editor();

		void initialiseScreen();

		void openFile(std::string f);
		void saveFile();

		void processKey();
};
