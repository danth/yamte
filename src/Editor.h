#pragma once

#include "Row.h"
#include "Cursor.h"
#include <string>


class Editor {
	private:
    Buffer buffer;
		Cursor cursor;
		std::string filename;

		void insertCharacter(char character);
		void insertNewline();
		void deleteCharacter();
	
	public:
		Editor();
		Buffer* getBuffer();
		Cursor* getCursor();
		bool isFileOpen();
		std::string getFilename();
		void openFile(std::string f);
		void processKey(int key);
};
