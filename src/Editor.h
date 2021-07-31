#pragma once

#include "Buffer.h"
#include "Cursor.h"
#include <string>
#include <ncurses.h>


class Editor {
	private:
    Buffer buffer;
		Cursor cursor;
		std::string filename;
    std::string message;
		bool input_mode;

		void insertCharacter(wchar_t character);
		void insertNewline();
		void deleteCharacter();

		void processKeyNormal(wchar_t key);
		void processKeyInsert(wchar_t key);
	
	public:
		Editor();

		std::string getFilename();
		std::string getMessage();
		Buffer* getBuffer();
		Cursor* getCursor();
		std::string getModeName();

		void openFile(std::string f);
		void saveFile();

		void processKey(wchar_t key);
};
