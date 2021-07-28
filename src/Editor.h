#pragma once

#include "Buffer.h"
#include "Cursor.h"
#include <string>


class Editor {
	private:
    Buffer buffer;
		Cursor cursor;
		std::string filename;
    std::string status_message;
		bool insert_mode;

		void insertCharacter(char character);
		void insertNewline();
		void deleteCharacter();

		void processKeyNormal(int key);
		void processKeyInsert(int key);
	
	public:
		Editor();
		Buffer* getBuffer();
		Cursor* getCursor();
		bool isFileOpen();
		std::string getFilename();
		std::string getStatusMessage();
		void openFile(std::string f);
		void saveFile();
		void processKey(int key);
};
