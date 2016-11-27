#ifndef PY_HPP
#define PY_HPP

#include <string>
#include <vector>
#include <map>

#include "parser.hpp"

namespace py {
	struct Argument {
		std::string name;
		std::string defl{""};
		bool hasDefault{false};

		Argument(std::string iname, std::string idefl = "", bool ihasDefault = false);
		Argument parse(std::string str);

		std::string toString();
	};

	enum class FunctionType { Free, Member, Class, Static };

	struct Function {
		std::string name;
		FunctionType type{FunctionType::Free};

		std::vector<Argument> arguments{};
		std::vector<std::string> body{};

		Function(); // TODO: this is not to be used, should be = deleted...

		Function(std::string iname, FunctionType ftype = FunctionType::Free);

		// append new line to function body
		void operator+=(std::string line);

		std::string toString(int extraTabs = 0);
	};

	struct Class {
		std::string name;
		std::string parent{};
		// explicit members?
		std::vector<std::string> fnames{};
		std::map<std::string, Function> functions{};

		Class(); // TODO: this is not to be used, should be = deleted...

		Class(std::string iname);

		Function &operator[](std::string name);

		std::string toString(int extraTabs = 0);
	};

	struct Context {
		std::vector<std::string> tnames{};
		std::map<std::string, Class> types{};

		Class &operator[](std::string name);

		std::string toString();

		static Context fromPContext(parser::Context &ctx);
	};
}

#endif // PY_HPP
