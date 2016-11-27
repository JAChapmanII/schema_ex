#ifndef SQL_HPP
#define SQL_HPP

#include <string>
#include <vector>
#include "parser.hpp"

bool isIdType(std::string type);
std::string getBaseIdType(std::string type);

std::string toCamelCase(std::string str);
std::string toSnakeCase(std::string name);

std::string combineTypes(std::string btype, std::string field);
std::string toIdColumn(std::string name);

std::string pluralize(std::string name);
std::string singularize(std::string name);


namespace sql {
	enum class Type { Int, Bool, String, Id, TableId };
	Type parse(std::string t);
	std::string toString(Type type);

	struct Column {
		std::string name{};
		Type type{};
		bool nullable{false};
		bool primaryKey{false};

		bool foreignKey{false};
		std::string foreignTable{""};

		// if itype is ::id_t, automatically calls makeFK
		Column(std::string iname, std::string itype, bool pk = false);

		void makeFK(std::string references);

		std::string toString();
		std::string buildFKConstraint();
	};

	struct Table {
		std::string typeName; // UpperCamelSingular TODO: not stored
		std::string name; // UpperCamelPlurals
		std::vector<Column> columns{};
		bool hasKey{true};

		// if hasKey, automatically adds toIdColumn(type) as key
		Table(std::string type, bool ihasKey = true);

		std::string toString();
	};

	std::vector<Table> genTables(parser::Context &ctx);
}

#endif // SQL_HPP
