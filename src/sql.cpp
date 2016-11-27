#include "sql.hpp"
using std::string;
using std::vector;
using std::to_string;

#include <oil/util.hpp>
#include <oil/err.hpp>

#include <iostream>
using std::cerr;
using std::endl;

bool isIdType(string type) {
	return type.size() >= 6 && type.substr(type.size() - 6) == "::id_t";
}
string getBaseIdType(string type) {
	return type.substr(0, type.size() - 6);
}

string toCamelCase(string str) {
	if(str.empty()) return "";
	string res = "";
	size_t start = 0;
	if(str.front() != '_')
		res += tolower(str.front()), start = 1;
	for(size_t i = start; i < str.size(); ++i) {
		if(str[i] == '_' && i < str.size() - 1) {
			res += toupper(str[i + 1]);
			++i;
		} else
			res += str[i];
	}
	return res;
}
string toSnakeCase(string name) {
	if(name.empty()) return "";
	string tCase(1, tolower(name[0]));
	for(size_t i = 1; i < name.size(); ++i) {
		if(isupper(name[i]) && tCase.back() != '_')
			tCase += "_";
		tCase += tolower(name[i]);
	}
	return tCase;
}
string combineTypes(string btype, string field) {
	field[0] = toupper(field[0]);
	return btype + field;
}

string toIdColumn(string name) {
	string ccase = toCamelCase(name);
	if(util::endsWith(ccase, "Id"))
		return ccase;
	return ccase + "Id";
}

string pluralize(string name) {
	if(name.length() >= 2 && name.substr(name.size() - 2) == "ss")
		return name + "es";
	else if(name.length() >= 1 && name.substr(name.size() - 1) == "s")
		return name;
	return name + "s";
}
string singularize(string name) {
	if(util::endsWith(name, "sses"))
		return name.substr(0, name.size() - 2);
	if(util::endsWith(name, "ies") || util::endsWith(name, "us"))
		return name;
	if(util::endsWith(name, "s"))
		return name.substr(0, name.size() - 1);
	return name;
}



sql::Type sql::parse(string t) {
	if(t == "int") return Type::Int;
	if(t == "bool") return Type::Bool;
	if(t == "string") return Type::String;
	if(t == "id") return Type::TableId;
	if(isIdType(t)) return Type::Id;
	throw make_except("unknown field type: '" + t + "'");
}
string sql::toString(Type type) {
	switch(type) {
		case Type::Int: return "INT";
		case Type::Bool: return "INT";
		case Type::String: return "TEXT";
		case Type::Id: return "UNIQUEIDENTIFIER";
		case Type::TableId: return "UNIQUEIDENTIFIER";
	}
	throw make_except("unkown type: " + to_string((int)type));
}


sql::Column::Column(string iname, string itype, bool pk)
		: name{iname}, type{parse(itype)}, primaryKey{pk} {
	if(type == Type::Id) {
		name = toIdColumn(iname);
		foreignKey = true;
		foreignTable = getBaseIdType(itype);
	}
}
void sql::Column::makeFK(string references) {
	foreignKey = true;
	foreignTable = references;
}
string sql::Column::toString() {
	return "`" + name + "` " + sql::toString(type)
		+ (nullable ? "" : " NOT NULL");
}
string sql::Column::buildFKConstraint() {
	return "FOREIGN KEY(" + name + ") REFERENCES "
		+ foreignTable + "(" + toIdColumn(foreignTable) + ")";
}


sql::Table::Table(string type, bool ihasKey)
		: typeName{type}, name{pluralize(type)}, hasKey{ihasKey} {
	if(hasKey)
		columns.emplace_back(toIdColumn(type), "id", true);
	cerr << "made table " << name << endl;
}

string sql::Table::toString() {
	vector<string> pkColumns;
	string columnDefs = "", constraintDefs = "";
	for(auto &column : columns) {
		columnDefs += "\t" + column.toString() + ",\n";
		if(column.foreignKey)
			constraintDefs += "\t" + column.buildFKConstraint() + ",\n";
		if(column.primaryKey)
			pkColumns.push_back(column.name);
	}
	return "CREATE TABLE `" + name + "` (\n"
		+ columnDefs + constraintDefs
		+ "\tPRIMARY KEY(" + util::join(pkColumns, ", ") + ")\n"
		+ ");";
}


vector<sql::Table> sql::genTables(parser::Context &ctx) {
	vector<Table> tables{};
	auto orderedNonIdTypes = ctx.getOrderedNonIdTypes();
	auto &types = ctx.types;
	auto &parentTypes = ctx.parentTypes;

	// in depth order, print create tables for struct types
	for(auto &typeName : orderedNonIdTypes) {
		auto &type = types[typeName];
		if(!type.composite || type.pureChild)
			continue;

		// our default key is added in constructor
		tables.emplace_back(typeName, true);
		auto &table = tables.back();

		auto chain = ctx.getParentTypeChain(typeName);
		// handle parent ids
		for(size_t i = chain.size() - 1; i > 0; --i)
			table.columns.emplace_back(chain[i], chain[i] + "::id_t", true);

		for(size_t i = 0; i < type.fields.size(); ++i) {
			auto &field = type.fields[i];
			auto &ft = types[field.type];
			if(ft.composite)
				continue;
			if(field.type == typeName + "::id_t")
				continue; // skip our own id since it is generated above

			if(ft.name == "map" || ft.name == "set" || ft.name == "vector")
				continue; // special subtable types are handled later

			// if this is an id type, ctor knows and adds fk
			table.columns.emplace_back(field.name, field.type, false);
		}
	}

	// in depth order, print child relations that aren't pure
	for(auto &typeName : orderedNonIdTypes) {
		auto &type = types[typeName];
		if(!type.composite || type.pureChild)
			continue;

		for(auto &field : type.fields) {
			auto &ft = types[field.type];
			if(ft.name == "set")
				continue; // sets are actual tables generated above
			if(ft.name != "map" && ft.name != "vector")
				continue; // impure child relations are only map and vector
			if(ft.name == "map" && types[ft.typeArguments[1]].composite)
				continue; // secretly a pure child

			// we still have our own primary key type but don't auto-gen it
			tables.emplace_back(combineTypes(typeName, field.name), false);
			auto &table = tables.back();

			table.columns.emplace_back(typeName, typeName + "::id_t", true);

			if(ft.name == "map") {
				string keyType = ft.typeArguments[0],
					valType = ft.typeArguments[1];

				table.columns.emplace_back(
						combineTypes(field.name, getBaseIdType(keyType)),
						keyType, true);
				table.columns.emplace_back("val", valType);
			}
			if(ft.name == "vector") {
				cerr << ft.typeArguments.size() << endl;
				string subType = ft.typeArguments[0];
				table.columns.emplace_back(subType, subType + "::id_t", true);
			}
		}
	}

	// generate all pure child relation tables
	for(auto &typeName : orderedNonIdTypes) {
		auto &type = types[typeName];
		if(!type.composite || !type.pureChild)
			continue;

		string parentType = parentTypes[typeName];
		cerr << "-- pure child " << typeName << " of " << parentType << endl;
		// pure child relations have no key of their own
		tables.emplace_back(typeName, false);
		auto &table = tables.back();

		string parentFieldName = ctx.pureChildFieldNames[typeName];
		parser::Field parentField{"{null}", parentFieldName};
		for(auto &field : types[parentType].fields)
			if(field.name == parentFieldName)
				parentField.type = field.type;

		string parentFieldKeyType = types[parentField.type].typeArguments[0];
		if(!isIdType(parentFieldKeyType))
			throw parentFieldKeyType + " is not id type";

		cerr << "-- " << parentField.name << " ! " << parentField.type
			<< " ! " << parentFieldKeyType << endl;

		vector<string> fields, constraints;

		table.columns.emplace_back(parentType, parentType + "::id_t", true);
		table.columns.emplace_back(getBaseIdType(parentFieldKeyType),
				parentFieldKeyType, true);

		for(auto &field : type.fields) {
			auto &ft = types[field.type];
			// should only have built in fields and ::id_t

			if(isIdType(ft.name) && ft.name == typeName + "::id_t")
				continue; // don't print key type for self
			table.columns.emplace_back(field.name, field.type);
		}
	}
	return tables;
}

