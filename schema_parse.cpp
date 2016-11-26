#include <iostream>
#include <fstream>
#include <string>
#include <vector>
#include <map>
#include <set>
#include <cctype>
using namespace std;

#include <oil/util.hpp>
#include <oil/err.hpp>

bool isIdType(string type) {
	return type.size() >= 6 && type.substr(type.size() - 6) == "::id_t";
}
string getBaseIdType(string type) {
	return type.substr(0, type.size() - 6);
}

string toCamelCase(string str) {
	string res = "";
	int start = 0;
	if(str.front() != '_')
		res += tolower(str.front()), start = 1;
	for(int i = start; i < str.size(); ++i) {
		if(str[i] == '_' && i < str.size() - 1) {
			res += toupper(str[i + 1]);
			++i;
		} else
			res += str[i];
	}
	return res;
}
string toSnakeCase(string name) {
	string tCase(1, tolower(name[0]));
	for(int i = 1; i < name.size(); ++i) {
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

namespace sql {
	enum class Type { Int, Bool, String, Id, TableId };
	Type parse(string t) {
		if(t == "int") return Type::Int;
		if(t == "bool") return Type::Bool;
		if(t == "string") return Type::String;
		if(t == "id") return Type::TableId;
		if(isIdType(t)) return Type::Id;
		throw make_except("unknown field type: '" + t + "'");
	}
	string toString(Type type) {
		switch(type) {
			case Type::Int: return "INT";
			case Type::Bool: return "INT";
			case Type::String: return "TEXT";
			case Type::Id: return "UNIQUEIDENTIFIER";
			case Type::TableId: return "UNIQUEIDENTIFIER";
		}
		throw make_except("unkown type: " + to_string((int)type));
	}
	struct Column {
		string name{};
		Type type{};
		bool nullable{false};
		bool primaryKey{false};

		bool foreignKey{false};
		string foreignTable{""};

		Column(string iname, string itype, bool pk = false)
				: name{iname}, type{parse(itype)}, primaryKey{pk} {
			if(type == Type::Id) {
				name = toIdColumn(iname);
				foreignKey = true;
				foreignTable = getBaseIdType(itype);
			}
		}
		void makeFK(string references) {
			foreignKey = true;
			foreignTable = references;
		}
		string toString() {
			return "`" + name + "` " + sql::toString(type)
				+ (nullable ? "" : " NOT NULL");
		}
		string buildFKConstraint() {
			return "FOREIGN KEY(" + name + ") REFERENCES "
				+ foreignTable + "(" + toIdColumn(foreignTable) + ")";
		}
	};
	struct Table {
		string typeName; // UpperCamelSingular
		string name; // UpperCamelPlurals
		vector<Column> columns{};
		bool hasKey{true};

		Table(string type, bool ihasKey = true)
				: typeName{type}, name{pluralize(type)}, hasKey{ihasKey} {
			if(hasKey)
				columns.emplace_back(toIdColumn(type), "id", true);
			cerr << "made table " << name << endl;
		}

		string toString() {
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
	};
};

struct Context;

struct Field {
	bool composite{false};

	string type{""};
	string name{""};

	Field(string itype, string iname)
		: type{itype}, name{iname} { }
};

struct Type {
	string name{"{null}"};
	bool composite{true};
	bool pureChild{false};

	vector<string> typeArguments{};
	vector<Field> fields{};

	Type(string iname, bool icomposite = true)
		: name{iname}, composite{icomposite} { }
	Type() { }

	string toString(Context *context);
};

namespace py {
	struct Argument {
		string name;
		string defl{""};
		bool hasDefault{false};

		Argument(string iname, string idefl = "", bool ihasDefault = false)
			: name{iname}, defl{idefl}, hasDefault{ihasDefault} { }

		Argument parse(string str) {
			size_t eq = str.find("=");
			if(eq == string::npos)
				return Argument{str};
			return Argument{
				util::trim(str.substr(0, eq)),
				util::trim(str.substr(eq + 1)),
				true
			};
		}

		string toString() { return name + (hasDefault ? " = " + defl : ""); }
	};
	enum class FunctionType { Free, Member, Class, Static };
	struct Function {
		string name;
		FunctionType type{FunctionType::Free};

		vector<Argument> arguments{};
		vector<string> body{};

		Function() : name{"{err}"} { cerr << "made bad func" << endl; throw -1; }
		Function(string iname, FunctionType ftype = FunctionType::Free)
			: name{iname}, type{ftype} { }

		void operator+=(string line) { body.push_back(line); }

		string toString(int extraTabs = 0) {
			string etabs(extraTabs, '\t');
			string res = "";
			if(type == FunctionType::Class)
				res += etabs + "@classmethod\n";
			if(type == FunctionType::Static)
				res += etabs += "@staticmethod\n";

			string earg = "";
			if(type == FunctionType::Member) earg = "self";
			if(type == FunctionType::Class) earg = "cls";
			if(!earg.empty() && !arguments.empty())
				earg += ", ";

			res += etabs + "def " + name + "(" + earg
				+ util::join(arguments, ", ", [](auto a) { return a.toString(); })
				+ "):\n";

			etabs += "\t";
			if(body.empty())
				return res + etabs + "pass";
			return res + util::join(body, "\n", [&etabs](auto l) {
						return etabs + l;
					});
		};
	};
	struct Class {
		string name;
		string parent{};
		// explicit members?
		vector<string> fnames{};
		map<string, Function> functions{};

		Class() : name{"{err}"} { cerr << "made bad class" << endl; throw -1; }
		Class(string iname) : name{iname} { }

		Function &operator[](string name) {
			if(!util::contains(functions, name)) {
				fnames.push_back(name);
				functions.emplace(name, Function{name, FunctionType::Member});
			}
			return functions[name];
		}

		string toString(int extraTabs = 0) {
			return string(extraTabs, '\t') + "class " + name
				+ (parent.empty() ? "" : "(" + parent + ")") + ":\n"
				+ util::join(fnames, "\n", [this, extraTabs](auto fn) {
						return functions[fn].toString(extraTabs + 1);
					});
		}
	};
	struct Context {
		vector<string> tnames{};
		map<string, Class> types{};

		Class &operator[](string name) {
			if(!util::contains(types, name)) {
				tnames.push_back(name);
				types.emplace(name, Class{name});
			}
			return types[name];
		}

		string toString() {
			return util::join(tnames, "\n\n", [this](auto tn) {
					return types[tn].toString();
				});
		}
	};
}

struct Context {
	map<string, Type> types{};
	string src{""};
	size_t idx{0};
	map<string, string> parentTypes{};
	map<string, int> typeDepth{};
	// TODO: need to account for ::id_t usage as weak dependency
	map<string, set<string>> weakDeps{};
	map<string, int> weakDepOrder{};
	map<string, string> pureChildFieldNames{};
	map<string, string> setChildFieldNames{};
	bool usedTranslatable{false};
	vector<sql::Table> tables{};

	Context(string isrc) : src{isrc} {
		types.emplace("int", Type{"int", false});
		types.emplace("bool", Type{"bool", false});
		types.emplace("string", Type{"string", false});
		types.emplace("set", Type{"set", false});
		types.emplace("map", Type{"map", false});
		types.emplace("vector", Type{"vector", false});

		parse();
		buildParentTypes();
		buildTypeDepths();

		cerr << "====" << endl;
		for(auto &pe : parentTypes) {
			cerr << pe.first << " is child of " << pe.second << endl;
		}

		// print pure child types
		for(auto &te : types) {
			if(te.second.pureChild)
				cerr << "pure child: " << te.second.name
					<< " => " << pureChildFieldNames[te.second.name] << endl;
		}

		genTables();
	}

	vector<string> getOrderedNonIdTypes() {
		size_t cnt = 0;
		map<int, vector<string>> levels{};
		for(auto &tde : typeDepth) {
			if(isIdType(tde.first))
				continue;
			levels[tde.second].push_back(tde.first);
			cnt++;
		}

		vector<string> ordered{};
		ordered.reserve(cnt);

		for(auto &level : levels)
			ordered.insert(ordered.end(), level.second.begin(), level.second.end());

		return ordered;
	}

	int getTypeDepth(string t) {
		int depth = 0;
		while(util::contains(parentTypes, t)) {
			depth++;
			t = parentTypes[t];
		}
		return depth;
	}
	void buildTypeDepths() {
		for(auto &te : types) {
			string bt = te.second.name;
			typeDepth[bt] = getTypeDepth(bt);
		}
	}

	void buildParentTypes() {
		for(auto &te : types) {
			if(!te.second.composite)
				continue;
			string bt = te.second.name;
			string idt = bt + "::id_t";
			parentTypes[idt] = bt;
			weakDeps[bt].insert(idt);
			for(auto &field : te.second.fields) {
				cerr << bt << " has " << types[field.type].name
					<< " (" << types[field.type].toString(this) << ")" << endl;
				if(types[field.type].name == "map") {
					string keyType = types[field.type].typeArguments[0];
					string valType = types[field.type].typeArguments[1];
					cerr << "\t" << types[valType].composite << endl;
					if(types[valType].composite) {
						parentTypes[valType] = bt;
						types[valType].pureChild = true;
						pureChildFieldNames[valType] = field.name;
					}
					weakDeps[bt].insert(valType);
					weakDeps[bt].insert(keyType);
				}
				if(types[field.type].name == "set") {
					string cType = types[field.type].typeArguments[0];
					parentTypes[cType] = bt;
					weakDeps[cType].insert(bt);
					setChildFieldNames[cType] = field.name;
				}
			}
		}
	}

	bool isspace() {
		char c = src[idx];
		return c == '\n' || c == '\t' || c == ' ';
	}
	void skipSpaces() {
		while(isspace())
			++idx;
		if(is("//")) {
			while(!is("\n"))
				idx++;
		}
	}
	bool is(string token) {
		if(idx + token.size() >= src.size())
			return false;
		for(size_t i = 0; i < token.size(); ++i)
			if(src[idx + i] != token[i])
				return false;
		return true;
	}
	void consume(string token) {
		skipSpaces();
		size_t ctx = 10;
		if(!is(token))
			throw "unable to consume " + token + " at " + to_string(idx) + "\n"
				+ src.substr(max((size_t)0, idx - ctx),
						min(src.size() - idx, ctx)) + "\n"
				+ string((int)ctx, '~') + "^";
		idx += token.size();
		skipSpaces();
	}

	string parseAttribute() {
		consume("[[");
		string ident = parseIdentifier();
		consume("]]");
		return ident;
	}

	string parseIdentifier() {
		skipSpaces();
		string ident = "";
		while(idx < src.size() && !isspace()
				&& src[idx] != '<' && src[idx] != '>'
				&& src[idx] != ']' && src[idx] != ';'
				&& src[idx] != ',') {
			ident += src[idx];
			idx++;
		}
		skipSpaces();
		return ident;
	}

	void declType(string type, bool composite = true) {
		if(util::contains(types, type))
			return;

		if(type.size() > 6
				&& type.substr(type.size() - 6) == "::id_t") {
			cerr << "declared type " << type << " as id_t, not composite" << endl;
			types[type] = Type{type, false};
		} else {
			cerr << "declared type " << type << " as composite" << endl;
			types[type] = Type{type, true};
		}
	}

	string parseCompoundIdentifier() {
		string baseIdent = parseIdentifier();
		if(is("<")) {
			consume("<");
			//cerr << "has compound identifier: " << baseIdent << endl;
			bool consumeComma = false;
			Type ttype{baseIdent, false};
			do {
				if(consumeComma)
					consume(",");
				string innerIdent = parseIdentifier();
				//cerr << "\t" << innerIdent << endl;
				declType(innerIdent);
				ttype.typeArguments.push_back(innerIdent);
				consumeComma = true;
			} while(is(","));
			consume(">");
			types[ttype.toString(this)] = ttype;
			return ttype.toString(this);
		}
		return baseIdent;
	}

	Field parseField() {
		string fieldType = parseCompoundIdentifier();
		string fieldName = parseIdentifier();
		consume(";");

		return Field{fieldType, fieldName};
	}

	void makeTranslatable(string typeName) {
		types[typeName].fields.push_back(Field{"string", "identifier"});
		string lmType = "map<Language::id_t, string>";
		types[lmType] = Type{"map", false};
		types[lmType].typeArguments.push_back("Language::id_t");
		types[lmType].typeArguments.push_back("string");
		types[typeName].fields.push_back(Field{lmType, "translations"});
	};
	void declStructType(string typeName, bool translatable = false) {
		declType(typeName);
		declType(typeName + "::id_t");
		types[typeName].fields.push_back(Field{typeName + "::id_t", "id"});

		if(translatable) {
			usedTranslatable = true;
			makeTranslatable(typeName);
		}
	}

	void parseType() {
		if(idx >= src.size()) return;

		bool translatable = false;
		vector<string> attributes{};
		while(src[idx] == '[') {
			string attr = parseAttribute();
			cerr << "attribute: " << attr << endl;
			if(attr == "translatable")
				translatable = true;
		}

		consume("struct");
		string typeName = parseIdentifier();
		cerr << "parsing type " << typeName << endl;

		declStructType(typeName, translatable);

		consume("{");

		skipSpaces();
		while(!is("}")) {
			auto f = parseField();
			types[typeName].fields.push_back(f);
			while(isspace() || is("//"))
				skipSpaces();
		}
		consume("}");
		consume(";");

		for(auto &field : types[typeName].fields) {
			cerr << "\t" << field.type << " => " << field.name << endl;
		}
	}

	void parse() {
		while(idx < src.size()) {
			skipSpaces();
			parseType();
		}

		if(usedTranslatable)
			declStructType("Language", true);
	}

	void genTables();
	vector<string> genPythonTypes();
	py::Context genPythonContext();

	vector<string> getParentTypeChain(string type) {
		vector<string> chain;
		chain.push_back(type);

		while(util::contains(parentTypes, type)) {
			type = parentTypes[type];
			chain.push_back(type);
		}
		return chain;
	}
};

string Type::toString(Context *context) {
	if(typeArguments.empty())
		return name;
	string str = name + "<" + context->types[typeArguments[0]].toString(context);
	for(int i = 1; i < typeArguments.size(); ++i)
		str += ", " + context->types[typeArguments[i]].toString(context);
	return str + ">";
}

void Context::genTables() {
	auto orderedNonIdTypes = getOrderedNonIdTypes();

	// in depth order, print create tables for struct types
	for(auto &typeName : orderedNonIdTypes) {
		auto &type = types[typeName];
		if(!type.composite || type.pureChild)
			continue;

		// our default key is added in constructor
		tables.emplace_back(typeName, true);
		auto &table = tables.back();

		auto chain = getParentTypeChain(typeName);
		// handle parent ids
		for(size_t i = chain.size() - 1; i > 0; --i)
			table.columns.emplace_back(chain[i], chain[i] + "::id_t", true);

		for(int i = 0; i < type.fields.size(); ++i) {
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

		string parentFieldName = pureChildFieldNames[typeName];
		Field parentField{"{null}", parentFieldName};
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
}

vector<string> Context::genPythonTypes() {
}
py::Context Context::genPythonContext() {
	py::Context ctx{};
	/*
	auto orderedNonIdTypes = getOrderedNonIdTypes();

	for(auto &typeName : orderedNonIdTypes) {
		auto &type = types[typeName];
		if(!type.composite) continue;

		vector<string> fpks = getParentTypeChain(type.name), pks{}, parents{};
		pks.reserve(fpks.size());
		pks.insert(pks.begin(), fpks.rbegin(), fpks.rend());
		parents.reserve(pks.size() - 1);
		parents.insert(parents.begin(), pks.begin(), pks.end() - 1);

		auto &pclass = ctx[type.name];
		auto &ctor = pclass["__init__"];
		Type *parentType{nullptr};
		Field *parentField{nullptr};
		Type *parentFieldType{nullptr};

		if(!parents.empty()) {
			for(auto &parent : parents) {
				ctor.arguments.push_back(toCamelCase(parent) + "ID");
				ctor +=
					"self." + toCamelCase(parent) + "ID"
					+ " = " + toCamelCase(parent) + "ID";
			}
			parentType = &types[parents.back()];
		}
		if(type.pureChild) {
			ctor += "# pure child";
			auto pcfname = pureChildFieldNames[type.name];
			for(auto &pf : types[parentTypes[type.name]].fields) {
				if(pf.name != pcfname)
					continue;
				parentField = &pf;
				parentFieldType = &types[parentField->type];
			}

			auto ktype = getBaseIdType(parentFieldType->typeArguments[0]);

			ctor.arguments.emplace_back(toCamelCase(ktype) + "ID");
			ctor += "self." + toCamelCase(ktype) + "ID = "
				+ toCamelCase(ktype) + "ID";
		} else {
			ctor +=
				"self." + toCamelCase(type.name) + "ID = util.randomString(6)"
					+ " if " + toCamelCase(type.name) + "ID is None else "
					+ toCamelCase(type.name) + "ID";
		}

		auto &loadAll = pclass["loadAll"];
		for(auto &field : type.fields) {
			if(field.name == "id") continue; // skip self id
			auto &ftype = types[field.type];
			if(ftype.name == "map") {
				auto &stype = types[ftype.typeArguments[1]];
				auto btype = getBaseIdType(ftype.typeArguments[0]);
				auto &loadMember = pclass[toCamelCase("load_" + field.name)];
				loadAll += "self." + loadMember.name + "()";
				if(!stype.composite) {
					loadMember += "tmp = schema."
						+ toCamelCase("_" + type.name + "_" + field.name)
						+ ".select({"
						+ "'" + tableCase(type.name) + "_id': "
						+ "self." + toCamelCase(type.name)
						+ "})";
					loadMember += "self." + field.name + " = "
						+ "{ t." + tableCase(btype) + "_id: t.val for t in tmp }";
				} else {
					loadMember += "tmp = " + stype.name + ".findAll("
						+ util::join(pks, ", ", [](auto p) {
								return toCamelCase(p) + "ID";
							}) + ")";
					loadMember += "self." + field.name + " = "
						+ "{ t." + toCamelCase(btype) + "ID: t for t in tmp }";
				}
				ctor += "self." + field.name + " = {} # " + field.type;
				continue;
			}
			if(ftype.name == "vector") {
				auto btype = ftype.typeArguments[0];
				auto &loadMember = pclass[toCamelCase("load_" + field.name)];
				loadAll += "self." + loadMember.name + "()";
				loadMember += "tmp = schema."
					+ toCamelCase("_" + type.name + "_" + field.name)
					+ ".select({"
					+ "'" + tableCase(type.name) + "_id': "
					+ "self." + toCamelCase(type.name)
					+ "})";
				loadMember += "self." + field.name + " = "
					+ "[t." + tableCase(btype) + "_id for t in tmp]";

				ctor += "self." + field.name + " = [] # " + field.type;
				continue;
			}
			if(ftype.name == "set") {
				auto &loadMember = pclass[toCamelCase("load_" + field.name)];
				loadAll += "self." + loadMember.name + "()";
				loadMember +=
						"self." + field.name + " = " + ftype.typeArguments[0]
						+ ".findAll("
						+ util::join(pks, ", ", [](auto p) {
							return toCamelCase(p) + "ID";
							}) + ")";

				ctor += "self." + field.name + " = [] # " + field.type;
				continue;
			}
			string val = "None";
			if(field.type == "int") val = "0";
			if(field.type == "bool") val = "False";
			if(field.type == "string") val = "''";

			ctor.arguments.emplace_back(field.name, val, true);

			ctor += "self." + field.name + " = " + field.name + " # " + field.type;
		}

		if(!type.pureChild)
			ctor.arguments.push_back({toCamelCase(type.name) + "ID", "None", true});

		auto &findAll = pclass["findAll"];
		findAll.type = py::FunctionType::Class;
		findAll.arguments.reserve(parents.size());
		for(auto &parent : parents) {
			string ident = toCamelCase(parent) + "ID";
			findAll.arguments.push_back({ident, "None", true});
		}

		findAll.body.push_back(
			"return [cls.fromLite(o) for o in schema."
			+ type.name + ".select({"
			+ util::join(parents, ", ", [](auto p) {
					return "'" + tableCase(p) + "_id': " + toCamelCase(p) + "ID";
				})
			+ "})");

		auto &fromLite = pclass["fromLite"];
		fromLite.type = py::FunctionType::Class;
		fromLite.arguments.push_back({"flite"});
		vector<string> fliteArgs{};
		for(auto &parent : parents)
			fliteArgs.push_back(tableCase(parent) + "_id");
		if(type.pureChild) {
			auto ktype = getBaseIdType(parentFieldType->typeArguments[0]);
			fliteArgs.push_back(tableCase(ktype) + "_id");
		}
		for(auto &field : type.fields) {
			if(field.name == "id") continue; // skip self id
			auto &ftype = types[field.type];
			if(ftype.name == "vector"
					|| ftype.name == "set"
					|| ftype.name == "map") {
				continue; // these can be loaded manually
			}
			if(isIdType(field.name)) {
				fliteArgs.push_back("None TODO");
			} else {
				fliteArgs.push_back(tableCase(field.name));
			}
		}
		if(!type.pureChild) {
			fliteArgs.push_back(tableCase(type.name) + "_id");
		}
		fromLite +=
			"return cls(" + util::join(fliteArgs, ", ", [](auto a) {
					return "flite." + a;
				}) + ")";
	}
	*/
	return ctx;
}

int main(int argc, char **argv) {
	if(argc < 2) {
		cout << "usage: ./schema_parse <schema.cpp>" << endl;
		return 0;
	}

	bool genSchema = true, genPython = false;
	if(argc > 2 && string{argv[2]} == "python")
		genSchema = false, genPython = true;

	ifstream in{argv[1]};
	string text{""}, line{""};
	while(getline(in, line))
		text += line + "\n";

	try {
		Context ctx{text};
		if(genSchema) {
			cout << "-- schema generated from " << argv[1] << endl;
			for(auto &table : ctx.tables)
				cout << table.toString() << endl;
		}

		if(genPython) {
			ctx.genTables();

			ofstream schema("./schema.py");
			schema << "from lite import StoreType" << endl
				<< "import util" << endl
				<< endl;
			/*
			for(auto &table : ctx.tables) {
				cout << pluralize(toCamelCase(table.name)) << "Table = '''" << endl
					<< table.format() << endl
					<< "'''" << endl;
			}
			cout << endl << "tables = [" << endl;
			for(auto &table : ctx.tables) {
				cout << "\t('" << pluralize(tableCase(table.name)) << "', "
					<< pluralize(toCamelCase(table.name)) << "Table)," << endl;
			}
			cout << "]" << endl;
			*/

			// hasKeys are special...
			/*
			map<string, int> prefixUsages{};
			for(auto &table : ctx.tables) {
				if(!table.hasKey) {
					schema << "# noKey: " << pluralize(tableCase(table.name))
						<< " : " << table.name << endl;
					schema << "class " << toCamelCase("_" + table.name) << "(StoreType):" << endl
						<< "\tpass" << endl
						<< endl;
					continue;
				}

				continue; // has keys handled down below in python ctx?
				string prefix = string{tolower(table.name.front())};
				int cnt = prefixUsages[prefix]++;
				prefix += string{(char)('0' + cnt)};

				cout << "# hasKey: " << pluralize(tableCase(table.name))
					<< " : " << table.name
					<< " : " << prefix << endl;
				cout << "class " << table.name << "(StoreType):" << endl
					<< "\tdef __init__(self):" << endl
					<< "\t\tself." << tableCase(table.name) << "_id "
					<< "= '" << prefix << "' + util.randomString(6)" << endl
					<< endl
					<< endl;
			}
			*/
			for(auto &table : ctx.tables) {
				cout << "class " << toCamelCase("_" + table.name)
					<< "(StoreType):" << endl
					<< "\tpass" << endl
					<< endl;
				schema << "class " << toCamelCase("_" + table.name)
					<< "(StoreType):" << endl
					<< "\tpass" << endl
					<< endl;
			}

			cout << endl
				<< "# =========================================" << endl;
			auto pctx = ctx.genPythonContext();
			cout << pctx.toString() << endl;
			ofstream store("./store.py");
			store << "import schema" << endl
				<< pctx.toString() << endl;
		}
	} catch(string &ex) {
		cerr << ex << endl;
	}
	return 0;
}

