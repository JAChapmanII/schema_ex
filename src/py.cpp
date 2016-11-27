#include "py.hpp"
using std::string;
using std::vector;
using std::map;

#include <oil/util.hpp>

#include "sql.hpp"

py::Argument::Argument(string iname, string idefl, bool ihasDefault)
	: name{iname}, defl{idefl}, hasDefault{ihasDefault} { }

py::Argument py::Argument::parse(string str) {
	size_t eq = str.find("=");
	if(eq == string::npos)
		return Argument{str};
	return Argument{
		util::trim(str.substr(0, eq)),
		util::trim(str.substr(eq + 1)),
		true
	};
}

string py::Argument::toString() {
	return name + (hasDefault ? " = " + defl : "");
}


py::Function::Function() : name{"{err}"} {
	throw make_except("made bad func (no name)");
}
py::Function::Function(string iname, FunctionType ftype)
	: name{iname}, type{ftype} { }

void py::Function::operator+=(string line) { body.push_back(line); }

string py::Function::toString(int extraTabs) {
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
}


py::Class::Class() : name{"{err}"} {
	throw make_except("made bad class (no name)");
}
py::Class::Class(string iname) : name{iname} { }

py::Function &py::Class::operator[](string name) {
	if(!util::contains(functions, name)) {
		fnames.push_back(name);
		functions.emplace(name, Function{name, FunctionType::Member});
	}
	return functions[name];
}

string py::Class::toString(int extraTabs) {
	return string(extraTabs, '\t') + "class " + name
		+ (parent.empty() ? "" : "(" + parent + ")") + ":\n"
		+ util::join(fnames, "\n", [this, extraTabs](auto fn) {
				return functions[fn].toString(extraTabs + 1);
			});
}


py::Class &py::Context::operator[](string name) {
	if(!util::contains(types, name)) {
		tnames.push_back(name);
		types.emplace(name, Class{name});
	}
	return types[name];
}

string py::Context::toString() {
	return util::join(tnames, "\n\n", [this](auto tn) {
			return types[tn].toString();
		});
}


py::Context py::Context::fromPContext(parser::Context &fctx) {
	py::Context ctx{};
	auto orderedNonIdTypes = fctx.getOrderedNonIdTypes();
	auto &types = fctx.types;
	auto &parentTypes = fctx.parentTypes;

	for(auto &typeName : orderedNonIdTypes) {
		auto &type = types[typeName];
		if(!type.composite) continue;

		vector<string> fpks = fctx.getParentTypeChain(type.name),
			pks{}, parents{};
		pks.reserve(fpks.size());
		pks.insert(pks.begin(), fpks.rbegin(), fpks.rend());
		parents.reserve(pks.size() - 1);
		parents.insert(parents.begin(), pks.begin(), pks.end() - 1);

		auto &pclass = ctx[type.name];
		auto &ctor = pclass["__init__"];
		parser::Field *parentField{nullptr};
		parser::Type *parentFieldType{nullptr};

		if(!parents.empty()) {
			for(auto &parent : parents) {
				ctor.arguments.push_back(toIdColumn(parent));
				ctor += "self." + toIdColumn(parent) + " = " + toIdColumn(parent);
			}
		}
		if(type.pureChild) {
			ctor += "# pure child";
			auto pcfname = fctx.pureChildFieldNames[type.name];
			for(auto &pf : types[parentTypes[type.name]].fields) {
				if(pf.name != pcfname)
					continue;
				parentField = &pf;
				parentFieldType = &types[parentField->type];
			}

			auto ktype = getBaseIdType(parentFieldType->typeArguments[0]);

			ctor.arguments.emplace_back(toCamelCase(ktype) + "Id");
			ctor += "self." + toCamelCase(ktype) + "Id = "
				+ toCamelCase(ktype) + "Id";
		} else {
			ctor +=
				"self." + toCamelCase(type.name) + "Id = util.randomString(6)"
					+ " if " + toCamelCase(type.name) + "Id is None else "
					+ toCamelCase(type.name) + "Id";
		}

		bool hasSimpleFields = false;
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
					auto backingType = toCamelCase(
							"_" + type.name + "_" + field.name);
					auto backingTypeId = toCamelCase(
							field.name + "_" + toIdColumn(btype));
					loadMember += "tmp = schema." + backingType
						+ ".select({"
						+ "'" + toIdColumn(type.name) + "': "
							+ "self." + toIdColumn(type.name)
						+ "})";
					loadMember += "self." + field.name + " = "
						+ "{ t." + backingTypeId + ": t.val for t in tmp }";

					auto &setMember = pclass[toCamelCase(
							"set_" + singularize(field.name))];
					string oKey = toCamelCase(field.name + "_" + btype) + "Id",
							schemaType = "schema."
								+ toCamelCase("_" + type.name + "_" + field.name);
					setMember.arguments.push_back({oKey});
					setMember.arguments.push_back({"val"});
					setMember += "self." + field.name + "[" + oKey + "] = val";
					setMember += "tmp = " + schemaType
						+ ".fromDict({"
						+ "'" + toCamelCase(type.name) + "Id': self."
						+ toCamelCase(type.name) + "Id, "
						+ "'" + oKey + "': " + oKey + ", "
						+ "'val': val"
						+ "})";
					setMember += "tmp.upsert()";

					auto &delMember = pclass[toCamelCase(
							"del_" + singularize(field.name))];
					delMember.arguments.push_back({oKey});
					delMember += "self." + field.name + ".pop(" + oKey + ", None)";
					delMember += schemaType + ".delSingle((self."
						+ toCamelCase(type.name) + "Id, " + oKey + ",))";
				} else {
					loadMember += "tmp = " + stype.name + ".findAll("
						+ util::join(pks, ", ", [](auto p) {
								return toCamelCase(p) + "Id";
							}) + ")";
					loadMember += "self." + field.name + " = "
						+ "{ t." + toCamelCase(btype) + "Id: t for t in tmp }";

					auto &addMember = pclass[toCamelCase(
							"add_" + singularize(field.name))];
					addMember += "# TODO";
					addMember += "pass";
				}
				ctor += "self." + field.name + " = {} # " + field.type;
				continue;
			}
			if(ftype.name == "vector") {
				auto btype = ftype.typeArguments[0];
				auto &loadMember = pclass[toCamelCase("load_" + field.name)];
				loadAll += "self." + loadMember.name + "()";
				loadMember += "# vector";
				auto schemaType = "schema."
					+ toCamelCase("_" + type.name + "_" + field.name);
				loadMember += "tmp = " + schemaType
					+ ".select({"
					+ "'" + toIdColumn(type.name) + "': " + toIdColumn(type.name)
					+ "})";
				loadMember += "self." + field.name + " = "
					+ "[t." + toIdColumn(btype) + " for t in tmp]";

				ctor += "self." + field.name + " = [] # " + field.type;

				auto &addMember = pclass[toCamelCase(
						"add_" + singularize(field.name))];
				addMember += "# vector";
				addMember.arguments.emplace_back(toIdColumn(btype));
				//addMember += "self." + field.name + " += [" + toIdColumn(btype) + "]";
				addMember += "tmp = " + schemaType + ".fromDict({"
					+ "'" + toIdColumn(type.name) + "': " + toIdColumn(type.name)
					+ ", '" + toIdColumn(btype) + "': " + toIdColumn(btype)
					+ "})";
				addMember += "tmp.upsert()";
				addMember += "self." + loadMember.name + "()";

				auto &delMember = pclass[toCamelCase(
						"del_" + singularize(field.name))];
				delMember += "# TODO vector";
				delMember.arguments.emplace_back(toIdColumn(btype));
				delMember += schemaType + ".delSingle(("
					+ toIdColumn(type.name) + ", " + toIdColumn(btype)
					+ ",))";
				delMember += "self." + loadMember.name + "()";

				continue;
			}
			if(ftype.name == "set") {
				auto &loadMember = pclass[toCamelCase("load_" + field.name)];
				loadAll += "self." + loadMember.name + "()";
				loadMember +=
						"self." + field.name + " = " + ftype.typeArguments[0]
						+ ".findAll("
					+ util::join(pks, ", ", [](auto p) {
							return toCamelCase(p) + "Id";
							}) + ")";

				ctor += "self." + field.name + " = [] # " + field.type;

				auto &addMember = pclass[toCamelCase(
						"add_" + singularize(field.name))];
				addMember += "# TODO";
				addMember += "pass";
				continue;
			}
			// TODO: helper this?
			string val = "None";
			if(field.type == "int") val = "0";
			if(field.type == "bool") val = "False";
			if(field.type == "string") val = "''";

			ctor.arguments.emplace_back(field.name, val, true);

			ctor += "self." + field.name + " = " + field.name + " # " + field.type;
			hasSimpleFields = true;
		}

		if(hasSimpleFields) {
			auto &upsert = pclass["upsert"];
			upsert += "tmp = schema." + pluralize(type.name) + ".fromObject(self)";
			upsert += "tmp.upsert()";
			/*
			for(auto &field : type.fields) {
				if(field.name == "id") continue; // skip self id
				auto &ftype = types[field.type];
				// TODO: helper this
				if(type.name == "set" || ftype.name == "map"
						|| ftype.name == "vector") {
					continue;
				}
				*/
		}

		if(!type.pureChild)
			ctor.arguments.push_back({toCamelCase(type.name) + "Id", "None", true});

		auto &findAll = pclass["findAll"];
		findAll.type = py::FunctionType::Class;
		findAll.arguments.reserve(parents.size());
		for(auto &parent : parents) {
			string ident = toCamelCase(parent) + "Id";
			findAll.arguments.push_back({ident, "None", true});
		}

		findAll.body.push_back(
			"return [cls.fromLite(o) for o in schema."
			+ pluralize(type.name) + ".select({"
			+ util::join(parents, ", ", [](auto p) {
					return "'" + toIdColumn(p) + "': " + toIdColumn(p);
				})
			+ "})]");

		auto &fromLite = pclass["fromLite"];
		fromLite.type = py::FunctionType::Class;
		fromLite.arguments.push_back({"flite"});
		vector<string> fliteArgs{};
		for(auto &parent : parents)
			fliteArgs.push_back(toIdColumn(parent));
		if(type.pureChild) {
			auto ktype = getBaseIdType(parentFieldType->typeArguments[0]);
			fliteArgs.push_back(toIdColumn(ktype));
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
				fliteArgs.push_back(field.name);
			}
		}
		if(!type.pureChild) {
			fliteArgs.push_back(toIdColumn(type.name));
		}
		fromLite +=
			"return cls(" + util::join(fliteArgs, ", ", [](auto a) {
					return "flite." + a;
				}) + ")";
	}
	return ctx;
}

