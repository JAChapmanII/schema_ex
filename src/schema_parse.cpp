#include <iostream>
#include <fstream>
#include <string>
#include <vector>
using namespace std;

#include <oil/util.hpp>
#include <oil/err.hpp>

#include "parser.hpp"
#include "sql.hpp"
#include "py.hpp"

void printSchemaInfoAsPython(vector<sql::Table> &tables, ostream &out) {
	for(auto &table : tables) {
		out << toCamelCase(table.name) << "Table = '''" << endl
			<< table.toString() << endl
			<< "'''" << endl;
	}
	out << endl << "tables = [" << endl;
	for(auto &table : tables) {
		out << "\t('" << table.name << "', "
			<< pluralize(toCamelCase(table.name)) << "Table)," << endl;
	}
	out << "]" << endl;
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
		parser::Context ctx{text};
		auto tables = sql::genTables(ctx);

		if(genSchema) {
			cout << "-- schema generated from " << argv[1] << endl;
			for(auto &table : tables)
				cout << table.toString() << endl;
		}

		if(genPython) {
			ofstream schema("./schema.py");
			schema << "from lite import StoreType" << endl
				<< "import util" << endl
				<< endl;

			//printSchemaInfoAsPython(ctx, cout);

			for(auto &table : tables) {
				cout << "class " << table.name << "(StoreType):" << endl
					<< "\tpass" << endl
					<< endl;
				schema << "class " << table.name << "(StoreType):" << endl
					<< "\tpass" << endl
					<< endl;
			}

			cout << endl
				<< "# =========================================" << endl;
			auto pctx = py::Context::fromPContext(ctx);
			cout << pctx.toString() << endl << endl;
			ofstream store("./store.py");
			store << "import schema" << endl
				<< "import util" << endl
				<< pctx.toString() << endl;
		}
	} catch(string &ex) {
		cerr << ex << endl;
	}
	return 0;
}

