struct User {
	string name;
	string hash;
	int lastLogin;
};
struct Department {
	set<User> employees;
};
