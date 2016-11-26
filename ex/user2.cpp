struct User {
	string name;
	string hash;
	int lastLogin;
};

struct Application { };
struct AppUser {
	User::id_t user;
	Application::id_t application;
	int level;
};
