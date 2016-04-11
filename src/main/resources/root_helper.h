#include <string>
#include <sstream>
#include <vector>
#include <fstream>
#include <memory>

std::vector<std::string> &split(const std::string &s, char delim, std::vector<std::string> &elems) {
    std::stringstream ss(s);
    std::string item;
    while (std::getline(ss, item, delim)) {
        elems.push_back(item);
    }
    return elems;
}


std::shared_ptr<std::vector<std::string> > splitString(const std::string &s, char delim) {
    std::vector<std::string> *elems = new std::vector<std::string>();
    split(s, delim, *elems);
    return std::shared_ptr<std::vector<std::string>>(elems);
}

std::string readFile(const std::string filename) {
    std::ifstream t(filename);
    std::stringstream buffer;
    buffer << t.rdbuf();
    return buffer.str();
}