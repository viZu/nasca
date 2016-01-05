#ifndef ROOT_SCALA_ANY_CPP
#define ROOT_SCALA_ANY_CPP

#include "Any.h"

bool _root__scala::Any::__bang__eq(std::shared_ptr<_root__scala::Any> ptr) {
	return !this->equals(ptr);
}

bool _root__scala::Any::__eq__eq(std::shared_ptr<_root__scala::Any> ptr) {
	return this->equals(ptr);
}

bool _root__scala::Any::equals(std::shared_ptr<_root__scala::Any> ptr) {
	return this == ptr.get();
}

int _root__scala::Any::hashCode() {
	return 0;
}

#endif