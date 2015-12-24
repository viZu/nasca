#ifndef SCALA_ANY_CPP
#define SCALA_ANY_CPP

#include "Any.h"

bool scala::Any::__bang__eq(std::shared_ptr<scala::Any> ptr) {
	return !this->equals(ptr);
}

bool scala::Any::__eq__eq(std::shared_ptr<scala::Any> ptr) {
	return this->equals(ptr);
}

bool scala::Any::equals(std::shared_ptr<scala::Any> ptr) {
	return this == ptr.get();
}

int scala::Any::hashCode() {
	return 0;
}

#endif