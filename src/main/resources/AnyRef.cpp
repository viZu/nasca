#ifndef ROOT_SCALA_ANYREF_CPP
#define ROOT_SCALA_ANYREF_CPP

#include "AnyRef.h"

bool _root__scala::AnyRef::eq(std::shared_ptr<_root__scala::Any> ptr) {
	return this == ptr.get();
}

bool _root__scala::AnyRef::ne(std::shared_ptr<_root__scala::Any> ptr) {
	return this != ptr.get();
}

#endif