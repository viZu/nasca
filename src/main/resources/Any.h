#ifndef ROOT_SCALA_ANY_H
#define ROOT_SCALA_ANY_H

#include <memory>

namespace _root__scala {

	class Any {

		public:
			bool __bang__eq(std::shared_ptr<_root__scala::Any>);
			bool __eq__eq(std::shared_ptr<_root__scala::Any>);
			bool equals(std::shared_ptr<_root__scala::Any>);
			int hashCode();

	};
	
}

#endif