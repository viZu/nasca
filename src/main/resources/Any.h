#ifndef SCALA_ANY_H
#define SCALA_ANY_H

#include <memory>

namespace scala {

	class Any {

		public:
			bool __bang__eq(std::shared_ptr<scala::Any>);
			bool __eq__eq(std::shared_ptr<scala::Any>);
			bool equals(std::shared_ptr<scala::Any>);
			int hashCode();

	};
	
}

#endif