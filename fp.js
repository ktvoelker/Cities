
/**
 * fp.js
 *
 * A type-checked functional programming framework for JavaScript.
 *
 * Karl Voelker <ktvoelker@gmail.com>
 */

/**
 * Principles for Getting Along with the System
 *
 * 1. Never say "new", "typeof", or "instanceof".
 * 2. Methods are messy. Only use "lookup" syntax to get at algebraic value members.
 * 3. Wrap every function with a typechecker.
 */

/**
 * The type system includes:
 *
 * 1. The JS primitive types number, boolean, and string.
 * 2. Algebraic types created by the system.
 * 3. Function types created by the system.
 * 4. The type of type values created by the system.
 * 5. The type of type classes created by the system.
 * 6. The type of type class instances created by the system.
 * 7. Unknown JS objects.
 *
 * Note A. Types in categories 2-6 can take type parameters.
 *
 * Note B. Function types can be variadic. A variadic function's last parameter
 * is a specially-declared list-typed parameter which contains the values of
 * all the trailing arguments.
 *
 * Note C. Primitive JS functions have the type (*Unknown) -> Unknown.
 *
 * Note D. Since everything is dynamic, there's no need to have a separate parameter
 * list for type parameters. They're just regular parameters!
 */

/**
 * Type classes
 *
 * 1. A type class exists as an object.
 *
 * 2. A type class instance exists as an object.
 *
 * 3. For an instance to be in effect, it must be registered with the type class.
 *
 * 4. Instance registration can be done globally, once per instance type.
 *
 * 5. Instance registration can also be done locally by providing the instance
 * and a callback to a special function. The instance then overrides any global or
 * outer local instance that had been in place.
 */

