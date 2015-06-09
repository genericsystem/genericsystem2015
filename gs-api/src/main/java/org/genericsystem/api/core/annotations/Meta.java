package org.genericsystem.api.core.annotations;

import java.lang.annotation.ElementType;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

/**
 * Indicates the Extends of a generic. A generic inherits directly of the supers.
 *
 * @author Nicolas Feybesse
 * @author Michael Ory
 */
@Retention(RetentionPolicy.RUNTIME)
@Target({ ElementType.TYPE })
public @interface Meta {
	/**
	 * The meta class.
	 *
	 * @return the meta class.
	 */
	Class<?> value();
}
