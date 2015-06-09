package org.genericsystem.api.core.annotations;

import java.lang.annotation.ElementType;
import java.lang.annotation.Inherited;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

/**
 * Indicates the dependencies of a generic.
 * 
 * @author Nicolas Feybesse
 * @author Michael Ory
 */
@Retention(RetentionPolicy.RUNTIME)
@Target({ ElementType.TYPE })
@Inherited
public @interface Dependencies {
	/**
	 * The classes of the dependencies of a generic.
	 * 
	 * @return An array of classes of the dependencies of a generic.
	 */
	Class<?>[] value();
}
