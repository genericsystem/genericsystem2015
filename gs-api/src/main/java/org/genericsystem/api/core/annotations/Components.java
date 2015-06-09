package org.genericsystem.api.core.annotations;

import java.lang.annotation.ElementType;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

/**
 * Indicates the components of a generic.
 * 
 * @author Nicolas Feybesse
 * @author Michael Ory
 */
@Retention(RetentionPolicy.RUNTIME)
@Target({ ElementType.TYPE })
public @interface Components {
	/**
	 * The classes of the components of a generic.
	 * 
	 * @return An array of classes of the components of a generic.
	 */
	Class<?>[] value();
}
