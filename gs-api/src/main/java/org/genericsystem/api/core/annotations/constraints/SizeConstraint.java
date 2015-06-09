package org.genericsystem.api.core.annotations.constraints;

import java.lang.annotation.ElementType;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

import org.genericsystem.api.core.ApiStatics;

/**
 * The System Property to allow a fixed number of links for a relation.
 * 
 * @author Nicolas Feybesse
 * @author Michael Ory
 */
@Retention(RetentionPolicy.RUNTIME)
@Target({ ElementType.TYPE })
public @interface SizeConstraint {
	/**
	 * The positions of the components.
	 * 
	 * @return An array of component positions.
	 */
	int[] value() default { ApiStatics.BASE_POSITION };
}
