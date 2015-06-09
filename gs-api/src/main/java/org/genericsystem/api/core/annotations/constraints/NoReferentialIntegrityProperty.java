package org.genericsystem.api.core.annotations.constraints;

import java.lang.annotation.ElementType;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

/**
 * The System Property for the opposite of the referential integrity.
 * 
 * @author Nicolas Feybesse
 */
@Retention(RetentionPolicy.RUNTIME)
@Target({ ElementType.TYPE })
public @interface NoReferentialIntegrityProperty {
	/**
	 * The positions of the components.
	 * 
	 * @return An array of component positions.
	 */
	int[] value();
}
