package org.genericsystem.api.core.annotations.constraints;

import java.lang.annotation.ElementType;
import java.lang.annotation.Inherited;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

/**
 * The System Property to constrain the type of a value.
 * 
 * @author Nicolas Feybesse
 */
@Retention(RetentionPolicy.RUNTIME)
@Target({ ElementType.TYPE })
@Inherited
public @interface NoInheritance {
}
