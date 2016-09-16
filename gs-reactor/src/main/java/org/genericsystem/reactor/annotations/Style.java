package org.genericsystem.reactor.annotations;

import java.lang.annotation.ElementType;
import java.lang.annotation.Inherited;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

/**
 * @author Nicolas Feybesse
 *
 */
@Retention(RetentionPolicy.RUNTIME)
@Target({ ElementType.TYPE })
public @interface Style {
	String propertyName();

	String propertyValue();

	@Retention(RetentionPolicy.RUNTIME)
	@Target({ ElementType.TYPE })
	@Inherited
	public @interface FlexDirection {
		String value();
	}

	@Retention(RetentionPolicy.RUNTIME)
	@Target({ ElementType.TYPE })
	@Inherited
	public @interface FlexGrow {
		String value();
	}

	@Retention(RetentionPolicy.RUNTIME)
	@Target({ ElementType.TYPE })
	@Inherited
	public @interface FlexWrap {
		String value();
	}

	@Retention(RetentionPolicy.RUNTIME)
	@Target({ ElementType.TYPE })
	@Inherited
	public @interface BackgroundColor {
		String value();
	}

	@Retention(RetentionPolicy.RUNTIME)
	@Target({ ElementType.TYPE })
	@Inherited
	public @interface AlignItems {
		String value();
	}

	@Retention(RetentionPolicy.RUNTIME)
	@Target({ ElementType.TYPE })
	@Inherited
	public @interface JustifyContent {
		String value();
	}

}
