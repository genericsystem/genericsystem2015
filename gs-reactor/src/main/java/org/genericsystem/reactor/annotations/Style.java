package org.genericsystem.reactor.annotations;

import java.lang.annotation.ElementType;
import java.lang.annotation.Repeatable;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

import org.genericsystem.reactor.annotations.Style.Styles;
import org.genericsystem.reactor.gscomponents.FlexDirection;
import org.genericsystem.reactor.gscomponents.GSTagImpl;

@Retention(RetentionPolicy.RUNTIME)
@Target({ ElementType.TYPE })
@Repeatable(Styles.class)
public @interface Style {
	Class<? extends GSTagImpl>[] path() default {};

	String name();

	String value();

	@Retention(RetentionPolicy.RUNTIME)
	@Target({ ElementType.TYPE })
	public @interface Styles {
		Style[] value();
	}

	@Retention(RetentionPolicy.RUNTIME)
	@Target({ ElementType.TYPE })
	@Repeatable(FlexDirections.class)
	public @interface FlexDirectionStyle {
		Class<? extends GSTagImpl>[] path() default {};

		FlexDirection value();
	}

	@Retention(RetentionPolicy.RUNTIME)
	@Target({ ElementType.TYPE })
	public @interface FlexDirections {
		FlexDirectionStyle[] value();
	}

	@Retention(RetentionPolicy.RUNTIME)
	@Target({ ElementType.TYPE })
	@Repeatable(KeepFlexDirections.class)
	public @interface KeepFlexDirection {
		Class<? extends GSTagImpl>[] path() default {};
	}

	@Retention(RetentionPolicy.RUNTIME)
	@Target({ ElementType.TYPE })
	public @interface KeepFlexDirections {
		KeepFlexDirection[] value();
	}

	@Retention(RetentionPolicy.RUNTIME)
	@Target({ ElementType.TYPE })
	@Repeatable(ReverseFlexDirections.class)
	public @interface ReverseFlexDirection {
		Class<? extends GSTagImpl>[] path() default {};
	}

	@Retention(RetentionPolicy.RUNTIME)
	@Target({ ElementType.TYPE })
	public @interface ReverseFlexDirections {
		ReverseFlexDirection[] value();
	}

	@Retention(RetentionPolicy.RUNTIME)
	@Target({ ElementType.TYPE })
	@Repeatable(GenericValueBackgroundColors.class)
	public @interface GenericValueBackgroundColor {
		Class<? extends GSTagImpl>[] path() default {};

		String value();
	}

	@Retention(RetentionPolicy.RUNTIME)
	@Target({ ElementType.TYPE })
	public @interface GenericValueBackgroundColors {
		GenericValueBackgroundColor[] value();
	}
}
