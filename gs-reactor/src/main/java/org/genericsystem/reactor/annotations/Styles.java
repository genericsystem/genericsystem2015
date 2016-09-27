package org.genericsystem.reactor.annotations;

import java.lang.annotation.ElementType;
import java.lang.annotation.Inherited;
import java.lang.annotation.Repeatable;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

import org.genericsystem.reactor.az.FlexDirection;

@Retention(RetentionPolicy.RUNTIME)
@Target({ ElementType.TYPE })
@Inherited
public @interface Styles {
	Style[] value();

	@Retention(RetentionPolicy.RUNTIME)
	@Target({ ElementType.TYPE })
	@Inherited
	@Repeatable(Styles.class)
	public @interface Style {
		String propertyName();

		String propertyValue();
	}

	@Retention(RetentionPolicy.RUNTIME)
	@Target({ ElementType.TYPE })
	@Inherited
	public @interface FlexDirectionStyle {
		FlexDirection value();
	}

	@Retention(RetentionPolicy.RUNTIME)
	@Target({ ElementType.TYPE })
	@Inherited
	public @interface KeepFlexDirection {
	}

	@Retention(RetentionPolicy.RUNTIME)
	@Target({ ElementType.TYPE })
	@Inherited
	public @interface ReverseFlexDirection {
	}

	@Retention(RetentionPolicy.RUNTIME)
	@Target({ ElementType.TYPE })
	@Inherited
	public @interface ParentFlexDirection {
		int pos() default 0;
	}

	@Retention(RetentionPolicy.RUNTIME)
	@Target({ ElementType.TYPE })
	@Inherited
	public @interface ChildFlexDirection {
		String[] value();
	}

	@Retention(RetentionPolicy.RUNTIME)
	@Target({ ElementType.TYPE })
	@Inherited
	public @interface Flex {
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
	public @interface GenericBackgroundColor {
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

	@Retention(RetentionPolicy.RUNTIME)
	@Target({ ElementType.TYPE })
	@Inherited
	public @interface Overflow {
		String value();
	}

	@Retention(RetentionPolicy.RUNTIME)
	@Target({ ElementType.TYPE })
	@Inherited
	public @interface Color {
		String value();
	}

	@Retention(RetentionPolicy.RUNTIME)
	@Target({ ElementType.TYPE })
	@Inherited
	public @interface MarginRight {
		String value();
	}

	@Retention(RetentionPolicy.RUNTIME)
	@Target({ ElementType.TYPE })
	@Inherited
	public @interface MarginBottom {
		String value();
	}

	@Retention(RetentionPolicy.RUNTIME)
	@Target({ ElementType.TYPE })
	@Inherited
	public @interface Height {
		String value();
	}

	@Retention(RetentionPolicy.RUNTIME)
	@Target({ ElementType.TYPE })
	@Inherited
	public @interface Width {
		String value();
	}
}
