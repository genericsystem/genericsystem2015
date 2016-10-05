package org.genericsystem.reactor.annotations;

import java.lang.annotation.ElementType;
import java.lang.annotation.Repeatable;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

import org.genericsystem.reactor.ca_gscomponents.FlexDirection;
import org.genericsystem.reactor.ca_gscomponents.GSTagImpl;

@Retention(RetentionPolicy.RUNTIME)
@Target({ ElementType.TYPE })
public @interface Styles {
	Style[] value();

	@Retention(RetentionPolicy.RUNTIME)
	@Target({ ElementType.TYPE })
	@Repeatable(Styles.class)
	public @interface Style {
		Class<? extends GSTagImpl>[] path() default {};

		String name();

		String value();
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
	@Repeatable(Flexs.class)
	public @interface Flex {
		Class<? extends GSTagImpl>[] path() default {};

		String value();
	}

	@Retention(RetentionPolicy.RUNTIME)
	@Target({ ElementType.TYPE })
	public @interface Flexs {
		Flex[] value();
	}

	@Retention(RetentionPolicy.RUNTIME)
	@Target({ ElementType.TYPE })
	@Repeatable(FlexWraps.class)
	public @interface FlexWrap {
		Class<? extends GSTagImpl>[] path() default {};

		String value();
	}

	@Retention(RetentionPolicy.RUNTIME)
	@Target({ ElementType.TYPE })
	public @interface FlexWraps {
		FlexWrap[] value();
	}

	@Retention(RetentionPolicy.RUNTIME)
	@Target({ ElementType.TYPE })
	@Repeatable(BackgroundColors.class)
	public @interface BackgroundColor {
		Class<? extends GSTagImpl>[] path() default {};

		String value();
	}

	@Retention(RetentionPolicy.RUNTIME)
	@Target({ ElementType.TYPE })
	public @interface BackgroundColors {
		BackgroundColor[] value();
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

	@Retention(RetentionPolicy.RUNTIME)
	@Target({ ElementType.TYPE })
	@Repeatable(AlignItemss.class)
	public @interface AlignItems {
		Class<? extends GSTagImpl>[] path() default {};

		String value();
	}

	@Retention(RetentionPolicy.RUNTIME)
	@Target({ ElementType.TYPE })
	public @interface AlignItemss {
		AlignItems[] value();
	}

	@Retention(RetentionPolicy.RUNTIME)
	@Target({ ElementType.TYPE })
	@Repeatable(JustifyContents.class)
	public @interface JustifyContent {
		Class<? extends GSTagImpl>[] path() default {};

		String value();
	}

	@Retention(RetentionPolicy.RUNTIME)
	@Target({ ElementType.TYPE })
	public @interface JustifyContents {
		JustifyContent[] value();
	}

	@Retention(RetentionPolicy.RUNTIME)
	@Target({ ElementType.TYPE })
	@Repeatable(Overflows.class)
	public @interface Overflow {
		Class<? extends GSTagImpl>[] path() default {};

		String value();
	}

	@Retention(RetentionPolicy.RUNTIME)
	@Target({ ElementType.TYPE })
	public @interface Overflows {
		Overflow[] value();
	}

	@Retention(RetentionPolicy.RUNTIME)
	@Target({ ElementType.TYPE })
	@Repeatable(Colors.class)
	public @interface Color {
		Class<? extends GSTagImpl>[] path() default {};

		String value();
	}

	@Retention(RetentionPolicy.RUNTIME)
	@Target({ ElementType.TYPE })
	public @interface Colors {
		Color[] value();
	}

	@Retention(RetentionPolicy.RUNTIME)
	@Target({ ElementType.TYPE })
	@Repeatable(MarginRights.class)
	public @interface MarginRight {
		Class<? extends GSTagImpl>[] path() default {};

		String value();
	}

	@Retention(RetentionPolicy.RUNTIME)
	@Target({ ElementType.TYPE })
	public @interface MarginRights {
		MarginRight[] value();
	}

	@Retention(RetentionPolicy.RUNTIME)
	@Target({ ElementType.TYPE })
	@Repeatable(MarginBottoms.class)
	public @interface MarginBottom {
		Class<? extends GSTagImpl>[] path() default {};

		String value();
	}

	@Retention(RetentionPolicy.RUNTIME)
	@Target({ ElementType.TYPE })
	public @interface MarginBottoms {
		MarginBottom[] value();
	}

	@Retention(RetentionPolicy.RUNTIME)
	@Target({ ElementType.TYPE })
	@Repeatable(Heights.class)
	public @interface Height {
		Class<? extends GSTagImpl>[] path() default {};

		String value();
	}

	@Retention(RetentionPolicy.RUNTIME)
	@Target({ ElementType.TYPE })
	public @interface Heights {
		Height[] value();
	}

	@Retention(RetentionPolicy.RUNTIME)
	@Target({ ElementType.TYPE })
	@Repeatable(Widths.class)
	public @interface Width {
		Class<? extends GSTagImpl>[] path() default {};

		String value();
	}

	@Retention(RetentionPolicy.RUNTIME)
	@Target({ ElementType.TYPE })
	public @interface Widths {
		Width[] value();
	}
}
