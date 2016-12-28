package org.genericsystem.reactor.annotations;

import java.lang.annotation.Annotation;
import java.lang.annotation.ElementType;
import java.lang.annotation.Repeatable;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;
import java.util.function.BiConsumer;

import org.genericsystem.reactor.Tag;
import org.genericsystem.reactor.annotations.Style.FlexDirectionStyle.FlexDirectionStyleProcessor;
import org.genericsystem.reactor.annotations.Style.FlexDirectionStyle.FlexDirections;
import org.genericsystem.reactor.annotations.Style.GenericValueBackgroundColor.GenericValueBackgroundColorProcessor;
import org.genericsystem.reactor.annotations.Style.GenericValueBackgroundColor.GenericValueBackgroundColors;
import org.genericsystem.reactor.annotations.Style.KeepFlexDirection.KeepFlexDirectionProcessor;
import org.genericsystem.reactor.annotations.Style.KeepFlexDirection.KeepFlexDirections;
import org.genericsystem.reactor.annotations.Style.ReverseFlexDirection.ReverseFlexDirectionProcessor;
import org.genericsystem.reactor.annotations.Style.ReverseFlexDirection.ReverseFlexDirections;
import org.genericsystem.reactor.annotations.Style.StyleProcessor;
import org.genericsystem.reactor.annotations.Style.Styles;
import org.genericsystem.reactor.gscomponents.FlexDirection;
import org.genericsystem.reactor.gscomponents.TagImpl;

@Retention(RetentionPolicy.RUNTIME)
@Target({ ElementType.TYPE })
@Repeatable(Styles.class)
@Process(value = StyleProcessor.class, repeatable = true)
public @interface Style {
	Class<? extends TagImpl>[] path() default {};

	String name();

	String value();

	int[] pos() default {};

	@Retention(RetentionPolicy.RUNTIME)
	@Target({ ElementType.TYPE })
	public @interface Styles {
		Style[] value();
	}

	public static class StyleProcessor implements BiConsumer<Annotation, Tag> {

		@Override
		public void accept(Annotation annotation, Tag tag) {
			tag.getRootTag().processStyle(tag, ((Style) annotation).name(), ((Style) annotation).value());
		}
	}

	@Retention(RetentionPolicy.RUNTIME)
	@Target({ ElementType.TYPE })
	@Repeatable(FlexDirections.class)
	@Process(FlexDirectionStyleProcessor.class)
	public @interface FlexDirectionStyle {
		Class<? extends TagImpl>[] path() default {};

		FlexDirection value();

		int[] pos() default {};

		@Retention(RetentionPolicy.RUNTIME)
		@Target({ ElementType.TYPE })
		public @interface FlexDirections {
			FlexDirectionStyle[] value();
		}

		public static class FlexDirectionStyleProcessor implements BiConsumer<Annotation, Tag> {
			@Override
			public void accept(Annotation annotation, Tag tag) {
				tag.getRootTag().processFlexDirectionStyle(tag, ((FlexDirectionStyle) annotation).value());
			}
		}
	}

	@Retention(RetentionPolicy.RUNTIME)
	@Target({ ElementType.TYPE })
	@Repeatable(KeepFlexDirections.class)
	@Process(KeepFlexDirectionProcessor.class)
	public @interface KeepFlexDirection {
		Class<? extends TagImpl>[] path() default {};

		int[] pos() default {};

		@Retention(RetentionPolicy.RUNTIME)
		@Target({ ElementType.TYPE })
		public @interface KeepFlexDirections {
			KeepFlexDirection[] value();
		}

		public static class KeepFlexDirectionProcessor implements BiConsumer<Annotation, Tag> {
			@Override
			public void accept(Annotation annotation, Tag tag) {
				tag.getRootTag().processKeepFlexDirection(tag);
			}
		}
	}

	@Retention(RetentionPolicy.RUNTIME)
	@Target({ ElementType.TYPE })
	@Repeatable(ReverseFlexDirections.class)
	@Process(ReverseFlexDirectionProcessor.class)
	public @interface ReverseFlexDirection {
		Class<? extends TagImpl>[] path() default {};

		int[] pos() default {};

		@Retention(RetentionPolicy.RUNTIME)
		@Target({ ElementType.TYPE })
		public @interface ReverseFlexDirections {
			ReverseFlexDirection[] value();
		}

		public static class ReverseFlexDirectionProcessor implements BiConsumer<Annotation, Tag> {
			@Override
			public void accept(Annotation annotation, Tag tag) {
				tag.getRootTag().processReverseFlexDirection(tag);
			}
		}
	}

	@Retention(RetentionPolicy.RUNTIME)
	@Target({ ElementType.TYPE })
	@Repeatable(GenericValueBackgroundColors.class)
	@Process(GenericValueBackgroundColorProcessor.class)
	public @interface GenericValueBackgroundColor {
		Class<? extends TagImpl>[] path() default {};

		String value();

		int[] pos() default {};

		@Retention(RetentionPolicy.RUNTIME)
		@Target({ ElementType.TYPE })
		public @interface GenericValueBackgroundColors {
			GenericValueBackgroundColor[] value();
		}

		public static class GenericValueBackgroundColorProcessor implements BiConsumer<Annotation, Tag> {

			@Override
			public void accept(Annotation annotation, Tag tag) {
				tag.getRootTag().processGenericValueBackgroundColor(tag, ((GenericValueBackgroundColor) annotation).value());
			}
		}
	}

}
