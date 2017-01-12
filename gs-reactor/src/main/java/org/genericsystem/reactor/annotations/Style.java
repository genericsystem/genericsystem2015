package org.genericsystem.reactor.annotations;

import java.lang.annotation.Annotation;
import java.lang.annotation.ElementType;
import java.lang.annotation.Repeatable;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;
import java.util.function.BiConsumer;

import org.genericsystem.api.core.TagAnnotation;
import org.genericsystem.reactor.Context;
import org.genericsystem.reactor.ExtendedAnnotationsManager.IGenericAnnotationProcessor;
import org.genericsystem.reactor.Tag;
import org.genericsystem.reactor.annotations.Style.FlexDirectionStyle.FlexDirectionStyleGenericProcessor;
import org.genericsystem.reactor.annotations.Style.FlexDirectionStyle.FlexDirectionStyleProcessor;
import org.genericsystem.reactor.annotations.Style.FlexDirectionStyle.FlexDirections;
import org.genericsystem.reactor.annotations.Style.GenericValueBackgroundColor.GenericValueBackgroundColorGenericProcessor;
import org.genericsystem.reactor.annotations.Style.GenericValueBackgroundColor.GenericValueBackgroundColorProcessor;
import org.genericsystem.reactor.annotations.Style.GenericValueBackgroundColor.GenericValueBackgroundColors;
import org.genericsystem.reactor.annotations.Style.KeepFlexDirection.KeepFlexDirectionGenericProcessor;
import org.genericsystem.reactor.annotations.Style.KeepFlexDirection.KeepFlexDirectionProcessor;
import org.genericsystem.reactor.annotations.Style.KeepFlexDirection.KeepFlexDirections;
import org.genericsystem.reactor.annotations.Style.ReverseFlexDirection.ReverseFlexDirectionGenericProcessor;
import org.genericsystem.reactor.annotations.Style.ReverseFlexDirection.ReverseFlexDirectionProcessor;
import org.genericsystem.reactor.annotations.Style.ReverseFlexDirection.ReverseFlexDirections;
import org.genericsystem.reactor.annotations.Style.StyleGenericProcessor;
import org.genericsystem.reactor.annotations.Style.StyleProcessor;
import org.genericsystem.reactor.annotations.Style.Styles;
import org.genericsystem.reactor.contextproperties.FlexDirectionDefaults;
import org.genericsystem.reactor.gscomponents.ExtendedRootTag.GTag;
import org.genericsystem.reactor.gscomponents.ExtendedRootTag.GTagAnnotation;
import org.genericsystem.reactor.gscomponents.ExtendedRootTag.GTagAnnotationContent;
import org.genericsystem.reactor.gscomponents.ExtendedRootTag.TagType.TagAnnotationAttribute;
import org.genericsystem.reactor.gscomponents.ExtendedRootTag.TagType.TagAnnotationContentAttribute;
import org.genericsystem.reactor.gscomponents.FlexDirection;
import org.genericsystem.reactor.gscomponents.TagImpl;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import io.vertx.core.json.JsonObject;

@Retention(RetentionPolicy.RUNTIME)
@Target({ ElementType.TYPE })
@Repeatable(Styles.class)
@Process(value = StyleProcessor.class, repeatable = true)
@GenericProcess(StyleGenericProcessor.class)
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

	public static class StyleGenericProcessor implements IGenericAnnotationProcessor {

		@Override
		public void setAnnotation(GTag gTag, Annotation annotation) {
			Style annotation_ = (Style) annotation;
			gTag.setAnnotation(Style.class, annotation_.name(), annotation_.value(), annotation_.path(), annotation_.pos());
		}

		@Override
		public void onRemove(Tag tag, Context context, GTagAnnotation gTagAnnotation, GTagAnnotationContent annotationContent) {
			tag.getDomNodeStyles(context).remove(gTagAnnotation.getValue().getName());
		}

		@Override
		public void onAdd(Tag tag, Context context, GTagAnnotation gTagAnnotation, GTagAnnotationContent annotationContent) {
			tag.getRootTag().processStyle(tag, context, gTagAnnotation.getValue().getName(), annotationContent.getContentValue());
		}
	}

	@Retention(RetentionPolicy.RUNTIME)
	@Target({ ElementType.TYPE })
	@Repeatable(FlexDirections.class)
	@Process(FlexDirectionStyleProcessor.class)
	@GenericProcess(FlexDirectionStyleGenericProcessor.class)
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

		public static class FlexDirectionStyleGenericProcessor implements IGenericAnnotationProcessor {

			public static final Logger log = LoggerFactory.getLogger(FlexDirectionStyleGenericProcessor.class);

			@Override
			public void setAnnotation(GTag gTag, Annotation annotation) {
				FlexDirectionStyle annotation_ = (FlexDirectionStyle) annotation;
				GTagAnnotation gTagAnnotation = (GTagAnnotation) gTag.setHolder(gTag.getRoot().find(TagAnnotationAttribute.class), new TagAnnotation(FlexDirectionStyle.class, annotation_.path(), annotation_.pos()));
				gTagAnnotation.setHolder(gTag.getRoot().find(TagAnnotationContentAttribute.class), new JsonObject().put("value", annotation_.value()).encodePrettily());
			}

			@Override
			public void onRemove(Tag tag, Context context, GTagAnnotation gTagAnnotation, GTagAnnotationContent annotationContent) {
				if (FlexDirectionDefaults.class.isAssignableFrom(tag.getClass()))
					((FlexDirectionDefaults) tag).setDirection(context, null);
				else
					log.warn("Warning: FlexDirection is applicable only to classes implementing FlexDirectionDefaults.");
			}

			@Override
			public void onAdd(Tag tag, Context context, GTagAnnotation gTagAnnotation, GTagAnnotationContent annotationContent) {
				tag.getRootTag().processFlexDirectionStyle(tag, context, FlexDirection.valueOf(annotationContent.getContentValue()));
			}
		}
	}

	@Retention(RetentionPolicy.RUNTIME)
	@Target({ ElementType.TYPE })
	@Repeatable(KeepFlexDirections.class)
	@Process(KeepFlexDirectionProcessor.class)
	@GenericProcess(KeepFlexDirectionGenericProcessor.class)
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

		public static class KeepFlexDirectionGenericProcessor extends ReverseFlexDirectionGenericProcessor {

			@Override
			public void setAnnotation(GTag gTag, Annotation annotation) {
				KeepFlexDirection annotation_ = (KeepFlexDirection) annotation;
				gTag.setAnnotation(KeepFlexDirection.class, null, null, annotation_.path(), annotation_.pos());
			}

			@Override
			public void onAdd(Tag tag, Context context, GTagAnnotation gTagAnnotation, GTagAnnotationContent annotationContent) {
				tag.getRootTag().processKeepFlexDirection(tag, context);
			}
		}
	}

	@Retention(RetentionPolicy.RUNTIME)
	@Target({ ElementType.TYPE })
	@Repeatable(ReverseFlexDirections.class)
	@Process(ReverseFlexDirectionProcessor.class)
	@GenericProcess(ReverseFlexDirectionGenericProcessor.class)
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

		public static class ReverseFlexDirectionGenericProcessor implements IGenericAnnotationProcessor {

			@Override
			public void setAnnotation(GTag gTag, Annotation annotation) {
				ReverseFlexDirection annotation_ = (ReverseFlexDirection) annotation;
				gTag.setAnnotation(ReverseFlexDirection.class, null, null, annotation_.path(), annotation_.pos());
			}

			@Override
			public void onRemove(Tag tag, Context context, GTagAnnotation gTagAnnotation, GTagAnnotationContent annotationContent) {
				((FlexDirectionDefaults) tag).stopTrackingDirection(context);
			}

			@Override
			public void onAdd(Tag tag, Context context, GTagAnnotation gTagAnnotation, GTagAnnotationContent annotationContent) {
				tag.getRootTag().processReverseFlexDirection(tag, context);
			}
		}
	}

	@Retention(RetentionPolicy.RUNTIME)
	@Target({ ElementType.TYPE })
	@Repeatable(GenericValueBackgroundColors.class)
	@Process(GenericValueBackgroundColorProcessor.class)
	@GenericProcess(GenericValueBackgroundColorGenericProcessor.class)
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

		public static class GenericValueBackgroundColorGenericProcessor implements IGenericAnnotationProcessor {

			@Override
			public void setAnnotation(GTag gTag, Annotation annotation) {
				GenericValueBackgroundColor annotation_ = (GenericValueBackgroundColor) annotation;
				gTag.setAnnotation(GenericValueBackgroundColor.class, null, annotation_.value(), annotation_.path(), annotation_.pos());
			}

			@Override
			public void onRemove(Tag tag, Context context, GTagAnnotation gTagAnnotation, GTagAnnotationContent annotationContent) {
				tag.getDomNodeStyles(context).remove("background-color");
			}

			@Override
			public void onAdd(Tag tag, Context context, GTagAnnotation gTagAnnotation, GTagAnnotationContent annotationContent) {
				tag.getRootTag().processGenericValueBackgroundColor(tag, context, annotationContent.getContentValue());
			}
		}
	}
}
