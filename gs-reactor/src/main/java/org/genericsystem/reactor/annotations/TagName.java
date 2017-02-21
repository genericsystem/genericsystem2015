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
import org.genericsystem.reactor.Tag;
import org.genericsystem.reactor.annotations.TagName.TagNameGenericProcessor;
import org.genericsystem.reactor.annotations.TagName.TagNameProcessor;
import org.genericsystem.reactor.annotations.TagName.TagNames;
import org.genericsystem.reactor.extended.ExtendedAnnotationsManager.IGenericAnnotationProcessor;
import org.genericsystem.reactor.extended.ExtendedRootTag.GTag;
import org.genericsystem.reactor.extended.ExtendedRootTag.GTagAnnotation;
import org.genericsystem.reactor.extended.ExtendedRootTag.GTagAnnotationContent;
import org.genericsystem.reactor.extended.ExtendedRootTag.TagType.TagAnnotationAttribute;
import org.genericsystem.reactor.extended.ExtendedRootTag.TagType.TagAnnotationContentAttribute;
import org.genericsystem.reactor.gscomponents.TagImpl;

import io.vertx.core.json.JsonObject;

@Retention(RetentionPolicy.RUNTIME)
@Target({ ElementType.TYPE })
@Repeatable(TagNames.class)
@Process(value = TagNameProcessor.class)
@GenericProcess(TagNameGenericProcessor.class)
public @interface TagName {

	public static final String A = "a";
	public static final String BUTTON = "button";
	public static final String CHECKBOX = "checkbox";
	public static final String DATALIST = "datalist";
	public static final String DIV = "div";
	public static final String FOOTER = "footer";
	public static final String H1 = "h1";
	public static final String H2 = "h2";
	public static final String H3 = "h3";
	public static final String H4 = "h4";
	public static final String H5 = "h5";
	public static final String H6 = "h6";
	public static final String HEADER = "header";
	public static final String IMG = "img";
	public static final String INPUT = "input";
	public static final String LABEL = "label";
	public static final String LI = "li";
	public static final String OPTION = "option";
	public static final String P = "p";
	public static final String RADIO = "radio";
	public static final String SECTION = "section";
	public static final String SELECT = "select";
	public static final String SPAN = "span";
	public static final String STRONG = "strong";
	public static final String TEXT = "text";
	public static final String UL = "ul";

	Class<? extends TagImpl>[] path() default {};

	String value();

	String type() default "text";

	int[] pos() default {};

	@Retention(RetentionPolicy.RUNTIME)
	@Target({ ElementType.TYPE })
	public @interface TagNames {
		TagName[] value();
	}

	public static class TagNameProcessor implements BiConsumer<Annotation, Tag> {

		@Override
		public void accept(Annotation annotation, Tag tag) {
			tag.getRootTag().processTagName(tag, ((TagName) annotation).value(), ((TagName) annotation).type());
		}
	}

	public static class TagNameGenericProcessor implements IGenericAnnotationProcessor {

		@Override
		public void setAnnotation(GTag gTag, Annotation annotation) {
			TagName tagName = (TagName) annotation;
			GTagAnnotation tagNameAnnotation = (GTagAnnotation) gTag.setHolder(gTag.getRoot().find(TagAnnotationAttribute.class), new TagAnnotation(TagName.class, tagName.path(), tagName.pos()));
			tagNameAnnotation.setHolder(gTag.getRoot().find(TagAnnotationContentAttribute.class), new JsonObject().put("value", tagName.value()).put("type", tagName.type()).encodePrettily());
		}

		@Override
		public void onRemove(Tag tag, Context context, GTagAnnotation gTagAnnotation, GTagAnnotationContent annotationContent) {
			if (tag.getMetaBinding() == null)
				context.removeTag(tag);
			else
				context.getParent().removeTag(tag);
		}

		@Override
		public void onAdd(Tag tag, Context context, GTagAnnotation gTagAnnotation, GTagAnnotationContent annotationContent) {
			if (tag.getMetaBinding() == null)
				context.addTag(tag);
			else
				context.getParent().addTag(tag);
		}

		@Override
		public void onRemove(Tag tag, GTagAnnotation gTagAnnotation, GTagAnnotationContent annotationContent) {
			tag.setTag(null);
			tag.setDomNodeClass(null);
		}

		@Override
		public void onAdd(Tag tag, GTagAnnotation gTagAnnotation, GTagAnnotationContent annotationContent) {
			JsonObject jsonContent = annotationContent.getJsonValue();
			tag.getRootTag().processTagName(tag, jsonContent.getString("value"), jsonContent.getString("type"));
		}
	}
}
