package org.genericsystem.reactor.annotations;

import java.lang.annotation.Annotation;
import java.lang.annotation.ElementType;
import java.lang.annotation.Repeatable;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;
import java.util.function.BiConsumer;

import org.genericsystem.reactor.Tag;
import org.genericsystem.reactor.annotations.Attribute.AttributeProcessor;
import org.genericsystem.reactor.annotations.Attribute.Attributes;
import org.genericsystem.reactor.gscomponents.GSTagImpl;

@Retention(RetentionPolicy.RUNTIME)
@Target({ ElementType.TYPE })
@Repeatable(Attributes.class)
@Process(value = AttributeProcessor.class, repeatable = true)
public @interface Attribute {
	Class<? extends GSTagImpl>[] path() default {};

	String name();

	String value();

	int[] pos() default {};

	@Retention(RetentionPolicy.RUNTIME)
	@Target({ ElementType.TYPE })
	public @interface Attributes {
		Attribute[] value();
	}

	public static class AttributeProcessor implements BiConsumer<Annotation, Tag> {

		@Override
		public void accept(Annotation annotation, Tag tag) {
			tag.addAttribute(((Attribute) annotation).name(), ((Attribute) annotation).value());
		}
	}

}
