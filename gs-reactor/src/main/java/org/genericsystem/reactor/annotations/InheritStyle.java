package org.genericsystem.reactor.annotations;

import java.lang.annotation.Annotation;
import java.lang.annotation.ElementType;
import java.lang.annotation.Repeatable;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;
import java.util.function.BiConsumer;

import org.genericsystem.reactor.Tag;
import org.genericsystem.reactor.annotations.InheritStyle.InheritStyleProcessor;
import org.genericsystem.reactor.annotations.InheritStyle.InheritStyles;
import org.genericsystem.reactor.gscomponents.TagImpl;

@Retention(RetentionPolicy.RUNTIME)
@Target({ ElementType.TYPE })
@Repeatable(InheritStyles.class)
@Process(value = InheritStyleProcessor.class, repeatable = true)
public @interface InheritStyle {
	Class<? extends TagImpl>[] path() default {};

	String[] value();

	int[] pos() default {};

	@Retention(RetentionPolicy.RUNTIME)
	@Target({ ElementType.TYPE })
	public @interface InheritStyles {
		InheritStyle[] value();
	}

	public static class InheritStyleProcessor implements BiConsumer<Annotation, Tag> {

		@Override
		public void accept(Annotation annotation, Tag tag) {
			for (String v : ((InheritStyle) annotation).value())
				tag.inheritStyle(v);
		}
	}
}