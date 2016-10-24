package org.genericsystem.reactor.annotations;

import java.lang.annotation.Annotation;
import java.lang.annotation.ElementType;
import java.lang.annotation.Repeatable;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;
import java.util.function.BiConsumer;

import org.genericsystem.reactor.Tag;
import org.genericsystem.reactor.annotations.StyleClass.StyleClassProcessor;
import org.genericsystem.reactor.annotations.StyleClass.StyleClasses;
import org.genericsystem.reactor.gscomponents.GSTagImpl;

@Retention(RetentionPolicy.RUNTIME)
@Target({ ElementType.TYPE })
@Repeatable(StyleClasses.class)
@Process(StyleClassProcessor.class)
public @interface StyleClass {
	Class<? extends GSTagImpl>[] path() default {};

	String[] value();

	int[] pos() default {};

	@Retention(RetentionPolicy.RUNTIME)
	@Target({ ElementType.TYPE })
	public @interface StyleClasses {
		StyleClass[] value();
	}

	public static class StyleClassProcessor implements BiConsumer<Annotation, Tag> {

		@Override
		public void accept(Annotation annotation, Tag tag) {
			for (String sc : ((StyleClass) annotation).value())
				tag.addStyleClass(sc);
		}
	}
}
