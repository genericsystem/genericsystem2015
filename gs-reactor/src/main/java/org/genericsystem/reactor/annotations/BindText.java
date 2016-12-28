package org.genericsystem.reactor.annotations;

import java.lang.annotation.Annotation;
import java.lang.annotation.ElementType;
import java.lang.annotation.Repeatable;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;
import java.util.function.BiConsumer;

import org.genericsystem.reactor.Tag;
import org.genericsystem.reactor.annotations.BindText.BindTextProcessor;
import org.genericsystem.reactor.annotations.BindText.BindTexts;
import org.genericsystem.reactor.context.TextBinding;
import org.genericsystem.reactor.context.TextBinding.GENERIC_STRING;
import org.genericsystem.reactor.gscomponents.TagImpl;

@Retention(RetentionPolicy.RUNTIME)
@Target({ ElementType.TYPE })
@Repeatable(BindTexts.class)
@Process(BindTextProcessor.class)
public @interface BindText {
	Class<? extends TagImpl>[] path() default {};

	Class<? extends TextBinding> value() default GENERIC_STRING.class;

	int[] pos() default {};

	@Retention(RetentionPolicy.RUNTIME)
	@Target({ ElementType.TYPE })
	public @interface BindTexts {
		BindText[] value();
	}

	public static class BindTextProcessor implements BiConsumer<Annotation, Tag> {

		@Override
		public void accept(Annotation annotation, Tag tag) {
			tag.getRootTag().processBindText(tag, ((BindText) annotation).value());
		}
	}
}
