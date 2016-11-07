package org.genericsystem.extendedcarcolor.app;

import java.lang.annotation.Annotation;
import java.lang.annotation.ElementType;
import java.lang.annotation.Repeatable;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;
import java.util.function.BiConsumer;

import org.genericsystem.extendedcarcolor.app.CCInheritStyle.CCInheritStyleProcessor;
import org.genericsystem.extendedcarcolor.app.CCInheritStyle.CCInheritStyles;
import org.genericsystem.reactor.Tag;
import org.genericsystem.reactor.annotations.Process;
import org.genericsystem.reactor.gscomponents.TagImpl;

@Retention(RetentionPolicy.RUNTIME)
@Target({ ElementType.TYPE })
@Repeatable(CCInheritStyles.class)
@Process(value = CCInheritStyleProcessor.class, repeatable = true)
public @interface CCInheritStyle {
	Class<? extends TagImpl>[] path() default {};

	String[] value();

	int[] pos() default {};

	@Retention(RetentionPolicy.RUNTIME)
	@Target({ ElementType.TYPE })
	public @interface CCInheritStyles {
		CCInheritStyle[] value();
	}

	public static class CCInheritStyleProcessor implements BiConsumer<Annotation, Tag> {

		@Override
		public void accept(Annotation annotation, Tag tag) {
			for (String v : ((CCInheritStyle) annotation).value())
				tag.inheritStyle(v);
		}
	}
}