package org.genericsystem.reactor.annotations;

import java.lang.annotation.Annotation;
import java.lang.annotation.ElementType;
import java.lang.annotation.Repeatable;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;
import java.util.function.BiConsumer;

import org.genericsystem.reactor.Tag;
import org.genericsystem.reactor.annotations.Children.ChildrenMult;
import org.genericsystem.reactor.annotations.Children.ChildrenProcessor;
import org.genericsystem.reactor.gscomponents.GSTagImpl;

/**
 * @author Nicolas Feybesse
 *
 */
@Retention(RetentionPolicy.RUNTIME)
@Target({ ElementType.TYPE })
@Repeatable(ChildrenMult.class)
@Process(ChildrenProcessor.class)
public @interface Children {
	Class<? extends GSTagImpl>[] path() default {};

	Class<? extends GSTagImpl>[] value();

	int[] pos() default {};

	@Retention(RetentionPolicy.RUNTIME)
	@Target({ ElementType.TYPE })
	public @interface ChildrenMult {
		Children[] value();
	}

	public static class ChildrenProcessor implements BiConsumer<Annotation, Tag> {

		@Override
		public void accept(Annotation annotation, Tag tag) {
			for (Class<? extends GSTagImpl> clazz : ((Children) annotation).value())
				tag.createTag(clazz);
		}
	}
}
