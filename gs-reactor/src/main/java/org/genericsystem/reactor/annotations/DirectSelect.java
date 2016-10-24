package org.genericsystem.reactor.annotations;

import java.lang.annotation.Annotation;
import java.lang.annotation.ElementType;
import java.lang.annotation.Repeatable;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;
import java.lang.reflect.InvocationTargetException;
import java.util.function.BiConsumer;

import org.genericsystem.reactor.Tag;
import org.genericsystem.reactor.annotations.DirectSelect.DirectSelects;
import org.genericsystem.reactor.annotations.DirectSelect.Processor;
import org.genericsystem.reactor.gscomponents.GSTagImpl;

/**
 * @author Nicolas Feybesse
 *
 */
@Retention(RetentionPolicy.RUNTIME)
@Target({ ElementType.TYPE })
@Repeatable(DirectSelects.class)
@Process(Processor.class)
public @interface DirectSelect {
	Class<? extends GSTagImpl>[] path() default {};

	Class<?>[] value();

	int[] pos() default {};

	@Retention(RetentionPolicy.RUNTIME)
	@Target({ ElementType.TYPE })
	public @interface DirectSelects {
		DirectSelect[] value();
	}

	public static class Processor implements BiConsumer<Annotation, Tag> {

		@Override
		public void accept(Annotation annotation, Tag tag) {
			try {
				Class<?>[] path = (Class<?>[]) annotation.annotationType().getDeclaredMethod("path").invoke(annotation);
				Class<?>[] selects = ((DirectSelect) annotation).value();
				Class<?> tagClass = path.length != 0 ? path[path.length - 1] : null;
				if (selects.length == 1 || tagClass == null)
					tag.select(selects[0]);
				else
					tag.select(selects[Tag.position(tag, tagClass)]);
			} catch (IllegalAccessException | IllegalArgumentException | InvocationTargetException | NoSuchMethodException | SecurityException e) {
				throw new IllegalStateException(e);
			}
		}
	}
}