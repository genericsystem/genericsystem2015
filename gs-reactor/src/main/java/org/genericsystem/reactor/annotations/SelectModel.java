package org.genericsystem.reactor.annotations;

import java.lang.annotation.Annotation;
import java.lang.annotation.ElementType;
import java.lang.annotation.Repeatable;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;
import java.util.function.BiConsumer;

import org.genericsystem.reactor.Tag;
import org.genericsystem.reactor.annotations.SelectModel.SelectModelProcessor;
import org.genericsystem.reactor.annotations.SelectModel.SelectModels;
import org.genericsystem.reactor.gscomponents.GSTagImpl;
import org.genericsystem.reactor.model.ObservableModelSelector;

@Retention(RetentionPolicy.RUNTIME)
@Target({ ElementType.TYPE })
@Repeatable(SelectModels.class)
@Process(SelectModelProcessor.class)
public @interface SelectModel {
	Class<? extends GSTagImpl>[] path() default {};

	Class<? extends ObservableModelSelector> value();

	int[] pos() default {};

	@Retention(RetentionPolicy.RUNTIME)
	@Target({ ElementType.TYPE })
	public @interface SelectModels {
		SelectModel[] value();
	}

	public static class SelectModelProcessor implements BiConsumer<Annotation, Tag> {

		@Override
		public void accept(Annotation annotation, Tag tag) {
			tag.select__(context -> {
				try {
					return ((SelectModel) annotation).value().newInstance().apply(context, tag);
				} catch (InstantiationException | IllegalAccessException e) {
					throw new IllegalStateException(e);
				}
			});
		}
	}
}