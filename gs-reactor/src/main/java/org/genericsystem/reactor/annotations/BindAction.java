package org.genericsystem.reactor.annotations;

import org.genericsystem.reactor.modelproperties.ActionDefaults;

import java.lang.annotation.Annotation;
import java.lang.annotation.ElementType;
import java.lang.annotation.Repeatable;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;
import java.util.function.BiConsumer;

import org.genericsystem.reactor.Tag;
import org.genericsystem.reactor.annotations.BindAction.BindActionProcessor;
import org.genericsystem.reactor.annotations.BindAction.BindActions;
import org.genericsystem.reactor.gscomponents.GSTagImpl;
import org.genericsystem.reactor.model.ContextAction;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

@Retention(RetentionPolicy.RUNTIME)
@Target({ ElementType.TYPE })
@Repeatable(BindActions.class)
@Process(BindActionProcessor.class)
public @interface BindAction {
	Class<? extends GSTagImpl>[] path() default {};

	Class<? extends ContextAction> value();

	int[] pos() default {};

	@Retention(RetentionPolicy.RUNTIME)
	@Target({ ElementType.TYPE })
	public @interface BindActions {
		BindAction[] value();
	}

	public static class BindActionProcessor implements BiConsumer<Annotation, Tag> {
		private static final Logger log = LoggerFactory.getLogger(BindActionProcessor.class);

		@Override
		public void accept(Annotation annotation, Tag tag) {
			if (ActionDefaults.class.isAssignableFrom(tag.getClass()))
				((ActionDefaults) tag).bindAction(context -> {
					try {
						((BindAction) annotation).value().newInstance().accept(context, tag);
					} catch (InstantiationException | IllegalAccessException e) {
						throw new IllegalStateException(e);
					}
				});
			else
				log.warn("BindAction is applicable only to tags implementing ActionDefaults.");
		}
	}
}