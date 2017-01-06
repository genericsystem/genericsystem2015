package org.genericsystem.reactor.annotations;

import java.lang.annotation.Annotation;
import java.lang.annotation.ElementType;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;
import java.util.Map;
import java.util.function.BiConsumer;

import org.genericsystem.common.Generic;
import org.genericsystem.defaults.tools.BindingsTools;
import org.genericsystem.reactor.Tag;
import org.genericsystem.reactor.annotations.BindSelection.BindSelectionProcessor;
import org.genericsystem.reactor.annotations.SwitcherSubSteps.SwitcherProcessor;
import org.genericsystem.reactor.context.ObservableListExtractor;
import org.genericsystem.reactor.gscomponents.TagImpl;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import javafx.beans.binding.ListBinding;
import javafx.beans.property.Property;
import javafx.beans.property.SimpleIntegerProperty;
import javafx.collections.FXCollections;
import javafx.collections.ObservableList;

@Retention(RetentionPolicy.RUNTIME)
@Target({ ElementType.TYPE })
@Process(SwitcherProcessor.class)
public @interface SwitcherSubSteps {
	Class<? extends TagImpl>[] path() default {};

	int[] pos() default {};

	Class<? extends ObservableListExtractor> value();

	int switchClassPos() default 0;

	public static class SwitcherProcessor implements BiConsumer<Annotation, Tag> {
		private static final Logger log = LoggerFactory.getLogger(BindSelectionProcessor.class);

		@Override
		public void accept(Annotation annotation, Tag tag) {
			tag.forEach2(context -> {
				Property<Map<Tag, SimpleIntegerProperty>> property = tag.<Map<Tag, SimpleIntegerProperty>> getProperty(Switcher.TAG_INDEX_MAP, context);
				Map<Tag, SimpleIntegerProperty> map = property.getValue();
				SimpleIntegerProperty sip = map.get(tag);
				if (sip == null)
					map.put(tag, sip = new SimpleIntegerProperty(0));
				SimpleIntegerProperty finalSip = sip;
				ObservableList<Generic> ol;
				try {
					ol = ((SwitcherSubSteps) annotation).value().newInstance().apply(context.getGenerics());
				} catch (Exception e) {
					throw new IllegalStateException(e);
				}
				ListBinding<Generic> binding = new ListBinding<Generic>() {
					{
						bind(finalSip);
					}

					@Override
					protected ObservableList<Generic> computeValue() {
						int i = finalSip.get();
						return i >= 0 && i < ol.size() ? FXCollections.singletonObservableList(ol.get(i)) : FXCollections.emptyObservableList();
					}
				};
				return BindingsTools.transmitSuccessiveInvalidations(binding);
			});
		}
	}
}
