package org.genericsystem.reactor.annotations;

import java.lang.annotation.Annotation;
import java.lang.annotation.ElementType;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;
import java.util.function.BiConsumer;
import java.util.function.Function;

import javafx.beans.binding.Bindings;
import javafx.beans.binding.ListBinding;
import javafx.beans.property.SimpleIntegerProperty;
import javafx.collections.FXCollections;
import javafx.collections.ObservableList;

import org.genericsystem.defaults.tools.BindingsTools;
import org.genericsystem.reactor.Context;
import org.genericsystem.reactor.Tag;
import org.genericsystem.reactor.annotations.Step.StepProcessor;
import org.genericsystem.reactor.gscomponents.Controller;
import org.genericsystem.reactor.gscomponents.Controller.SwitchStep;
import org.genericsystem.reactor.gscomponents.TagImpl;

@Retention(RetentionPolicy.RUNTIME)
@Target({ ElementType.TYPE })
@Process(StepProcessor.class)
public @interface Step {
	Class<? extends TagImpl>[] path() default {};

	int[] pos() default {};

	Class<? extends TagImpl> next();

	String prevText() default "<<";

	String nextText() default ">>";

	public static class StepProcessor implements BiConsumer<Annotation, Tag> {

		@Override
		public void accept(Annotation annotation, Tag tag) {
			if (tag.getMetaBinding() == null) {
				tag.addPrefixBinding(context -> {
					Controller controller = Controller.get(tag, context);
					SwitchStep subSteps = controller.getSwitchStep(tag);
					if (subSteps == null)
						subSteps = controller.addSwitchStep(tag, new SimpleIntegerProperty(1), ((Step) annotation).next(), ((Step) annotation).prevText(), ((Step) annotation).nextText());
				});
			} else {
				Function<Context, ObservableList<Object>> contextOl = tag.getMetaBinding().getBetweenChildren();
				tag.getMetaBinding().setBetweenChildren(context -> {
					ObservableList ol = contextOl.apply(context);
					Controller controller = Controller.get(tag, context);
					SwitchStep subSteps = controller.getSwitchStep(tag);
					if (subSteps == null)
						subSteps = controller.addSwitchStep(tag, Bindings.size(ol), ((Step) annotation).next(), ((Step) annotation).prevText(), ((Step) annotation).nextText());
					SimpleIntegerProperty indexProperty = subSteps.getIndexProperty();
					return BindingsTools.transmitSuccessiveInvalidations(new ListBinding() {
						{
							bind(indexProperty);
						}

						@Override
						protected ObservableList computeValue() {
							return (indexProperty.get() >= 0) && (indexProperty.get() < ol.size()) ? FXCollections.singletonObservableList(ol.get(indexProperty.get())) : FXCollections.emptyObservableList();
						}
					});
				});
			}
		}
	}
}
