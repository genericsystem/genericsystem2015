package org.genericsystem.reactor;

import org.genericsystem.reactor.modelproperties.ActionDefaults;
import org.genericsystem.reactor.modelproperties.GenericStringDefaults;
import org.genericsystem.reactor.modelproperties.SelectionDefaults;
import org.genericsystem.reactor.modelproperties.StepperDefaults;

import java.lang.reflect.InvocationTargetException;
import java.util.ArrayList;
import java.util.List;

import org.genericsystem.reactor.annotations.Attribute;
import org.genericsystem.reactor.annotations.BindAction;
import org.genericsystem.reactor.annotations.BindSelection;
import org.genericsystem.reactor.annotations.BindText;
import org.genericsystem.reactor.annotations.Children;
import org.genericsystem.reactor.annotations.DirectSelect;
import org.genericsystem.reactor.annotations.ForEach;
import org.genericsystem.reactor.annotations.Select;
import org.genericsystem.reactor.annotations.Select.SelectModel;
import org.genericsystem.reactor.annotations.SetStringExtractor;
import org.genericsystem.reactor.annotations.SetText;
import org.genericsystem.reactor.annotations.Stepper;
import org.genericsystem.reactor.annotations.Style;
import org.genericsystem.reactor.annotations.Style.FlexDirectionStyle;
import org.genericsystem.reactor.annotations.Style.GenericValueBackgroundColor;
import org.genericsystem.reactor.annotations.Style.KeepFlexDirection;
import org.genericsystem.reactor.annotations.Style.ReverseFlexDirection;
import org.genericsystem.reactor.annotations.StyleClass;
import org.genericsystem.reactor.gscomponents.GSDiv;
import org.genericsystem.reactor.gscomponents.GSTagImpl;
import org.genericsystem.reactor.model.ObservableListExtractor.NO_FOR_EACH;
import org.genericsystem.reactor.model.StringExtractor;
import org.genericsystem.reactor.model.TextBinding.GENERIC_STRING;

public class DownloaderAnnotation {

	private static volatile DownloaderAnnotation INSTANCE;

	List<InfoAnnotation> infoAnnotations;

	private DownloaderAnnotation() {
		infoAnnotations = new ArrayList<>();

		infoAnnotations.add(new InfoAnnotation(Children.class, (annotation, tag) -> {
			for (Class<? extends GSTagImpl> clazz : ((Children) annotation).value())
				tag.createTag(clazz);
		}, false));

		infoAnnotations.add(new InfoAnnotation(DirectSelect.class, (annotation, tag) -> {
			try {
				Class<?>[] path = (Class<?>[]) annotation.annotationType().getDeclaredMethod("path").invoke(annotation);
				Class<?>[] selects = ((DirectSelect) annotation).value();
				Class<?> tagClass = path.length != 0 ? path[path.length - 1] : null;
				if (selects.length == 1 || tagClass == null)
					tag.select(selects[0]);
				else
					tag.select(selects[tag.position(tag, tagClass)]);
			} catch (IllegalAccessException | IllegalArgumentException | InvocationTargetException | NoSuchMethodException | SecurityException e) {
				throw new IllegalStateException(e);
			}
		}, false));

		infoAnnotations.add(new InfoAnnotation(Select.class, (annotation, tag) -> {
			try {
				tag.select(((Select) annotation).value().newInstance());
			} catch (InstantiationException | IllegalAccessException e) {
				throw new IllegalStateException(e);
			}
		}, false));

		infoAnnotations.add(new InfoAnnotation(SelectModel.class, (annotation, tag) -> {
			tag.select__(context -> {
				try {
					return ((SelectModel) annotation).value().newInstance().apply(context, tag);
				} catch (InstantiationException | IllegalAccessException e) {
					throw new IllegalStateException(e);
				}
			});
		}, false));

		infoAnnotations.add(new InfoAnnotation(ForEach.class, (annotation, tag) -> {
			try {
				if (!NO_FOR_EACH.class.equals(((ForEach) annotation).value()))
					tag.forEach(((ForEach) annotation).value().newInstance());
			} catch (InstantiationException | IllegalAccessException e) {
				throw new IllegalStateException(e);
			}
		}, false));

		infoAnnotations.add(new InfoAnnotation(Stepper.class, (annotation, tag) -> {
			if (StepperDefaults.class.isAssignableFrom(tag.getClass())) {
				Stepper stepperAnn = (Stepper) annotation;
				((StepperDefaults) tag).stepper(tag.find(stepperAnn.switchClass(), stepperAnn.switchClassPos()), tag.find(stepperAnn.headerClass(), stepperAnn.headerClassPos()));
			} else
				tag.log.warn("Switch is applicable only to tags implementing SwitchDefaults.");
		}, false));

		infoAnnotations.add(new InfoAnnotation(BindSelection.class, (annotation, tag) -> {
			if (SelectionDefaults.class.isAssignableFrom(tag.getClass()))
				((SelectionDefaults) tag).bindSelection(tag.find(((BindSelection) annotation).value(), ((BindSelection) annotation).valuePos()));
			else
				tag.log.warn("BindSelection is applicable only to a class implementing SelectionDefaults.");
		}, false));

		infoAnnotations.add(new InfoAnnotation(SetStringExtractor.class, (annotation, tag) -> {
			try {
				tag.setStringExtractor(((SetStringExtractor) annotation).value().newInstance());
			} catch (InstantiationException | IllegalAccessException e) {
				throw new IllegalStateException(e);
			}
		}, false));

		infoAnnotations.add(new InfoAnnotation(StyleClass.class, (annotation, tag) -> {
			for (String sc : ((StyleClass) annotation).value())
				tag.addStyleClass(sc);
		}, false));

		infoAnnotations.add(new InfoAnnotation(FlexDirectionStyle.class, (annotation, tag) -> {
			if (GSDiv.class.isAssignableFrom(tag.getClass()))
				((GSDiv) tag).setDirection(((FlexDirectionStyle) annotation).value());
			else
				tag.log.warn("Warning: FlexDirection is applicable only to GSDiv extensions.");
		}, false));

		infoAnnotations.add(new InfoAnnotation(KeepFlexDirection.class, (annotation, tag) -> {
			if (GSDiv.class.isAssignableFrom(tag.getClass()))
				((GSDiv) tag).keepDirection();
			else
				tag.log.warn("Warning: KeepFlexDirection is applicable only to GSDiv extensions.");
		}, false));

		infoAnnotations.add(new InfoAnnotation(ReverseFlexDirection.class, (annotation, tag) -> {
			if (GSDiv.class.isAssignableFrom(tag.getClass()))
				((GSDiv) tag).reverseDirection();
			else
				tag.log.warn("Warning: ReverseFlexDirection is applicable only to GSDiv extensions.");
		}, false));

		infoAnnotations.add(new InfoAnnotation(SetText.class, (annotation, tag) -> tag.setText(((SetText) annotation).value()), false));

		infoAnnotations.add(new InfoAnnotation(BindText.class, (annotation, tag) -> {
			if (GENERIC_STRING.class.equals(((BindText) annotation).value()))
				tag.bindText();
			else
				tag.bindText(context -> {
					try {
						return ((BindText) annotation).value().newInstance().apply(context, tag);
					} catch (InstantiationException | IllegalAccessException e) {
						throw new IllegalStateException(e);
					}
				});
		}, false));

		infoAnnotations.add(new InfoAnnotation(BindAction.class, (annotation, tag) -> {
			if (ActionDefaults.class.isAssignableFrom(tag.getClass()))
				((ActionDefaults) tag).bindAction(context -> {
					try {
						((BindAction) annotation).value().newInstance().accept(context, tag);
					} catch (InstantiationException | IllegalAccessException e) {
						throw new IllegalStateException(e);
					}
				});
			else
				tag.log.warn("BindAction is applicable only to tags implementing ActionDefaults.");
		}, false));

		infoAnnotations.add(new InfoAnnotation(Attribute.class, (annotation, tag) -> tag.addAttribute(((Attribute) annotation).name(), ((Attribute) annotation).value()), true));

		infoAnnotations.add(new InfoAnnotation(Style.class, (annotation, tag) -> tag.addStyle(((Style) annotation).name(), ((Style) annotation).value()), true));

		infoAnnotations.add(new InfoAnnotation(GenericValueBackgroundColor.class, (annotation, tag) -> tag.addPrefixBinding(modelContext -> tag.addStyle(modelContext, "background-color",
				"Color".equals(StringExtractor.SIMPLE_CLASS_EXTRACTOR.apply(modelContext.getGeneric().getMeta())) ? ((GenericStringDefaults) tag).getGenericStringProperty(modelContext).getValue() : ((GenericValueBackgroundColor) annotation).value())),
				false));
	}

	public static DownloaderAnnotation getInstance() {
		if (INSTANCE == null) {
			synchronized (DownloaderAnnotation.class) {
				if (INSTANCE == null) {
					INSTANCE = new DownloaderAnnotation();
				}
			}
		}
		return INSTANCE;
	}

	public List<InfoAnnotation> getInfoAnnotations() {
		return infoAnnotations;
	}
}
