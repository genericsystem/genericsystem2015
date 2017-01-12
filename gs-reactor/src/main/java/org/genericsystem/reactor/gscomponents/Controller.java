package org.genericsystem.reactor.gscomponents;

import java.util.Map.Entry;

import javafx.beans.binding.Bindings;
import javafx.beans.property.Property;
import javafx.beans.property.SimpleIntegerProperty;
import javafx.beans.property.SimpleObjectProperty;
import javafx.beans.value.ObservableIntegerValue;
import javafx.beans.value.ObservableValue;
import javafx.collections.FXCollections;
import javafx.collections.ObservableMap;

import org.genericsystem.reactor.Context;
import org.genericsystem.reactor.Tag;
import org.genericsystem.reactor.annotations.Switcher;
import org.genericsystem.reactor.context.ContextAction;
import org.genericsystem.reactor.context.TagSwitcher;
import org.genericsystem.reactor.context.TextBinding;

public class Controller {
	private final Class<? extends TagImpl> firstClass;
	private final Property<Class<? extends Tag>> classProperty;
	private final ObservableMap<Tag, SwitchStep> switcherSteps = FXCollections.observableHashMap();

	public static Controller get(Tag tag, Context context) {
		return tag.<Controller> getProperty(Switcher.CONTROLLER, context).getValue();
	}

	public Controller(Class<? extends TagImpl> firstClass) {
		this.firstClass = firstClass;
		classProperty = new SimpleObjectProperty<>(firstClass);
	}

	public Property<Class<? extends Tag>> getClassProperty() {
		return classProperty;
	}

	public SwitchStep getSwitchStep(Tag tag) {
		return switcherSteps.get(tag);
	}

	public SwitchStep addSwitchStep(Tag tag, ObservableIntegerValue observableSize, Class<? extends TagImpl> nextClass, String prevText, String nextText) {
		SwitchStep result = new SwitchStep(tag, observableSize, nextClass, prevText, nextText);
		switcherSteps.put(tag, result);
		return result;
	}

	public ObservableValue<String> prevText(Tag tag) {
		return getSwitchStep(tag).prevText();
	}

	public ObservableValue<String> nextText(Tag tag) {
		return getSwitchStep(tag).nextText();
	}

	public void previous(Tag tag) {
		getSwitchStep(tag).previous();
	}

	public void next(Tag tag) {
		getSwitchStep(tag).next();
	}

	public ObservableValue<Boolean> hasPrev(Tag tag) {
		return getSwitchStep(tag).hasPrev();
	}

	public ObservableValue<Boolean> hasNext(Tag tag) {
		return getSwitchStep(tag).hasNext();
	}

	public SwitchStep getStep(Class<? extends TagImpl> clazz) {
		for (Entry<Tag, SwitchStep> entry : switcherSteps.entrySet())
			if (entry.getKey().getClass().equals(clazz))
				return entry.getValue();
		return null;
	}

	public SwitchStep getPreviousStep(Class<? extends Tag> clazz) {
		for (Entry<Tag, SwitchStep> entry : switcherSteps.entrySet())
			if (entry.getValue().getNextClass().equals(clazz) && !entry.getValue().getNextClass().equals(entry.getValue().getTag().getClass()))
				return entry.getValue();
		return null;
	}

	public ObservableValue<String> countText(Tag tag) {
		SimpleIntegerProperty indexProperty = getSwitchStep(tag).getIndexProperty();
		return Bindings.createStringBinding(() -> {
			int size = 0;
			SwitchStep step = getStep(firstClass);
			while (step != null) {
				if (tag.equals(step.tag))
					break;
				size += step.getObservableSize().intValue();
				step = step.getNextStep();
			}
			return "Step : " + Integer.toString(indexProperty.get() + size + 1);
		}, switcherSteps, indexProperty);
	}

	public class SwitchStep {
		private final Tag tag;
		private final Class<? extends TagImpl> nextClass;
		private final SimpleIntegerProperty indexProperty = new SimpleIntegerProperty(0);
		private final ObservableIntegerValue observableSize;
		private final ObservableValue<Boolean> hasPrev;
		private final ObservableValue<Boolean> hasNext;
		private final ObservableValue<String> prevText;
		private final ObservableValue<String> nextText;

		public SwitchStep(Tag tag, ObservableIntegerValue observableSize, Class<? extends TagImpl> nextClass, String prevText, String nextText) {
			this.tag = tag;
			this.observableSize = observableSize;
			this.nextClass = nextClass;
			this.hasPrev = Bindings.createBooleanBinding(() -> {
				return getPreviousStep(tag.getClass()) != null || (getIndexProperty().get() > 0);
			}, getIndexProperty());
			this.hasNext = Bindings.createBooleanBinding(() -> {
				return !tag.getClass().equals(nextClass) || (getIndexProperty().get() + 1 < getObservableSize().get());
			}, getIndexProperty(), getObservableSize());
			this.prevText = Bindings.createStringBinding(() -> {
				return prevText;
			}, getIndexProperty(), getObservableSize());
			this.nextText = Bindings.createStringBinding(() -> {
				return nextText;
			}, getIndexProperty(), getObservableSize());

		}

		public Object getNextClass() {
			return nextClass;
		}

		public SwitchStep getNextStep() {
			return !tag.getClass().equals(nextClass) ? getStep(nextClass) : null;
		}

		public SimpleIntegerProperty getIndexProperty() {
			return indexProperty;
		}

		public ObservableIntegerValue getObservableSize() {
			return observableSize;
		}

		public void previous() {
			if (indexProperty.get() > 0)
				indexProperty.set(indexProperty.get() - 1);
			else
				classProperty.setValue(getPreviousStep(tag.getClass()).getTag().getClass());
		}

		private Tag getTag() {
			return tag;
		}

		public void next() {
			if (indexProperty.get() + 1 < observableSize.get())
				indexProperty.set(indexProperty.get() + 1);
			else
				classProperty.setValue(nextClass);
		}

		public ObservableValue<Boolean> hasPrev() {
			return hasPrev;
		}

		public ObservableValue<Boolean> hasNext() {
			return hasNext;
		}

		public ObservableValue<String> prevText() {
			return prevText;
		}

		public ObservableValue<String> nextText() {
			return nextText;
		}
	}

	public static class MainSwitcher implements TagSwitcher {

		@Override
		public ObservableValue<Boolean> apply(Context context, Tag tag) {
			Property<Class<? extends Tag>> classProperty = Controller.get(tag, context).getClassProperty();
			return Bindings.createBooleanBinding(() -> tag.getClass().equals(classProperty.getValue()), classProperty);
		}
	}

	public static class PrevAction implements ContextAction {
		@Override
		public void accept(Context context, Tag tag) {
			Controller.get(tag.getParent(), context).previous(tag.getParent());
		}
	}

	public static class NextAction implements ContextAction {

		@Override
		public void accept(Context context, Tag tag) {
			Controller.get(tag.getParent(), context).next(tag.getParent());
		}

	}

	public static class CountTextBinding implements TextBinding {

		@Override
		public ObservableValue<String> apply(Context context, Tag tag) {
			return Controller.get(tag.getParent(), context).countText(tag.getParent());
		}

	}

	public static class PrevTextBinding implements TextBinding {

		@Override
		public ObservableValue<String> apply(Context context, Tag tag) {
			return Controller.get(tag.getParent(), context).prevText(tag.getParent());
		}

	}

	public static class NextTextBinding implements TextBinding {

		@Override
		public ObservableValue<String> apply(Context context, Tag tag) {
			return Controller.get(tag.getParent(), context).nextText(tag.getParent());
		}

	}

	public static class PrevSwitcher implements TagSwitcher {

		@Override
		public ObservableValue<Boolean> apply(Context context, Tag tag) {
			return Controller.get(tag.getParent(), context).hasPrev(tag.getParent());
		}

	}

	public static class NextSwitcher implements TagSwitcher {

		@Override
		public ObservableValue<Boolean> apply(Context context, Tag tag) {
			return Controller.get(tag.getParent(), context).hasNext(tag.getParent());
		}
	}

}