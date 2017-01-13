package org.genericsystem.reactor.gscomponents;

import java.util.Map.Entry;

import org.genericsystem.reactor.Context;
import org.genericsystem.reactor.Tag;
import org.genericsystem.reactor.context.ContextAction;
import org.genericsystem.reactor.context.TagSwitcher;
import org.genericsystem.reactor.context.TextBinding;

import javafx.beans.binding.Bindings;
import javafx.beans.property.Property;
import javafx.beans.property.SimpleIntegerProperty;
import javafx.beans.property.SimpleObjectProperty;
import javafx.beans.value.ObservableIntegerValue;
import javafx.beans.value.ObservableValue;
import javafx.collections.FXCollections;
import javafx.collections.ObservableMap;

public class Controller {
	public static final String CONTROLLER = "controller";
	private final Tag containerTag;
	private final Class<? extends TagImpl> firstClass;
	private final Property<Class<? extends Tag>> classProperty;
	private final ObservableMap<Tag, StepsStep> steps = FXCollections.observableHashMap();

	public static void initialize(Tag tag, Class<? extends TagImpl> firstClass) {
		tag.createNewInitializedProperty(CONTROLLER, context -> new Controller(tag, firstClass));
	}

	public static Controller get(Tag tag, Context context) {
		return tag.<Controller> getProperty(CONTROLLER, context).getValue();
	}

	public Controller(Tag containerTag, Class<? extends TagImpl> firstClass) {
		this.containerTag = containerTag;
		this.firstClass = firstClass;
		classProperty = new SimpleObjectProperty<>(firstClass);
	}

	public Property<Class<? extends Tag>> getClassProperty() {
		return classProperty;
	}

	public StepsStep getStep(Tag tag) {
		Tag tag_ = tag;
		StepsStep result = null;
		while (result == null && !containerTag.equals(tag_)) {
			result = steps.get(tag_);
			tag_ = tag_.getParent();
		}
		return result;
	}

	public StepsStep addStep(Tag tag, ObservableIntegerValue observableSize, Class<? extends TagImpl> nextClass, String prevText, String nextText) {
		StepsStep result = new StepsStep(tag, observableSize, nextClass, prevText, nextText);
		steps.put(tag, result);
		return result;
	}

	public ObservableValue<String> prevText(Tag tag) {
		return getStep(tag).prevText();
	}

	public ObservableValue<String> nextText(Tag tag) {
		return getStep(tag).nextText();
	}

	public void previous(Tag tag) {
		getStep(tag).prev();
	}

	public void next(Tag tag) {
		getStep(tag).next();
	}

	public ObservableValue<Boolean> hasPrev(Tag tag) {
		return getStep(tag).hasPrev();
	}

	public ObservableValue<Boolean> hasNext(Tag tag) {
		return getStep(tag).hasNext();
	}

	public StepsStep getStep(Class<? extends TagImpl> clazz) {
		for (Entry<Tag, StepsStep> entry : steps.entrySet())
			if (entry.getKey().getClass().equals(clazz))
				return entry.getValue();
		return null;
	}

	public StepsStep getPreviousStep(Class<? extends Tag> clazz) {
		for (Entry<Tag, StepsStep> entry : steps.entrySet())
			if (entry.getValue().getNextClass().equals(clazz) && !entry.getValue().getNextClass().equals(entry.getValue().getTag().getClass()))
				return entry.getValue();
		return null;
	}

	public ObservableValue<String> countText(Tag tag) {
		StepsStep tagStep = getStep(tag);
		Tag realTag = tagStep.getTag();
		SimpleIntegerProperty indexProperty = tagStep.getIndexProperty();
		return Bindings.createStringBinding(() -> {
			int size = 0;
			StepsStep step = getStep(firstClass);
			while (step != null) {
				if (realTag.equals(step.tag))
					break;
				size += step.getObservableSize().intValue();
				step = step.getNextStep();
			}
			return "Step : " + Integer.toString(indexProperty.get() + size + 1);
		}, steps, indexProperty);
	}

	public class StepsStep {
		private final Tag tag;
		private final Class<? extends TagImpl> nextClass;
		private final SimpleIntegerProperty indexProperty = new SimpleIntegerProperty(0);
		private final ObservableIntegerValue observableSize;
		private final ObservableValue<Boolean> hasPrev;
		private final ObservableValue<Boolean> hasNext;
		private final ObservableValue<String> prevText;
		private final ObservableValue<String> nextText;

		public StepsStep(Tag tag, ObservableIntegerValue observableSize, Class<? extends TagImpl> nextClass, String prevText, String nextText) {
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

		private Tag getTag() {
			return tag;
		}

		public Object getNextClass() {
			return nextClass;
		}

		public StepsStep getNextStep() {
			return !tag.getClass().equals(nextClass) ? getStep(nextClass) : null;
		}

		public SimpleIntegerProperty getIndexProperty() {
			return indexProperty;
		}

		public ObservableIntegerValue getObservableSize() {
			return observableSize;
		}

		public void prev() {
			if (indexProperty.get() > 0)
				indexProperty.set(indexProperty.get() - 1);
			else
				classProperty.setValue(getPreviousStep(tag.getClass()).getTag().getClass());
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
			Controller.get(tag, context).previous(tag);
		}
	}

	public static class NextAction implements ContextAction {

		@Override
		public void accept(Context context, Tag tag) {
			Controller.get(tag, context).next(tag);
		}

	}

	public static class CountTextBinding implements TextBinding {

		@Override
		public ObservableValue<String> apply(Context context, Tag tag) {
			return Controller.get(tag, context).countText(tag);
		}

	}

	public static class PrevTextBinding implements TextBinding {

		@Override
		public ObservableValue<String> apply(Context context, Tag tag) {
			return Controller.get(tag, context).prevText(tag);
		}

	}

	public static class NextTextBinding implements TextBinding {

		@Override
		public ObservableValue<String> apply(Context context, Tag tag) {
			return Controller.get(tag, context).nextText(tag);
		}

	}

	public static class PrevSwitcher implements TagSwitcher {

		@Override
		public ObservableValue<Boolean> apply(Context context, Tag tag) {
			return Controller.get(tag, context).hasPrev(tag);
		}

	}

	public static class NextSwitcher implements TagSwitcher {

		@Override
		public ObservableValue<Boolean> apply(Context context, Tag tag) {
			return Controller.get(tag, context).hasNext(tag);
		}
	}

	public static class LastSwitcher implements TagSwitcher {

		@Override
		public ObservableValue<Boolean> apply(Context context, Tag tag) {
			ObservableValue<Boolean> notLast = Controller.get(tag, context).hasNext(tag);
			return Bindings.createBooleanBinding(() -> !notLast.getValue(), notLast);
		}
	}
}