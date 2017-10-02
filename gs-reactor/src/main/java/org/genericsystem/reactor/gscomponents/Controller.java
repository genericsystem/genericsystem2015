package org.genericsystem.reactor.gscomponents;

import java.util.Map.Entry;

import org.genericsystem.defaults.tools.RxJavaHelpers;
import org.genericsystem.reactor.Context;
import org.genericsystem.reactor.Tag;
import org.genericsystem.reactor.context.ContextAction;
import org.genericsystem.reactor.context.TagSwitcher;
import org.genericsystem.reactor.context.TextBinding;

import io.reactivex.Observable;
import io.reactivex.subjects.BehaviorSubject;
import io.reactivex.subjects.ReplaySubject;
import io.reactivex.subjects.Subject;
import javafx.beans.binding.Bindings;
import javafx.beans.property.Property;
import javafx.beans.property.SimpleBooleanProperty;
import javafx.beans.property.SimpleObjectProperty;
import javafx.beans.value.ObservableValue;
import javafx.collections.FXCollections;
import javafx.collections.ObservableMap;

public class Controller {
	public static final String CONTROLLER = "controller";
	private final Tag containerTag;
	private final Subject<Class<? extends Tag>> classProperty;
	private final Subject<Integer> globalIncs = ReplaySubject.create();
	private final ObservableMap<Tag, StepsStep> steps = FXCollections.observableHashMap();
	private Property<Boolean> activeProperty = new SimpleBooleanProperty(true);

	public static void initialize(Tag tag, Class<? extends TagImpl> firstClass) {
		tag.setContextAttribute(CONTROLLER, c -> new Controller(tag, firstClass));
	}

	public static Controller get(Tag tag, Context context) {
		return tag.getContextAttribute(CONTROLLER, context);
	}

	public Controller(Tag containerTag, Class<? extends TagImpl> firstClass) {
		this.containerTag = containerTag;
		classProperty = BehaviorSubject.create();
		classProperty.onNext(firstClass);
	}

	public Observable<Class<? extends Tag>> getClassProperty() {
		return classProperty.hide();
	}

	public Property<Boolean> getActiveProperty() {
		return activeProperty;
	}

	public void setActiveProperty(boolean active) {
		activeProperty.setValue(active);
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

	public StepsStep addStep(Tag tag, Observable<Integer> observableSize, Class<? extends TagImpl> nextClass, String prevText, String nextText) {
		StepsStep result = new StepsStep(tag, observableSize, nextClass, prevText, nextText);
		steps.put(tag, result);
		return result;
	}

	public Observable<String> prevText(Tag tag) {
		return getStep(tag).prevText();
	}

	public Observable<String> nextText(Tag tag) {
		return getStep(tag).nextText();
	}

	public void previous(Tag tag) {
		getStep(tag).prev();
	}

	public void next(Tag tag) {
		getStep(tag).next();
	}

	public Observable<Boolean> hasPrev(Tag tag) {
		return getStep(tag).hasPrev();
	}

	public Observable<Boolean> hasNext(Tag tag) {
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

	public Observable<String> countText(Tag tag) {
		return globalIncs.hide().scan(0, (sum, curr) -> sum + curr).map(ind -> "Step: " + Integer.toString(ind + 1));
	}

	public class StepsStep {
		private final Tag tag;
		private final Class<? extends TagImpl> nextClass;
		// Used to indicate when to increase/decrease the index or go to the previous/next class.
		private final Subject<Integer> inc = ReplaySubject.create();
		private final Observable<Integer> observableSize;
		private final Observable<Integer> indexProperty;
		private final Observable<Boolean> hasPrev;
		private final Observable<Boolean> hasNext;
		private final Observable<String> prevText;
		private final Observable<String> nextText;

		public StepsStep(Tag tag, Observable<Integer> observableSize, Class<? extends TagImpl> nextClass, String prevText, String nextText) {
			this.tag = tag;
			this.observableSize = observableSize;
			indexProperty = observableSize.switchMap(size -> inc.hide().scan(0, (curr, inc) -> {
				int newIndex = curr + inc;
				if (newIndex >= 0 && newIndex < size)
					return newIndex;
				else {
					if (inc == -1)
						classProperty.onNext(getPreviousStep(tag.getClass()).getTag().getClass());
					else
						classProperty.onNext(nextClass);
					return curr;
				}
			})).distinctUntilChanged().replay().refCount();
			this.nextClass = nextClass;
			this.hasPrev = Observable.combineLatest(getIndexProperty(), RxJavaHelpers.valuesOf(activeProperty),
					(index, active) -> active && (getPreviousStep(tag.getClass()) != null || (index.intValue() > 0)));
			this.hasNext = Observable.combineLatest(getIndexProperty(), RxJavaHelpers.valuesOf(activeProperty), observableSize,
					(index, active, size) -> active && (!tag.getClass().equals(nextClass) || (index.intValue() + 1 < size)));
			this.prevText = Observable.just(prevText);
			this.nextText = Observable.just(nextText);
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

		public Observable<Integer> getIndexProperty() {
			return indexProperty;
		}

		public Observable<Integer> getObservableSize() {
			return observableSize;
		}

		public void prev() {
			if (activeProperty.getValue()) {
				inc.onNext(-1);
				globalIncs.onNext(-1);
			}
		}

		public void next() {
			if (activeProperty.getValue()) {
				inc.onNext(1);
				globalIncs.onNext(1);
			}
		}

		public Observable<Boolean> hasPrev() {
			return hasPrev;
		}

		public Observable<Boolean> hasNext() {
			return hasNext;
		}

		public Observable<String> prevText() {
			return prevText;
		}

		public Observable<String> nextText() {
			return nextText;
		}
	}

	public static class MainSwitcher implements TagSwitcher {

		@Override
		public ObservableValue<Boolean> apply(Context context, Tag tag) {
			Controller controller = Controller.get(tag, context);
			Property<Class<? extends Tag>> classProperty = new SimpleObjectProperty<>();
			controller.getClassProperty().subscribe(clazz -> classProperty.setValue(clazz));
			Property<Boolean> activeProperty = controller.getActiveProperty();
			return Bindings.createBooleanBinding(() -> !activeProperty.getValue() || tag.getClass().equals(classProperty.getValue()), classProperty, activeProperty);
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
		public Observable<String> apply(Context context, Tag tag) {
			return Controller.get(tag, context).countText(tag);
		}

	}

	public static class PrevTextBinding implements TextBinding {

		@Override
		public Observable<String> apply(Context context, Tag tag) {
			return Controller.get(tag, context).prevText(tag);
		}

	}

	public static class NextTextBinding implements TextBinding {

		@Override
		public Observable<String> apply(Context context, Tag tag) {
			return Controller.get(tag, context).nextText(tag);
		}

	}

	public static class CountTextSwitcher implements TagSwitcher {

		@Override
		public ObservableValue<Boolean> apply(Context context, Tag tag) {
			return Controller.get(tag, context).activeProperty;
		}
	}

	// TODO: Dispose subscriptions.
	public static class PrevSwitcher implements TagSwitcher {

		@Override
		public ObservableValue<Boolean> apply(Context context, Tag tag) {
			Property<Boolean> result = new SimpleBooleanProperty();
			Controller.get(tag, context).hasPrev(tag).subscribe(bool -> result.setValue(bool));
			return result;
		}

	}

	public static class NextSwitcher implements TagSwitcher {

		@Override
		public ObservableValue<Boolean> apply(Context context, Tag tag) {
			Property<Boolean> result = new SimpleBooleanProperty();
			Controller.get(tag, context).hasNext(tag).subscribe(bool -> result.setValue(bool));
			return result;
		}
	}

	public static class LastSwitcher implements TagSwitcher {

		@Override
		public ObservableValue<Boolean> apply(Context context, Tag tag) {
			Property<Boolean> result = new SimpleBooleanProperty();
			Controller.get(tag, context).hasNext(tag).subscribe(bool -> result.setValue(!bool));
			return result;
		}
	}
}