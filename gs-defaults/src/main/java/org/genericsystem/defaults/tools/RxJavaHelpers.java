package org.genericsystem.defaults.tools;

import java.util.AbstractMap.SimpleEntry;
import java.util.Map;
import java.util.Map.Entry;

import io.reactivex.Observable;
import io.reactivex.ObservableOnSubscribe;
import io.reactivex.rxjavafx.subscriptions.JavaFxSubscriptions;
import javafx.collections.ListChangeListener;
import javafx.collections.MapChangeListener;
import javafx.collections.ObservableList;
import javafx.collections.ObservableMap;

public class RxJavaHelpers {
	/**
	 * Creates an observable that emits all additions to an ObservableList.
	 *
	 * @param source      The source ObservableList for the item add events.
	 * @return An Observable emitting items added to the ObservableList.
	 */
	public static <T> Observable<T> additionsOf(final ObservableList<T> source) {
		return fromObservableListAdds(source);
	}

	/**
	 * Creates an observable that emits all removals from an ObservableList.
	 *
	 * @param source      The source ObservableList for the item removal events.
	 * @return An Observable emitting items removed from the ObservableList.
	 */
	public static <T> Observable<T> removalsOf(final ObservableList<T> source) {
		return fromObservableListRemovals(source);
	}

	/**
	 * Creates an observable that emits all additions to an ObservableMap.
	 *
	 * @param source      The source ObservableMap for the item add events.
	 * @return An Observable emitting Entry items added to the ObservableMap.
	 */
	public static <K,T> Observable<Map.Entry<K,T>> additionsOf(final ObservableMap<K,T> source) {
		return fromObservableMapAdds(source);
	}

	/**
	 * Creates an observable that emits all removals from an ObservableMap.
	 *
	 * @param source      The source ObservableMap for the item removal events.
	 * @return An Observable emitting Entry items removed from the ObservableMap.
	 */
	public static <K,T> Observable<Map.Entry<K,T>> removalsOf(final ObservableMap<K,T> source) {
		return fromObservableMapRemovals(source);
	}

	public static <T> Observable<T> fromObservableListAdds(final ObservableList<T> source) {

		return Observable.create((ObservableOnSubscribe<T>) subscriber -> {

			ListChangeListener<T> listener = c -> {
				while (c.next())
					if (c.wasAdded())
						c.getAddedSubList().forEach(subscriber::onNext);
			};
			source.addListener(listener);
			subscriber.setDisposable(JavaFxSubscriptions.unsubscribeInEventDispatchThread(() -> source.removeListener(listener)));
		});
	}

	public static <T> Observable<T> fromObservableListRemovals(final ObservableList<T> source) {

		return Observable.create((ObservableOnSubscribe<T>) subscriber -> {
			ListChangeListener<T> listener = c -> {
				while (c.next())
					if (c.wasRemoved())
						c.getRemoved().forEach(subscriber::onNext);
			};
			source.addListener(listener);
			subscriber.setDisposable(JavaFxSubscriptions.unsubscribeInEventDispatchThread(() -> source.removeListener(listener)));
		});
	}

	public static <K,T> Observable<Entry<K,T>> fromObservableMapAdds(final ObservableMap<K,T> source) {

		return Observable.create((ObservableOnSubscribe<Entry<K,T>>) subscriber -> {

			MapChangeListener<K,T> listener = c -> {
				if (c.wasAdded())
					subscriber.onNext(new SimpleEntry<K,T>(c.getKey(),c.getValueAdded()));
			};
			source.addListener(listener);
			subscriber.setDisposable(JavaFxSubscriptions.unsubscribeInEventDispatchThread(() -> source.removeListener(listener)));
		});
	}

	public static <K,T> Observable<Entry<K,T>> fromObservableMapRemovals(final ObservableMap<K,T> source) {

		return Observable.create((ObservableOnSubscribe<Entry<K,T>>) subscriber -> {

			MapChangeListener<K,T> listener = c -> {
				if (c.wasRemoved())
					subscriber.onNext(new SimpleEntry<K,T>(c.getKey(),c.getValueRemoved()));
			};
			source.addListener(listener);
			subscriber.setDisposable(JavaFxSubscriptions.unsubscribeInEventDispatchThread(() -> source.removeListener(listener)));
		});
	}
}