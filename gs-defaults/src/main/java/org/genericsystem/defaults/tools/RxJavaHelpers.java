package org.genericsystem.defaults.tools;

import java.util.AbstractMap.SimpleEntry;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Optional;

import io.reactivex.Observable;
import io.reactivex.ObservableEmitter;
import io.reactivex.ObservableOnSubscribe;
import io.reactivex.disposables.Disposables;
import javafx.beans.value.ChangeListener;
import javafx.beans.value.ObservableValue;
import javafx.collections.ListChangeListener;
import javafx.collections.MapChangeListener;
import javafx.collections.ObservableList;
import javafx.collections.ObservableMap;

public class RxJavaHelpers {
	/**
	 * Creates an Observable from a JavaFX ObservableValue that emits all successive non-null values
	 * taken by the ObservableValue.
	 *
	 * @param fxObservable the observed ObservableValue
	 * @param <T>          the type of the observed value
	 * @return an Observable emitting non-null values as the wrapped ObservableValue changes.
	 */
	public static <T> Observable<T> valuesOf(final ObservableValue<T> fxObservable) {
		return fromObservableValue(fxObservable);
	}

	/**
	 * Creates an Observable from a JavaFX ObservableValue that emits all successive values
	 * taken by the ObservableValue wrapped in an Optional.
	 *
	 * @param fxObservable the observed ObservableValue
	 * @param <T>          the type of the observed value
	 * @return an Observable emitting Optionals wrapping the successive values of the ObservableValue.
	 */
	public static <T> Observable<Optional<T>> optionalValuesOf(final ObservableValue<T> fxObservable) {
		return fromNullableObservableValue(fxObservable);
	}

	/**
	 * Creates an observable that emits the whole list every time it changes.
	 *
	 * @param source      The source ObservableList.
	 * @return An Observable emitting the new list after each change.
	 */
	public static <T> Observable<List<T>> changesOf(final ObservableList<T> source) {
		return fromObservableList(source);
	}

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

	private static <T> Observable<T> fromObservableValue(final ObservableValue<T> fxObservable) {
		return Observable.create((ObservableEmitter<T> emitter) -> {
			if (fxObservable.getValue() != null)
				emitter.onNext(fxObservable.getValue());

			final ChangeListener<T> listener = (observableValue, prev, current) -> {
				if (current != null)
					emitter.onNext(current);
			};

			fxObservable.addListener(listener);
			emitter.setDisposable(Disposables.fromRunnable(() -> fxObservable.removeListener(listener)));
		});
	}

	public static <T> Observable<T> prevFromObservableValue(final ObservableValue<T> fxObservable) {
		return Observable.create((ObservableEmitter<T> emitter) -> {

			final ChangeListener<T> listener = (observableValue, prev, current) -> {
				if (prev != null)
					emitter.onNext(prev);
			};

			fxObservable.addListener(listener);
			emitter.setDisposable(Disposables.fromRunnable(() -> fxObservable.removeListener(listener)));
		});
	}

	private static <T> Observable<Optional<T>> fromNullableObservableValue(final ObservableValue<T> fxObservable) {
		return Observable.create((ObservableEmitter<Optional<T>> emitter) -> {
			if (fxObservable.getValue() != null) {
				emitter.onNext(Optional.of(fxObservable.getValue()));
			}

			final ChangeListener<T> listener = (observableValue, prev, current) -> {
				emitter.onNext(Optional.ofNullable(current));
			};

			fxObservable.addListener(listener);
			emitter.setDisposable(Disposables.fromRunnable(() -> fxObservable.removeListener(listener)));
		});
	}

	private static <T> Observable<List<T>> fromObservableList(final ObservableList<T> source) {
		return Observable.create((ObservableOnSubscribe<List<T>>) subscriber -> {
			ListChangeListener<T> listener = c -> subscriber.onNext(source);
			source.addListener(listener);
			subscriber.setDisposable(Disposables.fromRunnable(() -> source.removeListener(listener)));
		}).startWith(source);
	}

	private static <T> Observable<T> fromObservableListAdds(final ObservableList<T> source) {

		return Observable.create((ObservableOnSubscribe<T>) subscriber -> {

			ListChangeListener<T> listener = c -> {
				while (c.next())
					if (c.wasAdded())
						c.getAddedSubList().forEach(subscriber::onNext);
			};
			source.addListener(listener);
			subscriber.setDisposable(Disposables.fromRunnable(() -> source.removeListener(listener)));
		});
	}

	private static <T> Observable<T> fromObservableListRemovals(final ObservableList<T> source) {

		return Observable.create((ObservableOnSubscribe<T>) subscriber -> {
			ListChangeListener<T> listener = c -> {
				while (c.next())
					if (c.wasRemoved())
						c.getRemoved().forEach(subscriber::onNext);
			};
			source.addListener(listener);
			subscriber.setDisposable(Disposables.fromRunnable(() -> source.removeListener(listener)));
		});
	}

	private static <K,T> Observable<Entry<K,T>> fromObservableMapAdds(final ObservableMap<K,T> source) {

		return Observable.create((ObservableOnSubscribe<Entry<K,T>>) subscriber -> {

			MapChangeListener<K,T> listener = c -> {
				if (c.wasAdded())
					subscriber.onNext(new SimpleEntry<K,T>(c.getKey(),c.getValueAdded()));
			};
			source.addListener(listener);
			subscriber.setDisposable(Disposables.fromRunnable(() -> source.removeListener(listener)));
		});
	}

	private static <K,T> Observable<Entry<K,T>> fromObservableMapRemovals(final ObservableMap<K,T> source) {

		return Observable.create((ObservableOnSubscribe<Entry<K,T>>) subscriber -> {

			MapChangeListener<K,T> listener = c -> {
				if (c.wasRemoved())
					subscriber.onNext(new SimpleEntry<K,T>(c.getKey(),c.getValueRemoved()));
			};
			source.addListener(listener);
			subscriber.setDisposable(Disposables.fromRunnable(() -> source.removeListener(listener)));
		});
	}
}