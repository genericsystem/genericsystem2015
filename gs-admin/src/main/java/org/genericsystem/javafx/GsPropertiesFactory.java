package org.genericsystem.javafx;

import java.util.HashMap;
import java.util.function.BiConsumer;
import java.util.function.Function;

import javafx.beans.property.Property;
import javafx.beans.property.SimpleObjectProperty;
import javafx.beans.value.ChangeListener;
import javafx.beans.value.ObservableValue;
import javafx.scene.control.TableColumn.CellDataFeatures;
import javafx.util.Callback;

/**
 * @author Nicolas Feybesse
 *
 * @param <S>
 * @param <T>
 */
public class GsPropertiesFactory<S, T> extends HashMap<S, Property<T>> implements Callback<CellDataFeatures<S, T>, ObservableValue<T>> {

	private static final long serialVersionUID = 7709729724315030415L;
	private final Function<S, T> getter;
	private final BiConsumer<S, T> setter;

	public GsPropertiesFactory(Function<S, T> getter, BiConsumer<S, T> setter) {
		this.getter = getter;
		this.setter = setter;
	}

	public ObservableValue<T> get(Object key, T value) {
		Property<T> observable = super.get(key);
		if (observable == null) {
			put((S) key, observable = buildProperty(value));
			observable.addListener(new ChangeListener<T>() {
				@Override
				public void changed(ObservableValue<? extends T> observable, T oldValue, T newValue) {
					System.out.println("Change : " + oldValue + " " + newValue);
					setter.accept((S) key, newValue);
				}
			});
		}
		return observable;
	};

	protected Property<T> buildProperty(T value) {
		return new SimpleObjectProperty<T>(value);
	}

	@Override
	public ObservableValue<T> call(CellDataFeatures<S, T> cellData) {
		return get(cellData.getValue(), getter.apply(cellData.getValue()));
	}
}