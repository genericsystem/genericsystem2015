package org.genericsystem.reactor.model;

import javafx.beans.binding.Bindings;
import javafx.beans.property.Property;
import javafx.beans.property.ReadOnlyStringWrapper;
import javafx.beans.property.SimpleObjectProperty;
import javafx.beans.value.ObservableValue;

import org.genericsystem.common.Generic;
import org.genericsystem.reactor.Model;

/**
 * @author Nicolas Feybesse
 *
 * @param <M>
 */
public class GenericModel extends Model {

	private final Generic[] generics;
	private final StringExtractor stringExtractor;
	private final Property<GenericModel> selection = new SimpleObjectProperty<GenericModel>();
	private final ObservableValue<String> selectionString = Bindings.createStringBinding(
			() -> getStringExtractor().apply(getSelection().getValue() != null ? getSelection().getValue().getGeneric() : null), getSelection());
	private boolean selector = false;

	public GenericModel(Generic[] generics, StringExtractor stringExtractor) {
		assert stringExtractor != null;
		this.generics = generics;
		this.stringExtractor = stringExtractor;
	}

	public GenericModel(Model parent, Generic[] generics, StringExtractor stringExtractor) {
		this(generics, stringExtractor);
		this.parent = parent;
	}

	public Generic[] getGenerics() {
		return generics;
	}

	// TODO move this in Model properties by tag management ?
	@Deprecated
	public Property<GenericModel> getSelection() {
		return selection;
	}

	// TODO move this in Model properties by tag management ?
	@Deprecated
	public ObservableValue<String> getSelectionString() {
		return selectionString;
	}

	public boolean isSelector() {
		return selector;
	}

	public void enableSelectorBehavior() {
		this.selector = true;
	}

	public static Generic[] addToGenerics(Generic generic, Generic[] generics) {
		Generic[] result = new Generic[generics.length + 1];
		result[0] = generic;
		System.arraycopy(generics, 0, result, 1, generics.length);
		return result;
	}

	public Generic getGeneric() {
		return generics[0];
	}

	// TODO KK no cache ?
	public ObservableValue<String> getString() {
		return new ReadOnlyStringWrapper(stringExtractor.apply(getGenerics()[0]));
	}

	public StringExtractor getStringExtractor() {
		return stringExtractor;
	}

	public void remove() {
		System.out.println("remove!!!");
		getGeneric().remove();
	}

	public void select() {
		System.out.println("select click!!!");
	}

	public void flush() {
		getGeneric().getCurrentCache().flush();
	}

	public void cancel() {
		getGeneric().getCurrentCache().clear();
	}
}
