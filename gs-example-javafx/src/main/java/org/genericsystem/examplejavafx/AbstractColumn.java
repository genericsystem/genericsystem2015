package org.genericsystem.examplejavafx;

import java.io.Serializable;
import java.util.HashMap;
import java.util.function.BiConsumer;
import java.util.function.Function;

import javafx.beans.property.SimpleObjectProperty;
import javafx.beans.value.ObservableValue;
import javafx.collections.FXCollections;
import javafx.scene.control.TableColumn;
import javafx.scene.control.cell.ComboBoxTableCell;
import javafx.util.StringConverter;
import javafx.util.converter.DefaultStringConverter;
import javafx.util.converter.IntegerStringConverter;

import org.genericsystem.mutability.Generic;

/**
 * @author Nicolas Feybesse
 *
 * @param <T>
 */
public class AbstractColumn<T> extends TableColumn<Generic,T> {
	protected final Observables<T> observables = new Observables<>();
	
	public AbstractColumn(Generic generic,String columnName,Function<Generic, T> getter,BiConsumer<Generic, T> setter) {
		super(columnName);
		setMinWidth(200);
		setCellValueFactory(cellData -> observables.get(cellData.getValue(),getter.apply(cellData.getValue())));
		setOnEditCommit((CellEditEvent<Generic, T> t) -> {
			System.out.println("coucou");
			Generic g =(t.getTableView().getItems().get(t.getTablePosition().getRow()));
			setter.accept(g, t.getNewValue());
			observables.get(g).set(t.getNewValue());
		});
		setEditable(true);	
	}
	
	public static class GenericColumn<T> extends AbstractColumn<T>{
		@SuppressWarnings("unchecked")
		public GenericColumn(Generic attribute,String columnName,Function<Generic, T> getter,BiConsumer<Generic, T> setter) {
			super(attribute, columnName, getter, setter);
			setCellFactory(tableColumn -> new EditingCell<Generic,T>(getDefaultConverter((Class<T>)attribute.getInstanceValueClassConstraint())));	
		}
	}
	
	public static class TargetComponentColumn extends AbstractColumn<Generic>{
		public TargetComponentColumn(Generic targetComponent,String columnName,Function<Generic, Generic> getter,BiConsumer<Generic, Generic> setter) {
			super(targetComponent, columnName, getter, setter);
			setCellFactory(tableColumn -> new ComboBoxTableCell<Generic,Generic>(new GenericStringConverter<>(targetComponent),FXCollections.<Generic>observableArrayList(targetComponent.getSubInstances().toList())));	
		}
	}
	
	public static class Observables<T> extends HashMap<Generic,SimpleObjectProperty<T>> {

		private static final long serialVersionUID = 7709729724315030415L;

		public ObservableValue<T> get(Object key,T value) {
			SimpleObjectProperty<T> observable = super.get(key);
			if(observable==null)
				put((Generic) key,observable=new SimpleObjectProperty<T>(value));
			return observable;
		};
	};
	
	static class GenericStringConverter<T extends Serializable> extends StringConverter<Generic> {

		private final StringConverter<T> instanceValueClassConverter;

		@SuppressWarnings("unchecked")
		GenericStringConverter(Generic component){
			this.instanceValueClassConverter = getDefaultConverter((Class<T>)component.getInstanceValueClassConstraint());
		}

		@SuppressWarnings("unchecked")
		@Override
		public String toString(Generic generic) {
			return instanceValueClassConverter.toString((T)generic.getValue());
		}

		@Override
		public Generic fromString(String string) {
			throw new IllegalStateException();
			//return component.getInstance(instanceValueClassConverter.fromString(string));
		}	
	}
	
	@SuppressWarnings("unchecked")
	static <T> StringConverter<T> getDefaultConverter(Class<T> clazz){
		if(Integer.class.equals(clazz))
			return (StringConverter<T>) new IntegerStringConverter();
		return (StringConverter<T>) new DefaultStringConverter();
	}

}