package org.genericsystem.javafx;

import java.io.Serializable;
import java.util.Objects;
import java.util.function.BiConsumer;
import java.util.function.Function;
import java.util.function.Supplier;

import javafx.beans.property.Property;
import javafx.beans.property.SimpleBooleanProperty;
import javafx.collections.ObservableList;
import javafx.scene.control.TableColumn;
import javafx.scene.control.cell.CheckBoxTableCell;
import javafx.scene.control.cell.ChoiceBoxTableCell;
import javafx.util.StringConverter;
import javafx.util.converter.BooleanStringConverter;
import javafx.util.converter.DefaultStringConverter;
import javafx.util.converter.IntegerStringConverter;

import org.genericsystem.admin.UiFunctions.AttributeUiFunctions;

/**
 * @author Nicolas Feybesse
 *
 * @param <T>
 */
public abstract class AbstractColumn<G, T> extends TableColumn<G, T> {

	public AbstractColumn(String columnName, Function<G, T> genericValueGetter, BiConsumer<G, T> genericValueSetter) {
		super(columnName);
		GsPropertiesFactory<G, T> observables = buildObservables(genericValueGetter, genericValueSetter);
		// setMinWidth(200);
		setCellValueFactory(observables);
	}

	public static <G> TableColumn<G, ?> buildColumn(String columnName, StringConverter<Serializable> converter, Function<G, Serializable> genericValueGetter, BiConsumer<G, Serializable> genericValueSetter) {
		return BooleanStringConverter.class.equals(converter.getClass()) ? new CheckBoxColumn<G>(columnName, (Function) genericValueGetter, (BiConsumer) genericValueSetter) : new EditColumn<G, Serializable>(columnName, converter, genericValueGetter,
				genericValueSetter);
	}

	protected GsPropertiesFactory<G, T> buildObservables(Function<G, T> getter, BiConsumer<G, T> setter) {
		return new GsPropertiesFactory<>(getter, setter);
	}

	public static class CheckBoxColumn<G> extends AbstractColumn<G, Boolean> {
		public CheckBoxColumn(String columnName, Function<G, Boolean> genericValueGetter, BiConsumer<G, Boolean> genericValueSetter) {
			super(columnName, genericValueGetter, genericValueSetter);
			setCellFactory(CheckBoxTableCell.<G> forTableColumn(this));
		}

		@Override
		public GsPropertiesFactory<G, Boolean> buildObservables(Function<G, Boolean> genericValueGetter, BiConsumer<G, Boolean> genericValueSetter) {
			return new GsPropertiesFactory<G, Boolean>(genericValueGetter, genericValueSetter) {
				private static final long serialVersionUID = 2551229764188937954L;

				@Override
				protected Property<Boolean> buildProperty(Boolean value) {
					return new SimpleBooleanProperty(Boolean.TRUE.equals(value));
				}
			};
		}
	}

	public static class EditColumn<G, T> extends AbstractColumn<G, T> {
		public EditColumn(String columnName, StringConverter<T> columnConverter, Function<G, T> genericValueGetter, BiConsumer<G, T> genericValueSetter) {
			super(columnName, genericValueGetter, genericValueSetter);
			setOnEditCommit((CellEditEvent<G, T> t) -> {
				G g = (t.getTableView().getItems().get(t.getTablePosition().getRow()));
				((GsPropertiesFactory<G, T>) getCellValueFactory()).get(g).setValue(t.getNewValue());
			});
			setEditable(true);
			setCellFactory(tableColumn -> new EditingTableCell<G, T>(columnConverter));
			setPrefWidth(140);
		}

	}

	public static class TargetComponentColumn<G> extends AbstractColumn<G, G> {
		public TargetComponentColumn(String columnName, Function<G, G> genericValueGetter, BiConsumer<G, G> genericValueSetter, Supplier<ObservableList<G>> chooseList) {
			super(columnName, genericValueGetter, genericValueSetter);
			setOnEditCommit((CellEditEvent<G, G> t) -> {
				G g = (t.getTableView().getItems().get(t.getTablePosition().getRow()));
				((GsPropertiesFactory<G, G>) getCellValueFactory()).get(g).setValue(t.getNewValue());
			});
			setEditable(true);
			setCellFactory(ChoiceBoxTableCell.<G, G> forTableColumn(chooseList.get()));
		}
	}

	public static class GenericComponentColumn<G> extends AbstractColumn<G, ObservableList<G>> {
		public GenericComponentColumn(G attribute, AttributeUiFunctions<G> attFunctions, int pos) {
			super(attribute.toString(), attFunctions.linksGetter, null);
			setCellFactory(tableColumn -> new LinksTableCell<G>(attribute, attFunctions, pos));
			setPrefWidth(320);
		}

		@Override
		protected GsPropertiesFactory<G, ObservableList<G>> buildObservables(Function<G, ObservableList<G>> getter, BiConsumer<G, ObservableList<G>> setter) {
			return new GsPropertiesFactory<>(getter, setter);
		}
	}

	@SuppressWarnings("unchecked")
	public static <G, T> StringConverter<T> getDefaultInstanceValueStringConverter(Class<?> clazz) {
		if (Boolean.class.equals(clazz))
			return (StringConverter<T>) new BooleanStringConverter();
		if (Class.class.equals(clazz))
			return (StringConverter<T>) new StringConverter<Class<?>>() {

				@Override
				public String toString(Class<?> clazz) {
					return clazz != null ? clazz.getSimpleName() : null;
				}

				@Override
				public Class<?> fromString(String className) {
					try {
						return Class.forName(className);
					} catch (ClassNotFoundException e) {
						throw new IllegalStateException(e);
					}
				}
			};
		if (Integer.class.equals(clazz))
			return (StringConverter<T>) new IntegerStringConverter();
		if (String.class.equals(clazz))
			return (StringConverter<T>) new DefaultStringConverter();

		return new StringConverter<T>() {

			@Override
			public String toString(Object object) {
				if (object == null)
					return "null";
				if (object instanceof Class)
					return ((Class<?>) object).getSimpleName();
				return Objects.toString(object);
			}

			@Override
			public T fromString(String string) {
				throw new IllegalStateException("Unsupported Class write opertaion for class : " + clazz);
			}

		};
	}

}