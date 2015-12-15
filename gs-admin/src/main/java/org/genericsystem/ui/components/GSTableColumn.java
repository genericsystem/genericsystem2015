package org.genericsystem.ui.components;

import java.util.Optional;
import java.util.function.BiConsumer;
import java.util.function.Consumer;
import java.util.function.Function;

import javafx.beans.property.ObjectProperty;
import javafx.beans.property.SimpleObjectProperty;
import javafx.beans.value.ObservableValue;
import javafx.event.ActionEvent;
import javafx.event.EventHandler;
import javafx.geometry.Pos;
import javafx.scene.control.Alert;
import javafx.scene.control.Alert.AlertType;
import javafx.scene.control.Button;
import javafx.scene.control.ButtonType;
import javafx.scene.control.TableCell;
import javafx.scene.control.TableColumn;
import javafx.scene.control.TableColumn.CellDataFeatures;
import javafx.scene.control.TableView;
import javafx.util.Callback;

import org.genericsystem.ui.Binding;
import org.genericsystem.ui.Element;

public class GSTableColumn<T> extends Element<TableColumn> {

	public <M> GSTableColumn(Element parent, String columnTitle, Function<M, String> stringConverter) {
		super(parent, TableColumn.class, TableView<M>::getColumns);
		setText(columnTitle);
		setCellValueFactory(features -> new SimpleObjectProperty<>(stringConverter.apply((M) features.getValue())));
	}

	public <M> GSTableColumn(Element parent, Function<T, ObservableValue<String>> columnTitleObservable, Function<M, String> stringConverter) {
		super(parent, TableColumn.class, TableView<M>::getColumns);
		setObservableText(columnTitleObservable);
		setCellValueFactory(features -> new SimpleObjectProperty<>(stringConverter.apply((M) features.getValue())));
	}

	public GSTableColumn<T> setCellValueFactory(Callback<CellDataFeatures<T, String>, ObservableValue<String>> valueFactory) {
		super.addBoot(TableColumn::cellValueFactoryProperty, valueFactory);
		return this;
	}

	public GSTableColumn<T> setPrefWidth(Number prefWidth) {
		super.addBoot(TableColumn::prefWidthProperty, prefWidth);
		return this;
	}

	public GSTableColumn<T> setObservableText(Function<T, ObservableValue<String>> columnTitleObservable) {
		addBinding(TableColumn::textProperty, columnTitleObservable);
		return this;
	}

	public GSTableColumn<T> setText(String columnTitle) {
		addBoot(TableColumn::textProperty, columnTitle);
		return this;
	}

	public static class ActionTableColumn<T> extends TableColumn {
		ObjectProperty<Consumer<T>> onActionProperty = new SimpleObjectProperty<Consumer<T>>();

		public ObjectProperty<Consumer<T>> getOnActionProperty() {
			return onActionProperty;
		}
	}

	public static class GSTableColumnAction<SUPERMODEL, T> extends Element<ActionTableColumn> {

		public GSTableColumnAction(Element parent, String columnTitle, Function<T, String> stringConverter, BiConsumer<SUPERMODEL, T> action) {
			super(parent, ActionTableColumn.class, TableView<T>::getColumns);
			setText(columnTitle);
			setCellValueFactory(features -> new SimpleObjectProperty<>(stringConverter.apply(features.getValue())));
			Callback<ActionTableColumn<T>, TableCell<T, String>> callbackDelete = col -> new DeleteButtonCell<>((Consumer<T>) model -> col.getOnActionProperty().getValue().accept(model));
			super.addBoot(TableColumn::cellFactoryProperty, callbackDelete);
			pushModelActionOnSupermodel(action);

		}

		// public GSTableColumnAction<SUPERMODEL, T> setAction(Consumer<T> action) {
		// bindings.add(Binding.bindAction(ActionTableColumn::getOnActionProperty, action));
		// return this;
		// }

		public GSTableColumnAction<SUPERMODEL, T> pushModelActionOnSupermodel(BiConsumer<SUPERMODEL, T> action) {
			bindings.add(Binding.pushModelActionOnSuperModel(ActionTableColumn::getOnActionProperty, action));
			return this;
		}

		public GSTableColumnAction<SUPERMODEL, T> setCellValueFactory(Callback<CellDataFeatures<T, String>, ObservableValue<String>> valueFactory) {
			super.addBoot(TableColumn::cellValueFactoryProperty, valueFactory);
			return this;
		}

		public GSTableColumnAction<SUPERMODEL, T> setPrefWidth(Number prefWidth) {
			super.addBoot(TableColumn::prefWidthProperty, prefWidth);
			return this;
		}

		public GSTableColumnAction<SUPERMODEL, T> setObservableText(Function<T, ObservableValue<String>> columnTitleObservable) {
			addBinding(TableColumn::textProperty, columnTitleObservable);
			return this;
		}

		public GSTableColumnAction<SUPERMODEL, T> setText(String columnTitle) {
			addBoot(TableColumn::textProperty, columnTitle);
			return this;
		}

		public class DeleteButtonCell<U> extends TableCell<T, String> {
			private final Button cellButton = new Button();

			private final Consumer<T> action;

			public DeleteButtonCell(Consumer<T> action) {
				setEditable(true);
				cellButton.setMaxWidth(200);
				cellButton.setAlignment(Pos.BASELINE_CENTER);
				this.action = action;
			}

			@Override
			protected void updateItem(String t, boolean empty) {
				super.updateItem(t, empty);
				if (empty || t == null) {
					cellButton.setText(null);
					setGraphic(null);
				} else {
					cellButton.setText("Delete");
					setGraphic(cellButton);
					cellButton.setOnAction(new EventHandler<ActionEvent>() {
						@Override
						public void handle(ActionEvent event) {
							Alert alert = new Alert(AlertType.CONFIRMATION);
							alert.setTitle("Confirmation Dialog");
							alert.setHeaderText("Confirmation is required");
							alert.setContentText("Are you sure you want to delete : " + t + " ?");

							Optional<ButtonType> result = alert.showAndWait();
							if (result.get() == ButtonType.OK) {
								action.accept((T) getTableRow().getItem());
							}
						}
					});

				}
			}
		}
	}

}
