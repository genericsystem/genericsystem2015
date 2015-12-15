package org.genericsystem.ui.components;

import java.util.Optional;
import java.util.function.Consumer;
import java.util.function.Function;

import javafx.beans.property.SimpleObjectProperty;
import javafx.beans.value.ObservableValue;
import javafx.collections.ObservableList;
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

import org.genericsystem.todoKernel.Element;

public class GSTableColumn<T> extends Element<TableColumn> {

	protected static Function<TableView<?>, ObservableList<?>> getColumns = TableView::getColumns;
	private Callback<CellDataFeatures<T, String>, ObservableValue<String>> callback;

	public GSTableColumn(Element parent, Function<T, String> function) {
		super(parent, TableColumn.class, getColumns);
		callback = features -> new SimpleObjectProperty<>(function.apply(features.getValue()));
		super.addBoot(TableColumn::cellValueFactoryProperty, callback);
	}

	public GSTableColumn setWidth(Number width) {
		super.addBoot(TableColumn::prefWidthProperty, width);
		return this;
	}

	public <T> GSTableColumn setObservableTextProperty(Function<T, ObservableValue<String>> function) {
		addBinding(TableColumn::textProperty, function);
		return this;
	}

	public <T> GSTableColumn setText(String value) {
		addBoot(TableColumn::textProperty, value);
		return this;
	}

	public static class GSTableColumnAction<T> extends GSTableColumn<T> {
		private Callback<TableColumn<T, String>, TableCell<T, String>> callbackDelete;

		public GSTableColumnAction(Element parent, Function<T, String> function, Consumer<T> consumer) {
			super(parent, function);
			callbackDelete = col -> new DeleteButtonCell<>(consumer);
			super.addBoot(TableColumn::cellFactoryProperty, callbackDelete);
		}

		public class DeleteButtonCell<T> extends TableCell<T, String> {
			private final Button cellButton = new Button();

			private final Consumer<T> consumer;

			public DeleteButtonCell(Consumer<T> consumer) {
				setEditable(true);
				cellButton.setMaxWidth(200);
				cellButton.setAlignment(Pos.BASELINE_CENTER);
				this.consumer = consumer;
			}

			public DeleteButtonCell() {
				setEditable(true);
				cellButton.setMaxWidth(200);
				cellButton.setAlignment(Pos.BASELINE_CENTER);
				this.consumer = e -> {
				};
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
								consumer.accept((T) getTableRow().getItem());
							}
						}
					});

				}
			}
		}
	}

}
