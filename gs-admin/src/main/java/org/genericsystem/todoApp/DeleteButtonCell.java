package org.genericsystem.todoApp;

import java.util.Optional;

import javafx.event.ActionEvent;
import javafx.event.EventHandler;
import javafx.geometry.Pos;
import javafx.scene.control.Alert;
import javafx.scene.control.Alert.AlertType;
import javafx.scene.control.Button;
import javafx.scene.control.ButtonType;
import javafx.scene.control.TableCell;

public class DeleteButtonCell<T> extends TableCell<T, String> {
	private final Button cellButton = new Button();

	public DeleteButtonCell() {

		cellButton.setMaxWidth(200);
		cellButton.setAlignment(Pos.BASELINE_CENTER);
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
						getTableView().getItems().remove(getTableRow().getItem());
					}
				}
			});

		}
	}
}