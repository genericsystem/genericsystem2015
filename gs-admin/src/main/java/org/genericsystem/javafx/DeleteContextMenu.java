package org.genericsystem.javafx;

import java.util.Optional;
import java.util.function.Consumer;
import java.util.function.Supplier;

import javafx.beans.binding.Bindings;
import javafx.collections.ObservableList;
import javafx.event.ActionEvent;
import javafx.event.EventHandler;
import javafx.scene.control.Alert;
import javafx.scene.control.Alert.AlertType;
import javafx.scene.control.ButtonType;
import javafx.scene.control.ContextMenu;
import javafx.scene.control.MenuItem;
import javafx.scene.control.TableRow;
import javafx.scene.image.Image;
import javafx.scene.image.ImageView;

/**
 * @author Nicolas Feybesse
 *
 * @param <G>
 */
public class DeleteContextMenu<G> extends ContextMenu {

	public DeleteContextMenu(TableRow<G> row, Supplier<ObservableList<G>> items, ContextMenu aboveMenu, Consumer<G> removeConsumer) {

		final MenuItem deleteItem = new MenuItem("Delete : " + row.getItem(), new ImageView(new Image(getClass().getResourceAsStream("not.png"))));
		deleteItem.setOnAction((EventHandler<ActionEvent>) event -> {
			Alert alert = new Alert(AlertType.CONFIRMATION);
			alert.setTitle("Confirmation Dialog");
			alert.setHeaderText("Confirmation is required");
			alert.setContentText("Delete : " + row.getItem() + " ?");
			Optional<ButtonType> result = alert.showAndWait();
			if (result.get() == ButtonType.OK) {
				removeConsumer.accept(row.getItem());
				items.get().remove(row.getItem());
			}

		});

		getItems().add(deleteItem);
		if (aboveMenu != null) {
			aboveMenu.getItems().forEach(item -> {
				MenuItem newItem = new MenuItem(item.getText(), new ImageView(((ImageView) item.getGraphic()).getImage()));
				newItem.onActionProperty().bind(item.onActionProperty());
				getItems().add(newItem);
			});
		}
		deleteItem.textProperty().bind(Bindings.createStringBinding(() -> "Delete : " + row.getItem(), row.itemProperty()));
	}
}