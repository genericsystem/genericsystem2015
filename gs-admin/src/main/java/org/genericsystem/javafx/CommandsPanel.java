package org.genericsystem.javafx;

import javafx.beans.value.ObservableValue;
import javafx.geometry.Insets;
import javafx.scene.control.Button;
import javafx.scene.layout.HBox;

import org.genericsystem.admin.UiFunctions;

public class CommandsPanel<G> extends HBox {

	private UiFunctions<G> gsFunctions;
	private Button flushButton;
	private Button cancelButton;
	private Button mountButton;
	private Button unmountButton;

	public CommandsPanel(ObservableValue<G> observableType, UiFunctions<G> gsFunctions) {
		super(5);

		this.gsFunctions = gsFunctions;

		flushButton = new Button("Flush");
		cancelButton = new Button("Cancel");
		mountButton = new Button("Mount");
		unmountButton = new Button("Unmount");

		flushButton.setOnAction(e -> gsFunctions.flushConsumer.accept(observableType.getValue()));
		cancelButton.setOnAction(e -> gsFunctions.cancelConsumer.accept(observableType.getValue()));
		mountButton.setOnAction(e -> gsFunctions.mountConsumer.accept(observableType.getValue()));
		unmountButton.setOnAction(e -> gsFunctions.unmountConsumer.accept(observableType.getValue()));

		setPadding(new Insets(15, 12, 15, 12));
		getChildren().addAll(flushButton, mountButton, unmountButton, cancelButton);
	}
}
