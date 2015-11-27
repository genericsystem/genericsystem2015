package org.genericsystem.javafx;

import javafx.beans.value.ObservableValue;
import javafx.geometry.Insets;
import javafx.scene.control.Button;
import javafx.scene.layout.HBox;

import org.genericsystem.admin.UiFunctions;

public class CommandsPanel<G> extends HBox {

	private UiFunctions<G> gsFunctions;
	private Button flushButton;
	private Button clearButton;
	private Button shiftTsButton;
	private Button mountButton;
	private Button unmountButton;

	public CommandsPanel(ObservableValue<G> observableType, UiFunctions<G> gsFunctions) {
		super(5);

		this.gsFunctions = gsFunctions;

		flushButton = new Button("Flush");
		clearButton = new Button("Clear");
		shiftTsButton = new Button("ShiftTS");
		mountButton = new Button("Mount");
		unmountButton = new Button("Unmount");

		flushButton.setOnAction(e -> gsFunctions.flushConsumer.accept(observableType.getValue()));
		clearButton.setOnAction(e -> gsFunctions.clearConsumer.accept(observableType.getValue()));
		shiftTsButton.setOnAction(e -> gsFunctions.shiftTsConsumer.accept(observableType.getValue()));
		mountButton.setOnAction(e -> gsFunctions.mountConsumer.accept(observableType.getValue()));
		unmountButton.setOnAction(e -> gsFunctions.unmountConsumer.accept(observableType.getValue()));

		setPadding(new Insets(15, 12, 15, 12));
		getChildren().addAll(flushButton, mountButton, unmountButton, clearButton, shiftTsButton);
	}
}
