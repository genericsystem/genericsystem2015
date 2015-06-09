package org.genericsystem.examplejavafx;

import java.util.Objects;

import javafx.application.Platform;
import javafx.beans.value.ChangeListener;
import javafx.beans.value.ObservableValue;
import javafx.event.Event;
import javafx.event.EventHandler;
import javafx.scene.control.ContentDisplay;
import javafx.scene.control.TableCell;
import javafx.scene.control.TableColumn;
import javafx.scene.control.TableColumn.CellEditEvent;
import javafx.scene.control.TablePosition;
import javafx.scene.control.TableView;
import javafx.scene.control.TextField;
import javafx.scene.input.KeyCode;
import javafx.scene.input.KeyEvent;
import javafx.util.StringConverter;


/**
 * @author Nicolas Feybesse
 *
 * @param <S>
 * @param <T>
 */
public class EditingCell<S,T> extends TableCell<S, T> {

	private final TextField textField;
	private StringConverter<T> converter;


	public EditingCell(StringConverter<T> converter) {
		this.converter = converter;

		textField = new TextField(converter.toString(getItem()));

		textField.focusedProperty().addListener(new ChangeListener<Boolean>() {
			public void changed(ObservableValue<? extends Boolean> observable, Boolean oldValue, Boolean newValue) {
				if(!newValue.booleanValue() && textField != null) {
					System.out.println("coucou lost focus : "+textField.getText());
					Platform.runLater(new Runnable() {
						@Override
						public void run() {
							commitEdit(converter.fromString(textField.getText()));
						}
					});
				}
			}
		});

		textField.setOnKeyReleased(new EventHandler<KeyEvent>() {
			@Override public void handle(KeyEvent t) {
				if (t.getCode() == KeyCode.ENTER) {
					commitEdit(converter.fromString(textField.getText()));
				} else if (t.getCode() == KeyCode.ESCAPE) {
					cancelEdit();
				}
			}
		});
	}

	@Override
	public void startEdit() {
		super.startEdit();
		setGraphic(textField);
		setContentDisplay(ContentDisplay.GRAPHIC_ONLY);
		Platform.runLater(new Runnable() {
			@Override
			public void run() {
				textField.requestFocus();
				textField.selectAll();
			}
		});
	}

	@Override
	public void cancelEdit() {
		super.cancelEdit();
		setText(converter.toString(getItem()));
		setContentDisplay(ContentDisplay.TEXT_ONLY);
	}

	@Override
	public void updateItem(T item, boolean empty) {
		super.updateItem(item, empty);
		if (empty) {
			setText(null);
			setGraphic(null);
		} else {
			if (isEditing()) {
				if (textField != null) {
					textField.setText(converter.toString(getItem()));
				}
				setGraphic(textField);
				setContentDisplay(ContentDisplay.GRAPHIC_ONLY);
			} else {
				setText(converter.toString(getItem()));
				setContentDisplay(ContentDisplay.TEXT_ONLY);
			}
		}
	}

	@Override
	public void commitEdit(T item) {
		if (! isEditing() && ! Objects.equals(item,getItem())) {
			TableView<S> table = getTableView();
			if (table != null) {
				TableColumn<S, T> column = getTableColumn();
				CellEditEvent<S, T> event = new CellEditEvent<>(table, 
						new TablePosition<S,T>(table, getIndex(), column), 
						TableColumn.editCommitEvent(), item);
				Event.fireEvent(column, event);
			}
		}

		super.commitEdit(item);

		setContentDisplay(ContentDisplay.TEXT_ONLY);
	}
}
