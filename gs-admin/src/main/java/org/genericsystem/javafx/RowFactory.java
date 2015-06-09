package org.genericsystem.javafx;

import java.util.function.Consumer;

import javafx.beans.binding.ObjectBinding;
import javafx.beans.value.ObservableValue;
import javafx.scene.control.ContextMenu;
import javafx.scene.control.TableRow;
import javafx.scene.control.TableView;
import javafx.util.Callback;

/**
 * @author Nicolas Feybesse
 *
 * @param <G>
 */
public class RowFactory<G> implements Callback<TableView<G>, TableRow<G>> {

	private final Consumer<G> removeConsumer;
	private ObservableValue<ContextMenu> rowMenu;

	public RowFactory(Consumer<G> removeConsumer) {
		this.removeConsumer = removeConsumer;
	}

	@Override
	public TableRow<G> call(TableView<G> tableView) {
		final TableRow<G> row = new TableRow<>();
		row.contextMenuProperty().bind(rowMenu = new ObjectBinding<ContextMenu>() {
			{
				super.bind(row.itemProperty());
			}

			@Override
			public void dispose() {
				super.unbind(row.itemProperty());
			}

			@Override
			protected ContextMenu computeValue() {
				return row.getItem() != null ? new DeleteContextMenu<G>(row, () -> tableView.getItems(), tableView.getContextMenu(), removeConsumer) : null;
			}
		});
		return row;
	}
}