package org.genericsystem.gui.javafx;

import javafx.beans.binding.Bindings;
import javafx.beans.binding.ListBinding;
import javafx.beans.value.ObservableValue;
import javafx.collections.FXCollections;
import javafx.collections.ObservableList;
import javafx.scene.Node;
import javafx.scene.layout.VBox;

import org.genericsystem.gui.context.GenericContext;
import org.genericsystem.gui.context.OldRootContext;
import org.genericsystem.gui.context.TableViewContext;

/**
 * @author Nicolas Feybesse
 *
 */
public class Crud extends VBox {

	private OldRootContext rootContext;
	public TableViewContext tableViewContext;
	public TableViewContext tableViewContext2;

	public Crud(OldRootContext rootContext) {
		this.rootContext = rootContext;
		tableViewContext = new TableViewContext(rootContext);
		GSTableView tableViewEngineInstances = new GSTableView(tableViewContext);

		Bindings.bindContent(getChildren(), new ListBinding<Node>() {
			ObservableValue<GenericContext> selectedContext = tableViewEngineInstances.getSelectionModel().selectedItemProperty();
			{
				super.bind(selectedContext);
			}

			@Override
			protected ObservableList<Node> computeValue() {
				ObservableList<Node> nodes = FXCollections.observableArrayList();
				nodes.add(tableViewEngineInstances);
				if (selectedContext.getValue() != null) {
					tableViewContext2 = new TableViewContext(selectedContext.getValue());
					GSTableView TableViewGenericInstances = new GSTableView(tableViewContext2);
					nodes.add(TableViewGenericInstances);
				}
				return nodes;
			}
		});
	}

	public OldRootContext getContext() {
		return rootContext;
	}
}
