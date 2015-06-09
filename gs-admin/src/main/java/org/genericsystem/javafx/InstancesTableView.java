package org.genericsystem.javafx;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.function.Function;

import javafx.beans.binding.Bindings;
import javafx.beans.binding.ListBinding;
import javafx.beans.binding.ObjectBinding;
import javafx.beans.value.ObservableValue;
import javafx.collections.FXCollections;
import javafx.collections.ObservableList;
import javafx.scene.control.ContextMenu;
import javafx.scene.control.TableColumn;
import javafx.scene.control.TableView;

import org.genericsystem.admin.UiFunctions;
import org.genericsystem.admin.UiFunctions.AttributeUiFunctions;
import org.genericsystem.javafx.AbstractColumn.GenericComponentColumn;
import org.genericsystem.javafx.AbstractColumn.TargetComponentColumn;
import org.genericsystem.mutability.Generic;

//import org.genericsystem.mutability.Generic;

/**
 * @author Nicolas Feybesse
 *
 */
public class InstancesTableView<G> extends TableView<G> {

	private final UiFunctions<G> gsFunctions;
	private final ObjectBinding<ContextMenu> contextMenuBinding;
	private final ListBinding<TableColumn<G, ?>> columnsBinding;
	private final ListBinding<G> itemsBinding;

	public InstancesTableView(ObservableValue<G> observableType, UiFunctions<G> gsFunctions) {
		this.gsFunctions = gsFunctions;
		setEditable(true);
		contextMenuProperty().bind(contextMenuBinding = new ObjectBinding<ContextMenu>() {
			{
				super.bind(observableType);
			}

			@Override
			public void dispose() {
				super.unbind(observableType);
			}

			@Override
			protected ContextMenu computeValue() {
				System.out.println("Compute context menu for : " + observableType.getValue());
				return observableType.getValue() != null ? new AddContextMenu<G>(observableType.getValue(), gsFunctions, () -> getItems()) : null;
			}
		});
		setRowFactory(new RowFactory<>(gsFunctions.removeConsumer));
		Bindings.bindContent(getColumns(), columnsBinding = new ListBinding<TableColumn<G, ?>>() {
			{
				super.bind(observableType);
			}

			@Override
			public void dispose() {
				super.unbind(observableType);
			}

			@Override
			protected ObservableList<TableColumn<G, ?>> computeValue() {
				System.out.println("Compute columns menu for : " + observableType.getValue());
				return FXCollections.observableArrayList(buildColumns(observableType));
			}
		});
		itemsProperty().set(itemsBinding = new ListBinding<G>() {
			{
				super.bind(observableType);
			}

			@Override
			public void dispose() {
				super.unbind(observableType);
			}

			@Override
			protected ObservableList<G> computeValue() {
				System.out.println("Compute items for : " + observableType.getValue());
				return observableType.getValue() != null ? gsFunctions.genericSubInstances.apply(observableType.getValue()) : FXCollections.emptyObservableList();
			}
		});
	}

	private List<TableColumn<G, ?>> buildColumns(ObservableValue<G> observableType) {
		List<TableColumn<G, ?>> columns = new ArrayList<TableColumn<G, ?>>();
		if (observableType.getValue() != null) {
			// columns.add(new EditColumn<G, Serializable>(observableType.getValue().toString(), gsFunctions.attributeConverter.apply(observableType.getValue()), gsFunctions.genericGetter, gsFunctions.genericSetter));
			columns.add(AbstractColumn.buildColumn(observableType.getValue().toString(), gsFunctions.attributeConverter.apply(observableType.getValue()), gsFunctions.genericGetter, gsFunctions.genericSetter));
			for (G attribute : gsFunctions.typeAttributes.apply(observableType.getValue())) {
				AttributeUiFunctions<G> attFunctions = gsFunctions.apply(attribute);
				Function<G, G> baseFirstLink = base -> gsFunctions.attributeGetter.apply(attribute).apply(base).stream().findFirst().orElse(null);
				List<G> attributeComponents = gsFunctions.genericComponents.apply(attribute);
				int pos = attributeComponents.indexOf(observableType.getValue());
				if ((((Generic) attribute).isSingularConstraintEnabled(pos) || ((Generic) attribute).isPropertyConstraintEnabled()) && attributeComponents.size() == 1)
					columns.add(AbstractColumn.buildColumn(attribute.toString(), gsFunctions.attributeConverter.apply(attribute), base -> gsFunctions.genericGetter.apply(baseFirstLink.apply(base)), (base, newValue) -> {
						G link = baseFirstLink.apply(base);
						if (link != null)
							gsFunctions.genericSetter.accept(link, newValue);
						else
							attFunctions.addAction.apply(newValue, Arrays.asList(base));
					}));
				else if (((Generic) attribute).isSingularConstraintEnabled(pos) && attributeComponents.size() == 2 && ((Generic) attribute).getInstanceValueGenerator() != null)
					columns.add(new TargetComponentColumn<G>(attribute.toString(), (base) -> gsFunctions.genericComponentGetter.apply(baseFirstLink.apply(base), pos == 0 ? 1 : 0), (base, newTarget) -> {
						G link = baseFirstLink.apply(base);
						if (link != null)
							gsFunctions.genericComponentSetter.accept(link, pos == 0 ? 1 : 0, newTarget);
						else {
							List<G> components = new ArrayList<G>(gsFunctions.genericComponents.apply(base));
							components.set(pos == 0 ? 1 : 0, newTarget);
							attFunctions.addAction.apply(null, components);
						}
					}, () -> gsFunctions.genericSubInstances.apply(gsFunctions.genericComponentGetter.apply(attribute, pos == 0 ? 1 : 0))));
				else
					columns.add(new GenericComponentColumn<G>(attribute, attFunctions, pos));
			}
		}
		return columns;
	}
}
