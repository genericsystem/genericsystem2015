package org.genericsystem.javafx;

import javafx.beans.binding.Bindings;
import javafx.beans.property.SimpleIntegerProperty;
import javafx.collections.ObservableList;
import javafx.scene.control.TableCell;

import org.genericsystem.admin.UiFunctions.AttributeUiFunctions;

/**
 * @author Nicolas Feybesse
 *
 * @param <G>
 */
public class LinksTableCell<G> extends TableCell<G, ObservableList<G>> {
	private final LinksTableView<G> linksTableView;

	private final G attribute;
	private final int pos;
	private final AttributeUiFunctions<G> attFunctions;

	public LinksTableCell(G attribute, AttributeUiFunctions<G> attFunctions, int pos) {
		this.attribute = attribute;
		this.pos = pos;
		this.attFunctions = attFunctions;

		this.linksTableView = new LinksTableView<>(attribute, attFunctions, pos);
		prefHeightProperty().bind(new SimpleIntegerProperty(112).multiply(Bindings.size(linksTableView.getItems()).add(1.01)));
		// setPrefHeight(((Generic) attribute).isSingularConstraintEnabled(0) ? 30 : 100);
		// setMinHeight(((Generic) attribute).isSingularConstraintEnabled(0) ? 30 : 100);
		// // setMaxHeight(((Generic) attribute).isSingularConstraintEnabled(0) ? 30 : 100);
		// linksTableView.setPrefHeight(((Generic) attribute).isSingularConstraintEnabled(0) ? 5 : 50);
		// linksTableView.setMinHeight(((Generic) attribute).isSingularConstraintEnabled(0) ? 5 : 50);
		// linksTableView.setMaxHeight(((Generic) attribute).isSingularConstraintEnabled(0) ? 5 : 50);
	}

	@Override
	protected void updateItem(ObservableList<G> observableLinks, boolean empty) {
		super.updateItem(observableLinks, empty);
		if (empty || observableLinks == null) {
			linksTableView.setItems(null);
			linksTableView.setContextMenu(null);
			setGraphic(null);
			setText(null);
		} else {
			G base = (G) getTableRow().getItem();
			if (base != null)
				linksTableView.setContextMenu(new AddContextMenu<>(base, attribute, attFunctions, pos, () -> linksTableView.getItems(), getTableRow().getContextMenu()));
			linksTableView.setItems(observableLinks);
			setGraphic(linksTableView);
		}
	}
}