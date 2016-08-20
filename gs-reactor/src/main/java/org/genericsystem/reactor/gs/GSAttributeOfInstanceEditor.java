package org.genericsystem.reactor.gs;

import java.util.stream.Collectors;

import org.genericsystem.api.core.ApiStatics;
import org.genericsystem.reactor.gs.GSSubcellDisplayer.GSSubcellAdder;
import org.genericsystem.reactor.gs.GSSubcellDisplayer.GSSubcellEditor;
import org.genericsystem.reactor.gs.GSSubcellDisplayer.GSSubcellEditorWithRemoval;
import org.genericsystem.reactor.model.GenericModel;

import javafx.collections.FXCollections;

public class GSAttributeOfInstanceEditor extends GSSection {

	public GSAttributeOfInstanceEditor(GSTag parent) {
		super(parent, FlexDirection.COLUMN);
		addStyle("flex", "1");
		addStyle("overflow", "hidden");
		new GSSubcellEditor(this) {
			{
				addStyle("flex", "1");
				// forEach_ should work here, but it causes errors…
				select((model, holders) -> model.getGeneric().isRequiredConstraintEnabled(ApiStatics.BASE_POSITION) && holders.size() == 1
						? FXCollections.observableArrayList(holders.stream().map(holder -> new GenericModel(model, GenericModel.addToGenerics(holder, model.getGenerics()))).collect(Collectors.toList())) : FXCollections.emptyObservableList());
			}
		};
		new GSSubcellEditorWithRemoval(this) {
			{
				addStyle("flex", "1");
				select((model, holders) -> (!model.getGeneric().isRequiredConstraintEnabled(ApiStatics.BASE_POSITION) && holders.size() == 1) || holders.size() > 1
						? FXCollections.observableArrayList(holders.stream().map(holder -> new GenericModel(model, GenericModel.addToGenerics(holder, model.getGenerics()))).collect(Collectors.toList())) : FXCollections.emptyObservableList());
			}
		};
		new GSSubcellAdder(this) {
			{
				select((model, holders) -> holders.isEmpty() || (model.getGeneric().getComponents().size() < 2 && !model.getGeneric().isPropertyConstraintEnabled())
						|| (model.getGeneric().getComponents().size() >= 2 && !model.getGeneric().isSingularConstraintEnabled(ApiStatics.BASE_POSITION)) ? FXCollections.singletonObservableList(model) : FXCollections.emptyObservableList());
			}
		};
	}
}
