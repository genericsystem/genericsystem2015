package org.genericsystem.reactor.gs;

import org.genericsystem.api.core.ApiStatics;
import org.genericsystem.common.Generic;
import org.genericsystem.defaults.tools.BindingsTools;
import org.genericsystem.reactor.Tag;
import org.genericsystem.reactor.gs.GSSubcellDisplayer.GSSubcellAdder;
import org.genericsystem.reactor.gs.GSSubcellDisplayer.GSSubcellEditor;
import org.genericsystem.reactor.gs.GSSubcellDisplayer.GSSubcellEditorWithRemoval;
import org.genericsystem.reactor.model.ObservableListExtractor;

import javafx.beans.binding.Bindings;
import javafx.beans.binding.ListBinding;
import javafx.collections.FXCollections;
import javafx.collections.ObservableList;

public class GSAttributeOfInstanceEditor extends GSDiv {

	public GSAttributeOfInstanceEditor(Tag parent) {
		super(parent, FlexDirection.COLUMN);
		addStyle("flex", "1");
		addStyle("overflow", "hidden");
		new GSMultiCheckBox(this, ((GSDiv) parent).getReverseDirection()) {
			{
				addStyle("flex", "1");
				select(gs -> gs[0].getComponents().size() == 2 && !gs[0].isSingularConstraintEnabled(ApiStatics.BASE_POSITION) ? gs[0] : null);
			}
		};
		new GenericColumn(this) {
			{
				addStyle("flex", "1");
				select(gs -> (gs[0].getComponents().size() != 2 || gs[0].isSingularConstraintEnabled(ApiStatics.BASE_POSITION)) ? gs[0] : null);
				new GSSubcellEditor(this) {
					{
						addStyle("flex", "1");
						forEach2(model -> BindingsTools.transmitSuccessiveInvalidations(new ListBinding<Generic>() {
							ObservableList<Generic> holders = ObservableListExtractor.HOLDERS.apply(model.getGenerics());
							{
								bind(holders);
							}

							@Override
							protected ObservableList<Generic> computeValue() {
								return model.getGeneric().isRequiredConstraintEnabled(ApiStatics.BASE_POSITION) && holders.size() == 1 ? FXCollections.observableArrayList(holders) : FXCollections.emptyObservableList();
							}
						}));
					}
				};
				new GSSubcellEditorWithRemoval(this) {
					{
						addStyle("flex", "1");
						forEach2(model -> BindingsTools.transmitSuccessiveInvalidations(new ListBinding<Generic>() {
							ObservableList<Generic> holders = ObservableListExtractor.HOLDERS.apply(model.getGenerics());
							{
								bind(holders);
							}

							@Override
							protected ObservableList<Generic> computeValue() {
								return (!model.getGeneric().isRequiredConstraintEnabled(ApiStatics.BASE_POSITION) && holders.size() == 1) || holders.size() > 1 ? FXCollections.observableArrayList(holders) : FXCollections.emptyObservableList();
							}
						}));
					}
				};
				new GSSubcellAdder(this) {
					{
						select__(model -> Bindings.createObjectBinding(() -> {
							ObservableList<Generic> holders = ObservableListExtractor.HOLDERS.apply(model.getGenerics());
							return holders.isEmpty() || (model.getGeneric().getComponents().size() < 2 && !model.getGeneric().isPropertyConstraintEnabled())
									|| (model.getGeneric().getComponents().size() >= 2 && !model.getGeneric().isSingularConstraintEnabled(ApiStatics.BASE_POSITION)) ? model : null;
						}, ObservableListExtractor.HOLDERS.apply(model.getGenerics())));
					}
				};
			}
		};
	}
}
