package org.genericsystem.reactor.gscomponents;

import org.genericsystem.api.core.ApiStatics;
import org.genericsystem.common.Generic;
import org.genericsystem.defaults.tools.BindingsTools;
import org.genericsystem.reactor.Tag;
import org.genericsystem.reactor.gscomponents.GSSubcellDisplayer.GSSubcellAdder;
import org.genericsystem.reactor.gscomponents.GSSubcellDisplayer.GSSubcellEditor;
import org.genericsystem.reactor.gscomponents.GSSubcellDisplayer.GSSubcellEditorWithRemoval;
import org.genericsystem.reactor.model.ObservableListExtractor;

import javafx.beans.binding.Bindings;
import javafx.beans.binding.ListBinding;
import javafx.collections.FXCollections;
import javafx.collections.ObservableList;

public class GSAttributeOfInstanceEditor extends GSDiv {

	public GSAttributeOfInstanceEditor() {
		super(FlexDirection.COLUMN);
		initEditor();
	}

	public GSAttributeOfInstanceEditor(Tag parent) {
		super(parent, FlexDirection.COLUMN);
		initEditor();
	}

	public void initEditor() {
		addStyle("flex", "1");
		addStyle("overflow", "auto");
		new GSMultiCheckBox(this) {
			{
				// reverseDirection();
				addStyle("flex", "1");
				select(gs -> gs[0].getComponents().size() == 2 && !gs[0].isSingularConstraintEnabled(gs[0].getComponents().indexOf(gs[2])) ? gs[0] : null);
			}
		};
		new GenericColumn(this) {
			{
				addStyle("flex", "1");
				addStyle("flex-wrap", "wrap");
				select(gs -> gs[0].getComponents().size() != 2 || gs[0].isSingularConstraintEnabled(gs[0].getComponents().indexOf(gs[2])) ? gs[0] : null);
				new GSSubcellEditor(this) {
					{
						addStyle("flex", "1 0 auto");
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
						addStyle("flex", "1 0 auto");
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
						addStyle("flex", "1 0 auto");
						select__(context -> {
							ObservableList<Generic> holders = ObservableListExtractor.HOLDERS.apply(context.getGenerics());
							return Bindings.createObjectBinding(() -> holders.isEmpty() || (!(context.getGeneric().getComponents().size() == 1 && context.getGeneric().isPropertyConstraintEnabled())
									&& !context.getGeneric().isSingularConstraintEnabled(context.getGeneric().getComponents().indexOf(context.getGenerics()[2]))) ? context : null, holders);
						});
					}
				};
			}
		};
	}
}
