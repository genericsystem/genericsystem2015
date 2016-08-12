package org.genericsystem.reactor.gs;

import java.util.stream.Collectors;

import org.genericsystem.api.core.ApiStatics;
import org.genericsystem.common.Generic;
import org.genericsystem.reactor.gs.GSSubcellDisplayer.GSSubcellAdder;
import org.genericsystem.reactor.gs.GSSubcellDisplayer.GSSubcellEditor;
import org.genericsystem.reactor.gs.GSSubcellDisplayer.GSSubcellEditorWithRemoval;
import org.genericsystem.reactor.gs.GSSubcellDisplayer.InstanceLinkTitleDisplayer;
import org.genericsystem.reactor.gstag.GSH1;
import org.genericsystem.reactor.model.GenericModel;
import org.genericsystem.reactor.model.ObservableListExtractor;
import org.genericsystem.reactor.model.StringExtractor;

import javafx.beans.binding.ListBinding;
import javafx.collections.FXCollections;
import javafx.collections.ObservableList;

/**
 * @author Nicolas Feybesse
 *
 * @param <M>
 */
public class GSEditor extends GSComposite {

	public GSEditor(GSTag parent) {
		this(parent, FlexDirection.COLUMN);
	}

	public GSEditor(GSTag parent, FlexDirection flexDirection) {
		super(parent, flexDirection);
		addStyle("flex", "1");
	}

	@Override
	protected void header() {
		new GSSection(this, GSEditor.this.getReverseDirection()) {
			{
				addStyle("flex", "0.3");
				addStyle("background-color", "#ffa500");
				addStyle("margin-right", "1px");
				addStyle("margin-bottom", "1px");
				addStyle("color", "red");
				addStyle("justify-content", "center");
				addStyle("align-items", "center");
				new GSH1(this) {
					{
						setStringExtractor(this, StringExtractor.TYPE_INSTANCE_EXTRACTOR);
						bindGenericText(this);
					}
				};
			}
		};
	}

	@Override
	protected void sections() {

		new GSComposite(this, GSEditor.this.getReverseDirection()) {
			{
				addStyle("flex", "1");
			}

			@Override
			protected void header() {
				new GSSection(this, GSEditor.this.getDirection()) {
					{
						addStyle("flex", "0.3");
						new InstanceLinkTitleDisplayer(this) {
							{
								select(gs -> gs[0].getMeta());
							}
						};
						new InstanceLinkTitleDisplayer(this) {
							{
								forEach_(ObservableListExtractor.ATTRIBUTES_OF_INSTANCES);
							}
						};
					}
				};
			}

			@Override
			protected void sections() {
				new GSSection(this, GSEditor.this.getDirection()) {
					{
						addStyle("flex", "1");
						new GSSection(this, FlexDirection.COLUMN) {
							{
								addStyle("flex", "1");
								addStyle("overflow", "hidden");
								new GSSubcellEditor(this);
							}
						};
						new GSSection(this, FlexDirection.COLUMN) {
							{
								forEach_(ObservableListExtractor.ATTRIBUTES_OF_INSTANCES);
								addStyle("flex", "1");
								addStyle("overflow", "hidden");
								new GSSubcellEditor(this) {
									{
										addStyle("flex", "1");
										// forEach_ should work here, but it causes errors…
										select__(model -> new ListBinding<GenericModel>() {
											ObservableList<Generic> holders = ObservableListExtractor.HOLDERS.apply(model.getGenerics());
											{
												bind(holders);
											}

											@Override
											protected ObservableList<GenericModel> computeValue() {
												return model.getGeneric().isRequiredConstraintEnabled(ApiStatics.BASE_POSITION) && holders.size() == 1
														? FXCollections.observableArrayList(holders.stream().map(holder -> new GenericModel(model, GenericModel.addToGenerics(holder, model.getGenerics()))).collect(Collectors.toList()))
														: FXCollections.emptyObservableList();
											}
										});
									}
								};
								new GSSubcellEditorWithRemoval(this) {
									{
										addStyle("flex", "1");
										select__(model -> new ListBinding<GenericModel>() {
											ObservableList<Generic> holders = ObservableListExtractor.HOLDERS.apply(model.getGenerics());
											{
												bind(holders);
											}

											@Override
											protected ObservableList<GenericModel> computeValue() {
												return (!model.getGeneric().isRequiredConstraintEnabled(ApiStatics.BASE_POSITION) && holders.size() == 1) || holders.size() > 1
														? FXCollections.observableArrayList(holders.stream().map(holder -> new GenericModel(model, GenericModel.addToGenerics(holder, model.getGenerics()))).collect(Collectors.toList()))
														: FXCollections.emptyObservableList();
											}
										});
									}
								};
								new GSSubcellAdder(this) {
									{
										select__(model -> new ListBinding<GenericModel>() {
											ObservableList<Generic> holders = ObservableListExtractor.HOLDERS.apply(model.getGenerics());
											{
												bind(holders);
											}

											@Override
											protected ObservableList<GenericModel> computeValue() {
												return holders.isEmpty() || (model.getGeneric().getComponents().size() < 2 && !model.getGeneric().isPropertyConstraintEnabled())
														|| (model.getGeneric().getComponents().size() >= 2 && !model.getGeneric().isSingularConstraintEnabled(ApiStatics.BASE_POSITION)) ? FXCollections.singletonObservableList(model)
																: FXCollections.emptyObservableList();
											}
										});
									}
								};
							}
						};
					}
				};
			};
		};
	}
}
