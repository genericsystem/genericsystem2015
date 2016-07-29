package org.genericsystem.reactor.gs;

import java.io.Serializable;

import org.genericsystem.api.core.ApiStatics;
import org.genericsystem.api.core.exceptions.RollbackException;
import org.genericsystem.common.Generic;
import org.genericsystem.reactor.Model.TriFunction;
import org.genericsystem.reactor.ReactorStatics;
import org.genericsystem.reactor.Tag;
import org.genericsystem.reactor.Visitor.CheckInputsValidityVisitor;
import org.genericsystem.reactor.Visitor.ClearVisitor;
import org.genericsystem.reactor.Visitor.HolderVisitor;
import org.genericsystem.reactor.gs.GSLinks.GSAttributeCreator;
import org.genericsystem.reactor.gs.GSLinks.LinkDisplayer;
import org.genericsystem.reactor.gs.GSLinks.LinkTitleDisplayer;
import org.genericsystem.reactor.gstag.GSButton;
import org.genericsystem.reactor.gstag.GSH1;
import org.genericsystem.reactor.gstag.GSHyperLink;
import org.genericsystem.reactor.gstag.GSLabel;
import org.genericsystem.reactor.model.GenericModel;
import org.genericsystem.reactor.model.ObservableListExtractor;
import org.genericsystem.reactor.model.StringExtractor;

import javafx.beans.binding.Bindings;
import javafx.beans.value.ObservableValue;
import javafx.util.StringConverter;

/**
 * @author Nicolas Feybesse
 *
 */
public class GSTable extends GSComposite {

	public GSTable(GSTag parent) {
		super(parent, FlexDirection.COLUMN);
	}

	public GSTable(GSTag parent, FlexDirection flexDirection) {
		super(parent, flexDirection);
		addStyle("flex", "1");
	}

	@Override
	protected void header() {
		titleHeader();
		columnsTitleSection();
		columnsInputSection();
	}

	protected void titleHeader() {
		new GSSection(this, this.getReverseDirection()) {
			{
				addStyle("background-color", "#ffa500");
				addStyle("margin-right", "1px");
				addStyle("margin-bottom", "1px");
				addStyle("color", "red");
				addStyle("justify-content", "center");
				new GSH1(this) {
					{
						bindText(GenericModel::getString);
					}
				};
			}
		};
	}

	protected void columnsTitleSection() {
		new GSComposite(this, this.getReverseDirection()) {

			@Override
			protected void header() {
				new GSSection(this, this.getDirection()) {
					{
						addStyle("flex", "1");
						addStyle("color", "#ffffff");
						addStyle("background-color", "#ffa5a5");
						addStyle("margin-right", "1px");
						addStyle("margin-bottom", "1px");
						addStyle("justify-content", "center");
						new GSLabel(this) {
							{
								bindText(GenericModel::getString);
							}
						};

					};
				};
			}

			@Override
			protected void sections() {
				new LinkTitleDisplayer(this, gs -> ObservableListExtractor.COMPONENTS.apply(gs).filtered(g -> !g.equals(gs[1]))) {
					{
						forEach(StringExtractor.SIMPLE_CLASS_EXTRACTOR, ObservableListExtractor.ATTRIBUTES_OF_TYPE);
						addStyle("flex", "1");
						addStyle("overflow", "hidden");
					}
				};
			}

			@Override
			protected void footer() {
				new GSSection(this, this.getDirection()) {
					{
						if (this.getDirection().equals(FlexDirection.ROW)) {
							addStyle("flex", "0");
							addStyle("min-width", "100px");
						} else {
							addStyle("flex", "1");
						}
						addStyle("min-width", "100px");
						addStyle("background-color", "#ffa5a5");
						addStyle("margin-right", "1px");
						addStyle("margin-bottom", "1px");
					}
				};
			}
		};
	}

	protected void columnsInputSection() {
		new GSComposite(this, this.getReverseDirection()) {

			@Override
			protected void header() {
				new GSSection(this, this.getReverseDirection()) {
					{
						addStyle("flex", "1");
						addStyle("background-color", "#dda5a5");
						addStyle("margin-right", "1px");
						addStyle("margin-bottom", "1px");
						new GSInputTextWithConversion(this) {
							{
								createNewProperty(ReactorStatics.ACTION);
								this.<TriFunction<Generic[], Serializable, Generic, Generic>> initProperty(ReactorStatics.ACTION, (gs, value, g) -> gs[0].setInstance(value));
							}

							@Override
							public StringConverter<?> getConverter(GenericModel model) {
								Class<?> clazz = model.getGeneric().getInstanceValueClassConstraint();
								if (clazz == null)
									clazz = String.class;
								return ApiStatics.STRING_CONVERTERS.get(clazz);
							}
						};
					}
				};
			}

			@Override
			protected void sections() {
				new GSAttributeCreator(this, FlexDirection.ROW) {
					{
						forEach(StringExtractor.SIMPLE_CLASS_EXTRACTOR, ObservableListExtractor.ATTRIBUTES_OF_TYPE);
					}
				};
			}

			@Override
			protected void footer() {
				new GSSection(this, this.getDirection()) {
					{
						if (this.getDirection().equals(FlexDirection.ROW)) {
							addStyle("flex", "0");
							addStyle("min-width", "100px");
						} else {
							addStyle("flex", "1");
						}
						addStyle("background-color", "#dda5a5");
						addStyle("margin-right", "1px");
						addStyle("margin-bottom", "1px");
						new GSButton(this) {
							{
								bindAttribute(ReactorStatics.DISABLED, ReactorStatics.DISABLED, model -> {
									ObservableValue<Boolean> observable = new CheckInputsValidityVisitor(model).isInvalid();
									return Bindings.createStringBinding(() -> Boolean.TRUE.equals(observable.getValue()) ? ReactorStatics.DISABLED : "", observable);
								});
								bindAction(modelContext -> {
									try {
										new HolderVisitor().visit(modelContext);
										new ClearVisitor().visit(modelContext);
									} catch (RollbackException e) {
										e.printStackTrace();
									}
								});
								setText("Add");
								addStyle("width", "100%");
								addStyle("height", "100%");
							}
						};
					}
				};
			}
		};
	}

	@Override
	protected void sections() {
		Tag<GenericModel> selectableTag = new GSComposite(this, this.getReverseDirection()) {
			{
				addStyle("flex", "1");
				forEach(StringExtractor.SIMPLE_CLASS_EXTRACTOR, ObservableListExtractor.SUBINSTANCES);
			}

			@Override
			protected void header() {
				new GSSection(this, this.getReverseDirection()) {
					{
						addStyle("flex", "1");
						addStyle("margin-right", "1px");
						addStyle("margin-bottom", "1px");
						addStyle("overflow", "hidden");
						addPrefixBinding(modelContext -> modelContext.getObservableStyles(this).put("background-color",
								"Color".equals(StringExtractor.SIMPLE_CLASS_EXTRACTOR.apply(modelContext.getGeneric().getMeta())) ? modelContext.getString().getValue() : "#bba5ff"));
						new GSHyperLink(this) {
							{
								bindText(GenericModel::getString);
								bindAction(model -> getProperty(ReactorStatics.SELECTION, model).setValue(model));
							}
						};

					}
				};
			}

			@Override
			protected void sections() {
				new GSSection(this, FlexDirection.COLUMN) {
					{
						addStyle("flex", "1");
						addStyle("overflow", "hidden");
						forEach(StringExtractor.SIMPLE_CLASS_EXTRACTOR, ObservableListExtractor.ATTRIBUTES_OF_INSTANCES);
						new LinkDisplayer(this) {
							{
								addStyle("flex", "1");
								forEach(StringExtractor.SIMPLE_CLASS_EXTRACTOR, ObservableListExtractor.HOLDERS);
							}
						};
					}
				};
			}

			@Override
			protected void footer() {
				new GSSection(this, this.getDirection()) {
					{
						if (this.getDirection().equals(FlexDirection.ROW)) {
							addStyle("flex", "0");
							addStyle("min-width", "100px");
						} else {
							addStyle("flex", "1");
						}
						addStyle("background-color", "#dda5e2");
						addStyle("margin-right", "1px");
						addStyle("margin-bottom", "1px");
						new GSButton(this) {
							{
								setText("Remove");
								bindAction(GenericModel::remove);
								addStyle("width", "100%");
								addStyle("height", "100%");
							}
						};
					}
				};
			}
		};
		bindSelection(selectableTag);
	}
}
