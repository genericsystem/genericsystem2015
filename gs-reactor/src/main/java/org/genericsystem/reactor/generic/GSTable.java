package org.genericsystem.reactor.generic;

import java.io.Serializable;
import java.util.Map;

import javafx.beans.binding.Bindings;
import javafx.beans.value.ChangeListener;
import javafx.beans.value.ObservableValue;
import javafx.util.StringConverter;

import org.genericsystem.api.core.ApiStatics;
import org.genericsystem.api.core.exceptions.RollbackException;
import org.genericsystem.common.Generic;
import org.genericsystem.reactor.Model.TriFunction;
import org.genericsystem.reactor.ReactorStatics;
import org.genericsystem.reactor.Tag;
import org.genericsystem.reactor.Visitor.CheckInputsValidityVisitor;
import org.genericsystem.reactor.Visitor.ClearVisitor;
import org.genericsystem.reactor.Visitor.HolderVisitor;
import org.genericsystem.reactor.composite.CompositeSelect.CompositeSelectWithEmptyEntry;
import org.genericsystem.reactor.generic.GSLinks.HtmlGenericInputText;
import org.genericsystem.reactor.generic.GSLinks.LinkDisplayer;
import org.genericsystem.reactor.generic.GSLinks.LinkTitleDisplayer;
import org.genericsystem.reactor.html.HtmlButton;
import org.genericsystem.reactor.html.HtmlCheckBox;
import org.genericsystem.reactor.html.HtmlH1;
import org.genericsystem.reactor.html.HtmlHyperLink;
import org.genericsystem.reactor.html.HtmlLabel;
import org.genericsystem.reactor.model.GenericModel;
import org.genericsystem.reactor.model.ObservableListExtractor;
import org.genericsystem.reactor.model.StringExtractor;

/**
 * @author Nicolas Feybesse
 *
 */
public class GSTable extends GSComposite {

	public GSTable(Tag<?> parent) {
		super(parent, FlexDirection.COLUMN);
	}

	public GSTable(Tag<?> parent, FlexDirection flexDirection) {
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
				new HtmlH1<GenericModel>(this) {
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
						new HtmlLabel<GenericModel>(this) {
							{
								bindText(GenericModel::getString);
							}
						};

					};
				};
			}

			@Override
			protected void sections() {
				new LinkTitleDisplayer(this, gs -> ObservableListExtractor.COMPONENTS.apply(gs).filtered(g -> !g.equals(gs[1])), this.getDirection()) {
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
						new HtmlGenericInputText(this) {
							{
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
				new GSSection(this, this.getDirection()) {
					{
						addStyle("flex", "1");
						addStyle("overflow", "hidden");
						forEach(StringExtractor.SIMPLE_CLASS_EXTRACTOR, ObservableListExtractor.ATTRIBUTES_OF_TYPE);
						new GSSection(this, this.getReverseDirection()) {
							{
								addStyle("flex", "1");
								addStyle("color", "#ffffff");
								addStyle("background-color", "#dda5a5");
								addStyle("margin-right", "1px");
								addStyle("margin-bottom", "1px");
								addStyle("overflow", "hidden");
								select(gs -> gs[0].getComponents().size() < 2 ? gs[0] : null);
								new GSSection(this, this.getDirection()) {
									{
										addStyle("justify-content", "center");
										addStyle("align-items", "center");
										addStyle("width", "100%");
										addStyle("height", "100%");
										new HtmlGenericInputText(this) {
											{
												select(gs -> !Boolean.class.equals(gs[0].getInstanceValueClassConstraint()) ? gs[0] : null);
												this.<TriFunction<Generic[], Serializable, Generic, Generic>> initProperty(ReactorStatics.ACTION, (gs, value, g) -> g.setHolder(gs[1], value));
											}

											@Override
											public StringConverter<?> getConverter(GenericModel model) {
												Class<?> clazz = model.getGeneric().getInstanceValueClassConstraint();
												if (clazz == null)
													clazz = String.class;
												return ApiStatics.STRING_CONVERTERS.get(clazz);
											}
										};
										new HtmlCheckBox<GenericModel>(this) {
											{
												select(gs -> Boolean.class.equals(gs[0].getInstanceValueClassConstraint()) ? gs[0] : null);
												bindOptionalBiDirectionalAttribute(ReactorStatics.VALUE, ReactorStatics.CHECKED, ReactorStatics.CHECKED);
												this.<TriFunction<Generic[], Serializable, Generic, Generic>> initProperty(ReactorStatics.ACTION, (gs, value, g) -> g.setHolder(gs[1], value));
											}
										};
									}
								};
							}
						};
						new GSSection(this, this.getReverseDirection()) {
							{
								addStyle("flex", "1");
								addStyle("color", "#ffffff");
								addStyle("background-color", "#dda5a5");
								addStyle("margin-right", "1px");
								addStyle("margin-bottom", "1px");
								addStyle("overflow", "hidden");
								forEach(StringExtractor.SIMPLE_CLASS_EXTRACTOR, gs -> ObservableListExtractor.COMPONENTS.apply(gs).filtered(g -> !g.equals(gs[1])));
								new CompositeSelectWithEmptyEntry(this) {
									{
										addStyle("width", "100%");
										addStyle("height", "100%");
										addPrefixBinding(model -> {
											if ("Color".equals(StringExtractor.SIMPLE_CLASS_EXTRACTOR.apply(model.getGeneric()))) {
												Map<String, String> map = model.getObservableStyles(this);
												ChangeListener<String> listener = (o, old, newValue) -> map.put("background-color", newValue);
												ObservableValue<String> observable = model.getObservableValue(this, ReactorStatics.SELECTION_STRING);
												observable.addListener(listener);
												map.put("background-color", observable.getValue());
											}
										});
										optionElement.addPrefixBinding(model -> {
											if ("Color".equals(StringExtractor.SIMPLE_CLASS_EXTRACTOR.apply(model.getGeneric().getMeta())))
												model.getObservableStyles(optionElement).put("background-color", model.getString().getValue());
										});
										// bindStyle("background-color", GenericModel::getSelectionString);
										// optionElement.bindStyle("background-color", GenericModel::getString);
									}
								};
							}
						};
					}
				};
			};

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
						new HtmlButton<GenericModel>(this) {
							{
								// storeProperty(ReactorStatics.DISABLED, model -> {
								// ObservableValue<Boolean> observable = new CheckInputsValidityVisitor(model).isInvalid();
								// return Bindings.createStringBinding(() -> Boolean.TRUE.equals(observable.getValue()) ? "disabled" : "", observable);
								// });
								bindAttribute("disabled", ReactorStatics.DISABLED, model -> {
									ObservableValue<Boolean> observable = new CheckInputsValidityVisitor(model).isInvalid();
									return Bindings.createStringBinding(() -> Boolean.TRUE.equals(observable.getValue()) ? "disabled" : "", observable);
								});// model -> new CheckInputsValidityVisitor(model).isInvalid(), "disabled");
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
						new HtmlHyperLink<GenericModel>(this) {
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
				new GSSection(this, this.getReverseDirection()) {
					{
						addStyle("flex", "1");
						addStyle("overflow", "hidden");
						forEach(StringExtractor.SIMPLE_CLASS_EXTRACTOR, ObservableListExtractor.ATTRIBUTES_OF_INSTANCES);
						new LinkDisplayer(this, this.getReverseDirection()) {
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
						new HtmlButton<GenericModel>(this) {
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
