package org.genericsystem.reactor.gs;

import org.genericsystem.reactor.Tag;
import org.genericsystem.reactor.gs.GSSubcellDisplayer.GSInstanceSubcellDisplayer;
import org.genericsystem.reactor.gs.GSSubcellDisplayer.LinkTitleDisplayer;
import org.genericsystem.reactor.gstag.GSButton;
import org.genericsystem.reactor.gstag.GSH1;
import org.genericsystem.reactor.gstag.GSHyperLink;
import org.genericsystem.reactor.gstag.GSLabel;
import org.genericsystem.reactor.model.GenericModel;
import org.genericsystem.reactor.model.ObservableListExtractor;
import org.genericsystem.reactor.model.StringExtractor;

/**
 * @author Nicolas Feybesse
 *
 */
public class GSTable extends GSComposite implements SelectionDefaults {

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
						setStringExtractor(StringExtractor.MANAGEMENT);
						bindGenericText();
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
						addStyle("align-items", "center");
						new GSLabel(this) {
							{
								bindGenericText();
							}
						};

					};
				};
			}

			@Override
			protected void sections() {
				new LinkTitleDisplayer(this) {
					{
						forEach_(ObservableListExtractor.ATTRIBUTES_OF_TYPE);
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
		new GSInstanceBuilder(this, this.getReverseDirection());
	}

	@Override
	protected void sections() {
		Tag<GenericModel> selectableTag = new GSComposite(this, this.getReverseDirection()) {
			{
				addStyle("flex", "1");
				forEach_(ObservableListExtractor.SUBINSTANCES);
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
								"Color".equals(StringExtractor.SIMPLE_CLASS_EXTRACTOR.apply(modelContext.getGeneric().getMeta())) ? getString(modelContext).getValue() : "#bba5ff"));
						new GSHyperLink(this) {
							{
								bindGenericText();
								bindAction(model -> getSelectionProperty(model).setValue(model));
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
						forEach_(ObservableListExtractor.ATTRIBUTES_OF_INSTANCES);
						new GSInstanceSubcellDisplayer(this) {
							{
								forEach_(ObservableListExtractor.HOLDERS);
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
