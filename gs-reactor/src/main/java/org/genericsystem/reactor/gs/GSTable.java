package org.genericsystem.reactor.gs;

import org.genericsystem.reactor.ReactorStatics;
import org.genericsystem.reactor.Tag;
import org.genericsystem.reactor.gs.GSLinks.GSInstanceCellDisplayer;
import org.genericsystem.reactor.gs.GSLinks.GSInstanceCreator;
import org.genericsystem.reactor.gs.GSLinks.LinkTitleDisplayer;
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
				new LinkTitleDisplayer(this) {
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
		new GSInstanceCreator(this, this.getReverseDirection());
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
						new GSInstanceCellDisplayer(this) {
							{
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
