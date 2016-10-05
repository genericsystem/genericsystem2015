package org.genericsystem.reactor.ca_gscomponents;

import org.genericsystem.reactor.Context;
import org.genericsystem.reactor.Tag;
import org.genericsystem.reactor.ba_htmltag.HtmlButton;
import org.genericsystem.reactor.ba_htmltag.HtmlHyperLink;
import org.genericsystem.reactor.model.ObservableListExtractor;
import org.genericsystem.reactor.model.StringExtractor;

public class GSRowDisplayer extends GSComposite {

	public GSRowDisplayer(Tag parent) {
		this(parent, FlexDirection.ROW);
	}

	public GSRowDisplayer(Tag parent, FlexDirection direction) {
		super(parent, direction);
	}

	@Override
	protected void header() {
		new GSDiv(this, this.getReverseDirection()) {
			{
				addStyle("flex", "1");
				addStyle("margin-right", "1px");
				addStyle("margin-bottom", "1px");
				addStyle("overflow", "hidden");
				addPrefixBinding(modelContext -> getDomNodeStyles(modelContext).put("background-color",
						"Color".equals(StringExtractor.SIMPLE_CLASS_EXTRACTOR.apply(modelContext.getGeneric().getMeta())) ? getGenericStringProperty(modelContext).getValue() : "#3393FF"));
				new HtmlHyperLink(this) {
					{
						addStyle("color", "White");
						bindText();
						bindAction(model -> getSelectionProperty(model).setValue(model));
					}
				};

			}
		};
	}

	@Override
	protected void sections() {
		new GSDiv(this, FlexDirection.COLUMN) {
			{
				addStyle("flex", "1");
				addStyle("overflow", "hidden");
				forEach(ObservableListExtractor.ATTRIBUTES_OF_INSTANCES);
				new GSSubcellDisplayer(this) {
					{
						forEach(ObservableListExtractor.HOLDERS);
					}

					@Override
					public void subCellStyle(Tag tag) {
						super.subCellStyle(tag);
						tag.addPrefixBinding(modelContext -> tag.getDomNodeStyles(modelContext).put("background-color",
								"Color".equals(StringExtractor.SIMPLE_CLASS_EXTRACTOR.apply(modelContext.getGeneric().getMeta())) ? tag.getGenericStringProperty(modelContext).getValue() : "#e5ed00"));
					}
				};
			}
		};
	}

	@Override
	protected void footer() {
		new GSDiv(this, this.getDirection()) {
			{
				if (this.getDirection().equals(FlexDirection.ROW)) {
					addStyle("flex", "0");
					addStyle("min-width", "100px");
				} else {
					addStyle("flex", "1");
				}
				addStyle("margin-right", "1px");
				addStyle("margin-bottom", "1px");
				new HtmlButton(this) {
					{
						setText("Remove");
						bindAction(Context::remove);
						addStyle("width", "100%");
						addStyle("height", "100%");
					}
				};
			}
		};

	}
}
