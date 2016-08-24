package org.genericsystem.reactor.gs;

import org.genericsystem.reactor.gs.GSSubcellDisplayer.GSInstanceSubcellDisplayer;
import org.genericsystem.reactor.gstag.HtmlButton;
import org.genericsystem.reactor.gstag.HtmlHyperLink;
import org.genericsystem.reactor.model.GenericModel;
import org.genericsystem.reactor.model.ObservableListExtractor;
import org.genericsystem.reactor.model.StringExtractor;

public class GSRowDisplayer extends GSComposite {

	
	public GSRowDisplayer(GSTag parent) {
		this(parent, FlexDirection.ROW);
	}
	
	public GSRowDisplayer(GSTag parent, FlexDirection direction){
		super(parent, direction);
	}

	protected void header() {
		new GSSection(this, this.getReverseDirection()) {
			{
				addStyle("flex", "1");
				addStyle("margin-right", "1px");
				addStyle("margin-bottom", "1px");
				addStyle("overflow", "hidden");
				addPrefixBinding(modelContext -> modelContext.getObservableStyles(this).put("background-color",
						"Color".equals(StringExtractor.SIMPLE_CLASS_EXTRACTOR.apply(modelContext.getGeneric().getMeta())) ? getGenericStringProperty(modelContext).getValue() : "#bba5ff"));
				new HtmlHyperLink(this) {
					{
						bindText();
						bindAction(model -> getSelectionProperty(model).setValue(model));
					}
				};

			}
		};
	}

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
				new HtmlButton(this) {
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
}
