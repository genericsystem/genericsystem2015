package org.genericsystem.carcolor;

import org.genericsystem.reactor.ReactorStatics;
import org.genericsystem.reactor.gs.FlexDirection;
import org.genericsystem.reactor.gs.GSSection;
import org.genericsystem.reactor.gstag.GSButton;
import org.genericsystem.reactor.html.HtmlHyperLink;
import org.genericsystem.reactor.model.GenericModel;

public class GSUserGuide extends GSSection {

	public GSUserGuide(GSSection parent) {
		super(parent, FlexDirection.COLUMN);
		addStyle("flex-direction", "column");
		addStyle("flex-wrap", "nowrap");
		addStyle("justify-content", "center");

		GSSection gSection = new GSSection(this, FlexDirection.COLUMN) {
			{

				addStyleClass("modal");
				createNewProperty(ReactorStatics.DISPLAY);
				initProperty(ReactorStatics.DISPLAY, "none");
				bindStyle(ReactorStatics.DISPLAY, ReactorStatics.DISPLAY);
				new GSSection(this, FlexDirection.COLUMN) {
					{
						addStyle("max-width", "40%");
						addStyle("max-height", "40%");
						addStyleClass("modal-content");
						new HtmlHyperLink<GenericModel>(this) {
							{
								addStyleClass("close");
								setText("×");
								bindAction(model -> {
									this.getParent().getProperty(ReactorStatics.DISPLAY, model).setValue("none");
								});
							}
						};
						new GSSection(this, FlexDirection.COLUMN) {
							{
								setText("toto");
							}
						};
					};
				};
			}
		};

		new GSButton(this) {
			{
				setText("User Guide");
				addStyleClass("buttonUser");

				bindAction(model -> {
					gSection.getProperty(ReactorStatics.DISPLAY, model).setValue("flex");
				});
			}
		};
	};
}
