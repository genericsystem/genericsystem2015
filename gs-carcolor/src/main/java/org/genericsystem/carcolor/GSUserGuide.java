package org.genericsystem.carcolor;

import org.genericsystem.reactor.ReactorStatics;
import org.genericsystem.reactor.gs.FlexDirection;
import org.genericsystem.reactor.gs.GSSection;
import org.genericsystem.reactor.gs.GSTag;
import org.genericsystem.reactor.gstag.GSButton;
import org.genericsystem.reactor.gstag.GSLi;
import org.genericsystem.reactor.gstag.GSUl;
import org.genericsystem.reactor.html.HtmlHyperLink;
import org.genericsystem.reactor.model.GenericModel;

public class GSUserGuide extends GSSection {

	public GSUserGuide(GSTag parent) {
		super(parent, FlexDirection.COLUMN);
		addStyle("flex-direction", "column");
		addStyle("flex-wrap", "nowrap");
		addStyle("justify-content", "center");

		GSSection gSection = new GSSection(this, FlexDirection.COLUMN) {
			{

				addStyleClass("modal");
				createNewInitializedProperty(ReactorStatics.DISPLAY, model -> "none");
				bindStyle(ReactorStatics.DISPLAY, ReactorStatics.DISPLAY);
				new GSSection(this, FlexDirection.COLUMN) {
					{
						addStyle("max-width", "40%");
						addStyleClass("modal-content");
						new HtmlHyperLink<GenericModel>(this) {
							{
								addStyleClass("close");
								setText(this, "×");
								bindAction(model -> {
									this.getParent().getProperty(ReactorStatics.DISPLAY, model).setValue("none");
								});
							}
						};
						new GSSection(this, FlexDirection.COLUMN) {
							{
								setText(this, "How to use CarColor Demo");
								new GSUl(this) {
									{
										setText(this, "Car(s) Managment");
										GSLi li1 = new GSLi(this);
										GSLi li2 = new GSLi(this);
										GSLi li3 = new GSLi(this);
										setText(li1, "Insert Car model");
										setText(li2, "Select color in the ComboBox");
										setText(li3, "Use \"add Button\" to update data");
									}
								};
								new GSUl(this) {
									{
										setText(this, "Color Managment");
										GSLi li1 = new GSLi(this);
										GSLi li2 = new GSLi(this);
										GSLi li3 = new GSLi(this);
										setText(li1, "Add new color");
										setText(li2, "Select car in the ComboBox");
										setText(li3, "Use \"add Button\" to update data");
									}
								};
								new GSUl(this) {
									{
										setText(this, "Global Tips");
										GSLi li1 = new GSLi(this);
										GSLi li2 = new GSLi(this);
										GSLi li3 = new GSLi(this);
										GSLi li4 = new GSLi(this);
										setText(li1, "Use \"Add Button\" to add an entry in the cache");
										setText(li2, "Use the \"Remove Button\" to delete the entry in your cache");
										setText(li3, "Use \"Save Button\" to persist the cache");
										setText(li4, "Use \"Cancel Button\" to release the cache");

									}
								};
								new GSSection(this, FlexDirection.COLUMN) {
									{
										addStyle("text-align", "center");
										setText(this, "To plenty enjoy the power of GS-REACTOR, go to Learning / Get Started");
									}
								};
							}
						};
					};
				};
			}
		};

		new GSButton(this) {
			{
				setText(this, "User Guide");
				addStyleClass("buttonUser");
				addStyle("flex", "1/3");

				bindAction(model -> {
					gSection.getProperty(ReactorStatics.DISPLAY, model).setValue("flex");
				});
			}
		};
	};
}
