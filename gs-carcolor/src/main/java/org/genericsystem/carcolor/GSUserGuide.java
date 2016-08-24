package org.genericsystem.carcolor;

import org.genericsystem.reactor.ReactorStatics;
import org.genericsystem.reactor.gs.FlexDirection;
import org.genericsystem.reactor.gs.GSSection;
import org.genericsystem.reactor.gs.GSTag;
import org.genericsystem.reactor.gstag.GSLi;
import org.genericsystem.reactor.gstag.GSModalButton;
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
								setText("×");
								bindAction(model -> {
									this.getParent().getProperty(ReactorStatics.DISPLAY, model).setValue("none");
								});
							}
						};
						new GSSection(this, FlexDirection.COLUMN) {
							{
								setText("How to use CarColor Demo");
								new GSUl(this) {
									{
										setText("Car(s) Managment");
										GSLi li1 = new GSLi(this);
										GSLi li2 = new GSLi(this);
										GSLi li3 = new GSLi(this);
										li1.setText("Insert Car model");
										li2.setText("Select color in the ComboBox");
										li3.setText("Use \"add Button\" to update data");
									}
								};
								new GSUl(this) {
									{
										setText("Color Managment");
										GSLi li1 = new GSLi(this);
										GSLi li2 = new GSLi(this);
										GSLi li3 = new GSLi(this);
										li1.setText("Add new color");
										li2.setText("Select car in the ComboBox");
										li3.setText("Use \"add Button\" to update data");
									}
								};
								new GSUl(this) {
									{
										setText("General Tips");
										GSLi li1 = new GSLi(this);
										GSLi li2 = new GSLi(this);
										GSLi li3 = new GSLi(this);
										GSLi li4 = new GSLi(this);
										li1.setText("Click \"Add Button\" to add an entry in the cache");
										li2.setText("Click the \"Remove Button\" to delete the entry in your cache");
										li3.setText("Click \"Save Button\" to persist the cache");
										li4.setText("Click \"Cancel Button\" to release the cache");

									}
								};
								new GSSection(this, FlexDirection.COLUMN) {
									{
										addStyle("text-align", "center");
										setText("To plenty enjoy the power of GS-REACTOR, go to Learning / Get Started");
									}
								};
							}
						};
					};
				};
			}
		};
		new GSSection(this, FlexDirection.ROW) {
			{
				addStyle("justify-content", "center");
				new GSModalButton(this, gSection) {
					{
						setText("User Guide");
						addStyle("flex", "0 1 auto");
					}
				};
			}
		};
	};
}
