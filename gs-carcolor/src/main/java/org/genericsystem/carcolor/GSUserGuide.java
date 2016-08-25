package org.genericsystem.carcolor;

import org.genericsystem.reactor.ReactorStatics;
import org.genericsystem.reactor.gs.FlexDirection;
import org.genericsystem.reactor.gs.GSModalButton;
import org.genericsystem.reactor.gs.GSSection;
import org.genericsystem.reactor.gs.GSTag;
import org.genericsystem.reactor.gstag.HtmlHyperLink;
import org.genericsystem.reactor.gstag.HtmlLi;
import org.genericsystem.reactor.gstag.HtmlUl;

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
						new HtmlHyperLink(this) {
							{
								addStyleClass("close");
								setText("×");
								bindAction(model -> {
									getProperty(ReactorStatics.DISPLAY, model).setValue("none");
								});
							}
						};
						new GSSection(this, FlexDirection.COLUMN) {
							{
								setText("How to use CarColor Demo");
								new HtmlUl(this) {
									{
										setText("Car(s) Managment");
										HtmlLi li1 = new HtmlLi(this);
										HtmlLi li2 = new HtmlLi(this);
										HtmlLi li3 = new HtmlLi(this);
										li1.setText("Insert Car model");
										li2.setText("Select color in the ComboBox");
										li3.setText("Use \"add Button\" to update data");
									}
								};
								new HtmlUl(this) {
									{
										setText("Color Managment");
										HtmlLi li1 = new HtmlLi(this);
										HtmlLi li2 = new HtmlLi(this);
										HtmlLi li3 = new HtmlLi(this);
										li1.setText("Add new color");
										li2.setText("Select car in the ComboBox");
										li3.setText("Use \"add Button\" to update data");
									}
								};
								new HtmlUl(this) {
									{
										setText("General Tips");
										HtmlLi li1 = new HtmlLi(this);
										HtmlLi li2 = new HtmlLi(this);
										HtmlLi li3 = new HtmlLi(this);
										HtmlLi li4 = new HtmlLi(this);
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
						inheritStyle("background-color");
						setText("User Guide");
						addStyle("flex", "0 1 auto");
					}
				};
			}
		};
	};
}
