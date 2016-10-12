package org.genericsystem.carcolor;

import org.genericsystem.reactor.Tag;
import org.genericsystem.reactor.gscomponents.FlexDirection;
import org.genericsystem.reactor.gscomponents.GSDiv;
import org.genericsystem.reactor.gscomponents.GSModalButton;
import org.genericsystem.reactor.htmltag.HtmlHyperLink;
import org.genericsystem.reactor.htmltag.HtmlLi;
import org.genericsystem.reactor.htmltag.HtmlUl;

public class GSUserGuide extends GSDiv {

	public GSUserGuide(Tag parent) {
		super(parent, FlexDirection.COLUMN);
		addStyle("justify-content", "center");

		GSDiv gSection = new GSDiv(this, FlexDirection.COLUMN) {
			{
				addStyleClass("modal");
				createInitializedDisplayProperty("none");
				bindStyle(DISPLAY, DISPLAY);

				new GSDiv(this, FlexDirection.COLUMN) {
					{
						addStyle("-webkit-border-radius", "30px");
						addStyle("border-radius", "30px");
						addStyle("-moz-border-radius", "30px");
						addStyle("max-width", "40%");

						addStyleClass("modal-content");
						new HtmlHyperLink(this) {
							{
								addStyleClass("close");
								setText("×");
								bindAction(model -> {
									getDisplayProperty(model).setValue("none");
								});
							}
						};
						new GSDiv(this, FlexDirection.COLUMN) {
							{
								addStyle("padding", "35px");
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
										li1.setText("Add new color of any CSS color style (ex : Black, rgb(0,0,0), #000000, ...)");
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
								new GSDiv(this, FlexDirection.COLUMN) {
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
		new GSDiv(this, FlexDirection.ROW) {
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
