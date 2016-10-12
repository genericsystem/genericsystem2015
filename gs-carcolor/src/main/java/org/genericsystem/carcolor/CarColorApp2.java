package org.genericsystem.carcolor;

import org.genericsystem.reactor.modelproperties.SelectionDefaults;

import org.genericsystem.reactor.htmltag.HtmlButton;
import org.genericsystem.reactor.htmltag.HtmlH1;
import org.genericsystem.reactor.htmltag.HtmlImg;

import org.genericsystem.reactor.gscomponents.FlexDirection;
import org.genericsystem.reactor.gscomponents.GSApp;
import org.genericsystem.reactor.gscomponents.GSDiv;

import org.genericsystem.reactor.gscomponents3.DivWithTitle.TitledInstancesTable;
import org.genericsystem.reactor.gscomponents3.Modal.ModalEditor;
import org.genericsystem.reactor.gscomponents3.Monitor;
import org.genericsystem.reactor.gscomponents3.Responsive;

import org.genericsystem.carcolor.CarColorApp.CarColorScript;
import org.genericsystem.carcolor.CarColorApp2.PageContent;
import org.genericsystem.carcolor.CarColorApp2.PageHeader;
import org.genericsystem.carcolor.model.Car;
import org.genericsystem.carcolor.model.CarColor;
import org.genericsystem.carcolor.model.Color;
import org.genericsystem.carcolor.model.Power;
import org.genericsystem.reactor.annotations.Attribute;
import org.genericsystem.reactor.annotations.DependsOnModel;
import org.genericsystem.reactor.annotations.DirectSelect;
import org.genericsystem.reactor.annotations.ReactorDependencies;
import org.genericsystem.reactor.annotations.RunScript;
import org.genericsystem.reactor.annotations.SetText;
import org.genericsystem.reactor.annotations.Style;
import org.genericsystem.reactor.annotations.Style.FlexDirectionStyle;
import org.genericsystem.reactor.appserver.ApplicationServer;

@RunScript(CarColorScript.class)
@DependsOnModel({ Car.class, Power.class, Color.class, CarColor.class })
@Style(name = "background-color", value = "#00afeb")
@ReactorDependencies({ ModalEditor.class, PageHeader.class, PageContent.class, Monitor.class })
public class CarColorApp2 extends GSApp implements SelectionDefaults {
	public static void main(String[] mainArgs) {
		ApplicationServer.startSimpleGenericApp(mainArgs, CarColorApp2.class, "/cars");
	}

	@Override
	public void init() {
		createSelectionProperty();
	}

	@Style(name = "justify-content", value = "space-around")
	@Style(name = "padding", value = "10px")
	@FlexDirectionStyle(FlexDirection.ROW)
	@ReactorDependencies({ Logo.class, TitleDiv.class, UserGuideButtonDiv.class })
	public static class PageHeader extends GSDiv {
	}

	@ReactorDependencies({ CarInstancesTable.class, ColorInstancesTable.class })
	public static class PageContent extends Responsive {
	}

	@DirectSelect(Car.class)
	public static class CarInstancesTable extends TitledInstancesTable {

	}

	@DirectSelect(Color.class)
	public static class ColorInstancesTable extends TitledInstancesTable {

	}

	@FlexDirectionStyle(FlexDirection.ROW)
	@Style(name = "flex", value = "0 1 auto")
	@Style(name = "align-items", value = "center")
	@ReactorDependencies(HtmlImg.class)
	@Attribute(path = HtmlImg.class, name = "src", value = "logoTransp.png")
	@Attribute(path = HtmlImg.class, name = "alt", value = "logo")
	@Style(path = HtmlImg.class, name = "height", value = "auto")
	@Style(path = HtmlImg.class, name = "width", value = "150px")
	public static class Logo extends GSDiv {
	}

	@FlexDirectionStyle(FlexDirection.ROW)
	@Style(name = "justify-content", value = "center")
	@Style(name = "flex", value = "3")
	@Style(name = "align-items", value = "center")
	@Style(name = "color", value = "White")
	@Style(name = "text-shadow", value = "1px 1px 2px black, 0 0 25px blue, 0 0 5px darkblue")
	@ReactorDependencies(HtmlH1.class)
	@SetText(path = HtmlH1.class, value = "Reactor Live Demo")
	public static class TitleDiv extends GSDiv {
	}

	@Style(name = "justify-content", value = "center")
	@Style(name = "align-items", value = "center")
	@Style(name = "flex", value = "1")
	@ReactorDependencies({ UserGuide2.class, GuideButton.class })
	public static class UserGuideButtonDiv extends GSDiv {
	}

	@SetText("User Guide")
	@Style(name = "flex", value = "0 1 auto")
	public static class GuideButton extends HtmlButton {
		@Override
		public void init() {
			inheritStyle("background-color");
			bindAction(model -> getParent().find(UserGuide2.class).getDisplayProperty(model).setValue("flex"));
		}
	}
}
