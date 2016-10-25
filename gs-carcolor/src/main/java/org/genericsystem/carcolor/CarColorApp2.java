package org.genericsystem.carcolor;

import org.genericsystem.carcolor.CarColorApp.CarColorScript;
import org.genericsystem.carcolor.CarColorApp2.UserGuideButtonDiv;
import org.genericsystem.carcolor.model.Car;
import org.genericsystem.carcolor.model.CarColor;
import org.genericsystem.carcolor.model.Color;
import org.genericsystem.carcolor.model.Power;
import org.genericsystem.reactor.annotations.BindAction;
import org.genericsystem.reactor.annotations.Children;
import org.genericsystem.reactor.annotations.CustomAnnotations;
import org.genericsystem.reactor.annotations.DependsOnModel;
import org.genericsystem.reactor.annotations.DirectSelect;
import org.genericsystem.reactor.annotations.InheritStyle;
import org.genericsystem.reactor.annotations.RunScript;
import org.genericsystem.reactor.annotations.SetText;
import org.genericsystem.reactor.annotations.Style;
import org.genericsystem.reactor.appserver.ApplicationServer;
import org.genericsystem.reactor.gscomponents.GSApp;
import org.genericsystem.reactor.gscomponents.GSDiv;
import org.genericsystem.reactor.gscomponents3.AppHeader;
import org.genericsystem.reactor.gscomponents3.AppHeader.Logo;
import org.genericsystem.reactor.gscomponents3.AppHeader.TitleDiv;
import org.genericsystem.reactor.gscomponents3.DivWithTitle.TitledInstancesTable;
import org.genericsystem.reactor.gscomponents3.Modal.ModalEditor;
import org.genericsystem.reactor.gscomponents3.Monitor;
import org.genericsystem.reactor.gscomponents3.Responsive;
import org.genericsystem.reactor.htmltag.HtmlButton;
import org.genericsystem.reactor.htmltag.HtmlH1;
import org.genericsystem.reactor.model.ContextAction.MODAL_DISPLAY_FLEX;

@CustomAnnotations(InheritStyle.class)
@RunScript(CarColorScript.class)
@DependsOnModel({ Car.class, Power.class, Color.class, CarColor.class })
@Style(name = "background-color", value = "#00afeb")
@Children({ ModalEditor.class, AppHeader.class, Responsive.class, Monitor.class })
@Children(path = Responsive.class, value = { TitledInstancesTable.class, TitledInstancesTable.class })
@Children(path = AppHeader.class, value = { Logo.class, TitleDiv.class, UserGuideButtonDiv.class })
@SetText(path = { AppHeader.class, TitleDiv.class, HtmlH1.class }, value = "Reactor Live Demo")
@DirectSelect(path = { Responsive.class, TitledInstancesTable.class }, value = { Car.class, Color.class })
public class CarColorApp2 extends GSApp {
	public static void main(String[] mainArgs) {
		ApplicationServer.startSimpleGenericApp(mainArgs, CarColorApp2.class, "/cars");
	}

	@Style(name = "justify-content", value = "center")
	@Style(name = "align-items", value = "center")
	@Style(name = "flex", value = "1")
	@Children({ UserGuide2.class, GuideButton.class })
	public static class UserGuideButtonDiv extends GSDiv {
	}

	@SetText("User Guide")
	@Style(name = "flex", value = "0 1 auto")
	@BindAction(MODAL_DISPLAY_FLEX.class)
	public static class GuideButton extends HtmlButton {
		@Override
		public void init() {
			inheritStyle("background-color");
		}
	}
}
