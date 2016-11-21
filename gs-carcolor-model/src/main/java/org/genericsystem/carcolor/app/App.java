package org.genericsystem.carcolor.app;

import org.genericsystem.reactor.gscomponents.AppHeader;
import org.genericsystem.reactor.gscomponents.AppHeader.AppTitleDiv;
import org.genericsystem.reactor.gscomponents.AppHeader.Logo;
import org.genericsystem.reactor.gscomponents.DivWithTitle.TitledInstancesTable;
import org.genericsystem.reactor.gscomponents.FlexDiv;
import org.genericsystem.reactor.gscomponents.HtmlTag.HtmlButton;
import org.genericsystem.reactor.gscomponents.HtmlTag.HtmlH1;
import org.genericsystem.reactor.gscomponents.Modal.ModalEditor;
import org.genericsystem.reactor.gscomponents.Monitor;
import org.genericsystem.reactor.gscomponents.Responsive;
import org.genericsystem.reactor.gscomponents.RootTagImpl;

import org.genericsystem.carcolor.app.App.CarColorScript;
import org.genericsystem.carcolor.app.App.UserGuideButtonDiv;
import org.genericsystem.carcolor.model.Car;
import org.genericsystem.carcolor.model.CarColor;
import org.genericsystem.carcolor.model.Color;
import org.genericsystem.carcolor.model.Power;
import org.genericsystem.common.Generic;
import org.genericsystem.common.Root;
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
import org.genericsystem.reactor.appserver.Script;
import org.genericsystem.reactor.model.ContextAction.MODAL_DISPLAY_FLEX;

@CustomAnnotations(InheritStyle.class)
@RunScript(CarColorScript.class)
@DependsOnModel({ Car.class, Power.class, Color.class, CarColor.class })
@Style(name = "background-color", value = "#00afeb")
@Children({ ModalEditor.class, AppHeader.class, Responsive.class, Monitor.class })
@Children(path = Responsive.class, value = { TitledInstancesTable.class, TitledInstancesTable.class })
@Children(path = AppHeader.class, value = { Logo.class, AppTitleDiv.class, UserGuideButtonDiv.class })
@SetText(path = { AppHeader.class, AppTitleDiv.class, HtmlH1.class }, value = "Reactor Live Demo")
@DirectSelect(path = { Responsive.class, TitledInstancesTable.class }, value = { Car.class, Color.class })
public class App extends RootTagImpl {
	public static void main(String[] mainArgs) {
		ApplicationServer.startSimpleGenericApp(mainArgs, App.class, "/cars");
	}

	public App() {
		addPrefixBinding(context -> getAdminModeProperty(context).setValue(true));
	}

	@Style(name = "justify-content", value = "center")
	@Style(name = "align-items", value = "center")
	@Style(name = "flex", value = "1")
	@Children({ UserGuide.class, GuideButton.class })
	public static class UserGuideButtonDiv extends FlexDiv {
	}

	@SetText("User Guide")
	@Style(name = "flex", value = "0 1 auto")
	@BindAction(MODAL_DISPLAY_FLEX.class)
	@InheritStyle("background-color")
	public static class GuideButton extends HtmlButton {

	}

	public static class CarColorScript implements Script {

		@Override
		public void run(Root engine) {
			Generic car = engine.find(Car.class);
			Generic power = engine.find(Power.class);
			Generic carColor = engine.find(CarColor.class);
			Generic color = engine.find(Color.class);
			Generic red = color.setInstance("Red");
			Generic black = color.setInstance("Black");
			Generic green = color.setInstance("Green");
			color.setInstance("Blue");
			color.setInstance("Orange");
			color.setInstance("White");
			color.setInstance("Yellow");
			Generic audiS4 = car.setInstance("Audi S4");
			audiS4.setHolder(power, 333);
			audiS4.setLink(carColor, "Audi S4 Green", green);
			Generic bmwM3 = car.setInstance("BMW M3");
			bmwM3.setHolder(power, 450);
			bmwM3.setLink(carColor, "BMW M3 Red", red);
			Generic ferrariF40 = car.setInstance("Ferrari F40");
			ferrariF40.setHolder(power, 478);
			ferrariF40.setLink(carColor, "Ferrari F40 red", red);
			Generic miniCooper = car.setInstance("Mini Cooper");
			miniCooper.setHolder(power, 175);
			miniCooper.setLink(carColor, "Mini Cooper", black);
			car.setInstance("Audi A4 3.0 TDI").setHolder(power, 233);
			car.setInstance("Peugeot 106 GTI").setHolder(power, 120);
			car.setInstance("Peugeot 206 S16").setHolder(power, 136);
			// power.enableRequiredConstraint(ApiStatics.BASE_POSITION);
			engine.getCurrentCache().flush();
		}
	}
}
