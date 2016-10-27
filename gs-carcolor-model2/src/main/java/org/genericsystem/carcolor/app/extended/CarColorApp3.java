package org.genericsystem.carcolor.app.extended;

import org.genericsystem.carcolor.app.CCInheritStyle;
import org.genericsystem.carcolor.app.UserGuide2;
import org.genericsystem.carcolor.app.extended.CarColorApp3.CarColorScript2;
import org.genericsystem.carcolor.app.extended.CarColorApp3.GroupDiv;
import org.genericsystem.carcolor.app.extended.CarColorApp3.GroupDiv2;
import org.genericsystem.carcolor.app.extended.CarColorApp3.UserGuideButtonDiv;
import org.genericsystem.carcolor.model.extended.Airbag;
import org.genericsystem.carcolor.model.extended.Bike;
import org.genericsystem.carcolor.model.extended.Car;
import org.genericsystem.carcolor.model.extended.Color;
import org.genericsystem.carcolor.model.extended.Energy;
import org.genericsystem.carcolor.model.extended.Mileage;
import org.genericsystem.carcolor.model.extended.MileageUnit;
import org.genericsystem.carcolor.model.extended.Power;
import org.genericsystem.carcolor.model.extended.Price;
import org.genericsystem.carcolor.model.extended.SideCar;
import org.genericsystem.carcolor.model.extended.Transmission;
import org.genericsystem.carcolor.model.extended.UsedCar;
import org.genericsystem.carcolor.model.extended.Vehicle;
import org.genericsystem.carcolor.model.extended.VehicleColor;
import org.genericsystem.carcolor.model.extended.VehicleEnergy;
import org.genericsystem.carcolor.model.extended.VehicleEngine;
import org.genericsystem.carcolor.model.extended.VehicleVehicleEngine;
import org.genericsystem.common.Generic;
import org.genericsystem.common.Root;
import org.genericsystem.reactor.annotations.Children;
import org.genericsystem.reactor.annotations.CustomAnnotations;
import org.genericsystem.reactor.annotations.DependsOnModel;
import org.genericsystem.reactor.annotations.DirectSelect;
import org.genericsystem.reactor.annotations.RunScript;
import org.genericsystem.reactor.annotations.SetText;
import org.genericsystem.reactor.annotations.Style;
import org.genericsystem.reactor.annotations.Style.FlexDirectionStyle;
import org.genericsystem.reactor.appserver.ApplicationServer;
import org.genericsystem.reactor.appserver.Script;
import org.genericsystem.reactor.gscomponents.FlexDirection;
import org.genericsystem.reactor.gscomponents.GSApp;
import org.genericsystem.reactor.gscomponents.GSDiv;
import org.genericsystem.reactor.gscomponents3.AppHeader;
import org.genericsystem.reactor.gscomponents3.AppHeader.Logo;
import org.genericsystem.reactor.gscomponents3.AppHeader.TitleDiv;
import org.genericsystem.reactor.gscomponents3.DivWithTitle.TitledInstancesTable;
import org.genericsystem.reactor.gscomponents3.GSComposite;
import org.genericsystem.reactor.gscomponents3.InstancesTable;
import org.genericsystem.reactor.gscomponents3.Modal.ModalEditor;
import org.genericsystem.reactor.gscomponents3.Monitor;
import org.genericsystem.reactor.gscomponents3.Responsive;
import org.genericsystem.reactor.htmltag.HtmlButton;
import org.genericsystem.reactor.htmltag.HtmlH1;

@CustomAnnotations(CCInheritStyle.class)
@RunScript(CarColorScript2.class)
@DependsOnModel({ Car.class, Power.class, Color.class, Airbag.class, Bike.class, Energy.class, Mileage.class, MileageUnit.class, Price.class, SideCar.class, Transmission.class, UsedCar.class, VehicleColor.class, Vehicle.class, VehicleEngine.class,
		VehicleEnergy.class, VehicleVehicleEngine.class })
@Style(name = "background-color", value = "#00afeb")
@Style(path = { Responsive.class, TitledInstancesTable.class, InstancesTable.class, GSComposite.class }, name = "flex", value = "0 1 auto")
@Children({ ModalEditor.class, AppHeader.class, Responsive.class, Monitor.class })
@Children(path = Responsive.class, value = { TitledInstancesTable.class, GroupDiv.class, GroupDiv2.class, TitledInstancesTable.class })
@Children(path = AppHeader.class, value = { Logo.class, TitleDiv.class, UserGuideButtonDiv.class })
@SetText(path = { AppHeader.class, TitleDiv.class, HtmlH1.class }, value = "Reactor Live Demo")
@DirectSelect(path = { Responsive.class, TitledInstancesTable.class }, value = { Vehicle.class, VehicleEngine.class })
public class CarColorApp3 extends GSApp {
	public static void main(String[] mainArgs) {
		ApplicationServer.startSimpleGenericApp(mainArgs, CarColorApp3.class, "cars2");
	}

	@Children({ TitledInstancesTable.class, TitledInstancesTable.class })
	@DirectSelect(path = TitledInstancesTable.class, value = { Bike.class, Car.class })
	@Style(name = "flex", value = "1")
	@Style(path = { TitledInstancesTable.class, InstancesTable.class }, name = "flex", value = "0 1 auto")
	@Style(path = { TitledInstancesTable.class, InstancesTable.class, GSComposite.class }, name = "flex", value = "0 1 auto")
	public static class GroupDiv extends GSDiv {

	}

	@FlexDirectionStyle(FlexDirection.ROW)
	@Children({ TitledInstancesTable.class, TitledInstancesTable.class })
	@DirectSelect(path = TitledInstancesTable.class, value = { Color.class, Energy.class })
	@Style(name = "flex", value = "1")
	@Style(path = { TitledInstancesTable.class, InstancesTable.class }, name = "flex", value = "0 1 auto")
	@Style(path = { TitledInstancesTable.class, GSComposite.class }, name = "flex", value = "0 1 auto")
	public static class GroupDiv2 extends GSDiv {

	}

	@Style(name = "justify-content", value = "center")
	@Style(name = "align-items", value = "center")
	@Style(name = "flex", value = "1")
	@Children({ UserGuide2.class, GuideButton.class })
	public static class UserGuideButtonDiv extends GSDiv {
	}

	@SetText("User Guide")
	@Style(name = "flex", value = "0 1 auto")
	@CCInheritStyle("background-color")
	public static class GuideButton extends HtmlButton {
		@Override
		public void init() {
			bindAction(model -> getParent().find(UserGuide2.class).getDisplayProperty(model).setValue("flex"));
		}
	}

	public static class CarColorScript2 implements Script {

		@Override
		public void run(Root engine) {
			Generic car = engine.find(Car.class);
			Generic power = engine.find(Power.class);
			Generic vehicleColor = engine.find(VehicleColor.class);
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
			System.out.println(engine.find(Vehicle.class).isAncestorOf(audiS4));
			// audiS4.setLink(vehicleColor, "Audi S4 Green", green);
			Generic bmwM3 = car.setInstance("BMW M3");
			bmwM3.setHolder(power, 450);
			// bmwM3.setLink(vehicleColor, "BMW M3 Red", red);
			Generic ferrariF40 = car.setInstance("Ferrari F40");
			ferrariF40.setHolder(power, 478);
			// ferrariF40.setLink(vehicleColor, "Ferrari F40 red", red);
			Generic miniCooper = car.setInstance("Mini Cooper");
			miniCooper.setHolder(power, 175);
			// miniCooper.setLink(vehicleColor, "Mini Cooper", black);
			car.setInstance("Audi A4 3.0 TDI").setHolder(power, 233);
			car.setInstance("Peugeot 106 GTI").setHolder(power, 120);
			car.setInstance("Peugeot 206 S16").setHolder(power, 136);
			// power.enableRequiredConstraint(ApiStatics.BASE_POSITION);
			engine.getCurrentCache().flush();
		}
	}
}
