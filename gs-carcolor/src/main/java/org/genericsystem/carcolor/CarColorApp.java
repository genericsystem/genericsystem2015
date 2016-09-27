package org.genericsystem.carcolor;

import org.genericsystem.carcolor.CarColorApp.CarColorScript;
import org.genericsystem.carcolor.model.Car;
import org.genericsystem.carcolor.model.CarColor;
import org.genericsystem.carcolor.model.Color;
import org.genericsystem.carcolor.model.Power;
import org.genericsystem.common.Generic;
import org.genericsystem.common.Root;
import org.genericsystem.reactor.annotations.DependsOnModel;
import org.genericsystem.reactor.annotations.RunScript;
import org.genericsystem.reactor.appserver.ApplicationServer;
import org.genericsystem.reactor.appserver.Script;
import org.genericsystem.reactor.az.FlexDirection;
import org.genericsystem.reactor.az.GSApp;
import org.genericsystem.reactor.az.GSEditor;
import org.genericsystem.reactor.az.GSHeader;
import org.genericsystem.reactor.az.GSLogo;
import org.genericsystem.reactor.az.GSModal;
import org.genericsystem.reactor.az.GSMonitor;
import org.genericsystem.reactor.az.GSResponsive;
import org.genericsystem.reactor.az.GSTable;
import org.genericsystem.reactor.modelproperties.SelectionDefaults;

/**
 * @author Nicolas Feybesse
 *
 */
@RunScript(CarColorScript.class)
@DependsOnModel({ Car.class, Power.class, Color.class, CarColor.class })
public class CarColorApp extends GSApp implements SelectionDefaults {

	public static void main(String[] mainArgs) {
		ApplicationServer.startSimpleGenericApp(mainArgs, CarColorApp.class, "/cars");
	}

	public CarColorApp() {
		createSelectionProperty();
		addStyle("background-color", "#00afeb");

		new GSHeader(this, "Reactor Live Demo", GSLogo::new, "", GSUserGuide::new, "");
		new GSModal(this, contentSection -> new GSEditor(contentSection, FlexDirection.COLUMN) {
			{
				addStyle("min-height", "300px");
			}
		});
		new GSResponsive(this, FlexDirection.COLUMN, firstSection -> new GSTable(firstSection) {
			{
				select(Car.class);
			}
		}, secondSection -> new GSTable(secondSection) {
			{
				select(Color.class);
			}
		});

		new GSMonitor(this);
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
