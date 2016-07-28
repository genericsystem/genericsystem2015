package org.genericsystem.carcolor;

import org.genericsystem.carcolor.CarColorApp.CarColorScript;
import org.genericsystem.carcolor.model.Car;
import org.genericsystem.carcolor.model.CarColor;
import org.genericsystem.carcolor.model.Color;
import org.genericsystem.carcolor.model.Power;
import org.genericsystem.common.Generic;
import org.genericsystem.common.Root;
import org.genericsystem.reactor.ReactorStatics;
import org.genericsystem.reactor.annotations.DependsOnModel;
import org.genericsystem.reactor.annotations.RunScript;
import org.genericsystem.reactor.appserver.ApplicationServer;
import org.genericsystem.reactor.appserver.Script;
import org.genericsystem.reactor.gs.FlexDirection;
import org.genericsystem.reactor.gs.GSApp;
import org.genericsystem.reactor.gs.GSEditor;
import org.genericsystem.reactor.gs.GSMonitor;
import org.genericsystem.reactor.gs.GSTable;
import org.genericsystem.reactor.model.StringExtractor;

import io.vertx.core.http.ServerWebSocket;

/**
 * @author Nicolas Feybesse
 *
 */
@RunScript(CarColorScript.class)
@DependsOnModel({ Car.class, Power.class, Color.class, CarColor.class })
public class CarColorApp extends GSApp {

	public static void main(String[] mainArgs) {
		ApplicationServer.sartSimpleGenericApp(mainArgs, CarColorApp.class, "/cars");
	}

	public CarColorApp(Root engine, ServerWebSocket webSocket) {
		super(webSocket);
		createNewProperty(ReactorStatics.SELECTION);
		addStyle("background-color", "#3393ff");

		new GenericH1Section(this, "Generic System Reactor Live Demo") {
			{
				new GSUserGuide(this) {
					{
						addStyle("vertical-align", "center");
						addStyle("float", "right");
					}
				};

			}
		};

		// new GSSection(this, FlexDirection.ROW) {
		// {
		// new GSH1(this, "User Guide");
		// new GSButton(this) {
		// {
		//
		// }
		// };
		// }
		// };

		new GSTable(this).select(StringExtractor.MANAGEMENT, Car.class);
		new GSModal(this, contentSection -> new GSEditor(contentSection, FlexDirection.COLUMN).addStyle("min-height", "300px"));
		new GSTable(this).select(StringExtractor.MANAGEMENT, Color.class);
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
