package org.genericsystem.carcolor;

import io.vertx.core.http.ServerWebSocket;

import org.genericsystem.api.core.ApiStatics;
import org.genericsystem.carcolor.model.Car;
import org.genericsystem.carcolor.model.CarColor;
import org.genericsystem.carcolor.model.Color;
import org.genericsystem.carcolor.model.Power;
import org.genericsystem.common.AbstractRoot;
import org.genericsystem.common.Generic;
import org.genericsystem.common.Statics;
import org.genericsystem.kernel.Engine;
import org.genericsystem.reactor.appserver.ApplicationServer;
import org.genericsystem.reactor.appserver.ApplicationsDeploymentConfig;
import org.genericsystem.reactor.flex.FlexDirection;
import org.genericsystem.reactor.flex.FlexEditor;
import org.genericsystem.reactor.flex.FlexSection;
import org.genericsystem.reactor.flex.FlexTable;
import org.genericsystem.reactor.html.HtmlApp;
import org.genericsystem.reactor.model.EngineModel;
import org.genericsystem.reactor.model.GenericModel;
import org.genericsystem.reactor.model.GenericModel.StringExtractor;
import org.genericsystem.reactor.model.SelectorModel;

public class CarColorApp extends HtmlApp<EngineModel> {
	private static final String MAIN_COLOR = "#3393ff";

	public static void main(String[] args) {
		ApplicationsDeploymentConfig appsConfig = new ApplicationsDeploymentConfig(Statics.DEFAULT_HOST, args.length != 0 ? Integer.parseInt(args[0])
				: Statics.DEFAULT_PORT);
		appsConfig.addApplication("/", CarColorApp.class, EngineModel.class, Engine.class, System.getenv("HOME") + "/genericsystem/cars/", Car.class,
				Power.class, Color.class, CarColor.class);
		new ApplicationServer(appsConfig).start();

	}

	public CarColorApp(AbstractRoot engine, ServerWebSocket webSocket) {
		super(webSocket);
		runScript(engine);
		new FlexSection<GenericModel>(this, FlexDirection.COLUMN) {
			{
				addStyle("justify-content", "center");
				new H1FlexElement(this, "Reactive System Live Demo").addStyle("background-color", MAIN_COLOR);
				new FlexSection<SelectorModel>(this, FlexDirection.COLUMN) {
					{
						select(StringExtractor.SIMPLE_CLASS_EXTRACTOR, gs -> gs[0], SelectorModel::new);
						new FlexTable(this).select(StringExtractor.MANAGEMENT, Car.class, GenericModel::new);
						new FlexEditor(this, FlexDirection.COLUMN).select_(SelectorModel::getSelection);
						new FlexTable(this).select(StringExtractor.MANAGEMENT, Color.class, GenericModel::new);
					}
				};
				new SaveCancelFlexRow(this).addStyle("background-color", MAIN_COLOR);
			}
		};
	}

	void runScript(AbstractRoot engine) {
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
		power.enableRequiredConstraint(ApiStatics.BASE_POSITION);
		engine.getCurrentCache().flush();
	}

}