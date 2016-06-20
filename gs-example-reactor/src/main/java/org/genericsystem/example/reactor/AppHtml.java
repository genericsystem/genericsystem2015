package org.genericsystem.example.reactor;

import org.genericsystem.carcolor.model.Car;
import org.genericsystem.carcolor.model.CarColor;
import org.genericsystem.carcolor.model.Color;
import org.genericsystem.carcolor.model.Power;
import org.genericsystem.common.AbstractRoot;
import org.genericsystem.common.Generic;
import org.genericsystem.kernel.Engine;
import org.genericsystem.reactor.appserver.ApplicationServer;
import org.genericsystem.reactor.appserver.ApplicationsDeploymentConfig;
import org.genericsystem.reactor.composite.CompositeSelect.ColorsSelect;
import org.genericsystem.reactor.flex.CompositeFlexElement.ColorTitleCompositeFlexElement;
import org.genericsystem.reactor.flex.FlexDirection;
import org.genericsystem.reactor.flex.FlexElement;
import org.genericsystem.reactor.flex.FlexTable;
import org.genericsystem.reactor.flex.FlexTag;
import org.genericsystem.reactor.html.HtmlApp;
import org.genericsystem.reactor.model.CompositeModel;
import org.genericsystem.reactor.model.EngineModel;
import org.genericsystem.reactor.model.CompositeModel.StringExtractor;
import org.genericsystem.reactor.model.InputCompositeModel;
import org.genericsystem.reactor.model.SelectorModel;

import io.vertx.core.http.ServerWebSocket;

public class AppHtml extends HtmlApp<EngineModel> {

	public static void main(String[] args) {
		ApplicationsDeploymentConfig appsConfig = new ApplicationsDeploymentConfig();
		appsConfig.addApplication("/apphtml", AppHtml.class, EngineModel.class, Engine.class, System.getenv("HOME") + "/genericsystem/cars/", Car.class,
				Power.class, Color.class, CarColor.class);
		new ApplicationServer(appsConfig).start();
	}

	public AppHtml(AbstractRoot engine, ServerWebSocket webSocket) {
		super(webSocket);
		runScript(engine);
		new FlexElement<CompositeModel>(this, FlexTag.SECTION, FlexDirection.COLUMN) {
			{
				addStyle("justify-content", "center");
				new ColorsSelect<SelectorModel>(this).select(StringExtractor.EXTRACTOR, Color.class, SelectorModel::new);
				new ColorTitleCompositeFlexElement<>(this).select(StringExtractor.MANAGEMENT, Color.class);
				new H1FlexElement(this, FlexTag.HEADER, "Reactive System Live Demo").addStyle("background-color", "#ffa500");
				new FlexTable(this).select(StringExtractor.MANAGEMENT, Car.class, InputCompositeModel::new);
				new FlexTable(this).select(StringExtractor.MANAGEMENT, Color.class, InputCompositeModel::new);
				new FlexTable(this).select(StringExtractor.MANAGEMENT, Engine.class, InputCompositeModel::new);
				new SaveCancelFlexRow(this, FlexTag.FOOTER).addStyle("background-color", "#ffa500");
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
	}

}
