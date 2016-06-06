package org.genericsystem.example.reactor;

import io.vertx.core.http.ServerWebSocket;

import org.genericsystem.carcolor.model.Car;
import org.genericsystem.carcolor.model.CarColor;
import org.genericsystem.carcolor.model.Color;
import org.genericsystem.carcolor.model.Power;
import org.genericsystem.common.AbstractRoot;
import org.genericsystem.common.Generic;
import org.genericsystem.kernel.Engine;
import org.genericsystem.reactor.appserver.ApplicationServer;
import org.genericsystem.reactor.appserver.ApplicationsDeploymentConfig;
import org.genericsystem.reactor.composite.CompositeModel;
import org.genericsystem.reactor.composite.CompositeModel.ObservableListExtractor;
import org.genericsystem.reactor.composite.CompositeModel.StringExtractor;
import org.genericsystem.reactor.composite.CompositeSectionHtml.TitleCompositeSectionHtml;
import org.genericsystem.reactor.composite.CompositeSelectHtmlTemplate.CompositeSelectHtml;
import org.genericsystem.reactor.composite.table.TypeTableHtmlTemplate.TypeTableHtml;
import org.genericsystem.reactor.html.HtmlApp;
import org.genericsystem.reactor.html.HtmlDiv;

public class AppHtml extends HtmlApp<AppModel> {

	public static void main(String[] args) {
		ApplicationsDeploymentConfig appsConfig = new ApplicationsDeploymentConfig();
		appsConfig.addApplication("/apphtml", AppHtml.class, AppModel.class, Engine.class, System.getenv("HOME") + "/genericsystem/cars/", Car.class, Power.class, Color.class, CarColor.class);
		new ApplicationServer(appsConfig).start();
	}

	public AppHtml(AbstractRoot engine, ServerWebSocket webSocket) {
		super(webSocket);
		runScript(engine);
		new HtmlDiv<AppModel>(this) {
			{
				addStyle("display", "flex");
				addStyle("flex-direction", "column");
				addStyle("flex-wrap", "nowrap");
				addStyle("justify-content", "center");
				new AppHeaderHtml(this);
				new CompositeSelectHtml<CompositeModel>(this) {
					{
						select(Color.class);
						setObservableListExtractor(gs -> gs[0].getObservableSubInstances());
					}
				};

				new TitleCompositeSectionHtml<CompositeModel>(this) {
					{
						select(StringExtractor.MANAGEMENT, Car.class);
					}
				};

				new TypeTableHtml<CompositeModel>(this) {
					{
						select(StringExtractor.MANAGEMENT, Car.class);
						setSubObservableListExtractor(ObservableListExtractor.from(Power.class, CarColor.class));
					}
				};

				// new ColumnTitleTypeTableHtml<CompositeModel>(div).select(StringExtractor.MANAGEMENT, Car.class).setSubObservableListExtractor(ObservableListExtractor.from(Power.class, CarColor.class));
				new AppFooterHtml(this);
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
