package org.genericsystem.example.reactor;

import org.genericsystem.api.core.ApiStatics;
import org.genericsystem.carcolor.model.Car;
import org.genericsystem.carcolor.model.CarColor;
import org.genericsystem.carcolor.model.Color;
import org.genericsystem.carcolor.model.Diesel;
import org.genericsystem.carcolor.model.Power;
import org.genericsystem.common.AbstractRoot;
import org.genericsystem.common.Generic;
import org.genericsystem.common.Statics;
import org.genericsystem.kernel.Engine;
import org.genericsystem.reactor.appserver.ApplicationServer;
import org.genericsystem.reactor.appserver.ApplicationsDeploymentConfig;
import org.genericsystem.reactor.composite.CompositeSelect.ColorsSelect;
import org.genericsystem.reactor.flex.CompositeFlexSection.ColorCompositeRadio;
import org.genericsystem.reactor.flex.CompositeFlexSection.ColorTitleCompositeFlexElement;
import org.genericsystem.reactor.flex.FlexDirection;
import org.genericsystem.reactor.flex.FlexEditor;
import org.genericsystem.reactor.flex.FlexSection;
import org.genericsystem.reactor.flex.FlexTable;
import org.genericsystem.reactor.html.HtmlApp;
import org.genericsystem.reactor.model.EngineModel;
import org.genericsystem.reactor.model.GenericModel;
import org.genericsystem.reactor.model.GenericModel.StringExtractor;
import org.genericsystem.reactor.model.InputGenericModel;
import org.genericsystem.reactor.model.SelectorModel;

import io.vertx.core.http.ServerWebSocket;

public class AppHtml extends HtmlApp<EngineModel> {

	public static void main(String[] args) {
		int port = 8082;
		ApplicationsDeploymentConfig appsConfig = new ApplicationsDeploymentConfig(Statics.DEFAULT_HOST, port);
		appsConfig.addApplication("/apphtml", AppHtml.class, EngineModel.class, Engine.class, System.getenv("HOME") + "/genericsystem/cars/", Car.class,
				Power.class, Diesel.class, Color.class, CarColor.class);
		new ApplicationServer(appsConfig).start();
	}

	public AppHtml(AbstractRoot engine, ServerWebSocket webSocket) {
		super(webSocket);
		runScript(engine);
		new FlexSection<GenericModel>(this, FlexDirection.COLUMN) {
			{
				addStyle("justify-content", "center");
				new ColorsSelect<SelectorModel>(this).select(StringExtractor.EXTRACTOR, Color.class, SelectorModel::new);
				new ColorTitleCompositeFlexElement<>(this).select(StringExtractor.MANAGEMENT, Color.class);
				new ColorCompositeRadio<SelectorModel>(this, FlexDirection.ROW).select(StringExtractor.EXTRACTOR, Color.class, SelectorModel::new);
				new H1FlexElement(this, "Reactive System Live Demo").addStyle("background-color", "#ffa500");

				new FlexSection<SelectorModel>(this, FlexDirection.COLUMN) {
					{
						select(StringExtractor.SIMPLE_CLASS_EXTRACTOR, gs -> gs[0], SelectorModel::new);
						new FlexTable(this).select(StringExtractor.MANAGEMENT, Car.class, InputGenericModel::new);
						new FlexTable(this, FlexDirection.ROW).select(StringExtractor.MANAGEMENT, Car.class, InputGenericModel::new);
						new FlexEditor(this, FlexDirection.ROW) {
							{
								select(SelectorModel::getSelection);
								addStyle("justify-content", "center");
							}
						};

						new FlexEditor(this, FlexDirection.COLUMN).select(SelectorModel::getSelection);
					}
				};

				new FlexTable(this).select(StringExtractor.MANAGEMENT, Color.class, InputGenericModel::new);
				new FlexTable(this).select(StringExtractor.MANAGEMENT, Engine.class, InputGenericModel::new);
				new SaveCancelFlexRow(this).addStyle("background-color", "#ffa500");
			}
		};
	}

	void runScript(AbstractRoot engine) {
		Generic car = engine.find(Car.class);
		Generic power = engine.find(Power.class);
		Generic diesel = engine.find(Diesel.class);
		Generic person = engine.setInstance("Person");
		Generic category = engine.setInstance("Category");
		Generic carColor = engine.find(CarColor.class);
		Generic color = engine.find(Color.class);
		Generic carPerson = car.setRelation("CarDriverOwner", category, person);
		carPerson.enablePropertyConstraint();
		Generic red = color.setInstance("Red");
		Generic black = color.setInstance("Black");
		Generic green = color.setInstance("Green");
		color.setInstance("Blue");
		color.setInstance("Orange");
		color.setInstance("White");
		color.setInstance("Yellow");
		Generic jdoe = person.setInstance("John Doe");
		Generic hoover = person.setInstance("Edgar Hoover");
		Generic jsnow = person.setInstance("Jon Snow");
		Generic driver = category.setInstance("Driver");
		Generic owner = category.setInstance("Owner");
		Generic audiS4 = car.setInstance("Audi S4");
		audiS4.setHolder(power, 333);
		audiS4.setHolder(diesel, false);
		audiS4.setLink(carColor, "Audi S4 Green", green);
		audiS4.setLink(carPerson, "Audi S4 owner", owner, jsnow);
		audiS4.setLink(carPerson, "Audi S4 driver", driver, hoover);
		Generic bmwM3 = car.setInstance("BMW M3");
		bmwM3.setHolder(power, 450);
		bmwM3.setHolder(diesel, false);
		bmwM3.setLink(carColor, "BMW M3 Red", red);
		bmwM3.setLink(carPerson, "BMW M3 owner", owner, jdoe);
		bmwM3.setLink(carPerson, "BMW M3 owner", driver, jdoe);
		Generic ferrariF40 = car.setInstance("Ferrari F40");
		ferrariF40.setHolder(power, 478);
		ferrariF40.setHolder(diesel, false);
		ferrariF40.setLink(carColor, "Ferrari F40 red", red);
		Generic miniCooper = car.setInstance("Mini Cooper");
		miniCooper.setHolder(power, 175);
		miniCooper.setHolder(diesel, true);
		miniCooper.setLink(carColor, "Mini Cooper", black);
		car.setInstance("Audi A4 3.0 TDI").setHolder(power, 233);
		car.setInstance("Peugeot 106 GTI").setHolder(power, 120);
		car.setInstance("Peugeot 206 S16").setHolder(power, 136);
		power.enableRequiredConstraint(ApiStatics.BASE_POSITION);
		engine.getCurrentCache().flush();
	}

}
