package org.genericsystem.reactor.example;

import org.genericsystem.api.core.ApiStatics;
import org.genericsystem.carcolor.model.Car;
import org.genericsystem.carcolor.model.CarColor;
import org.genericsystem.carcolor.model.CarColor2;
import org.genericsystem.carcolor.model.Color;
import org.genericsystem.carcolor.model.Power;
import org.genericsystem.carcolor.model.UsedCar;
import org.genericsystem.common.Generic;
import org.genericsystem.common.Root;
import org.genericsystem.kernel.Engine;
import org.genericsystem.reactor.annotations.DependsOnModel;
import org.genericsystem.reactor.annotations.RunScript;
import org.genericsystem.reactor.appserver.ApplicationServer;
import org.genericsystem.reactor.appserver.Script;
import org.genericsystem.reactor.az.FlexDirection;
import org.genericsystem.reactor.az.GSApp;
import org.genericsystem.reactor.az.GSEditor;
import org.genericsystem.reactor.az.GSMonitorExtended;
import org.genericsystem.reactor.az.GSStepEditor;
import org.genericsystem.reactor.az.GSTable;
import org.genericsystem.reactor.az.GSComposite.ColorCompositeRadio;
import org.genericsystem.reactor.az.GSComposite.ColorTitleCompositeFlexElement;
import org.genericsystem.reactor.example.AppHtml.ExampleReactorScript;
import org.genericsystem.reactor.model.ObservableListExtractor;
import org.genericsystem.reactor.modelproperties.SelectionDefaults;

@DependsOnModel({ Car.class, Power.class, UsedCar.class, Color.class, CarColor.class, CarColor2.class })
@RunScript(ExampleReactorScript.class)
public class AppHtml extends GSApp implements SelectionDefaults {

	public static void main(String[] mainArgs) {
		ApplicationServer.startSimpleGenericApp(mainArgs, AppHtml.class, "/example-reactor");
	}

	public AppHtml() {
		addStyle("justify-content", "center");
		new ColorTitleCompositeFlexElement(this).select(Color.class);
		new ColorCompositeRadio(this, FlexDirection.ROW).select(Color.class);
		new GenericH1Section(this, "Generic System Reactor Live Demo").addStyle("background-color", "#ffa500");

		select(gs -> gs[0]);
		createSelectionProperty();
		new GSTable(this).select(Car.class);
		new GSTable(this, FlexDirection.ROW).select(Car.class);
		new GSEditor(this, FlexDirection.ROW) {
			{
				select__(model -> getSelectionProperty(model));
				addStyle("justify-content", "center");
			}
		};
		new GSEditor(this, FlexDirection.COLUMN).select__(model -> getSelectionProperty(model));
		new GSStepEditor(this, FlexDirection.ROW) {
			{
				select__(model -> getSelectionProperty(model));
				switcher_(switchedTag, ObservableListExtractor.ATTRIBUTES_OF_INSTANCES, instanceNameTag);
			}
		};
		new GSStepEditor(this, FlexDirection.COLUMN) {
			{
				select__(model -> getSelectionProperty(model));
				switcher_(switchedTag, ObservableListExtractor.ATTRIBUTES_OF_INSTANCES, instanceNameTag);
			}
		};

		new GSTable(this).select(Color.class);

		new GSTable(this).select(Engine.class);
		new GSMonitorExtended(this).addStyle("background-color", "#ffa500");
	}

	public static class ExampleReactorScript implements Script {

		@Override
		public void run(Root engine) {
			Generic car = engine.find(Car.class);
			Generic power = engine.find(Power.class);
			Generic diesel = engine.find(UsedCar.class);
			car.setAttribute("Description");
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
}
