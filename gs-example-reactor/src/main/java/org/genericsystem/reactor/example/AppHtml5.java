package org.genericsystem.reactor.example;

import org.genericsystem.carcolor.model.Car;
import org.genericsystem.carcolor.model.CarColor;
import org.genericsystem.carcolor.model.CarColor2;
import org.genericsystem.carcolor.model.Color;
import org.genericsystem.carcolor.model.Power;
import org.genericsystem.carcolor.model.UsedCar;
import org.genericsystem.reactor.annotations.DependsOnModel;
import org.genericsystem.reactor.annotations.DirectSelect;
import org.genericsystem.reactor.annotations.ReactorDependencies;
import org.genericsystem.reactor.annotations.RunScript;
import org.genericsystem.reactor.annotations.Styles.Flex;
import org.genericsystem.reactor.annotations.Styles.FlexWrap;
import org.genericsystem.reactor.appserver.ApplicationServer;
import org.genericsystem.reactor.az.GSApp;
import org.genericsystem.reactor.az3.TitledDiv.TitledInstancesTable;
import org.genericsystem.reactor.example.AppHtml.ExampleReactorScript;
import org.genericsystem.reactor.example.AppHtml5.CarInstancesTable;
import org.genericsystem.reactor.example.AppHtml5.ColorInstancesTable;
import org.genericsystem.reactor.modelproperties.SelectionDefaults;

@DependsOnModel({ Car.class, Power.class, UsedCar.class, Color.class, CarColor.class, CarColor2.class })
@RunScript(ExampleReactorScript.class)
@ReactorDependencies({ CarInstancesTable.class, ColorInstancesTable.class })
@FlexWrap("wrap")
@Flex("1 1 0%")
public class AppHtml5 extends GSApp implements SelectionDefaults {

	public static void main(String[] mainArgs) {
		ApplicationServer.startSimpleGenericApp(mainArgs, AppHtml5.class, "/example-reactor");
	}

	@DirectSelect(Car.class)
	public static class CarInstancesTable extends TitledInstancesTable {

	}

	@DirectSelect(Color.class)
	public static class ColorInstancesTable extends TitledInstancesTable {

	}
}
