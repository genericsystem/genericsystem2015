package org.genericsystem.reactor.example;

import org.genericsystem.reactor.example.AppHtml.ExampleReactorScript;
import org.genericsystem.carcolor.model.Car;
import org.genericsystem.carcolor.model.CarColor;
import org.genericsystem.carcolor.model.CarColor2;
import org.genericsystem.carcolor.model.Color;
import org.genericsystem.carcolor.model.Power;
import org.genericsystem.carcolor.model.UsedCar;
import org.genericsystem.reactor.annotations.Children;
import org.genericsystem.reactor.annotations.DependsOnModel;
import org.genericsystem.reactor.annotations.DirectSelect;
import org.genericsystem.reactor.annotations.RunScript;
import org.genericsystem.reactor.annotations.SelectModel;
import org.genericsystem.reactor.annotations.Style;
import org.genericsystem.reactor.appserver.ApplicationServer;
import org.genericsystem.reactor.gscomponents.GSApp;
import org.genericsystem.reactor.gscomponents3.DivWithTitle.TitledHorizontalInstanceEditor;
import org.genericsystem.reactor.gscomponents3.DivWithTitle.TitledHorizontalInstanceStepEditor;
import org.genericsystem.reactor.gscomponents3.DivWithTitle.TitledHorizontalInstancesTable;
import org.genericsystem.reactor.gscomponents3.DivWithTitle.TitledInstanceEditor;
import org.genericsystem.reactor.gscomponents3.DivWithTitle.TitledInstanceStepEditor;
import org.genericsystem.reactor.gscomponents3.DivWithTitle.TitledInstancesTable;
import org.genericsystem.reactor.gscomponents3.Monitor.MonitorExtended;
import org.genericsystem.reactor.model.ObservableModelSelector;

@DependsOnModel({ Car.class, Power.class, UsedCar.class, Color.class, CarColor.class, CarColor2.class })
@RunScript(ExampleReactorScript.class)
@Children({ TitledInstancesTable.class, TitledHorizontalInstancesTable.class, TitledInstancesTable.class, TitledInstanceEditor.class, TitledHorizontalInstanceEditor.class, TitledInstanceStepEditor.class, TitledHorizontalInstanceStepEditor.class,
		MonitorExtended.class })
@DirectSelect(path = TitledInstancesTable.class, pos = 0, value = Car.class)
@DirectSelect(path = TitledHorizontalInstancesTable.class, pos = 0, value = Car.class)
@DirectSelect(path = TitledInstancesTable.class, pos = 2, value = Color.class)
@SelectModel(path = TitledInstanceEditor.class, value = ObservableModelSelector.SELECTION_SELECTOR.class)
@Style(name = "flex-wrap", value = "wrap")
@Style(name = "flex", value = "1 1 0%")
public class AppHtml3 extends GSApp {

	public static void main(String[] mainArgs) {
		ApplicationServer.startSimpleGenericApp(mainArgs, AppHtml3.class, "/example-reactor");
	}
}
