package org.genericsystem.reactor.example;

import org.genericsystem.reactor.example.App.ExampleReactorScript;

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
import org.genericsystem.reactor.annotations.SelectContext;
import org.genericsystem.reactor.annotations.Style;
import org.genericsystem.reactor.appserver.ApplicationServer;
import org.genericsystem.reactor.context.ObservableContextSelector;
import org.genericsystem.reactor.gscomponents.DivWithTitle.TitledHorizontalInstanceEditor;
import org.genericsystem.reactor.gscomponents.DivWithTitle.TitledHorizontalInstanceStepEditor;
import org.genericsystem.reactor.gscomponents.DivWithTitle.TitledHorizontalInstancesTable;
import org.genericsystem.reactor.gscomponents.DivWithTitle.TitledInstanceEditor;
import org.genericsystem.reactor.gscomponents.DivWithTitle.TitledInstanceStepEditor;
import org.genericsystem.reactor.gscomponents.DivWithTitle.TitledInstancesTable;
import org.genericsystem.reactor.gscomponents.Monitor.MonitorExtended;
import org.genericsystem.reactor.gscomponents.RootTagImpl;

@DependsOnModel({ Car.class, Power.class, UsedCar.class, Color.class, CarColor.class, CarColor2.class })
@RunScript(ExampleReactorScript.class)
@Children({ TitledInstancesTable.class, TitledHorizontalInstancesTable.class, TitledInstancesTable.class, TitledInstanceEditor.class, TitledHorizontalInstanceEditor.class, TitledInstanceStepEditor.class, TitledHorizontalInstanceStepEditor.class,
		MonitorExtended.class })
@DirectSelect(path = TitledInstancesTable.class, pos = 0, value = Car.class)
@DirectSelect(path = TitledHorizontalInstancesTable.class, pos = 0, value = Car.class)
@DirectSelect(path = TitledInstancesTable.class, pos = 2, value = Color.class)
@SelectContext(path = TitledInstanceEditor.class, value = ObservableContextSelector.SELECTION_SELECTOR.class)
@Style(name = "flex-wrap", value = "wrap")
@Style(name = "flex", value = "1 1 0%")
public class App2 extends RootTagImpl {

	public static void main(String[] mainArgs) {
		ApplicationServer.startSimpleGenericApp(mainArgs, App2.class, "/example-reactor");
	}

	public App2() {
		addPrefixBinding(context -> getAdminModeProperty(context).setValue(true));
	}
}
