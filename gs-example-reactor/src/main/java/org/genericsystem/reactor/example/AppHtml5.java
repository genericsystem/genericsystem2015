package org.genericsystem.reactor.example;

import org.genericsystem.reactor.modelproperties.SelectionDefaults;

import org.genericsystem.reactor.example.AppHtml.ExampleReactorScript;
import org.genericsystem.reactor.example.AppHtml5.CarHorizontalInstancesTable;
import org.genericsystem.reactor.example.AppHtml5.CarInstancesTable;
import org.genericsystem.reactor.example.AppHtml5.ColorInstancesTable;
import org.genericsystem.reactor.example.AppHtml5.HorizontalSelectedInstanceEditor;
import org.genericsystem.reactor.example.AppHtml5.HorizontalSelectedInstanceStepEditor;
import org.genericsystem.reactor.example.AppHtml5.SelectedInstanceEditor;
import org.genericsystem.reactor.example.AppHtml5.SelectedInstanceStepEditor;

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
import org.genericsystem.reactor.annotations.Select.SelectModel;
import org.genericsystem.reactor.annotations.Styles.Style;
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
@ReactorDependencies({ CarInstancesTable.class, CarHorizontalInstancesTable.class, ColorInstancesTable.class, SelectedInstanceEditor.class, HorizontalSelectedInstanceEditor.class, SelectedInstanceStepEditor.class,
		HorizontalSelectedInstanceStepEditor.class, MonitorExtended.class })
@Style(name = "flex-wrap", value = "wrap")
@Style(name = "flex", value = "1 1 0%")
public class AppHtml5 extends GSApp implements SelectionDefaults {

	public static void main(String[] mainArgs) {
		ApplicationServer.startSimpleGenericApp(mainArgs, AppHtml5.class, "/example-reactor");
	}

	public AppHtml5() {
		createSelectionProperty();
	}

	@DirectSelect(Car.class)
	public static class CarInstancesTable extends TitledInstancesTable {

	}

	@DirectSelect(Car.class)
	public static class CarHorizontalInstancesTable extends TitledHorizontalInstancesTable {

	}

	@DirectSelect(Color.class)
	public static class ColorInstancesTable extends TitledInstancesTable {

	}

	@SelectModel(ObservableModelSelector.SELECTION_SELECTOR.class)
	public static class SelectedInstanceEditor extends TitledInstanceEditor {
	}

	@SelectModel(ObservableModelSelector.SELECTION_SELECTOR.class)
	public static class HorizontalSelectedInstanceEditor extends TitledHorizontalInstanceEditor {

	}

	@SelectModel(ObservableModelSelector.SELECTION_SELECTOR.class)
	public static class SelectedInstanceStepEditor extends TitledInstanceStepEditor {
	}

	@SelectModel(ObservableModelSelector.SELECTION_SELECTOR.class)
	public static class HorizontalSelectedInstanceStepEditor extends TitledHorizontalInstanceStepEditor {

	}
}
