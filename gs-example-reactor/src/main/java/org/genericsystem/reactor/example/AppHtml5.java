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
import org.genericsystem.reactor.annotations.Select.SelectModel;
import org.genericsystem.reactor.annotations.Styles.FlexDirectionStyle;
import org.genericsystem.reactor.annotations.Styles.Style;
import org.genericsystem.reactor.appserver.ApplicationServer;
import org.genericsystem.reactor.example.AppHtml.ExampleReactorScript;
import org.genericsystem.reactor.example.AppHtml5.CarInstancesTable;
import org.genericsystem.reactor.example.AppHtml5.ColorInstancesTable;
import org.genericsystem.reactor.example.AppHtml5.ColumnSelectedInstanceEditor;
import org.genericsystem.reactor.example.AppHtml5.ColumnSelectedInstanceStepEditor;
import org.genericsystem.reactor.example.AppHtml5.SelectedInstanceEditor;
import org.genericsystem.reactor.example.AppHtml5.SelectedInstanceStepEditor;
import org.genericsystem.reactor.gscomponents.FlexDirection;
import org.genericsystem.reactor.gscomponents.GSApp;
import org.genericsystem.reactor.gscomponents2.DivWithTitle.TitledInstanceEditor;
import org.genericsystem.reactor.gscomponents2.DivWithTitle.TitledInstanceStepEditor;
import org.genericsystem.reactor.gscomponents2.DivWithTitle.TitledInstancesTable;
import org.genericsystem.reactor.gscomponents2.InstanceEditor;
import org.genericsystem.reactor.gscomponents2.InstanceStepEditor;
import org.genericsystem.reactor.model.ObservableModelSelector;
import org.genericsystem.reactor.modelproperties.SelectionDefaults;

@DependsOnModel({ Car.class, Power.class, UsedCar.class, Color.class, CarColor.class, CarColor2.class })
@RunScript(ExampleReactorScript.class)
@ReactorDependencies({ CarInstancesTable.class, ColorInstancesTable.class, SelectedInstanceEditor.class, ColumnSelectedInstanceEditor.class, SelectedInstanceStepEditor.class, ColumnSelectedInstanceStepEditor.class })
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

	@DirectSelect(Color.class)
	public static class ColorInstancesTable extends TitledInstancesTable {

	}

	@SelectModel(ObservableModelSelector.SELECTION_SELECTOR.class)
	public static class SelectedInstanceEditor extends TitledInstanceEditor {
	}

	@FlexDirectionStyle(path = InstanceEditor.class, value = FlexDirection.ROW)
	public static class ColumnSelectedInstanceEditor extends SelectedInstanceEditor {

	}

	@SelectModel(ObservableModelSelector.SELECTION_SELECTOR.class)
	public static class SelectedInstanceStepEditor extends TitledInstanceStepEditor {
	}

	@FlexDirectionStyle(path = InstanceStepEditor.class, value = FlexDirection.ROW)
	public static class ColumnSelectedInstanceStepEditor extends SelectedInstanceStepEditor {

	}
}
