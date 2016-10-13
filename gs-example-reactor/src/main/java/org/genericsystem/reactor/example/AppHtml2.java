package org.genericsystem.reactor.example;

import org.genericsystem.reactor.modelproperties.SelectionDefaults;

import org.genericsystem.reactor.example.AppHtml.ExampleReactorScript;

import org.genericsystem.carcolor.model.Car;
import org.genericsystem.carcolor.model.CarColor;
import org.genericsystem.carcolor.model.CarColor2;
import org.genericsystem.carcolor.model.Color;
import org.genericsystem.carcolor.model.Power;
import org.genericsystem.carcolor.model.UsedCar;
import org.genericsystem.reactor.annotations.DependsOnModel;
import org.genericsystem.reactor.annotations.DirectSelect;
import org.genericsystem.reactor.annotations.Children;
import org.genericsystem.reactor.annotations.RunScript;
import org.genericsystem.reactor.annotations.Select.SelectModel;
import org.genericsystem.reactor.annotations.Style;
import org.genericsystem.reactor.appserver.ApplicationServer;
import org.genericsystem.reactor.gscomponents.GSApp;
import org.genericsystem.reactor.gscomponents2.GSEditor;
import org.genericsystem.reactor.gscomponents2.GSEditor.HorizontalGSEditor;
import org.genericsystem.reactor.gscomponents2.GSTable;
import org.genericsystem.reactor.gscomponents2.GSTable.HorizontalTable;
import org.genericsystem.reactor.model.ObservableModelSelector;

@DependsOnModel({ Car.class, Power.class, UsedCar.class, Color.class, CarColor.class, CarColor2.class })
@RunScript(ExampleReactorScript.class)
@Children({ GSTable.class, HorizontalTable.class, GSEditor.class, HorizontalGSEditor.class, GSTable.class })
@DirectSelect(path = GSTable.class, pos = 0, value = Car.class)
@DirectSelect(path = HorizontalTable.class, value = Car.class)
@SelectModel(path = GSEditor.class, value = ObservableModelSelector.SELECTION_SELECTOR.class)
@DirectSelect(path = GSTable.class, pos = 2, value = Color.class)
@Style(name = "justify-content", value = "center")
public class AppHtml2 extends GSApp implements SelectionDefaults {

	public static void main(String[] mainArgs) {
		ApplicationServer.startSimpleGenericApp(mainArgs, AppHtml2.class, "/example-reactor");
	}

	public AppHtml2() {
		createSelectionProperty();
	}
}
