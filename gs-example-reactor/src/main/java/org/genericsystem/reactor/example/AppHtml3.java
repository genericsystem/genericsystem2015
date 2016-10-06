package org.genericsystem.reactor.example;

import org.genericsystem.carcolor.model.Car;
import org.genericsystem.carcolor.model.CarColor;
import org.genericsystem.carcolor.model.CarColor2;
import org.genericsystem.carcolor.model.Color;
import org.genericsystem.carcolor.model.Power;
import org.genericsystem.carcolor.model.UsedCar;
import org.genericsystem.reactor.aa_modelproperties.SelectionDefaults;
import org.genericsystem.reactor.annotations.DependsOnModel;
import org.genericsystem.reactor.annotations.RunScript;
import org.genericsystem.reactor.appserver.ApplicationServer;
import org.genericsystem.reactor.ca_gscomponents.GSApp;
import org.genericsystem.reactor.ca_gscomponents2.GSEditor;
import org.genericsystem.reactor.ca_gscomponents2.GSTable;
import org.genericsystem.reactor.ca_gscomponents2.GSEditor.HorizontalGSEditor;
import org.genericsystem.reactor.ca_gscomponents2.GSTable.HorizontalTable;
import org.genericsystem.reactor.example.AppHtml.ExampleReactorScript;

@DependsOnModel({ Car.class, Power.class, UsedCar.class, Color.class, CarColor.class, CarColor2.class })
@RunScript(ExampleReactorScript.class)
public class AppHtml3 extends GSApp implements SelectionDefaults {

	public static void main(String[] mainArgs) {
		ApplicationServer.startSimpleGenericApp(mainArgs, AppHtml3.class, "/example-reactor");
	}

	public AppHtml3() {
		addStyle("justify-content", "center");
		createSelectionProperty();

		new GSTable(this).select(Car.class);
		new HorizontalTable(this).select(Car.class);

		new GSEditor(this).select__(this::getSelectionProperty);
		new HorizontalGSEditor(this).select__(this::getSelectionProperty);

		new GSTable(this).select(Color.class);
	}
}
