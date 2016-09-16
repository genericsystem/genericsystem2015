package org.genericsystem.example.reactor;

import org.genericsystem.carcolor.model.Car;
import org.genericsystem.carcolor.model.CarColor;
import org.genericsystem.carcolor.model.CarColor2;
import org.genericsystem.carcolor.model.Color;
import org.genericsystem.carcolor.model.Power;
import org.genericsystem.carcolor.model.UsedCar;
import org.genericsystem.example.reactor.AppHtml.ExampleReactorScript;
import org.genericsystem.example.reactor.AppHtml4.GSLabeledInstancesComposite;
import org.genericsystem.example.reactor.AppHtml4.GSLabeledInstancesComposite.GSContentComponentLabel;
import org.genericsystem.example.reactor.AppHtml4.GSLabeledTitleInstancesComposite;
import org.genericsystem.example.reactor.AppHtml4.GSLabeledTitleInstancesComposite.GSFirstComponentLabel;
import org.genericsystem.example.reactor.AppHtml4.GSLabeledTitleInstancesComposite.GSRowContentComponent;
import org.genericsystem.reactor.annotations.DependsOnModel;
import org.genericsystem.reactor.annotations.DirectSelect;
import org.genericsystem.reactor.annotations.ForEach;
import org.genericsystem.reactor.annotations.Parent;
import org.genericsystem.reactor.annotations.ReactorDependencies;
import org.genericsystem.reactor.annotations.RunScript;
import org.genericsystem.reactor.annotations.Style;
import org.genericsystem.reactor.annotations.Styles;
import org.genericsystem.reactor.appserver.ApplicationServer;
import org.genericsystem.reactor.gs.GSApp;
import org.genericsystem.reactor.gs3.GSComposite.GSComponent.GSContentComponent;
import org.genericsystem.reactor.gs3.GSComposite.GSComponent.GSHeaderComponent;
import org.genericsystem.reactor.gs3.GSComposite.GSHeaderComposite;
import org.genericsystem.reactor.gstag.HtmlLabel.GSLabelDisplayer;
import org.genericsystem.reactor.model.ObservableListExtractor;
import org.genericsystem.reactor.modelproperties.SelectionDefaults;

@DependsOnModel({ Car.class, Power.class, UsedCar.class, Color.class, CarColor.class, CarColor2.class })
@RunScript(ExampleReactorScript.class)
@ReactorDependencies({ GSLabeledInstancesComposite.class, GSLabeledTitleInstancesComposite.class })
public class AppHtml4 extends GSApp implements SelectionDefaults {

	public static void main(String[] mainArgs) {
		ApplicationServer.startSimpleGenericApp(mainArgs, AppHtml4.class, "/example-reactor");
	}

	@Styles({ @Style(propertyName = "background-color", propertyValue = "red") })
	@DirectSelect(Car.class)
	@ReactorDependencies({ GSContentComponentLabel.class })
	public static class GSLabeledInstancesComposite extends GSHeaderComposite {

		@ForEach(ObservableListExtractor.SUBINSTANCES.class)
		@Parent(GSContentComponent.class)
		public static class GSContentComponentLabel extends GSLabelDisplayer {

		}
	}

	@Styles({ @Style(propertyName = "flex-flow", propertyValue = "row"), @Style(propertyName = "background-color", propertyValue = "green"), })
	@DirectSelect(Car.class)
	@ReactorDependencies({ GSFirstComponentLabel.class, GSRowContentComponent.class, GSContentComponentLabel.class })
	public static class GSLabeledTitleInstancesComposite extends GSHeaderComposite {

		@Style(propertyName = "flex-flow", propertyValue = "row wrap")
		public static class GSRowContentComponent extends GSContentComponent {

		}

		@Parent(GSHeaderComponent.class)
		@Styles({ @Style(propertyName = "background-color", propertyValue = "orange") })
		public static class GSFirstComponentLabel extends GSLabelDisplayer {

		}
	}

}