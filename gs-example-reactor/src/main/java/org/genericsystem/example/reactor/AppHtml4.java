package org.genericsystem.example.reactor;

import org.genericsystem.carcolor.model.Car;
import org.genericsystem.carcolor.model.CarColor;
import org.genericsystem.carcolor.model.CarColor2;
import org.genericsystem.carcolor.model.Color;
import org.genericsystem.carcolor.model.Power;
import org.genericsystem.carcolor.model.UsedCar;
import org.genericsystem.example.reactor.AppHtml.ExampleReactorScript;
import org.genericsystem.example.reactor.AppHtml4.GSAttributesComposite;
import org.genericsystem.example.reactor.AppHtml4.GSAttributesComposite.GSAttributesContentComponent;
import org.genericsystem.example.reactor.AppHtml4.GSInstancesComposite;
import org.genericsystem.example.reactor.AppHtml4.GSInstancesComposite.GSInstancesContentComponent.GSContentComponentLabel;
import org.genericsystem.example.reactor.AppHtml4.GSRowInstancesComposite;
import org.genericsystem.example.reactor.AppHtml4.GSRowLabeledAttributesComposite;
import org.genericsystem.example.reactor.AppHtml4.GSRowLabeledAttributesComposite.GSAttributesContentComponent_;
import org.genericsystem.example.reactor.AppHtml4.GSRowTypeLabeledAttributesComposite;
import org.genericsystem.example.reactor.AppHtml4.GSRowTypeLabeledAttributesComposite.GSHeaderComponent.GSHeaderComponentLabel;
import org.genericsystem.example.reactor.AppHtml4.GSTypeLabeledInstancesComposite;
import org.genericsystem.example.reactor.AppHtml4.GSTypeLabeledInstancesComposite.GSHeaderComponent.GSHeaderComponentTypeAttributes;
import org.genericsystem.reactor.RootTagImpl;
import org.genericsystem.reactor.annotations.DependsOnModel;
import org.genericsystem.reactor.annotations.DirectSelect;
import org.genericsystem.reactor.annotations.ForEach;
import org.genericsystem.reactor.annotations.ReactorDependencies;
import org.genericsystem.reactor.annotations.RunScript;
import org.genericsystem.reactor.annotations.Style.AlignItems;
import org.genericsystem.reactor.annotations.Style.BackgroundColor;
import org.genericsystem.reactor.annotations.Style.FlexDirection;
import org.genericsystem.reactor.annotations.Style.FlexGrow;
import org.genericsystem.reactor.appserver.ApplicationServer;
import org.genericsystem.reactor.gs.GSApp;
import org.genericsystem.reactor.gs.GSDiv;
import org.genericsystem.reactor.gs3.GSComposite.GSContentComponent;
import org.genericsystem.reactor.gstag.HtmlLabel.GSLabelDisplayer;
import org.genericsystem.reactor.model.ObservableListExtractor;
import org.genericsystem.reactor.modelproperties.SelectionDefaults;

@DependsOnModel({ Car.class, Power.class, UsedCar.class, Color.class, CarColor.class, CarColor2.class })
@RunScript(ExampleReactorScript.class)
@ReactorDependencies({ GSInstancesComposite.class, GSRowInstancesComposite.class, GSAttributesComposite.class, GSRowLabeledAttributesComposite.class, GSRowTypeLabeledAttributesComposite.class, GSTypeLabeledInstancesComposite.class })
public class AppHtml4 extends GSApp implements SelectionDefaults {

	public static void main(String[] mainArgs) {
		ApplicationServer.startSimpleGenericApp(mainArgs, AppHtml4.class, "/example-reactor");
	}

	@ReactorDependencies({ GSContentComponent.class })
	public static abstract class GSComposite extends RootTagImpl {
		@FlexGrow("1")
		public static class GSContentComponent extends GSDiv {

		}
	}

	@BackgroundColor("Green")
	@DirectSelect(Car.class)
	@ReactorDependencies({ GSContentComponentLabel.class })
	public static class GSInstancesComposite extends GSComposite {
		@ForEach(ObservableListExtractor.SUBINSTANCES.class)
		public static class GSInstancesContentComponent extends GSContentComponent {
			@FlexGrow("1")
			public static class GSContentComponentLabel extends GSLabelDisplayer {

			}
		}
	}

	@BackgroundColor("Blue")
	@FlexDirection("row")
	public static class GSRowInstancesComposite extends GSInstancesComposite {

	}

	@BackgroundColor("Red")
	@ReactorDependencies({ GSAttributesContentComponent.class, GSContentComponentLabel.class })
	public static class GSAttributesComposite extends GSInstancesComposite {
		@ForEach(ObservableListExtractor.ATTRIBUTES_OF_TYPE.class)
		public static class GSAttributesContentComponent extends GSInstancesContentComponent {

		}
	}

	@BackgroundColor("Purple")
	@FlexDirection("row")
	@ReactorDependencies({ GSAttributesContentComponent_.class, GSContentComponentLabel.class })
	public static class GSRowLabeledAttributesComposite extends GSAttributesComposite {
		@AlignItems("center")
		public static class GSAttributesContentComponent_ extends GSInstancesContentComponent {

		}
	}

	@BackgroundColor("Brown")
	@ReactorDependencies({ GSHeaderComponentLabel.class, GSAttributesContentComponent_.class, GSContentComponentLabel.class })
	public static class GSRowTypeLabeledAttributesComposite extends GSRowLabeledAttributesComposite {
		@AlignItems("center")
		@FlexGrow("1")
		public static class GSHeaderComponent extends GSDiv {
			public static class GSHeaderComponentLabel extends GSLabelDisplayer {

			}
		}
	}

	@BackgroundColor("Orange")
	@ReactorDependencies({ GSHeaderComponentTypeAttributes.class, GSContentComponentLabel.class })
	public static class GSTypeLabeledInstancesComposite extends GSInstancesComposite {
		public static class GSHeaderComponent extends GSDiv {
			@BackgroundColor("Grey")
			public static class GSHeaderComponentTypeAttributes extends GSRowTypeLabeledAttributesComposite {

			}
		}
	}
}