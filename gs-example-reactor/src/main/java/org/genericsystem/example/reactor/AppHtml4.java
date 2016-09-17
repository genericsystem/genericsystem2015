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
import org.genericsystem.example.reactor.AppHtml4.GSComposite.GSContentComponent;
import org.genericsystem.example.reactor.AppHtml4.GSInstanceRowLabeledAttributesComposite.GSHeaderComponent_;
import org.genericsystem.example.reactor.AppHtml4.GSInstanceRowLabeledAttributesComposite.GSHeaderComponent_.GSHeaderComponentLabel2;
import org.genericsystem.example.reactor.AppHtml4.GSInstanceRowLabeledAttributesComposite.GSRowInstanceAttributesContentComponent;
import org.genericsystem.example.reactor.AppHtml4.GSInstancesComposite;
import org.genericsystem.example.reactor.AppHtml4.GSInstancesComposite.GSInstancesContentComponent;
import org.genericsystem.example.reactor.AppHtml4.GSInstancesComposite.GSInstancesContentComponent.GSContentComponentLabel;
import org.genericsystem.example.reactor.AppHtml4.GSRowInstancesComposite;
import org.genericsystem.example.reactor.AppHtml4.GSRowLabeledTypeAttributesComposite;
import org.genericsystem.example.reactor.AppHtml4.GSRowLabeledTypeAttributesComposite.GSTypeAttributesContentComponent;
import org.genericsystem.example.reactor.AppHtml4.GSTypeLabeledInstancesComposite;
import org.genericsystem.example.reactor.AppHtml4.GSTypeLabeledInstancesComposite.GSHeaderComponent___;
import org.genericsystem.example.reactor.AppHtml4.GSTypeLabeledInstancesComposite.GSInstancesContentComponent__;
import org.genericsystem.example.reactor.AppHtml4.GSTypeLabeledInstancesComposite.GSInstancesContentComponent__.GSInstanceRowLabeledAttributesComposite_;
import org.genericsystem.example.reactor.AppHtml4.GSTypeRowLabeledTypeAttributesComposite;
import org.genericsystem.example.reactor.AppHtml4.GSTypeRowLabeledTypeAttributesComposite.GSHeaderComponent;
import org.genericsystem.example.reactor.AppHtml4.GSTypeRowLabeledTypeAttributesComposite.GSHeaderComponent.GSHeaderComponentLabel;
import org.genericsystem.reactor.RootTagImpl;
import org.genericsystem.reactor.annotations.DependsOnModel;
import org.genericsystem.reactor.annotations.DirectSelect;
import org.genericsystem.reactor.annotations.ForEach;
import org.genericsystem.reactor.annotations.ReactorDependencies;
import org.genericsystem.reactor.annotations.RunScript;
import org.genericsystem.reactor.annotations.Style;
import org.genericsystem.reactor.annotations.Style.BackgroundColor;
import org.genericsystem.reactor.annotations.Style.Flex;
import org.genericsystem.reactor.annotations.Style.FlexDirection;
import org.genericsystem.reactor.annotations.Style.FlexWrap;
import org.genericsystem.reactor.annotations.Style.Overflow;
import org.genericsystem.reactor.appserver.ApplicationServer;
import org.genericsystem.reactor.gs.GSApp;
import org.genericsystem.reactor.gstag.HtmlLabel.GSLabelDisplayer;
import org.genericsystem.reactor.model.ObservableListExtractor;
import org.genericsystem.reactor.model.ObservableListExtractor.ATTRIBUTES_OF_INSTANCES;
import org.genericsystem.reactor.modelproperties.SelectionDefaults;

@DependsOnModel({ Car.class, Power.class, UsedCar.class, Color.class, CarColor.class, CarColor2.class })
@RunScript(ExampleReactorScript.class)
@ReactorDependencies({ GSInstancesComposite.class, GSRowInstancesComposite.class, GSAttributesComposite.class, GSRowLabeledTypeAttributesComposite.class, GSTypeRowLabeledTypeAttributesComposite.class, GSTypeLabeledInstancesComposite.class })
@FlexWrap("wrap")
@FlexDirection("row")
@Flex("1 1 0%")
public class AppHtml4 extends GSApp implements SelectionDefaults {

	public static void main(String[] mainArgs) {
		ApplicationServer.startSimpleGenericApp(mainArgs, AppHtml4.class, "/example-reactor");
	}

	@Flex("1 1 0%")
	@Overflow("hidden")
	@ReactorDependencies({ GSContentComponent.class })
	public static abstract class GSComposite extends RootTagImpl {
		@Flex("1 1 0%")
		@Overflow("hidden")
		public static class GSContentComponent extends RootTagImpl {

		}
	}

	@BackgroundColor("Green")
	@DirectSelect(Car.class)
	@ReactorDependencies({ GSInstancesContentComponent.class })
	public static class GSInstancesComposite extends GSComposite {
		@ForEach(ObservableListExtractor.SUBINSTANCES.class)
		@ReactorDependencies({ GSContentComponentLabel.class })
		@Flex("1 1 0%")
		@Overflow("hidden")
		public static class GSInstancesContentComponent extends GSContentComponent {
			@Overflow("hidden")
			public static class GSContentComponentLabel extends GSLabelDisplayer {

			}
		}
	}

	@BackgroundColor("Blue")
	@FlexDirection("row")
	@DirectSelect(Car.class)
	public static class GSRowInstancesComposite extends GSInstancesComposite {

	}

	@BackgroundColor("Red")
	@DirectSelect(Car.class)
	@ReactorDependencies({ GSAttributesContentComponent.class })
	public static class GSAttributesComposite extends GSInstancesComposite {
		@ForEach(ObservableListExtractor.ATTRIBUTES_OF_TYPE.class)
		public static class GSAttributesContentComponent extends GSInstancesContentComponent {

		}
	}

	@BackgroundColor("Orange")
	@DirectSelect(Car.class)
	@FlexDirection("row")
	@Flex("1 1 0%")
	@Overflow("hidden")
	@ReactorDependencies({ GSHeaderComponent_.class, GSRowInstanceAttributesContentComponent.class })
	public static class GSInstanceRowLabeledAttributesComposite extends GSRowLabeledTypeAttributesComposite {
		@Flex("1 1 0%")
		@Overflow("hidden")
		@ReactorDependencies({ GSHeaderComponentLabel2.class })
		public static class GSHeaderComponent_ extends RootTagImpl {
			public static class GSHeaderComponentLabel2 extends GSLabelDisplayer {

			}
		}

		@ForEach(ATTRIBUTES_OF_INSTANCES.class)
		public static class GSRowInstanceAttributesContentComponent extends GSTypeAttributesContentComponent {

		}
	}

	@BackgroundColor("Purple")
	@DirectSelect(Car.class)
	@FlexDirection("row")
	@ReactorDependencies({ GSTypeAttributesContentComponent.class })
	public static class GSRowLabeledTypeAttributesComposite extends GSAttributesComposite {
		@FlexDirection("row")
		public static class GSTypeAttributesContentComponent extends GSAttributesContentComponent {

		}
	}

	@BackgroundColor("Grey")
	@Style.Color("White")
	@DirectSelect(Car.class)
	@ReactorDependencies({ GSHeaderComponent.class, GSTypeAttributesContentComponent.class })
	public static class GSTypeRowLabeledTypeAttributesComposite extends GSRowLabeledTypeAttributesComposite {
		@Flex("1 1 0%")
		@Overflow("hidden")
		@FlexDirection("row")
		@ReactorDependencies({ GSHeaderComponentLabel.class })
		public static class GSHeaderComponent extends RootTagImpl {
			public static class GSHeaderComponentLabel extends GSLabelDisplayer {

			}
		}
	}

	@DirectSelect(Car.class)
	@ReactorDependencies({ GSHeaderComponent___.class, GSInstancesContentComponent__.class })
	public static class GSTypeLabeledInstancesComposite extends GSInstancesComposite {
		@FlexDirection("row")
		public static class GSHeaderComponent___ extends GSTypeRowLabeledTypeAttributesComposite {

		}

		@ReactorDependencies({ GSInstanceRowLabeledAttributesComposite_.class })
		@FlexDirection("row")
		public static class GSInstancesContentComponent__ extends GSInstancesContentComponent {
			public static class GSInstanceRowLabeledAttributesComposite_ extends GSInstanceRowLabeledAttributesComposite {

			}
		}
	}
}
