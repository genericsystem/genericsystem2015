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
import org.genericsystem.example.reactor.AppHtml4.GSComposite.GSContentComponent.GSContentComponentLabel;
import org.genericsystem.example.reactor.AppHtml4.GSInstanceRowLabeledAttributesComposite.GSHeaderComponent_;
import org.genericsystem.example.reactor.AppHtml4.GSInstanceRowLabeledAttributesComposite.GSHeaderComponent_.GSHeaderComponentLabel2;
import org.genericsystem.example.reactor.AppHtml4.GSInstanceRowLabeledAttributesComposite.GSRowInstanceAttributesContentComponent;
import org.genericsystem.example.reactor.AppHtml4.GSInstancesComposite;
import org.genericsystem.example.reactor.AppHtml4.GSInstancesComposite.GSInstancesContentComponent;
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
import org.genericsystem.reactor.annotations.DependsOnModel;
import org.genericsystem.reactor.annotations.DirectSelect;
import org.genericsystem.reactor.annotations.ForEach;
import org.genericsystem.reactor.annotations.ReactorDependencies;
import org.genericsystem.reactor.annotations.RunScript;
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

	@org.genericsystem.reactor.annotations.Style.Flex("1 1 0%")
	@org.genericsystem.reactor.annotations.Style.Overflow("hidden")
	@org.genericsystem.reactor.annotations.ReactorDependencies({ GSContentComponent.class })
	public static abstract class GSComposite extends org.genericsystem.reactor.gs3.CompositeTagImpl {
		@org.genericsystem.reactor.annotations.Style.Flex("1 1 0%")
		@org.genericsystem.reactor.annotations.Style.Overflow("hidden")
		@org.genericsystem.reactor.annotations.ReactorDependencies({ GSContentComponentLabel.class })
		public static class GSContentComponent extends org.genericsystem.reactor.gs3.CompositeTagImpl {
			@Overflow("hidden")
			public static class GSContentComponentLabel extends GSLabelDisplayer {

			}
		}
	}

	@org.genericsystem.reactor.annotations.Style.BackgroundColor("Green")
	@org.genericsystem.reactor.annotations.DirectSelect(Car.class)
	@org.genericsystem.reactor.annotations.ReactorDependencies({ GSInstancesContentComponent.class })
	public static class GSInstancesComposite extends GSComposite {
		@org.genericsystem.reactor.annotations.ForEach(ObservableListExtractor.SUBINSTANCES.class)
		public static class GSInstancesContentComponent extends GSContentComponent {

		}
	}

	@BackgroundColor("Blue")
	@FlexDirection("row")
	@DirectSelect(Car.class)
	public static class GSRowInstancesComposite extends GSInstancesComposite {

	}

	@org.genericsystem.reactor.annotations.Style.BackgroundColor("Red")
	@org.genericsystem.reactor.annotations.DirectSelect(Car.class)
	@org.genericsystem.reactor.annotations.ReactorDependencies({ GSAttributesContentComponent.class })
	public static class GSAttributesComposite extends GSInstancesComposite {
		@ForEach(ObservableListExtractor.ATTRIBUTES_OF_TYPE.class)
		public static class GSAttributesContentComponent extends GSInstancesContentComponent {

		}
	}

	@org.genericsystem.reactor.annotations.Style.BackgroundColor("Orange")
	@org.genericsystem.reactor.annotations.DirectSelect(Car.class)
	@org.genericsystem.reactor.annotations.Style.FlexDirection("row")
	@org.genericsystem.reactor.annotations.Style.Flex("1 1 0%")
	@org.genericsystem.reactor.annotations.Style.Overflow("hidden")
	@org.genericsystem.reactor.annotations.ReactorDependencies({ GSHeaderComponent_.class, GSRowInstanceAttributesContentComponent.class })
	public static class GSInstanceRowLabeledAttributesComposite extends GSRowLabeledTypeAttributesComposite {
		@org.genericsystem.reactor.annotations.Style.Flex("1 1 0%")
		@org.genericsystem.reactor.annotations.Style.Overflow("hidden")
		@org.genericsystem.reactor.annotations.ReactorDependencies({ GSHeaderComponentLabel2.class })
		public static class GSHeaderComponent_ extends org.genericsystem.reactor.gs3.CompositeTagImpl {
			public static class GSHeaderComponentLabel2 extends GSLabelDisplayer {

			}
		}

		@ForEach(ATTRIBUTES_OF_INSTANCES.class)
		public static class GSRowInstanceAttributesContentComponent extends GSTypeAttributesContentComponent {

		}
	}

	@org.genericsystem.reactor.annotations.Style.BackgroundColor("Purple")
	@org.genericsystem.reactor.annotations.DirectSelect(Car.class)
	@org.genericsystem.reactor.annotations.Style.FlexDirection("row")
	@org.genericsystem.reactor.annotations.ReactorDependencies({ GSTypeAttributesContentComponent.class })
	public static class GSRowLabeledTypeAttributesComposite extends GSAttributesComposite {
		@FlexDirection("row")
		public static class GSTypeAttributesContentComponent extends GSAttributesContentComponent {

		}
	}

	@org.genericsystem.reactor.annotations.Style.BackgroundColor("Grey")
	@org.genericsystem.reactor.annotations.Style.Color("White")
	@org.genericsystem.reactor.annotations.DirectSelect(Car.class)
	@org.genericsystem.reactor.annotations.ReactorDependencies({ GSHeaderComponent.class, GSTypeAttributesContentComponent.class })
	public static class GSTypeRowLabeledTypeAttributesComposite extends GSRowLabeledTypeAttributesComposite {
		@org.genericsystem.reactor.annotations.Style.Flex("1 1 0%")
		@org.genericsystem.reactor.annotations.Style.Overflow("hidden")
		@org.genericsystem.reactor.annotations.Style.FlexDirection("row")
		@org.genericsystem.reactor.annotations.ReactorDependencies({ GSHeaderComponentLabel.class })
		public static class GSHeaderComponent extends org.genericsystem.reactor.gs3.CompositeTagImpl {
			public static class GSHeaderComponentLabel extends GSLabelDisplayer {

			}
		}
	}

	@org.genericsystem.reactor.annotations.DirectSelect(Car.class)
	@org.genericsystem.reactor.annotations.ReactorDependencies({ GSHeaderComponent___.class, GSInstancesContentComponent__.class })
	public static class GSTypeLabeledInstancesComposite extends GSInstancesComposite {
		@FlexDirection("row")
		public static class GSHeaderComponent___ extends GSTypeRowLabeledTypeAttributesComposite {

		}

		@org.genericsystem.reactor.annotations.ReactorDependencies({ GSInstanceRowLabeledAttributesComposite_.class })
		@org.genericsystem.reactor.annotations.Style.FlexDirection("row")
		public static class GSInstancesContentComponent__ extends GSInstancesContentComponent {
			public static class GSInstanceRowLabeledAttributesComposite_ extends GSInstanceRowLabeledAttributesComposite {

			}
		}
	}
}
