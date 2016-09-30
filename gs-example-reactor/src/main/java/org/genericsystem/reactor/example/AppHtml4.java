package org.genericsystem.reactor.example;

import org.genericsystem.carcolor.model.Car;
import org.genericsystem.carcolor.model.CarColor;
import org.genericsystem.carcolor.model.CarColor2;
import org.genericsystem.carcolor.model.Color;
import org.genericsystem.carcolor.model.Power;
import org.genericsystem.carcolor.model.UsedCar;
import org.genericsystem.reactor.annotations.ForEach;
import org.genericsystem.reactor.annotations.ReactorDependencies;
import org.genericsystem.reactor.annotations.DependsOnModel;
import org.genericsystem.reactor.annotations.DirectSelect;
import org.genericsystem.reactor.annotations.RunScript;
import org.genericsystem.reactor.annotations.Styles;
import org.genericsystem.reactor.annotations.Styles.BackgroundColor;
import org.genericsystem.reactor.annotations.Styles.Flex;
import org.genericsystem.reactor.annotations.Styles.FlexDirectionStyle;
import org.genericsystem.reactor.annotations.Styles.FlexWrap;
import org.genericsystem.reactor.annotations.Styles.Overflow;
import org.genericsystem.reactor.appserver.ApplicationServer;
import org.genericsystem.reactor.az.FlexDirection;
import org.genericsystem.reactor.az.GSApp;
import org.genericsystem.reactor.az.GSDiv;
import org.genericsystem.reactor.example.AppHtml.ExampleReactorScript;
import org.genericsystem.reactor.example.AppHtml4.GSAttributesComposite;
import org.genericsystem.reactor.example.AppHtml4.GSAttributesComposite.GSAttributesContentComponent;
import org.genericsystem.reactor.example.AppHtml4.GSComposite.GSContentComponent;
import org.genericsystem.reactor.example.AppHtml4.GSComposite.GSContentComponent.GSContentComponentLabel;
import org.genericsystem.reactor.example.AppHtml4.GSInstanceRowLabeledAttributesComposite.GSHeaderComponent_;
import org.genericsystem.reactor.example.AppHtml4.GSInstanceRowLabeledAttributesComposite.GSHeaderComponent_.GSHeaderComponentLabel2;
import org.genericsystem.reactor.example.AppHtml4.GSInstanceRowLabeledAttributesComposite.GSRowInstanceAttributesContentComponent;
import org.genericsystem.reactor.example.AppHtml4.GSInstancesComposite;
import org.genericsystem.reactor.example.AppHtml4.GSInstancesComposite.GSInstancesContentComponent;
import org.genericsystem.reactor.example.AppHtml4.GSRowInstancesComposite;
import org.genericsystem.reactor.example.AppHtml4.GSRowLabeledTypeAttributesComposite;
import org.genericsystem.reactor.example.AppHtml4.GSRowLabeledTypeAttributesComposite.GSTypeAttributesContentComponent;
import org.genericsystem.reactor.example.AppHtml4.GSTypeLabeledInstancesComposite;
import org.genericsystem.reactor.example.AppHtml4.GSTypeLabeledInstancesComposite.GSHeaderComponent___;
import org.genericsystem.reactor.example.AppHtml4.GSTypeLabeledInstancesComposite.GSInstancesContentComponent__;
import org.genericsystem.reactor.example.AppHtml4.GSTypeLabeledInstancesComposite.GSInstancesContentComponent__.GSInstanceRowLabeledAttributesComposite_;
import org.genericsystem.reactor.example.AppHtml4.GSTypeRowLabeledTypeAttributesComposite;
import org.genericsystem.reactor.example.AppHtml4.GSTypeRowLabeledTypeAttributesComposite.GSHeaderComponent;
import org.genericsystem.reactor.example.AppHtml4.GSTypeRowLabeledTypeAttributesComposite.GSHeaderComponent.GSHeaderComponentLabel;
import org.genericsystem.reactor.gstag.HtmlLabel.GSLabelDisplayer;
import org.genericsystem.reactor.model.ObservableListExtractor;
import org.genericsystem.reactor.model.ObservableListExtractor.ATTRIBUTES_OF_INSTANCES;
import org.genericsystem.reactor.modelproperties.SelectionDefaults;

@DependsOnModel({ Car.class, Power.class, UsedCar.class, Color.class, CarColor.class, CarColor2.class })
@RunScript(ExampleReactorScript.class)
@ReactorDependencies({ GSInstancesComposite.class, GSRowInstancesComposite.class, GSAttributesComposite.class, GSRowLabeledTypeAttributesComposite.class, GSTypeRowLabeledTypeAttributesComposite.class, GSTypeLabeledInstancesComposite.class })
@FlexWrap("wrap")
@Flex("1 1 0%")
public class AppHtml4 extends GSApp implements SelectionDefaults {

	public static void main(String[] mainArgs) {
		ApplicationServer.startSimpleGenericApp(mainArgs, AppHtml4.class, "/example-reactor");
	}

	@Flex("1 1 0%")
	@Overflow("hidden")
	@ReactorDependencies({ GSContentComponent.class })
	public static abstract class GSComposite extends GSDiv {
		@Flex("1 1 0%")
		@Overflow("hidden")
		@ReactorDependencies({ GSContentComponentLabel.class })
		public static class GSContentComponent extends GSDiv {
			@Overflow("hidden")
			public static class GSContentComponentLabel extends GSLabelDisplayer {

			}
		}
	}

	@BackgroundColor("Green")
	@DirectSelect(Car.class)
	@ReactorDependencies({ GSInstancesContentComponent.class })
	public static class GSInstancesComposite extends GSComposite {
		@ForEach(ObservableListExtractor.SUBINSTANCES.class)
		public static class GSInstancesContentComponent extends GSContentComponent {

		}
	}

	@BackgroundColor("Blue")
	@FlexDirectionStyle(FlexDirection.ROW)
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
	@FlexDirectionStyle(FlexDirection.ROW)
	@Flex("1 1 0%")
	@Overflow("hidden")
	@ReactorDependencies({ GSHeaderComponent_.class, GSRowInstanceAttributesContentComponent.class })
	public static class GSInstanceRowLabeledAttributesComposite extends GSRowLabeledTypeAttributesComposite {
		@Flex("1 1 0%")
		@Overflow("hidden")
		@ReactorDependencies({ GSHeaderComponentLabel2.class })
		public static class GSHeaderComponent_ extends GSDiv {
			public static class GSHeaderComponentLabel2 extends GSLabelDisplayer {

			}
		}

		@ForEach(ATTRIBUTES_OF_INSTANCES.class)
		public static class GSRowInstanceAttributesContentComponent extends GSTypeAttributesContentComponent {

		}
	}

	@BackgroundColor("Purple")
	@DirectSelect(Car.class)
	@FlexDirectionStyle(FlexDirection.ROW)
	@ReactorDependencies({ GSTypeAttributesContentComponent.class })
	public static class GSRowLabeledTypeAttributesComposite extends GSAttributesComposite {
		@FlexDirectionStyle(FlexDirection.ROW)
		public static class GSTypeAttributesContentComponent extends GSAttributesContentComponent {

		}
	}

	@BackgroundColor("Grey")
	@Styles.Color("White")
	@DirectSelect(Car.class)
	@ReactorDependencies({ GSHeaderComponent.class, GSTypeAttributesContentComponent.class })
	public static class GSTypeRowLabeledTypeAttributesComposite extends GSRowLabeledTypeAttributesComposite {
		@Flex("1 1 0%")
		@Overflow("hidden")
		@FlexDirectionStyle(FlexDirection.ROW)
		@ReactorDependencies({ GSHeaderComponentLabel.class })
		public static class GSHeaderComponent extends GSDiv {
			public static class GSHeaderComponentLabel extends GSLabelDisplayer {

			}
		}
	}

	@DirectSelect(Car.class)
	@ReactorDependencies({ GSHeaderComponent___.class, GSInstancesContentComponent__.class })
	public static class GSTypeLabeledInstancesComposite extends GSInstancesComposite {
		@FlexDirectionStyle(FlexDirection.ROW)
		public static class GSHeaderComponent___ extends GSTypeRowLabeledTypeAttributesComposite {

		}

		@ReactorDependencies({ GSInstanceRowLabeledAttributesComposite_.class })
		@FlexDirectionStyle(FlexDirection.ROW)
		public static class GSInstancesContentComponent__ extends GSInstancesContentComponent {
			public static class GSInstanceRowLabeledAttributesComposite_ extends GSInstanceRowLabeledAttributesComposite {

			}
		}
	}
}
