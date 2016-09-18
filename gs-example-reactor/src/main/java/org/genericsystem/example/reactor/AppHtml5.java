package org.genericsystem.example.reactor;

import org.genericsystem.carcolor.model.Car;
import org.genericsystem.carcolor.model.CarColor;
import org.genericsystem.carcolor.model.CarColor2;
import org.genericsystem.carcolor.model.Color;
import org.genericsystem.carcolor.model.Power;
import org.genericsystem.carcolor.model.UsedCar;
import org.genericsystem.example.reactor.AppHtml.ExampleReactorScript;
import org.genericsystem.example.reactor.AppHtml5.GSInstancesComposite;
import org.genericsystem.example.reactor.AppHtml5.GSRowInstancesComposite;
import org.genericsystem.example.reactor.AppHtml5.GSTypeAttributes;
import org.genericsystem.example.reactor.AppHtml5.GSTypeAttributesRow;
import org.genericsystem.example.reactor.AppHtml5.GSTypeLabeledInstancesComposite;
import org.genericsystem.example.reactor.AppHtml5.GSTypeLabeledInstancesComposite.GSHeaderComponent_;
import org.genericsystem.example.reactor.AppHtml5.GSTypeLabeledInstancesComposite.GSHeaderComponent_.GSTypeAttributesRow_;
import org.genericsystem.example.reactor.AppHtml5.GSTypeLabeledInstancesComposite.GSInstancesContentComponent_;
import org.genericsystem.example.reactor.AppHtml5.GSTypeLabeledInstancesComposite.GSInstancesContentComponent_.GSInstanceAttributesRow_;
import org.genericsystem.example.reactor.AppHtml5.GSTypeTableInstancesComposite;
import org.genericsystem.example.reactor.AppHtml5.GSTypeTableInstancesComposite.GSInstancesContentComponent__;
import org.genericsystem.example.reactor.AppHtml5.GSTypeTableInstancesComposite.GSInstancesContentComponent__.GSInstanceAttributesRow__;
import org.genericsystem.example.reactor.AppHtml5.GSTypeTableInstancesComposite.GSInstancesContentComponent__.GSInstanceAttributesRow__.GSHoldersContentComponent;
import org.genericsystem.example.reactor.AppHtml5.GSTypeTableInstancesComposite.GSInstancesContentComponent__.GSInstanceAttributesRow__.GSHoldersContentComponent.GSHolders_;
import org.genericsystem.reactor.annotations.DependsOnModel;
import org.genericsystem.reactor.annotations.DirectSelect;
import org.genericsystem.reactor.annotations.ForEach.ChildForEach;
import org.genericsystem.reactor.annotations.ReactorDependencies;
import org.genericsystem.reactor.annotations.RunScript;
import org.genericsystem.reactor.annotations.Style;
import org.genericsystem.reactor.annotations.Style.BackgroundColor;
import org.genericsystem.reactor.annotations.Style.ChildFlexDirection;
import org.genericsystem.reactor.annotations.Style.Flex;
import org.genericsystem.reactor.annotations.Style.FlexDirection;
import org.genericsystem.reactor.annotations.Style.FlexWrap;
import org.genericsystem.reactor.appserver.ApplicationServer;
import org.genericsystem.reactor.gs.GSApp;
import org.genericsystem.reactor.gs3.GSComposite;
import org.genericsystem.reactor.gs3.GSComposite.GSContentComponent;
import org.genericsystem.reactor.gs3.GSComposite.GSHeaderComponent;
import org.genericsystem.reactor.model.ObservableListExtractor;
import org.genericsystem.reactor.modelproperties.SelectionDefaults;

@DependsOnModel({ Car.class, Power.class, UsedCar.class, Color.class, CarColor.class, CarColor2.class })
@RunScript(ExampleReactorScript.class)
@ReactorDependencies({ GSInstancesComposite.class, GSRowInstancesComposite.class, GSTypeAttributes.class, GSTypeAttributesRow.class, GSTypeLabeledInstancesComposite.class, GSTypeTableInstancesComposite.class })
@FlexWrap("wrap")
@FlexDirection("row")
@Flex("1 1 0%")
public class AppHtml5 extends GSApp implements SelectionDefaults {

	public static void main(String[] mainArgs) {
		ApplicationServer.startSimpleGenericApp(mainArgs, AppHtml5.class, "/example-reactor");
	}

	@BackgroundColor("Green")
	@DirectSelect(Car.class)
	@ChildForEach(ObservableListExtractor.SUBINSTANCES.class)
	public static class GSInstancesComposite extends GSComposite {

	}

	@BackgroundColor("Blue")
	@FlexDirection("row")
	@DirectSelect(Car.class)
	@ChildForEach(ObservableListExtractor.SUBINSTANCES.class)
	public static class GSRowInstancesComposite extends GSComposite {

	}

	@BackgroundColor("Red")
	@DirectSelect(Car.class)
	@ChildForEach(ObservableListExtractor.ATTRIBUTES_OF_TYPE.class)
	@ReactorDependencies({ GSHeaderComponent.class, GSContentComponent.class })
	public static class GSTypeAttributes extends GSComposite {

	}

	@BackgroundColor("Purple")
	@Style.Color("White")
	@DirectSelect(Car.class)
	@FlexDirection("row")
	@ChildForEach(ObservableListExtractor.ATTRIBUTES_OF_TYPE.class)
	@ChildFlexDirection("row")
	@ReactorDependencies({ GSHeaderComponent.class, GSContentComponent.class })
	public static class GSTypeAttributesRow extends GSComposite {

	}

	@BackgroundColor("Orange")
	@FlexDirection("row")
	@ChildFlexDirection("row")
	@ChildForEach(ObservableListExtractor.ATTRIBUTES_OF_INSTANCES.class)
	@ReactorDependencies({ GSHeaderComponent.class, GSContentComponent.class })
	public static class GSInstanceAttributesRow extends GSComposite {

	}

	@BackgroundColor("Brown")
	@ChildForEach(ObservableListExtractor.HOLDERS.class)
	@ReactorDependencies({ GSContentComponent.class })
	public static class GSHolders extends GSComposite {

	}

	@DirectSelect(Car.class)
	@ReactorDependencies({ GSHeaderComponent_.class, GSInstancesContentComponent_.class })
	public static class GSTypeLabeledInstancesComposite extends GSInstancesComposite {
		@ReactorDependencies({ GSTypeAttributesRow_.class })
		public static class GSHeaderComponent_ extends GSHeaderComponent {
			public static class GSTypeAttributesRow_ extends GSTypeAttributesRow {

			}
		}

		@ReactorDependencies({ GSInstanceAttributesRow_.class })
		public static class GSInstancesContentComponent_ extends GSContentComponent {
			public static class GSInstanceAttributesRow_ extends GSInstanceAttributesRow {

			}
		}
	}

	@DirectSelect(Car.class)
	@ReactorDependencies({ GSHeaderComponent_.class, GSInstancesContentComponent__.class })
	public static class GSTypeTableInstancesComposite extends GSTypeLabeledInstancesComposite {
		@ReactorDependencies({ GSInstanceAttributesRow__.class })
		public static class GSInstancesContentComponent__ extends GSInstancesContentComponent_ {
			@ReactorDependencies({ GSHeaderComponent.class, GSHoldersContentComponent.class })
			public static class GSInstanceAttributesRow__ extends GSInstanceAttributesRow_ {
				@ReactorDependencies({ GSHolders_.class })
				public static class GSHoldersContentComponent extends GSContentComponent {
					public static class GSHolders_ extends GSHolders {

					}
				}
			}
		}
	}

}
