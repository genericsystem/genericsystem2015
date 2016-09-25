package org.genericsystem.example.reactor;

import org.genericsystem.api.core.annotations.Components;
import org.genericsystem.api.core.annotations.Meta;
import org.genericsystem.api.core.annotations.SystemGeneric;
import org.genericsystem.api.core.annotations.value.IntValue;
import org.genericsystem.api.core.annotations.value.StringValue;
import org.genericsystem.carcolor.model.Car;
import org.genericsystem.carcolor.model.CarColor;
import org.genericsystem.carcolor.model.CarColor2;
import org.genericsystem.carcolor.model.Color;
import org.genericsystem.carcolor.model.Power;
import org.genericsystem.carcolor.model.UsedCar;
import org.genericsystem.example.reactor.AppHtml.ExampleReactorScript;
import org.genericsystem.example.reactor.AppHtml5.AudiTT;
import org.genericsystem.example.reactor.AppHtml5.AudiTT.AudiTTGreen;
import org.genericsystem.example.reactor.AppHtml5.AudiTT.AudiTTPower;
import org.genericsystem.example.reactor.AppHtml5.AudiTT.Green;
import org.genericsystem.example.reactor.AppHtml5.GSCell.GSSubcell;
import org.genericsystem.example.reactor.AppHtml5.GSInstancesComposite;
import org.genericsystem.example.reactor.AppHtml5.GSRowInstancesComposite;
import org.genericsystem.example.reactor.AppHtml5.GSTypeAttributes;
import org.genericsystem.example.reactor.AppHtml5.GSTypeAttributesRow;
import org.genericsystem.example.reactor.AppHtml5.GSTypeLabeledInstancesComposite;
import org.genericsystem.example.reactor.AppHtml5.GSTypeLabeledInstancesComposite.GSHeaderComponent_;
import org.genericsystem.example.reactor.AppHtml5.GSTypeLabeledInstancesComposite.GSHeaderComponent_.GSTypeAttributesRow_;
import org.genericsystem.example.reactor.AppHtml5.GSTypeLabeledInstancesComposite.GSInstanceContentComponent_;
import org.genericsystem.example.reactor.AppHtml5.GSTypeLabeledInstancesComposite.GSInstanceContentComponent_.GSInstanceAttributesRow_;
import org.genericsystem.example.reactor.AppHtml5.GSTypeTableInstancesComposite;
import org.genericsystem.example.reactor.AppHtml5.GSTypeTableInstancesComposite.GSInstanceContentComponent__;
import org.genericsystem.example.reactor.AppHtml5.GSTypeTableInstancesComposite.GSInstanceContentComponent__.GSInstanceAttributesRow__;
import org.genericsystem.example.reactor.AppHtml5.GSTypeTableInstancesComposite.GSInstanceContentComponent__.GSInstanceAttributesRow__.GSHoldersContentComponent;
import org.genericsystem.example.reactor.AppHtml5.GSTypeTableInstancesComposite.GSInstanceContentComponent__.GSInstanceAttributesRow__.GSHoldersContentComponent.GSCell__;
import org.genericsystem.example.reactor.AppHtml5.GSTypeTableInstancesComposite.GSInstanceContentComponent__.GSInstanceAttributesRow__.GSInstanceNameComponent;
import org.genericsystem.example.reactor.AppHtml5.GSTypeTableInstancesComposite.GSInstanceContentComponent__.GSInstanceAttributesRow__.GSInstanceNameComponent.GSInstanceNameSubcell_;
import org.genericsystem.example.reactor.AppHtml5.GSValueComponents;
import org.genericsystem.example.reactor.AppHtml5.TestCell;
import org.genericsystem.example.reactor.AppHtml5.TestCell.GSCell_;
import org.genericsystem.example.reactor.AppHtml5.Unit;
import org.genericsystem.reactor.annotations.DependsOnModel;
import org.genericsystem.reactor.annotations.DirectSelect;
import org.genericsystem.reactor.annotations.ForEach.ChildForEach;
import org.genericsystem.reactor.annotations.Parent;
import org.genericsystem.reactor.annotations.ReactorDependencies;
import org.genericsystem.reactor.annotations.ReactorDependencies.ChildReactorDependencies;
import org.genericsystem.reactor.annotations.RunScript;
import org.genericsystem.reactor.annotations.Styles;
import org.genericsystem.reactor.annotations.Styles.BackgroundColor;
import org.genericsystem.reactor.annotations.Styles.ChildFlexDirection;
import org.genericsystem.reactor.annotations.Styles.Flex;
import org.genericsystem.reactor.annotations.Styles.FlexDirectionStyle;
import org.genericsystem.reactor.annotations.Styles.FlexWrap;
import org.genericsystem.reactor.appserver.ApplicationServer;
import org.genericsystem.reactor.gs.FlexDirection;
import org.genericsystem.reactor.gs.GSApp;
import org.genericsystem.reactor.gs3.GSComposite;
import org.genericsystem.reactor.gs3.GSComposite.GSContentComponent;
import org.genericsystem.reactor.gs3.GSComposite.GSHeaderComponent;
import org.genericsystem.reactor.model.ObservableListExtractor;
import org.genericsystem.reactor.modelproperties.SelectionDefaults;

@DependsOnModel({ Car.class, Power.class, UsedCar.class, Color.class, CarColor.class, CarColor2.class, AudiTT.class, Green.class, AudiTTGreen.class, AudiTTPower.class, Unit.class })
@RunScript(ExampleReactorScript.class)
@ReactorDependencies({ GSInstancesComposite.class, GSRowInstancesComposite.class, GSTypeAttributes.class, GSTypeAttributesRow.class, GSValueComponents.class, GSTypeLabeledInstancesComposite.class, GSTypeTableInstancesComposite.class, TestCell.class })
@FlexWrap("wrap")
@Flex("1 1 0%")
public class AppHtml5 extends GSApp implements SelectionDefaults {

	public static void main(String[] mainArgs) {
		ApplicationServer.startSimpleGenericApp(mainArgs, AppHtml5.class, "/example-reactor");
	}

	@org.genericsystem.reactor.annotations.Styles.BackgroundColor("Green")
	@org.genericsystem.reactor.annotations.DirectSelect(Car.class)
	@org.genericsystem.reactor.annotations.ForEach.ChildForEach(ObservableListExtractor.SUBINSTANCES.class)
	public static class GSInstancesComposite extends org.genericsystem.reactor.gs3.GSComposite {

	}

	@BackgroundColor("Blue")
	@FlexDirectionStyle(FlexDirection.ROW)
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
	@Styles.Color("White")
	@DirectSelect(Car.class)
	@FlexDirectionStyle(FlexDirection.ROW)
	@ChildForEach(ObservableListExtractor.ATTRIBUTES_OF_TYPE.class)
	@ChildFlexDirection("row")
	@ReactorDependencies({ GSHeaderComponent.class, GSContentComponent.class })
	public static class GSTypeAttributesRow extends GSComposite {

	}

	@org.genericsystem.reactor.annotations.Styles.BackgroundColor("Orange")
	@org.genericsystem.reactor.annotations.Styles.FlexDirectionStyle(FlexDirection.ROW)
	@org.genericsystem.reactor.annotations.Styles.ChildFlexDirection("row")
	@org.genericsystem.reactor.annotations.ForEach.ChildForEach(ObservableListExtractor.ATTRIBUTES_OF_INSTANCES.class)
	@org.genericsystem.reactor.annotations.ReactorDependencies({ GSHeaderComponent.class, GSContentComponent.class })
	public static class GSInstanceAttributesRow extends org.genericsystem.reactor.gs3.GSComposite {

	}

	@org.genericsystem.reactor.annotations.Styles.BackgroundColor("Brown")
	@org.genericsystem.reactor.annotations.ForEach.ChildForEach(ObservableListExtractor.HOLDERS.class)
	@org.genericsystem.reactor.annotations.ReactorDependencies({ GSContentComponent.class })
	public static class GSHolders extends org.genericsystem.reactor.gs3.GSComposite {

	}

	@DirectSelect(Power.class)
	@BackgroundColor("Yellow")
	@FlexDirectionStyle(FlexDirection.ROW)
	@ChildForEach(ObservableListExtractor.COMPONENTS.class)
	@ReactorDependencies({ GSHeaderComponent.class, GSContentComponent.class })
	public static class GSValueComponents extends GSComposite {

	}

	@SystemGeneric
	@Meta(Car.class)
	@StringValue("Audi TT")
	public static class AudiTT {

		@SystemGeneric
		@Meta(Color.class)
		@StringValue("green")
		public static class Green {

		}

		@Meta(Power.class)
		@IntValue(432)
		@SystemGeneric
		@Components(AudiTT.class)
		public static class AudiTTPower {
		}

		@Meta(CarColor.class)
		@SystemGeneric
		@Components({ AudiTT.class, Green.class })
		public static class AudiTTGreen {

		}
	}

	@org.genericsystem.reactor.annotations.DirectSelect(AudiTT.class)
	@org.genericsystem.reactor.annotations.ReactorDependencies(GSCell_.class)
	public static class TestCell extends org.genericsystem.reactor.gs3.GSComposite {
		@DirectSelect(CarColor.class)
		public static class GSCell_ extends GSCell {

		}
	}

	// @org.genericsystem.reactor.annotations.ReactorDependencies(GSCellContentComponent.class)
	// public static class GSCell extends GSHolders {
	// @org.genericsystem.reactor.annotations.ReactorDependencies({ GSSubcell.class })
	// public static class GSCellContentComponent extends GSContentComponent {
	// public static class GSSubcell extends GSValueComponents {
	//
	// }
	// }
	// }

	@ChildReactorDependencies(GSSubcell.class)
	public static class GSCell extends GSHolders {
		@Parent(GSContentComponent.class)
		public static class GSSubcell extends GSValueComponents {

		}
	}

	@org.genericsystem.reactor.annotations.DirectSelect(Car.class)
	@org.genericsystem.reactor.annotations.ReactorDependencies({ GSHeaderComponent_.class, GSInstanceContentComponent_.class })
	@ChildReactorDependencies(GSInstanceContentComponent_.class)
	public static class GSTypeLabeledInstancesComposite extends GSInstancesComposite {
		@org.genericsystem.reactor.annotations.ReactorDependencies({ GSTypeAttributesRow_.class })
		public static class GSHeaderComponent_ extends GSHeaderComponent {
			public static class GSTypeAttributesRow_ extends GSTypeAttributesRow {

			}
		}

		@org.genericsystem.reactor.annotations.ReactorDependencies({ GSInstanceAttributesRow_.class })
		public static class GSInstanceContentComponent_ extends GSContentComponent {
			public static class GSInstanceAttributesRow_ extends GSInstanceAttributesRow {

			}
		}
	}

	@SystemGeneric
	@Components(Power.class)
	@StringValue("Unit")
	public static class Unit {
	}

	@org.genericsystem.reactor.annotations.DirectSelect(Car.class)
	@org.genericsystem.reactor.annotations.ReactorDependencies({ GSHeaderComponent_.class, GSInstanceContentComponent__.class })
	@ChildReactorDependencies(GSInstanceContentComponent__.class)
	public static class GSTypeTableInstancesComposite extends GSTypeLabeledInstancesComposite {
		@org.genericsystem.reactor.annotations.ReactorDependencies({ GSInstanceAttributesRow__.class })
		public static class GSInstanceContentComponent__ extends GSInstanceContentComponent_ {
			@org.genericsystem.reactor.annotations.ReactorDependencies({ GSInstanceNameComponent.class, GSHoldersContentComponent.class })
			public static class GSInstanceAttributesRow__ extends GSInstanceAttributesRow_ {
				@org.genericsystem.reactor.annotations.ReactorDependencies(GSInstanceNameSubcell_.class)
				public static class GSInstanceNameComponent extends GSHeaderComponent {
					@BackgroundColor("Orange")
					public static class GSInstanceNameSubcell_ extends GSValueComponents {

					}
				}

				@org.genericsystem.reactor.annotations.ReactorDependencies({ GSCell__.class })
				public static class GSHoldersContentComponent extends GSContentComponent {
					public static class GSCell__ extends GSCell {

					}
				}
			}
		}
	}
}
