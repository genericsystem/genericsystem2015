package org.genericsystem.reactor.example;

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
import org.genericsystem.reactor.annotations.DependsOnModel;
import org.genericsystem.reactor.annotations.DirectSelect;
import org.genericsystem.reactor.annotations.ForEach.ChildForEach;
import org.genericsystem.reactor.annotations.ReactorDependencies;
import org.genericsystem.reactor.annotations.ReactorDependencies.ChildReactorDependencies;
import org.genericsystem.reactor.annotations.RunScript;
import org.genericsystem.reactor.annotations.Styles;
import org.genericsystem.reactor.annotations.Styles.BackgroundColor;
import org.genericsystem.reactor.annotations.Styles.ChildFlexDirection;
import org.genericsystem.reactor.annotations.Styles.Flex;
import org.genericsystem.reactor.annotations.Styles.FlexDirectionStyle;
import org.genericsystem.reactor.annotations.Styles.FlexWrap;
import org.genericsystem.reactor.annotations.Styles.MarginBottom;
import org.genericsystem.reactor.annotations.Styles.MarginRight;
import org.genericsystem.reactor.annotations.Styles.Style;
import org.genericsystem.reactor.appserver.ApplicationServer;
import org.genericsystem.reactor.az.FlexDirection;
import org.genericsystem.reactor.az.GSApp;
import org.genericsystem.reactor.az3.GSComposite;
import org.genericsystem.reactor.az3.GSComposite.GSContentComponent;
import org.genericsystem.reactor.az3.GSComposite.GSHeaderComponent;
import org.genericsystem.reactor.example.AppHtml.ExampleReactorScript;
import org.genericsystem.reactor.example.AppHtml5.AudiTT;
import org.genericsystem.reactor.example.AppHtml5.AudiTT.AudiTTGreen;
import org.genericsystem.reactor.example.AppHtml5.AudiTT.AudiTTPower;
import org.genericsystem.reactor.example.AppHtml5.AudiTT.Green;
import org.genericsystem.reactor.example.AppHtml5.GSInstancesComposite;
import org.genericsystem.reactor.example.AppHtml5.GSRowInstancesComposite;
import org.genericsystem.reactor.example.AppHtml5.GSTypeAttributes;
import org.genericsystem.reactor.example.AppHtml5.GSTypeAttributesRow;
import org.genericsystem.reactor.example.AppHtml5.GSTypeLabeledInstancesComposite;
import org.genericsystem.reactor.example.AppHtml5.GSTypeTableInstancesComposite;
import org.genericsystem.reactor.example.AppHtml5.GSValueComponents;
import org.genericsystem.reactor.example.AppHtml5.TestCell;
import org.genericsystem.reactor.example.AppHtml5.Unit;
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

	@BackgroundColor("Green")
	@DirectSelect(Car.class)
	@ChildForEach(ObservableListExtractor.SUBINSTANCES.class)
	public static class GSInstancesComposite extends GSComposite {

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

	@BackgroundColor("Orange")
	@FlexDirectionStyle(FlexDirection.ROW)
	@ChildFlexDirection("row")
	@ChildForEach(ObservableListExtractor.ATTRIBUTES_OF_INSTANCES.class)
	@ReactorDependencies({ GSHeaderComponent.class, GSContentComponent.class })
	public static class GSInstanceAttributesRow extends GSComposite {

	}

	@BackgroundColor("Brown")
	@ChildForEach(ObservableListExtractor.HOLDERS.class)
	@ChildReactorDependencies(decorate = GSContentComponent.class, value = GSValueComponents2.class)
	@MarginRight("1px")
	@MarginBottom("1px")
	public static class GSHolders extends GSComposite {

	}

	// @DirectSelect(Power.class)
	@BackgroundColor("Purple")
	@FlexDirectionStyle(FlexDirection.ROW)
	@ChildForEach(ObservableListExtractor.COMPONENTS.class)
	@ReactorDependencies({ GSHeaderComponent.class, GSContentComponent.class })
	public static class GSValueComponents extends GSComposite {

	}

	@ChildForEach(ObservableListExtractor.OTHER_COMPONENTS_1.class)
	public static class GSValueComponents1 extends GSValueComponents {

	}

	@BackgroundColor("Yellow")
	@ChildForEach(ObservableListExtractor.OTHER_COMPONENTS_2.class)
	public static class GSValueComponents2 extends GSValueComponents {

	}

	@DirectSelect(AudiTT.class)
	@ReactorDependencies(GSCell.class)
	public static class TestCell extends GSComposite {
	}

	@DirectSelect(CarColor.class)
	public static class GSCell extends GSHolders {

	}

	@DirectSelect(Car.class)
	@ReactorDependencies({ GSHeaderComponent.class, GSContentComponent.class })
	@ChildReactorDependencies(decorate = GSHeaderComponent.class, value = GSTypeAttributesRow.class)
	@ChildReactorDependencies(decorate = GSContentComponent.class, value = GSInstanceAttributesRow.class)
	@ChildFlexDirection("row")
	@Style(propertyName = "margin", propertyValue = "4px")
	public static class GSTypeLabeledInstancesComposite extends GSInstancesComposite {

	}

	@DirectSelect(Car.class)
	@ChildReactorDependencies(decorate = GSHeaderComponent.class, value = GSTypeAttributesRowWithComponents.class)
	@ChildReactorDependencies(decorate = GSContentComponent.class, value = GSInstanceAttributesRowWithComponents.class)
	public static class GSTypeTableInstancesComposite extends GSTypeLabeledInstancesComposite {

	}

	@Styles.Color("#000000")
	@BackgroundColor("purple")
	@ChildReactorDependencies(decorate = GSHeaderComponent.class, value = GSValueComponents1.class)
	@ChildReactorDependencies(decorate = GSContentComponent.class, value = GSValueComponents1.class)
	public static class GSTypeAttributesRowWithComponents extends GSTypeAttributesRow {

	}

	@ChildReactorDependencies(decorate = GSHeaderComponent.class, value = GSValueComponents2.class)
	@ChildReactorDependencies(decorate = GSContentComponent.class, value = GSHolders.class)
	public static class GSInstanceAttributesRowWithComponents extends GSInstanceAttributesRow {

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

	public static class Unit {
	}
}
