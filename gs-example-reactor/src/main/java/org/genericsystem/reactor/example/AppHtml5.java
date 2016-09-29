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
import org.genericsystem.reactor.annotations.ForEach;
import org.genericsystem.reactor.annotations.ReactorDependencies;
import org.genericsystem.reactor.annotations.RunScript;
import org.genericsystem.reactor.annotations.Select;
import org.genericsystem.reactor.annotations.Styles;
import org.genericsystem.reactor.annotations.Styles.BackgroundColor;
import org.genericsystem.reactor.annotations.Styles.Flex;
import org.genericsystem.reactor.annotations.Styles.FlexDirectionStyle;
import org.genericsystem.reactor.annotations.Styles.FlexWrap;
import org.genericsystem.reactor.annotations.Styles.GenericValueBackgroundColor;
import org.genericsystem.reactor.annotations.Styles.KeepFlexDirection;
import org.genericsystem.reactor.annotations.Styles.MarginBottom;
import org.genericsystem.reactor.annotations.Styles.MarginRight;
import org.genericsystem.reactor.annotations.Styles.ReverseFlexDirection;
import org.genericsystem.reactor.annotations.Styles.Style;
import org.genericsystem.reactor.appserver.ApplicationServer;
import org.genericsystem.reactor.az.FlexDirection;
import org.genericsystem.reactor.az.GSApp;
import org.genericsystem.reactor.az3.GSComposite;
import org.genericsystem.reactor.az3.GSComposite.GSContentComponent;
import org.genericsystem.reactor.az3.GSComposite.GSHeaderComponent;
import org.genericsystem.reactor.az3.GSComposite.GSTable;
import org.genericsystem.reactor.az3.GSComposite.GSTable.GSContentRow;
import org.genericsystem.reactor.az3.GSComposite.GSTable.GSHeaderRow;
import org.genericsystem.reactor.example.AppHtml.ExampleReactorScript;
import org.genericsystem.reactor.example.AppHtml5.AudiTT;
import org.genericsystem.reactor.example.AppHtml5.AudiTT.AudiTTGreen;
import org.genericsystem.reactor.example.AppHtml5.AudiTT.AudiTTPower;
import org.genericsystem.reactor.example.AppHtml5.AudiTT.Green;
import org.genericsystem.reactor.example.AppHtml5.GSInstancesComposite;
import org.genericsystem.reactor.example.AppHtml5.GSInstancesTable;
import org.genericsystem.reactor.example.AppHtml5.GSRowInstancesComposite;
import org.genericsystem.reactor.example.AppHtml5.GSTypeAttributes;
import org.genericsystem.reactor.example.AppHtml5.GSTypeAttributesRow;
import org.genericsystem.reactor.example.AppHtml5.GSTypeLabeledInstancesComposite;
import org.genericsystem.reactor.example.AppHtml5.GSTypeTableInstancesComposite;
import org.genericsystem.reactor.example.AppHtml5.GSValueComponents;
import org.genericsystem.reactor.example.AppHtml5.TestCell;
import org.genericsystem.reactor.example.AppHtml5.Unit;
import org.genericsystem.reactor.model.ObservableListExtractor;
import org.genericsystem.reactor.model.ObservableValueSelector;
import org.genericsystem.reactor.modelproperties.SelectionDefaults;

@DependsOnModel({ Car.class, Power.class, UsedCar.class, Color.class, CarColor.class, CarColor2.class, AudiTT.class, Green.class, AudiTTGreen.class, AudiTTPower.class, Unit.class })
@RunScript(ExampleReactorScript.class)
@ReactorDependencies({ GSInstancesComposite.class, GSRowInstancesComposite.class, GSTypeAttributes.class, GSTypeAttributesRow.class, GSValueComponents.class, GSTypeLabeledInstancesComposite.class, GSTypeTableInstancesComposite.class, TestCell.class,
		GSInstancesTable.class })
@FlexWrap("wrap")
@Flex("1 1 0%")
public class AppHtml5 extends GSApp implements SelectionDefaults {

	public static void main(String[] mainArgs) {
		ApplicationServer.startSimpleGenericApp(mainArgs, AppHtml5.class, "/example-reactor");
	}

	@BackgroundColor("Green")
	@DirectSelect(Car.class)
	@ForEach(decorate = GSContentComponent.class, value = ObservableListExtractor.SUBINSTANCES.class)
	public static class GSInstancesComposite extends GSComposite {

	}

	@BackgroundColor("Blue")
	@FlexDirectionStyle(FlexDirection.ROW)
	@DirectSelect(Car.class)
	@ForEach(decorate = GSContentComponent.class, value = ObservableListExtractor.SUBINSTANCES.class)
	public static class GSRowInstancesComposite extends GSComposite {

	}

	@BackgroundColor("Red")
	@DirectSelect(Car.class)
	@ForEach(decorate = GSContentComponent.class, value = ObservableListExtractor.ATTRIBUTES_OF_TYPE.class)
	@ReactorDependencies({ GSHeaderComponent.class, GSContentComponent.class })
	public static class GSTypeAttributes extends GSComposite {

	}

	@BackgroundColor(decorate = GSContentComponent.class, value = "Purple")
	@BackgroundColor(decorate = GSHeaderComponent.class, value = "Purple")
	@MarginRight(decorate = GSContentComponent.class, value = "1px")
	@MarginBottom(decorate = GSContentComponent.class, value = "1px")
	@MarginRight(decorate = GSHeaderComponent.class, value = "1px")
	@MarginBottom(decorate = GSHeaderComponent.class, value = "1px")
	@Styles.Color("White")
	@DirectSelect(Car.class)
	@KeepFlexDirection
	@ForEach(decorate = GSContentComponent.class, value = ObservableListExtractor.ATTRIBUTES_OF_TYPE.class)
	@FlexDirectionStyle(decorate = GSContentComponent.class, value = FlexDirection.ROW)
	@FlexDirectionStyle(decorate = GSHeaderComponent.class, value = FlexDirection.ROW)
	@ReactorDependencies({ GSHeaderComponent.class, GSContentComponent.class })
	public static class GSTypeAttributesRow extends GSComposite {

	}

	@BackgroundColor("Orange")
	@KeepFlexDirection
	@ForEach(decorate = GSContentComponent.class, value = ObservableListExtractor.ATTRIBUTES_OF_INSTANCES.class)
	@FlexDirectionStyle(decorate = GSContentComponent.class, value = FlexDirection.ROW)
	@FlexDirectionStyle(decorate = GSHeaderComponent.class, value = FlexDirection.ROW)
	@ReactorDependencies({ GSHeaderComponent.class, GSContentComponent.class })
	@MarginRight(decorate = GSHeaderComponent.class, value = "1px")
	@MarginBottom(decorate = GSHeaderComponent.class, value = "1px")
	@MarginRight(decorate = GSContentComponent.class, value = "1px")
	@MarginBottom(decorate = GSContentComponent.class, value = "1px")
	public static class GSInstanceAttributesRow extends GSComposite {

	}

	@BackgroundColor("Brown")
	@ForEach(decorate = GSContentComponent.class, value = ObservableListExtractor.HOLDERS.class)
	@ReactorDependencies(decorate = GSContentComponent.class, value = GSValueComponents2.class)
	public static class GSHolders extends GSComposite {

	}

	// @DirectSelect(Power.class)
	@BackgroundColor(decorate = GSContentComponent.class, value = "Purple")
	@FlexDirectionStyle(FlexDirection.ROW)
	@ForEach(decorate = GSContentComponent.class, value = ObservableListExtractor.COMPONENTS.class)
	@ReactorDependencies({ GSHeaderComponent.class, GSContentComponent.class })
	@Select(decorate = GSHeaderComponent.class, value = ObservableValueSelector.STRICT_ATTRIBUTE_SELECTOR.class)
	@MarginRight(decorate = GSContentComponent.class, value = "1px")
	@MarginBottom(decorate = GSContentComponent.class, value = "1px")
	public static class GSValueComponents extends GSComposite {

	}

	@ForEach(decorate = GSContentComponent.class, value = ObservableListExtractor.OTHER_COMPONENTS_1.class)
	public static class GSValueComponents1 extends GSValueComponents {

	}

	@GenericValueBackgroundColor(decorate = GSContentComponent.class)
	@BackgroundColor(decorate = GSHeaderComponent.class, value = "Yellow")
	@ForEach(decorate = GSContentComponent.class, value = ObservableListExtractor.OTHER_COMPONENTS_2.class)
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
	@ReactorDependencies(decorate = GSHeaderComponent.class, value = GSTypeAttributesRow.class)
	@ReactorDependencies(decorate = GSContentComponent.class, value = GSInstanceAttributesRow.class)
	@Style(name = "margin", value = "4px")
	@ReverseFlexDirection(decorate = GSHeaderComponent.class)
	@ReverseFlexDirection(decorate = GSContentComponent.class)
	public static class GSTypeLabeledInstancesComposite extends GSInstancesComposite {

	}

	@DirectSelect(Car.class)
	@ReactorDependencies(decorate = GSHeaderComponent.class, value = GSTypeAttributesRowWithComponents.class)
	@ReactorDependencies(decorate = GSContentComponent.class, value = GSInstanceAttributesRowWithComponents.class)
	public static class GSTypeTableInstancesComposite extends GSTypeLabeledInstancesComposite {

	}

	@ReactorDependencies(decorate = GSHeaderComponent.class, value = GSValueComponents1.class)
	@ReactorDependencies(decorate = GSContentComponent.class, value = GSValueComponents1.class)
	public static class GSTypeAttributesRowWithComponents extends GSTypeAttributesRow {

	}

	@ReactorDependencies(decorate = GSHeaderComponent.class, value = GSValueComponents2.class)
	@ReactorDependencies(decorate = GSContentComponent.class, value = GSHolders.class)
	public static class GSInstanceAttributesRowWithComponents extends GSInstanceAttributesRow {

	}

	@DirectSelect(Car.class)
	@Style(name = "margin", value = "4px")
	@ForEach(decorate = { GSHeaderRow.class, GSContentComponent.class }, value = ObservableListExtractor.ATTRIBUTES_OF_TYPE.class)
	@ForEach(decorate = GSContentRow.class, value = ObservableListExtractor.SUBINSTANCES.class)
	@ForEach(decorate = { GSContentRow.class, GSContentComponent.class }, value = ObservableListExtractor.ATTRIBUTES_OF_INSTANCES.class)
	@ReactorDependencies({ GSHeaderRow.class, GSContentRow.class })
	@ReactorDependencies(decorate = GSHeaderRow.class, value = { GSHeaderComponent.class, GSContentComponent.class })
	@ReactorDependencies(decorate = GSContentRow.class, value = { GSHeaderComponent.class, GSContentComponent.class })
	@ReactorDependencies(decorate = { GSHeaderRow.class, GSHeaderComponent.class }, value = GSValueComponents1.class)
	@ReactorDependencies(decorate = { GSHeaderRow.class, GSContentComponent.class }, value = GSValueComponents1.class)
	@ReactorDependencies(decorate = { GSContentRow.class, GSHeaderComponent.class }, value = GSValueComponents2.class)
	@ReactorDependencies(decorate = { GSContentRow.class, GSContentComponent.class }, value = GSHolders.class)
	public static class GSInstancesTable extends GSTable {

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
