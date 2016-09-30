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
import org.genericsystem.reactor.annotations.Styles.AlignItems;
import org.genericsystem.reactor.annotations.Styles.BackgroundColor;
import org.genericsystem.reactor.annotations.Styles.Flex;
import org.genericsystem.reactor.annotations.Styles.FlexDirectionStyle;
import org.genericsystem.reactor.annotations.Styles.FlexWrap;
import org.genericsystem.reactor.annotations.Styles.GenericValueBackgroundColor;
import org.genericsystem.reactor.annotations.Styles.JustifyContent;
import org.genericsystem.reactor.annotations.Styles.MarginBottom;
import org.genericsystem.reactor.annotations.Styles.MarginRight;
import org.genericsystem.reactor.annotations.Styles.Style;
import org.genericsystem.reactor.appserver.ApplicationServer;
import org.genericsystem.reactor.az.FlexDirection;
import org.genericsystem.reactor.az.GSApp;
import org.genericsystem.reactor.az.GSCheckBoxWithValue.GSCheckBoxDisplayer;
import org.genericsystem.reactor.az3.GSComposite;
import org.genericsystem.reactor.az3.GSComposite.GSContentComponent;
import org.genericsystem.reactor.az3.GSComposite.GSHeaderComponent;
import org.genericsystem.reactor.az3.GSComposite.GSHeaderComponent.GSHeaderComponentLabel;
import org.genericsystem.reactor.az3.GSComposite.GSTable;
import org.genericsystem.reactor.az3.GSComposite.GSTable.GSContentRow;
import org.genericsystem.reactor.az3.GSComposite.GSTable.GSHeaderRow;
import org.genericsystem.reactor.example.AppHtml.ExampleReactorScript;
import org.genericsystem.reactor.example.AppHtml5.AudiTT;
import org.genericsystem.reactor.example.AppHtml5.AudiTT.AudiTTGreen;
import org.genericsystem.reactor.example.AppHtml5.AudiTT.AudiTTPower;
import org.genericsystem.reactor.example.AppHtml5.AudiTT.Green;
import org.genericsystem.reactor.example.AppHtml5.GSInstancesTable;
import org.genericsystem.reactor.example.AppHtml5.Unit;
import org.genericsystem.reactor.model.ObservableListExtractor;
import org.genericsystem.reactor.model.ObservableValueSelector;
import org.genericsystem.reactor.modelproperties.SelectionDefaults;

@DependsOnModel({ Car.class, Power.class, UsedCar.class, Color.class, CarColor.class, CarColor2.class, AudiTT.class, Green.class, AudiTTGreen.class, AudiTTPower.class, Unit.class })
@RunScript(ExampleReactorScript.class)
@ReactorDependencies({ GSInstancesTable.class })
@FlexWrap("wrap")
@Flex("1 1 0%")
public class AppHtml5 extends GSApp implements SelectionDefaults {

	public static void main(String[] mainArgs) {
		ApplicationServer.startSimpleGenericApp(mainArgs, AppHtml5.class, "/example-reactor");
	}

	@BackgroundColor("Brown")
	@GenericValueBackgroundColor(decorate = { GSValueComponents.class, GSContentComponent.class })
	@ReactorDependencies(value = GSValueComponents.class)
	@ReactorDependencies(decorate = { GSValueComponents.class, GSHeaderComponent.class }, value = { GSHeaderComponentLabel.class, GSCheckBoxDisplayer.class })
	@ForEach(decorate = GSValueComponents.class, value = ObservableListExtractor.HOLDERS.class)
	@ForEach(decorate = { GSValueComponents.class, GSContentComponent.class }, value = ObservableListExtractor.OTHER_COMPONENTS_2.class)
	@Select(decorate = { GSValueComponents.class, GSHeaderComponent.class, GSHeaderComponentLabel.class }, value = ObservableValueSelector.LABEL_DISPLAYER.class)
	@Select(decorate = { GSValueComponents.class, GSHeaderComponent.class, GSCheckBoxDisplayer.class }, value = ObservableValueSelector.CHECK_BOX_DISPLAYER.class)
	public static class GSHolders extends GSComposite {

	}

	// @DirectSelect(Power.class)
	@BackgroundColor(decorate = GSContentComponent.class, value = "Yellow")
	@BackgroundColor(decorate = GSHeaderComponent.class, value = "Yellow")
	@JustifyContent(decorate = GSHeaderComponent.class, value = "center")
	@AlignItems(decorate = GSHeaderComponent.class, value = "center")
	@JustifyContent(decorate = GSContentComponent.class, value = "center")
	@AlignItems(decorate = GSContentComponent.class, value = "center")
	@FlexDirectionStyle(FlexDirection.ROW)
	@ForEach(decorate = GSContentComponent.class, value = ObservableListExtractor.OTHER_COMPONENTS_1.class)
	@Select(decorate = GSHeaderComponent.class, value = ObservableValueSelector.STRICT_ATTRIBUTE_SELECTOR.class)
	@ReactorDependencies({ GSHeaderComponent.class, GSContentComponent.class })
	@MarginRight(decorate = GSHeaderComponent.class, value = "1px")
	@MarginBottom(decorate = GSHeaderComponent.class, value = "1px")
	@MarginRight(decorate = GSContentComponent.class, value = "1px")
	@MarginBottom(decorate = GSContentComponent.class, value = "1px")
	public static class GSValueComponents extends GSComposite {

	}

	@DirectSelect(Car.class)
	@Style(name = "margin", value = "4px")
	@Styles.Color(decorate = GSHeaderRow.class, value = "white")
	@BackgroundColor(decorate = { GSHeaderRow.class, GSValueComponents.class, GSContentComponent.class }, value = "Purple")
	@BackgroundColor(decorate = { GSHeaderRow.class, GSValueComponents.class, GSHeaderComponent.class }, value = "Purple")
	@BackgroundColor(decorate = { GSHeaderRow.class, GSContentComponent.class, GSValueComponents.class, GSContentComponent.class }, value = "Purple")
	@BackgroundColor(decorate = { GSHeaderRow.class, GSContentComponent.class, GSValueComponents.class, GSHeaderComponent.class }, value = "Purple")
	@AlignItems(decorate = { GSContentRow.class, GSValueComponents.class, GSHeaderComponent.class }, value = "flex-start")
	@ReactorDependencies({ GSHeaderRow.class, GSContentRow.class })
	@ReactorDependencies(decorate = GSHeaderRow.class, value = { GSValueComponents.class, GSContentComponent.class })
	@ReactorDependencies(decorate = GSContentRow.class, value = { GSValueComponents.class, GSHolders.class })
	@ReactorDependencies(decorate = { GSHeaderRow.class, GSContentComponent.class }, value = GSValueComponents.class)
	@ForEach(decorate = { GSHeaderRow.class, GSContentComponent.class }, value = ObservableListExtractor.ATTRIBUTES_OF_TYPE.class)
	@ForEach(decorate = GSContentRow.class, value = ObservableListExtractor.SUBINSTANCES.class)
	@ForEach(decorate = { GSContentRow.class, GSHolders.class }, value = ObservableListExtractor.ATTRIBUTES_OF_INSTANCES.class)
	@ForEach(decorate = { GSContentRow.class, GSValueComponents.class, GSContentComponent.class }, value = ObservableListExtractor.OTHER_COMPONENTS_2.class)
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
