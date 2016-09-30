package org.genericsystem.reactor.example;

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
import org.genericsystem.reactor.az3.GSComposite.Content;
import org.genericsystem.reactor.az3.GSComposite.GSTable;
import org.genericsystem.reactor.az3.GSComposite.GSTable.ContentRow;
import org.genericsystem.reactor.az3.GSComposite.GSTable.HeaderRow;
import org.genericsystem.reactor.az3.GSComposite.Header;
import org.genericsystem.reactor.az3.GSComposite.Header.HeaderLabel;
import org.genericsystem.reactor.example.AppHtml.ExampleReactorScript;
import org.genericsystem.reactor.example.AppHtml5.GSInstancesTable;
import org.genericsystem.reactor.model.ObservableListExtractor;
import org.genericsystem.reactor.model.ObservableValueSelector;
import org.genericsystem.reactor.modelproperties.SelectionDefaults;

@DependsOnModel({ Car.class, Power.class, UsedCar.class, Color.class, CarColor.class, CarColor2.class })
@RunScript(ExampleReactorScript.class)
@ReactorDependencies({ GSInstancesTable.class })
@FlexWrap("wrap")
@Flex("1 1 0%")
public class AppHtml5 extends GSApp implements SelectionDefaults {

	public static void main(String[] mainArgs) {
		ApplicationServer.startSimpleGenericApp(mainArgs, AppHtml5.class, "/example-reactor");
	}

	@BackgroundColor("Brown")
	@GenericValueBackgroundColor(path = { GSValueComponents.class, Content.class }, value = "#e5ed00")
	@ReactorDependencies(value = GSValueComponents.class)
	@ReactorDependencies(path = { GSValueComponents.class, Header.class }, value = { HeaderLabel.class, GSCheckBoxDisplayer.class })
	@ForEach(path = GSValueComponents.class, value = ObservableListExtractor.HOLDERS.class)
	@ForEach(path = { GSValueComponents.class, Content.class }, value = ObservableListExtractor.OTHER_COMPONENTS_2.class)
	@Select(path = { GSValueComponents.class, Header.class, HeaderLabel.class }, value = ObservableValueSelector.LABEL_DISPLAYER.class)
	@Select(path = { GSValueComponents.class, Header.class, GSCheckBoxDisplayer.class }, value = ObservableValueSelector.CHECK_BOX_DISPLAYER.class)
	public static class GSHolders extends GSComposite {

	}

	// @DirectSelect(Power.class)
	@BackgroundColor(path = Content.class, value = "Yellow")
	@BackgroundColor(path = Header.class, value = "Yellow")
	@JustifyContent(path = Header.class, value = "center")
	@AlignItems(path = Header.class, value = "center")
	@JustifyContent(path = Content.class, value = "center")
	@AlignItems(path = Content.class, value = "center")
	@FlexDirectionStyle(FlexDirection.ROW)
	@ForEach(path = Content.class, value = ObservableListExtractor.OTHER_COMPONENTS_1.class)
	@Select(path = Header.class, value = ObservableValueSelector.STRICT_ATTRIBUTE_SELECTOR.class)
	@ReactorDependencies({ Header.class, Content.class })
	@MarginRight(path = Header.class, value = "1px")
	@MarginBottom(path = Header.class, value = "1px")
	@MarginRight(path = Content.class, value = "1px")
	@MarginBottom(path = Content.class, value = "1px")
	public static class GSValueComponents extends GSComposite {

	}

	@DirectSelect(Car.class)
	@Style(name = "margin", value = "4px")
	@Styles.Color(path = HeaderRow.class, value = "white")
	@BackgroundColor(path = { HeaderRow.class, GSValueComponents.class, Content.class }, value = "Purple")
	@BackgroundColor(path = { HeaderRow.class, GSValueComponents.class, Header.class }, value = "Purple")
	@BackgroundColor(path = { HeaderRow.class, Content.class, GSValueComponents.class, Content.class }, value = "Purple")
	@BackgroundColor(path = { HeaderRow.class, Content.class, GSValueComponents.class, Header.class }, value = "Purple")
	@AlignItems(path = { ContentRow.class, GSValueComponents.class, Header.class }, value = "flex-start")
	@ReactorDependencies({ HeaderRow.class, ContentRow.class })
	@ReactorDependencies(path = HeaderRow.class, value = { GSValueComponents.class, Content.class })
	@ReactorDependencies(path = ContentRow.class, value = { GSValueComponents.class, GSHolders.class })
	@ReactorDependencies(path = { HeaderRow.class, Content.class }, value = GSValueComponents.class)
	@ForEach(path = { HeaderRow.class, Content.class }, value = ObservableListExtractor.ATTRIBUTES_OF_TYPE.class)
	@ForEach(path = ContentRow.class, value = ObservableListExtractor.SUBINSTANCES.class)
	@ForEach(path = { ContentRow.class, GSHolders.class }, value = ObservableListExtractor.ATTRIBUTES_OF_INSTANCES.class)
	@ForEach(path = { ContentRow.class, GSValueComponents.class, Content.class }, value = ObservableListExtractor.OTHER_COMPONENTS_2.class)
	public static class GSInstancesTable extends GSTable {

	}
}
