package org.genericsystem.example.reactor;

import org.genericsystem.carcolor.model.Car;
import org.genericsystem.carcolor.model.CarColor;
import org.genericsystem.carcolor.model.CarColor2;
import org.genericsystem.carcolor.model.Color;
import org.genericsystem.carcolor.model.Power;
import org.genericsystem.carcolor.model.UsedCar;
import org.genericsystem.example.reactor.AppHtml.ExampleReactorScript;
import org.genericsystem.reactor.Tag;
import org.genericsystem.reactor.annotations.DependsOnModel;
import org.genericsystem.reactor.annotations.RunScript;
import org.genericsystem.reactor.appserver.ApplicationServer;
import org.genericsystem.reactor.gs.FlexDirection;
import org.genericsystem.reactor.gs.GSApp;
import org.genericsystem.reactor.gs3.Table.BooleanDisplayer;
import org.genericsystem.reactor.gs3.Table.BooleanValueSubCell;
import org.genericsystem.reactor.gs3.Table.Cell;
import org.genericsystem.reactor.gs3.Table.ComponentLabel;
import org.genericsystem.reactor.gs3.Table.ComponentSubCell;
import org.genericsystem.reactor.gs3.Table.Row;
import org.genericsystem.reactor.gs3.Table.RowName;
import org.genericsystem.reactor.gs3.Table.RowNameDisplayer;
import org.genericsystem.reactor.gs3.Table.SubCell;
import org.genericsystem.reactor.gs3.Table.SubCell2;
import org.genericsystem.reactor.gs3.Table.ValueDisplayer;
import org.genericsystem.reactor.gs3.Table.ValueSubCell;
import org.genericsystem.reactor.gstag.HtmlHyperLink;
import org.genericsystem.reactor.model.ObservableListExtractor;
import org.genericsystem.reactor.model.StringExtractor;
import org.genericsystem.reactor.modelproperties.SelectionDefaults;

@DependsOnModel({ Car.class, Power.class, UsedCar.class, Color.class, CarColor.class, CarColor2.class })
@RunScript(ExampleReactorScript.class)
public class AppHtml3 extends GSApp implements SelectionDefaults {

	public static void main(String[] mainArgs) {
		ApplicationServer.startSimpleGenericApp(mainArgs, AppHtml3.class, "/example-reactor");
	}

	public AppHtml3() {
		addStyle("justify-content", "center");
		createSelectionProperty();

		TreeRootTag table1 = new TreeRootTagImpl(this, RowNameDisplayer.class, ComponentLabel.class, BooleanDisplayer.class, ValueDisplayer.class, SubCell2.class);
		initTable(table1);
		table1.select(Car.class);

		TreeRootTag table2 = new TreeRootTagImpl(this, RowNameDisplayer.class, ComponentLabel.class, BooleanDisplayer.class, ValueDisplayer.class);
		initTable(table2);
		table2.select(Color.class);
	}

	public void initTable(TreeRootTag table) {
		Tag row = table.find(Row.class);
		row.forEach(ObservableListExtractor.SUBINSTANCES);
		row.addStyle("flex", "1");
		row.addStyle("flex-direction", FlexDirection.ROW.toString());

		Tag rowTitle = table.find(RowName.class);
		rowTitle.addStyle("flex", "1");
		rowTitle.addStyle("margin-right", "1px");
		rowTitle.addStyle("margin-bottom", "1px");
		rowTitle.addStyle("overflow", "hidden");
		rowTitle.addPrefixBinding(modelContext -> rowTitle.getDomNodeStyles(modelContext).put("background-color",
				"Color".equals(StringExtractor.SIMPLE_CLASS_EXTRACTOR.apply(modelContext.getGeneric().getMeta())) ? rowTitle.getGenericStringProperty(modelContext).getValue() : "#3393FF"));

		HtmlHyperLink rowTitleLink = (HtmlHyperLink) table.find(RowNameDisplayer.class);
		rowTitleLink.addStyle("color", "White");
		rowTitleLink.bindText();
		rowTitleLink.bindAction(model -> rowTitleLink.getSelectionProperty(model).setValue(model));

		Tag cell = table.find(Cell.class);
		cell.forEach(ObservableListExtractor.ATTRIBUTES_OF_INSTANCES);
		cell.addStyle("flex", "1");
		cell.addStyle("flex-direction", FlexDirection.COLUMN.toString());
		cell.addStyle("margin-bottom", "1px");
		cell.addStyle("margin-right", "1px");

		Tag subcell = table.find(SubCell.class);
		subcell.forEach(ObservableListExtractor.HOLDERS);
		subcell.addStyle("flex", "1");
		subcell.addStyle("flex-direction", FlexDirection.ROW.toString());

		Tag componentSubCell = table.find(ComponentSubCell.class);
		componentSubCell.forEach(gs -> ObservableListExtractor.COMPONENTS.apply(gs).filtered(g -> !g.equals(gs[2])));
		componentSubCell.addStyle("flex", "1");
		componentSubCell.addStyle("justify-content", "center");
		componentSubCell.addStyle("align-items", "center");
		componentSubCell.addStyle("margin-bottom", "1px");
		componentSubCell.addStyle("margin-right", "1px");
		componentSubCell.addPrefixBinding(modelContext -> componentSubCell.getDomNodeStyles(modelContext).put("background-color",
				"Color".equals(StringExtractor.SIMPLE_CLASS_EXTRACTOR.apply(modelContext.getGeneric().getMeta())) ? componentSubCell.getGenericStringProperty(modelContext).getValue() : "#e5ed00"));

		Tag booleanValueSubCell = table.find(BooleanValueSubCell.class);
		booleanValueSubCell.select(gs -> gs[1].getComponents().size() == 1 && Boolean.class.equals(gs[0].getInstanceValueClassConstraint()) ? gs[0] : null);
		booleanValueSubCell.addStyle("justify-content", "center");
		booleanValueSubCell.addStyle("align-items", "center");
		booleanValueSubCell.addStyle("flex", "1");
		booleanValueSubCell.addStyle("background-color", "#e5ed00");

		Tag valueSubCell = table.find(ValueSubCell.class);
		valueSubCell.select(gs -> gs[1].getComponents().size() == 1 && !Boolean.class.equals(gs[0].getInstanceValueClassConstraint()) ? gs[0] : null);
		valueSubCell.addStyle("justify-content", "center");
		valueSubCell.addStyle("align-items", "center");
		valueSubCell.addStyle("flex", "1");
		valueSubCell.addStyle("background-color", "#e5ed00");

		table.find(ComponentSubCell.class).bindText();
		table.find(ValueDisplayer.class).bindText();
		table.find(BooleanDisplayer.class);
	}
}
