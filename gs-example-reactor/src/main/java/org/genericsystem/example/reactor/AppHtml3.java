package org.genericsystem.example.reactor;

import org.genericsystem.carcolor.model.Car;
import org.genericsystem.carcolor.model.CarColor;
import org.genericsystem.carcolor.model.CarColor2;
import org.genericsystem.carcolor.model.Color;
import org.genericsystem.carcolor.model.Power;
import org.genericsystem.carcolor.model.UsedCar;
import org.genericsystem.example.reactor.AppHtml.ExampleReactorScript;
import org.genericsystem.reactor.annotations.DependsOnModel;
import org.genericsystem.reactor.annotations.RunScript;
import org.genericsystem.reactor.appserver.ApplicationServer;
import org.genericsystem.reactor.gs.GSApp;
import org.genericsystem.reactor.gs3.GSEditor.BooleanHolderAdderInput;
import org.genericsystem.reactor.gs3.GSEditor.BooleanHolderAdditionLink;
import org.genericsystem.reactor.gs3.GSEditor.BooleanHolderEditorInput;
import org.genericsystem.reactor.gs3.GSEditor.Checkbox;
import org.genericsystem.reactor.gs3.GSEditor.ComponentAdderSelect;
import org.genericsystem.reactor.gs3.GSEditor.DirectRelationComponentEditor;
import org.genericsystem.reactor.gs3.GSEditor.EditorTitleContent;
import org.genericsystem.reactor.gs3.GSEditor.HolderAdderInput;
import org.genericsystem.reactor.gs3.GSEditor.HolderAdditionLink;
import org.genericsystem.reactor.gs3.GSEditor.HolderEditorInput;
import org.genericsystem.reactor.gs3.GSEditor.HorizontalGSEditor;
import org.genericsystem.reactor.gs3.GSEditor.InstanceComponentName;
import org.genericsystem.reactor.gs3.GSEditor.InstanceNameEditor;
import org.genericsystem.reactor.gs3.GSEditor.InstanceType;
import org.genericsystem.reactor.gs3.GSEditor.InstanceTypeAttribute;
import org.genericsystem.reactor.gs3.GSEditor.RemovalLink;
import org.genericsystem.reactor.gs3.GSEditor.ReversedRelationDisplayer;
import org.genericsystem.reactor.gs3.Table.AttributeNameDisplayer;
import org.genericsystem.reactor.gs3.Table.BooleanDisplayer;
import org.genericsystem.reactor.gs3.Table.ComponentLabel;
import org.genericsystem.reactor.gs3.Table.ComponentNameDisplayer;
import org.genericsystem.reactor.gs3.Table.EmptyCell;
import org.genericsystem.reactor.gs3.Table.HorizontalTable;
import org.genericsystem.reactor.gs3.Table.RemoveButton;
import org.genericsystem.reactor.gs3.Table.RowNameDisplayer;
import org.genericsystem.reactor.gs3.Table.TitleContent;
import org.genericsystem.reactor.gs3.Table.TypeNameDisplayer;
import org.genericsystem.reactor.gs3.Table.ValueDisplayer;
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

		new TreeRootTagImpl(this, TitleContent.class, TypeNameDisplayer.class, AttributeNameDisplayer.class, ComponentNameDisplayer.class, RowNameDisplayer.class, ComponentLabel.class, EmptyCell.class, BooleanDisplayer.class, ValueDisplayer.class,
				RemoveButton.class).select(Car.class);
		new TreeRootTagImpl(this, TitleContent.class, TypeNameDisplayer.class, AttributeNameDisplayer.class, ComponentNameDisplayer.class, RowNameDisplayer.class, ComponentLabel.class, EmptyCell.class, BooleanDisplayer.class, ValueDisplayer.class,
				RemoveButton.class, HorizontalTable.class).select(Car.class);

		new TreeRootTagImpl(this, EditorTitleContent.class, HorizontalGSEditor.class, TypeNameDisplayer.class, AttributeNameDisplayer.class, ComponentNameDisplayer.class, InstanceComponentName.class, InstanceType.class, InstanceTypeAttribute.class,
				InstanceNameEditor.class, Checkbox.class, ReversedRelationDisplayer.class, DirectRelationComponentEditor.class, BooleanHolderEditorInput.class, HolderEditorInput.class, RemovalLink.class, BooleanHolderAdderInput.class,
				BooleanHolderAdditionLink.class, HolderAdderInput.class, HolderAdditionLink.class, ComponentAdderSelect.class).select__(context -> getSelectionProperty(context));
		new TreeRootTagImpl(this, EditorTitleContent.class, TypeNameDisplayer.class, AttributeNameDisplayer.class, ComponentNameDisplayer.class, InstanceType.class, InstanceComponentName.class, InstanceTypeAttribute.class, InstanceNameEditor.class,
				Checkbox.class, ReversedRelationDisplayer.class, DirectRelationComponentEditor.class, BooleanHolderEditorInput.class, HolderEditorInput.class, RemovalLink.class, BooleanHolderAdderInput.class, BooleanHolderAdditionLink.class,
				HolderAdderInput.class, HolderAdditionLink.class, ComponentAdderSelect.class).select__(context -> getSelectionProperty(context));

		new TreeRootTagImpl(this, TitleContent.class, TypeNameDisplayer.class, AttributeNameDisplayer.class, ComponentNameDisplayer.class, RowNameDisplayer.class, ComponentLabel.class, EmptyCell.class, BooleanDisplayer.class, ValueDisplayer.class,
				RemoveButton.class).select(Color.class);
	}
}
