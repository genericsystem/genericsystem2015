package org.genericsystem.example.reactor;

import org.genericsystem.carcolor.model.Car;
import org.genericsystem.carcolor.model.CarColor;
import org.genericsystem.carcolor.model.CarColor2;
import org.genericsystem.carcolor.model.Color;
import org.genericsystem.carcolor.model.Power;
import org.genericsystem.carcolor.model.UsedCar;
import org.genericsystem.example.reactor.AppHtml.ExampleReactorScript;
import org.genericsystem.reactor.TagImpl;
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
import org.genericsystem.reactor.gs3.GSEditor.EditorTitle.EditorTitleContent;
import org.genericsystem.reactor.gs3.GSEditor.HolderAdderInput;
import org.genericsystem.reactor.gs3.GSEditor.HolderAdditionLink;
import org.genericsystem.reactor.gs3.GSEditor.HolderEditorInput;
import org.genericsystem.reactor.gs3.GSEditor.InstanceComponentName;
import org.genericsystem.reactor.gs3.GSEditor.InstanceNameEditor;
import org.genericsystem.reactor.gs3.GSEditor.InstanceType;
import org.genericsystem.reactor.gs3.GSEditor.InstanceTypeAttribute;
import org.genericsystem.reactor.gs3.GSEditor.RemovalLink;
import org.genericsystem.reactor.gs3.GSEditor.ReversedRelationDisplayer;
import org.genericsystem.reactor.gs3.GSTable;
import org.genericsystem.reactor.gs3.GSTable.TitleRow.TypeAttribute.AttributeName.AttributeNameDisplayer;
import org.genericsystem.reactor.gs3.GSTable.TitleRow.TypeAttribute.RelationName.ComponentName.ComponentNameDisplayer;
import org.genericsystem.reactor.gs3.GSTable.TitleRow.TypeName.TypeNameDisplayer;
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

		// Class<? extends TagImpl>[] tableClasses = new Class[] { TableTitleContent.class, TypeNameDisplayer.class, AttributeNameDisplayer.class, ComponentNameDisplayer.class, InstanceNameBuilderInput.class, HolderBuilderInput.class,
		// BooleanHolderBuilderInput.class, ComponentBuilderSelect.class, AddButton.class, RowNameDisplayer.class, ComponentLabel.class, EmptyCell.class, BooleanDisplayer.class, ValueDisplayer.class, RemoveButton.class };
		Class<? extends TagImpl>[] editorClasses = new Class[] { EditorTitleContent.class, TypeNameDisplayer.class, AttributeNameDisplayer.class, ComponentNameDisplayer.class, InstanceComponentName.class, InstanceType.class, InstanceTypeAttribute.class,
				InstanceNameEditor.class, Checkbox.class, ReversedRelationDisplayer.class, DirectRelationComponentEditor.class, BooleanHolderEditorInput.class, HolderEditorInput.class, RemovalLink.class, BooleanHolderAdderInput.class,
				BooleanHolderAdditionLink.class, HolderAdderInput.class, HolderAdditionLink.class, ComponentAdderSelect.class };

		new GSTable(this).select(Car.class);
		// new RootTagImpl(this, Stream.concat(Stream.of(tableClasses), Stream.of(HorizontalTable.class)).toArray(Class[]::new)).select(Car.class);

		// new RootTagImpl(this, Stream.concat(Stream.of(editorClasses), Stream.of(HorizontalGSEditor.class)).toArray(Class[]::new)).select__(context -> getSelectionProperty(context));
		// new RootTagImpl(this, editorClasses).select__(context -> getSelectionProperty(context));

		new GSTable(this).select(Color.class);
	}
}
