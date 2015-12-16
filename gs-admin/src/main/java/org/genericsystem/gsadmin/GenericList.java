package org.genericsystem.gsadmin;

import java.util.function.Function;

import javafx.beans.property.Property;
import javafx.beans.property.SimpleStringProperty;
import javafx.collections.ObservableList;
import javafx.scene.Group;

import org.genericsystem.common.Generic;
import org.genericsystem.distributed.GSDeploymentOptions;
import org.genericsystem.distributed.cacheonclient.CocClientEngine;
import org.genericsystem.distributed.cacheonclient.CocServer;
import org.genericsystem.kernel.Statics;
import org.genericsystem.ui.Element;
import org.genericsystem.ui.components.GSButton;
import org.genericsystem.ui.components.GSHBox;
import org.genericsystem.ui.components.GSTableButtonColumn;
import org.genericsystem.ui.components.GSTableColumn;
import org.genericsystem.ui.components.GSTableView;
import org.genericsystem.ui.components.GSTextField;
import org.genericsystem.ui.components.GSVBox;
import org.genericsystem.ui.utils.Transformation;

public class GenericList {

	private final String path = System.getenv("HOME") + "/test/ObservableListChain";
	private CocServer server;
	private CocClientEngine engine;
	private Transformation<GenericWrapper, Generic> genericWrapperList;
	private ObservableList<Generic> dependenciesObservableList;
	private Property<String> name = new SimpleStringProperty();

	/********************************************************************************/
	public static void init(Element<Group> scene) {

		GSVBox vbox = new GSVBox(scene, Group::getChildren).setPrefHeight(600);
		{
			GSHBox hbox = new GSHBox(vbox);
			{
				new GSTextField(hbox).bindTextProperty(GenericList::getName).setPrefWidth(250);
				new GSButton(hbox, "Create Todo", GenericList::create).setPrefWidth(100);
			}
			new GSHBox(vbox).forEach(GenericList::getGenerics,/* Todo::getParentProperty, */GenericWrapper::init);
		}
	}

	/********************************************************************************/
	public static void initTable(Element<Group> scene) {
		GSVBox vbox = new GSVBox(scene, Group::getChildren).setPrefHeight(500);
		{
			GSHBox hbox = new GSHBox(vbox);
			{
				new GSTextField(hbox).bindTextProperty(GenericList::getName).setPrefWidth(150);
				new GSButton(hbox, "Create Todo", GenericList::create).setPrefWidth(100);

			}

			GSTableView tableView = new GSTableView(vbox);
			tableView.setObservableListItems(GenericList::getGenerics);
			{
				Function<GenericWrapper, String> converter = genericWrapper -> genericWrapper.getObservable().getValue();
				new GSTableColumn<GenericWrapper>(tableView, "Generic", converter).setPrefWidth(150);
				new GSTableButtonColumn<>(tableView, "Delete", converter, GenericList::remove).setPrefWidth(150);
			}
		}
	}

	/***********************************************************************************/

	public GenericList() throws InterruptedException {
		server = new CocServer(new GSDeploymentOptions(Statics.ENGINE_VALUE, Statics.DEFAULT_PORT, path));
		server.start();
		engine = new CocClientEngine(Statics.ENGINE_VALUE, null, Statics.DEFAULT_PORT);
		dependenciesObservableList = engine.getCurrentCache().getObservableDependencies(engine);
		genericWrapperList = new Transformation<GenericWrapper, Generic>(dependenciesObservableList, generic -> new GenericWrapper(generic));
	}

	/*********************************************************************************/
	public void create() {
		new GenericWrapper(engine.addInstance(name.getValue()));
	}

	public void flush() {
		engine.getCurrentCache().flush();
	}

	public void clear() {
		engine.getCurrentCache().clear();

	}

	public void mount() {
		engine.getCurrentCache().mount();
	}

	public void unmount() {
		engine.getCurrentCache().unmount();
	}

	public void remove(GenericWrapper genWrapper) {
		genWrapper.remove();
	}

	/************************************************************************************/
	public Property getName() {
		return name;
	}

	public ObservableList<GenericWrapper> getGenerics() {
		return genericWrapperList;
	}
}
