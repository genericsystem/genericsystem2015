package org.genericsystem.gsadmin;

import javafx.beans.property.Property;
import javafx.beans.property.SimpleObjectProperty;
import javafx.beans.property.SimpleStringProperty;
import javafx.collections.ObservableList;
import javafx.scene.Group;

import org.genericsystem.admin.model.Car;
import org.genericsystem.admin.model.CarColor;
import org.genericsystem.admin.model.Color;
import org.genericsystem.admin.model.Color.Red;
import org.genericsystem.admin.model.Color.Yellow;
import org.genericsystem.admin.model.Power;
import org.genericsystem.common.Generic;
import org.genericsystem.distributed.GSDeploymentOptions;
import org.genericsystem.distributed.cacheonclient.CocClientEngine;
import org.genericsystem.distributed.cacheonclient.CocServer;
import org.genericsystem.kernel.Statics;
import org.genericsystem.ui.Element;
import org.genericsystem.ui.components.GSButton;
import org.genericsystem.ui.components.GSHBox;
import org.genericsystem.ui.components.GSLabel;
import org.genericsystem.ui.components.GSTextField;
import org.genericsystem.ui.components.GSVBox;
import org.genericsystem.ui.utils.Transformation;

public class GenericList {

	private final String path = System.getenv("HOME") + "/test/ObservableListChain";
	private CocServer server;
	private CocClientEngine engine;
	private Transformation<TypeWrapper, Generic> genericWrapperList;
	private ObservableList<Generic> dependenciesObservableList;
	private Property<String> name = new SimpleStringProperty();
	private Property<TypeWrapper> selection = new SimpleObjectProperty<>();

	/********************************************************************************/
	public static void init(Element<Group> scene) {

		GSVBox vbox = new GSVBox(scene, Group::getChildren).setPrefHeight(600);
		{
			GSHBox hboxCreate = new GSHBox(vbox);
			{
				new GSTextField(hboxCreate).bindTextProperty(GenericList::getName).setPrefWidth(350);
				new GSButton(hboxCreate, "Create Todo", GenericList::create).setPrefWidth(200);
			}
			new GSHBox(vbox).forEach(GenericList::getGenerics).include(TypeWrapper::init);

			GSHBox hboxCommand = new GSHBox(vbox).setSpacing(10);
			{
				new GSButton(hboxCommand, "Flush").setAction(GenericList::flush);
				new GSButton(hboxCommand, "Clear").setAction(GenericList::clear);
				new GSButton(hboxCommand, "Mount").setAction(GenericList::mount);
				new GSButton(hboxCommand, "Unmount").setAction(GenericList::unmount);
			}

			GSHBox selectionContext = new GSHBox(vbox).select(GenericList::getSelection).include(InstanceWrapper::init);

			GSVBox selectionContext2 = new GSVBox(vbox).select(GenericList::getSelection);

			GSHBox box = new GSHBox(selectionContext2).forEach(TypeWrapper::getInstanceWrapperList).setSpacing(100);
			{
				new GSLabel(box, InstanceWrapper::getObservable).setPrefWidth(80);
				GSVBox vb = new GSVBox(box).setSpacing(0).forEach(InstanceWrapper::getAttributeObservableList).setPrefWidth(80);
				{
					GSVBox vbHolder = new GSVBox(vb).setSpacing(0).forEach(AttributeWrapper::getHoldersObservableList);
					{
						new GSLabel(vbHolder, HolderWrapper::getObservable);
					}
				}
			}
		}
	}

	/***********************************************************************************/
	public GenericList() throws InterruptedException {
		server = new CocServer(new GSDeploymentOptions(Statics.ENGINE_VALUE, 8083, "test").addClasses(Car.class, Power.class, CarColor.class, Color.class));
		server.start();
		engine = new CocClientEngine(Statics.ENGINE_VALUE, null, 8083, Car.class, Power.class, CarColor.class, Color.class);

		Generic type = engine.find(Car.class);
		Generic base = type.setInstance("myBmw");
		assert base.isAlive();
		type.setInstance("myAudi");
		type.setInstance("myMercedes");

		Generic attribute = engine.find(Power.class);
		Generic relation = engine.find(CarColor.class);
		base.setHolder(attribute, 333);
		base.setLink(relation, "myBmwRed", engine.find(Red.class));
		base.setLink(relation, "myBmwYellow", engine.find(Yellow.class));
		Generic base2 = type.setInstance("myMercedes");
		base2.setHolder(attribute, 333);
		base2.setLink(relation, "myMercedesYellow", engine.find(Yellow.class));
		engine.getCurrentCache().flush();

		dependenciesObservableList = engine.getCurrentCache().getObservableDependencies(engine);
		genericWrapperList = new Transformation<TypeWrapper, Generic>(dependenciesObservableList, generic -> new TypeWrapper(generic));
	}

	/*********************************************************************************/
	public void create() {
		engine.addInstance(name.getValue());
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

	public void remove(TypeWrapper genWrapper) {
		genWrapper.remove();
	}

	/************************************************************************************/
	public Property getName() {
		return name;
	}

	public ObservableList<TypeWrapper> getGenerics() {
		return genericWrapperList;
	}

	public Property<TypeWrapper> getSelection() {
		return selection;
	}
}
