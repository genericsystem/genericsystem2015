package org.genericsystem.gsadmin;

import javafx.beans.property.Property;
import javafx.beans.property.SimpleObjectProperty;
import javafx.beans.property.SimpleStringProperty;
import javafx.collections.FXCollections;
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

		GSVBox mainPanel = new GSVBox(scene, Group::getChildren).setPrefHeight(600);
		{
			GSHBox creationPanel = new GSHBox(mainPanel);
			{
				new GSTextField(creationPanel).bindTextProperty(GenericList::getName).setPrefWidth(350);
				new GSButton(creationPanel, "Create Todo", GenericList::create).setPrefWidth(200);
			}

			new GSHBox(mainPanel).forEach(GenericList::getGenerics).include(TypeWrapper::init);

			GSHBox commandPanel = new GSHBox(mainPanel).setSpacing(10);
			{
				new GSButton(commandPanel, "Flush").setAction(GenericList::flush);
				new GSButton(commandPanel, "Clear").setAction(GenericList::clear);
				new GSButton(commandPanel, "Mount").setAction(GenericList::mount);
				new GSButton(commandPanel, "Unmount").setAction(GenericList::unmount);
			}

			new GSHBox(mainPanel).select(GenericList::getSelection).include(InstanceWrapper::init);
		}
	}

	/***********************************************************************************/
	Transformation<AttributeWrapper, Generic> attributes;

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

		dependenciesObservableList = FXCollections.observableArrayList(engine.getSubInstances().toList());
		genericWrapperList = new Transformation<TypeWrapper, Generic>(dependenciesObservableList, generic -> new TypeWrapper(generic));

		ObservableList<Generic> attEngine = FXCollections.observableArrayList();
		attEngine.add(engine);
		attEngine.addAll(engine.getAttributes().toList());
		attributes = new Transformation<AttributeWrapper, Generic>(attEngine, att -> new AttributeWrapper(att, engine));
		System.out.println(attributes.size());

		attributes.forEach(e -> {
			e.attribute.getRelations(1).forEach(e2 -> System.out.println(e2.getValue()));
		});
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
	public ObservableList<AttributeWrapper> getAttributes() {
		return attributes;
	}

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
