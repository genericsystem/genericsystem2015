package org.genericsystem.todoApp;

import java.io.File;
import java.util.Objects;

import javafx.beans.binding.Bindings;
import javafx.beans.property.SimpleDoubleProperty;
import javafx.beans.property.SimpleStringProperty;
import javafx.beans.property.StringProperty;
import javafx.beans.value.ObservableValue;
import javafx.collections.ObservableList;
import javafx.scene.Node;
import javafx.scene.control.Button;
import javafx.scene.control.Label;
import javafx.scene.control.TextField;
import javafx.scene.layout.HBox;
import javafx.scene.layout.VBox;

import org.genericsystem.common.Generic;
import org.genericsystem.distributed.GSDeploymentOptions;
import org.genericsystem.distributed.cacheonclient.CocClientEngine;
import org.genericsystem.distributed.cacheonclient.CocServer;
import org.genericsystem.kernel.Statics;
import org.genericsystem.todoApp.binding.Binding;

public class GenericList {

	private static final String path = System.getenv("HOME") + "/test/ObservableListChain";

	private static CocServer server;
	private static CocClientEngine engine;

	private static ObservableList<Generic> dependenciesObservableList;
	Transformation<GenericWrapper, Generic> genericWrapperList;

	private StringProperty name = new SimpleStringProperty();
	private ObservableValue<String> createButtonTextProperty = new SimpleStringProperty("Create Generic");
	private ObservableValue<String> flushButtonTextProperty = new SimpleStringProperty("Flush");
	private ObservableValue<String> clearButtonTextProperty = new SimpleStringProperty("Clear");
	private ObservableValue<String> mountButtonTextProperty = new SimpleStringProperty("Mount");
	private ObservableValue<String> unmountButtonTextProperty = new SimpleStringProperty("Unmount");
	private ObservableValue<Number> height = new SimpleDoubleProperty(700);

	public GenericList() throws InterruptedException {

		// cleanDirectory();

		server = new CocServer(new GSDeploymentOptions(Statics.ENGINE_VALUE, Statics.DEFAULT_PORT, path));
		server.start();

		engine = new CocClientEngine(Statics.ENGINE_VALUE, null, Statics.DEFAULT_PORT);

		dependenciesObservableList = engine.getCurrentCache().getObservableDependencies(engine);

		genericWrapperList = new Transformation<GenericWrapper, Generic>(dependenciesObservableList, generic -> new GenericWrapper(generic));
	}

	public StringProperty getName() {
		return name;
	}

	public ObservableList<GenericWrapper> getGenerics() {
		return genericWrapperList;
	}

	public ObservableValue<String> getCreateButtonTextProperty() {
		return createButtonTextProperty;
	}

	public ObservableValue<String> getFlushButtonTextProperty() {
		return flushButtonTextProperty;
	}

	public ObservableValue<String> getClearButtonTextProperty() {
		return clearButtonTextProperty;
	}

	public ObservableValue<String> getMountButtonTextProperty() {
		return mountButtonTextProperty;
	}

	public ObservableValue<String> getUnmountButtonTextProperty() {
		return unmountButtonTextProperty;
	}

	public ObservableValue<Number> getHeight() {
		return height;
	}

	private void cleanDirectory() {
		File file = new File(path);
		if (file.exists())
			for (File f : file.listFiles()) {
				if (!".lock".equals(f.getName()))
					f.delete();
			}
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

	public void create() {
		engine.addInstance(name.getValue());
	}

	public void remove(GenericWrapper genericWrapper) {
		genericWrapper.remove();
	}

	protected static class GenericWrapper {

		private Generic generic;
		private StringProperty stringProperty = new SimpleStringProperty();
		private ObservableValue<String> removeButtonTextProperty = Bindings.concat("Remove : ", stringProperty);

		public GenericWrapper(Generic generic) {
			this.generic = generic;
			stringProperty.set(Objects.toString(this.generic.getValue()));
		}

		public void remove() {
			generic.remove();
		}

		public ObservableValue<String> getObservable() {
			return stringProperty;
		}

		public ObservableValue<String> getRemoveButtonTextProperty() {
			return removeButtonTextProperty;
		}
	}

	@SuppressWarnings("unused")
	public Node init() {
		Element mainVBox = new Element(null, VBox.class, Binding.bindProperty(VBox::prefHeightProperty, GenericList::getHeight));

		Element genericCreateHBox = new Element(mainVBox, HBox.class);
		Element genericsCreateLabel = new Element(genericCreateHBox, TextField.class, Binding.bindInputText(TextField::textProperty, GenericList::getName));
		Element genericsCreateButton = new Element(genericCreateHBox, Button.class, Binding.bindProperty(Button::textProperty, GenericList::getCreateButtonTextProperty), Binding.bindAction(Button::onActionProperty, GenericList::create));

		Element genericsHBox = new Element(mainVBox, HBox.class);
		Element genericsFlushButton = new Element(genericsHBox, Button.class, Binding.bindProperty(Button::textProperty, GenericList::getFlushButtonTextProperty), Binding.bindAction(Button::onActionProperty, GenericList::flush));
		Element genericsClearButton = new Element(genericsHBox, Button.class, Binding.bindProperty(Button::textProperty, GenericList::getClearButtonTextProperty), Binding.bindAction(Button::onActionProperty, GenericList::clear));
		Element genericsMountButton = new Element(genericsHBox, Button.class, Binding.bindProperty(Button::textProperty, GenericList::getMountButtonTextProperty), Binding.bindAction(Button::onActionProperty, GenericList::mount));
		Element genericsUnmountButton = new Element(genericsHBox, Button.class, Binding.bindProperty(Button::textProperty, GenericList::getUnmountButtonTextProperty), Binding.bindAction(Button::onActionProperty, GenericList::unmount));

		Element genericVBox = new Element(mainVBox, VBox.class, Binding.forEach(GenericList::getGenerics));
		Element genericHBox = new Element(genericVBox, HBox.class);
		Element genericLabel = new Element(genericHBox, Label.class, Binding.bindProperty(Label::textProperty, GenericWrapper::getObservable));
		Element genericRemoveButton = new Element(genericHBox, Button.class, Binding.bindAction(Button::onActionProperty, GenericList::remove, GenericWrapper.class), Binding.bindProperty(Button::textProperty, GenericWrapper::getRemoveButtonTextProperty));

		return (Node) mainVBox.apply(this).getNode();
	}
}
