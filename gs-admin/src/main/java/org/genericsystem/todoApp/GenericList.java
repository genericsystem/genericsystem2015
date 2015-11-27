package org.genericsystem.todoApp;

import java.io.File;
import java.util.Objects;
import java.util.stream.Collectors;

import javafx.beans.property.SimpleStringProperty;
import javafx.beans.property.StringProperty;
import javafx.collections.FXCollections;
import javafx.collections.ObservableList;
import javafx.scene.Node;
import javafx.scene.control.Button;
import javafx.scene.control.Label;
import javafx.scene.control.TextField;
import javafx.scene.layout.VBox;

import org.genericsystem.common.Generic;
import org.genericsystem.distributed.GSDeploymentOptions;
import org.genericsystem.distributed.cacheonclient.CocClientEngine;
import org.genericsystem.distributed.cacheonclient.CocServer;
import org.genericsystem.kernel.Statics;
import org.genericsystem.todoApp.binding.Binder.ClickBinder;
import org.genericsystem.todoApp.binding.Binder.ForeachBinder;
import org.genericsystem.todoApp.binding.Binder.LabelBinder;
import org.genericsystem.todoApp.binding.Binder.TextFieldBinder;
import org.genericsystem.todoApp.binding.Binding;

public class GenericList {

	private static final String path = System.getenv("HOME") + "/test/ObservableListChain";

	private static CocServer server;
	private static CocClientEngine engine;
	private static ObservableList<Generic> dependenciesObservableList;

	public StringProperty name = new SimpleStringProperty();

	private ObservableList<GenericWrapper> genericList;

	private void cleanDirectory() {
		File file = new File(path);
		if (file.exists())
			for (File f : file.listFiles()) {
				if (!".lock".equals(f.getName()))
					f.delete();
			}
	}

	public GenericList() throws InterruptedException {

		// cleanDirectory();

		server = new CocServer(new GSDeploymentOptions(Statics.ENGINE_VALUE, Statics.DEFAULT_PORT, path));
		server.start();

		engine = new CocClientEngine(Statics.ENGINE_VALUE, null, Statics.DEFAULT_PORT);

		dependenciesObservableList = engine.getCurrentCache().getDependenciesObservableList(engine);

		Thread.sleep(1000);
		System.out.println(dependenciesObservableList);

		genericList = FXCollections.observableArrayList();
		genericList.addAll(dependenciesObservableList.stream().map(dep -> new GenericWrapper(dep)).collect(Collectors.toList()));
		System.out.println(genericList);
	}

	public void create() {
		engine.addInstance(name.getValue());
		genericList.clear();
		genericList.addAll(dependenciesObservableList.stream().map(dep -> new GenericWrapper(dep)).collect(Collectors.toList()));
	}

	public void remove(GenericWrapper genericWrapper) {
		genericWrapper.remove();
		genericList.clear();
		genericList.addAll(dependenciesObservableList.stream().map(dep -> new GenericWrapper(dep)).collect(Collectors.toList()));

	}

	public ObservableList<GenericWrapper> getGenericList() {
		return genericList;
	}

	protected static class GenericWrapper {

		private Generic generic;
		public StringProperty name = new SimpleStringProperty();

		public GenericWrapper(Generic generic) {
			this.generic = generic;
			name.set(Objects.toString(this.generic.getValue()));
		}

		public void remove() {
			generic.remove();
		}

		public StringProperty getString() {
			return name;
		}
	}

	@SuppressWarnings("unused")
	public Node init() {
		Element genericsVBox = new Element(null, VBox.class, "");
		Element genericsHBox = new Element(genericsVBox, VBox.class, "", Binding.bindToMethod(GenericList.class, GenericList::getGenericList, ForeachBinder.foreach()));
		Element genericsLabel = new Element(genericsHBox, Label.class, "", Binding.bindToMethod(GenericWrapper.class, GenericWrapper::getString, LabelBinder.textBind()));
		Element genericsRemoveButton = new Element(genericsHBox, Button.class, "remove", Binding.bindToMethod(GenericList.class, "remove", ClickBinder.methodBind(), GenericWrapper.class));

		Element genericsCreatLabel = new Element(genericsVBox, TextField.class, "", Binding.bindToField(GenericList.class, "name", TextFieldBinder.inputTextBind()));
		Element genericsCreateButton = new Element(genericsVBox, Button.class, "create", Binding.bindToMethod(GenericList.class, "create", ClickBinder.methodBind()));

		return genericsVBox.apply(this).getNode();
	}
}
