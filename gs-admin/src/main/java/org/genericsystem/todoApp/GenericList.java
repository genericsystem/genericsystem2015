package org.genericsystem.todoApp;

import java.io.File;
import java.util.Objects;

import javafx.beans.InvalidationListener;
import javafx.beans.property.SimpleStringProperty;
import javafx.beans.property.StringProperty;
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

	public StringProperty name = new SimpleStringProperty();

	public StringProperty getName() {
		return name;
	}

	private void cleanDirectory() {
		File file = new File(path);
		if (file.exists())
			for (File f : file.listFiles()) {
				if (!".lock".equals(f.getName()))
					f.delete();
			}
	}

	public GenericList() throws InterruptedException {

		cleanDirectory();

		server = new CocServer(new GSDeploymentOptions(Statics.ENGINE_VALUE, Statics.DEFAULT_PORT, path));
		server.start();

		engine = new CocClientEngine(Statics.ENGINE_VALUE, null, Statics.DEFAULT_PORT);

		dependenciesObservableList = engine.getCurrentCache().getObservableDependencies(engine);
		dependenciesObservableList.addListener((InvalidationListener) l -> System.out.println("Invalidation of dependenciesObservableList"));

		genericWrapperList = new Transformation<GenericWrapper, Generic>(dependenciesObservableList, generic -> new GenericWrapper(generic));
		genericWrapperList.addListener((InvalidationListener) l -> System.out.println("Invalidation of genericWrapperList"));

		//
		// Thread.sleep(1000);
		// System.out.println(dependenciesObservableList);
		// genericWrapperList = FXCollections.observableArrayList();
		// genericWrapperList.addAll(dependenciesObservableList.stream().map(dep -> new GenericWrapper(dep)).collect(Collectors.toList()));
		// System.out.println(genericWrapperList);
	}

	// private void initGenericList() {
	// genericWrapperList.clear();
	// genericWrapperList.addAll(dependenciesObservableList.stream().map(dep -> new GenericWrapper(dep)).collect(Collectors.toList()));
	//
	// }

	public void flush() {
		engine.getCurrentCache().flush();
	}

	public void clear() {
		engine.getCurrentCache().clear();
		// try {
		// Thread.sleep(100);
		// initGenericList();
		// } catch (InterruptedException e) {
		// // TODO Auto-generated catch block
		// e.printStackTrace();
		// }
		// genericList.addAll(dependenciesObservableList.stream().map(dep -> new GenericWrapper(dep)).collect(Collectors.toList()));

	}

	public void mount() {
		engine.getCurrentCache().mount();
		// initGenericList();
	}

	public void unmount() {
		engine.getCurrentCache().unmount();
		// initGenericList();
	}

	public void create() {
		engine.addInstance(name.getValue());
		// initGenericList();
	}

	public void remove(GenericWrapper genericWrapper) {
		genericWrapper.remove();
		// initGenericList();
	}

	public ObservableList<GenericWrapper> getGenericList() {
		return genericWrapperList;
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

		Element genericsHBox = new Element(genericsVBox, VBox.class, "", Binding.forEach(GenericList::getGenericList));
		Element genericsLabel = new Element(genericsHBox, Label.class, "", Binding.bindText(GenericWrapper::getString));
		Element genericsRemoveButton = new Element(genericsHBox, Button.class, "remove", Binding.bindAction(GenericList::remove, GenericWrapper.class));
		Element genericsCreatLabel = new Element(genericsVBox, TextField.class, "", Binding.bindInputText(GenericList::getName));

		Element genericsHB = new Element(genericsVBox, HBox.class, "");
		Element genericsCreateButton = new Element(genericsHB, Button.class, "create", Binding.bindAction(GenericList::create));
		Element genericsFlushButton = new Element(genericsHB, Button.class, "flush", Binding.bindAction(GenericList::flush));
		Element genericsClearButton = new Element(genericsHB, Button.class, "clear", Binding.bindAction(GenericList::clear));
		Element genericsMountButton = new Element(genericsHB, Button.class, "mount", Binding.bindAction(GenericList::mount));
		Element genericsUnmountButton = new Element(genericsHB, Button.class, "unmount", Binding.bindAction(GenericList::unmount));

		return genericsVBox.apply(this).getNode();
	}
}
