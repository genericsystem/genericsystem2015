package org.genericsystem.reactor.appserver;

import java.lang.reflect.InvocationTargetException;

import org.genericsystem.common.AbstractCache;
import org.genericsystem.common.Root;
import org.genericsystem.reactor.Model;
import org.genericsystem.reactor.Tag.RootTag;
import org.genericsystem.reactor.ViewContext.RootViewContext;
import org.genericsystem.reactor.annotations.RunScript;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import io.vertx.core.http.ServerWebSocket;

public class PersistentApplication<M extends Model> {
	private final Class<? extends RootTag<M>> htmlAppClass;
	private final RootTag<M> tagTree;
	private final Root engine;
	private final Class<M> modelClass;
	private final String rootId;
	protected static Logger log = LoggerFactory.getLogger(PersistentApplication.class);

	public PersistentApplication(Class<? extends RootTag<M>> htmlAppClass, Class<M> modelClass, Root engine, String rootId) {
		this.htmlAppClass = htmlAppClass;
		this.modelClass = modelClass;
		this.engine = engine;
		this.rootId = rootId;
		RunScript script = getApplicationClass().getAnnotation(RunScript.class);
		if (script != null) {
			AbstractCache cache = engine.newCache();
			log.info("Script will run ...");
			Script runner;
			try {
				runner = script.value().newInstance();
			} catch (InstantiationException | IllegalAccessException e) {
				throw new IllegalStateException(e);
			}
			cache.safeConsum(unused -> runner.run(getEngine()));
			log.info("Script has run");
		}
		try {
			tagTree = getApplicationClass().newInstance();
		} catch (InstantiationException | IllegalAccessException | IllegalArgumentException | SecurityException e) {
			throw new IllegalStateException(e);
		}
	}

	public String getRootId() {
		return rootId;
	}

	public Root getEngine() {
		return engine;
	}

	public Class<? extends RootTag<M>> getApplicationClass() {
		return htmlAppClass;
	}

	public void close() {
		engine.close();
	}

	public RootViewContext<?> init(ServerWebSocket socket) {
		try {
			return tagTree.init(modelClass.getConstructor(Root.class).newInstance(engine), rootId, socket);
		} catch (InstantiationException | IllegalAccessException | IllegalArgumentException | InvocationTargetException | NoSuchMethodException | SecurityException e) {
			throw new IllegalStateException(e);
		}
	}
}
