package org.genericsystem.reactor.appserver;

import io.vertx.core.http.ServerWebSocket;

import java.lang.reflect.InvocationTargetException;

import org.genericsystem.common.AbstractCache;
import org.genericsystem.common.Root;
import org.genericsystem.reactor.Context;
import org.genericsystem.reactor.HtmlDomNode.RootHtmlDomNode;
import org.genericsystem.reactor.Tag.RootTag;
import org.genericsystem.reactor.annotations.RunScript;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class PersistentApplication {
	private final Class<? extends RootTag> htmlAppClass;
	private final RootTag tagTree;
	private final Root engine;
	private final Class<Context> modelClass;
	private final String rootId;
	protected static Logger log = LoggerFactory.getLogger(PersistentApplication.class);

	public PersistentApplication(Class<? extends RootTag> htmlAppClass, Class<Context> modelClass, Root engine, String rootId) {
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

	public Class<? extends RootTag> getApplicationClass() {
		return htmlAppClass;
	}

	public void close() {
		engine.close();
	}

	public RootHtmlDomNode init(ServerWebSocket socket) {
		try {
			return tagTree.init(modelClass.getConstructor(Root.class).newInstance(engine), rootId, socket);
		} catch (InstantiationException | IllegalAccessException | IllegalArgumentException | InvocationTargetException | NoSuchMethodException | SecurityException e) {
			throw new IllegalStateException(e);
		}
	}
}
