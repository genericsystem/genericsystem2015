package org.genericsystem.reactor.appserver;

import io.vertx.core.http.ServerWebSocket;

import java.lang.reflect.InvocationTargetException;

import org.genericsystem.common.AbstractCache;
import org.genericsystem.common.Root;
import org.genericsystem.reactor.HtmlDomNode;
import org.genericsystem.reactor.Model;
import org.genericsystem.reactor.annotations.RunScript;
import org.genericsystem.reactor.html.HtmlApp;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class PersistentApplication {
	private final Class<? extends HtmlApp<?>> htmlAppClass;
	private final Root engine;
	private final Class<? extends Model> modelClass;
	private final String rootId;
	protected static Logger log = LoggerFactory.getLogger(PersistentApplication.class);

	public PersistentApplication(Class<? extends HtmlApp<?>> htmlAppClass, Class<? extends Model> modelClass, Root engine, String rootId) {
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
	}

	public String getRootId() {
		return rootId;
	}

	public Root getEngine() {
		return engine;
	}

	public Class<? extends App<?>> getApplicationClass() {
		return htmlAppClass;
	}

	public void close() {
		engine.close();
	}

	public static interface App<M extends Model> {
		App<M> init(M rootModelContext, String rootId, ServerWebSocket webSocket);

		HtmlDomNode getNodeById(String string);
	}

	@SuppressWarnings("unchecked")
	public App<?> buildApp(ServerWebSocket socket) {
		try {
			return ((App<Model>) getApplicationClass().getConstructor(Root.class).newInstance(getEngine())).init(modelClass.getConstructor(Root.class).newInstance(engine), rootId, socket);
		} catch (InstantiationException | IllegalAccessException | IllegalArgumentException | InvocationTargetException | NoSuchMethodException | SecurityException e) {
			throw new IllegalStateException(e);
		}
	}
}
