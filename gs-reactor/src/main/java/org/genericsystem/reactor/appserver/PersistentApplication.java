package org.genericsystem.reactor.appserver;

import io.vertx.core.http.ServerWebSocket;

import java.lang.reflect.InvocationTargetException;

import org.genericsystem.common.AbstractCache;
import org.genericsystem.common.AbstractRoot;
import org.genericsystem.reactor.Model;
import org.genericsystem.reactor.Tag;
import org.genericsystem.reactor.annotations.RunScript;
import org.genericsystem.reactor.html.HtmlApp;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class PersistentApplication {
	private final Class<? extends HtmlApp<?>> htmlAppClass;
	private final AbstractRoot engine;
	private final Class<? extends Model> modelClass;
	private final String rootId;
	protected static Logger log = LoggerFactory.getLogger(PersistentApplication.class);

	public PersistentApplication(Class<? extends HtmlApp<?>> htmlAppClass, Class<? extends Model> modelClass, AbstractRoot engine, String rootId) {
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

	public AbstractRoot getEngine() {
		return engine;
	}

	public Class<? extends HtmlApp<?>> getApplicationClass() {
		return htmlAppClass;
	}

	public void close() {
		engine.close();
	}

	@SuppressWarnings("unchecked")
	public Tag<?> newHtmlApp(ServerWebSocket socket) {
		try {
			return ((HtmlApp<Model>) getApplicationClass().getConstructor(AbstractRoot.class, ServerWebSocket.class).newInstance(getEngine(), socket)).init(modelClass.getConstructor(AbstractRoot.class).newInstance(engine), rootId);
		} catch (InstantiationException | IllegalAccessException | IllegalArgumentException | InvocationTargetException | NoSuchMethodException | SecurityException e) {
			throw new IllegalStateException(e);
		}
	}
}
