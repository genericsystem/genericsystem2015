package org.genericsystem.reactor.appserver;

import java.lang.reflect.InvocationTargetException;

import org.genericsystem.common.AbstractCache;
import org.genericsystem.common.Root;
import org.genericsystem.kernel.Cache;
import org.genericsystem.reactor.Context;
import org.genericsystem.reactor.HtmlDomNode.Sender;
import org.genericsystem.reactor.RootHtmlDomNode;
import org.genericsystem.reactor.RootTag;
import org.genericsystem.reactor.annotations.RunScript;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class PersistentApplication {

	private final Class<? extends RootTag> htmlAppClass;
	private final Root engine;
	private final Class<Context> modelClass;
	private final String rootId;
	private String indexHtml = "";
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
			cache.safeExecute(() -> runner.run(getEngine()));
			log.info("Script has run");
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

	public String getIndexHtml() {
		return indexHtml;
	}

	public void setIndexHtml(String indexHtml) {
		this.indexHtml = indexHtml;
	}

	public RootHtmlDomNode init(Sender send, RootTag tagTree, Cache cache) {
		try {
			return tagTree.init(modelClass.getConstructor(Root.class, Cache.class).newInstance(engine, cache), rootId, send);
		} catch (InstantiationException | IllegalAccessException | IllegalArgumentException | InvocationTargetException | NoSuchMethodException | SecurityException e) {
			throw new IllegalStateException(e);
		}
	}
}
