package org.genericsystem.angular;

import java.io.File;
import java.io.IOException;
import java.io.Serializable;
import java.util.Objects;
import java.util.function.Consumer;

import org.genericsystem.angular.annotation.Column;
import org.genericsystem.angular.annotation.Table;
import org.genericsystem.angular.model.Car;
import org.genericsystem.angular.model.Power;
import org.genericsystem.api.core.Snapshot;
import org.genericsystem.common.Generic;
import org.genericsystem.kernel.Cache;
import org.genericsystem.kernel.Engine;

import io.vertx.core.AbstractVerticle;
import io.vertx.core.Handler;
import io.vertx.core.Vertx;
import io.vertx.core.VertxOptions;
import io.vertx.core.json.JsonArray;
import io.vertx.core.json.JsonObject;
import io.vertx.core.logging.Logger;
import io.vertx.core.logging.LoggerFactory;
import io.vertx.ext.web.Router;
import io.vertx.ext.web.RoutingContext;
import io.vertx.ext.web.Session;
import io.vertx.ext.web.handler.BodyHandler;
import io.vertx.ext.web.handler.CookieHandler;
import io.vertx.ext.web.handler.SessionHandler;
import io.vertx.ext.web.handler.StaticHandler;
import io.vertx.ext.web.sstore.LocalSessionStore;

public class Server extends AbstractVerticle {

	private static final Logger log = LoggerFactory.getLogger(Server.class);

	private static Engine engine = new Engine(System.getenv("HOME") + "/genericsytem/carsAngular", Car.class, Power.class);

	public static void main(String[] args) {
		Generic car = engine.find(Car.class);
		Generic power = engine.find(Power.class);
		Generic car1 = car.setInstance("Audi TT");
		Generic car2 = car.setInstance("R5");
		car1.setHolder(power, 100);
		car2.setHolder(power, 200);
		engine.getCurrentCache().flush();

		String dir = "gs-example-angular2/src/main/java/" + Server.class.getPackage().getName().replace(".", "/");
		try {
			// We need to use the canonical file. Without the file name is .
			File current = new File(".").getCanonicalFile();
			if (dir.startsWith(current.getName()) && !dir.equals(current.getName())) {
				dir = dir.substring(current.getName().length() + 1);
			}
		} catch (IOException e) {
			// Ignore it.
		}
		System.setProperty("vertx.cwd", dir);
		Consumer<Vertx> runner = vertx -> {
			try {
				vertx.deployVerticle(Server.class.getName());
			} catch (Throwable t) {
				t.printStackTrace();
			}
		};
		Vertx vertx = Vertx.vertx(new VertxOptions().setClustered(false));
		runner.accept(vertx);
	}

	@Override
	public void start() throws Exception {
		Router router = Router.router(vertx);
		router.route().handler(BodyHandler.create());
		router.route().handler(CookieHandler.create());
		router.route().handler(SessionHandler.create(LocalSessionStore.create(vertx)));
		router.route().handler(CacheHandler.create(engine));
		JsonArray jsonArray = getGSJsonCRUD();
		router.get("/api/types").handler(ctx -> {
			ctx.response().end(jsonArray.encode());
		});
		initREST(router);

		// Create a router endpoint for the static content.
		router.route().handler(StaticHandler.create());
		vertx.createHttpServer().requestHandler(router::accept).listen(8080);

	}

	public String getTableName(Generic type) {
		if (!type.isSystem())
			return null;
		Class<?> clazz = engine.findAnnotedClass(type);
		Table annotation = clazz.getAnnotation(Table.class);
		return annotation != null ? annotation.value() : null;
	}

	public String getColumnName(Generic attribute) {
		if (!attribute.isSystem())
			return null;
		Class<?> clazz = engine.findAnnotedClass(attribute);
		Column annotation = clazz.getAnnotation(Column.class);
		return annotation != null ? annotation.value() : null;
	}

	public JsonArray getAttributesJsonArray(Generic type) {
		JsonArray attributesArray = new JsonArray();
		for (Generic attribute : getAttributes(type)) {
			String columnName = getColumnName(attribute);
			if (columnName != null)
				attributesArray.add(new JsonObject().put("columnName", columnName));
		}
		return attributesArray;
	}

	public Snapshot<Generic> getAttributes(Generic type) {
		return type.getAttributes().filter(att -> att.getComponents().size() == 1 && type.inheritsFrom(att.getBaseComponent()));
	}

	public Snapshot<Generic> getTypes() {
		Cache cache = engine.newCache();
		cache.start();
		return engine.getSubInstances().filter(typ -> typ.getComponents().isEmpty()).filter(typ -> getTableName(typ) != null);
	}

	public Serializable convert(Generic attribute, String value) {
		Class<? extends Serializable> classConstraint = attribute.getInstanceValueClassConstraint();
		if (classConstraint.equals(java.lang.Integer.class))
			return Integer.parseInt(value);
		else if (classConstraint.equals(java.lang.Long.class))
			return Long.parseLong(value);
		else if (classConstraint.equals(java.lang.Double.class))
			return Double.parseDouble(value);
		return null;
	}

	public JsonObject getJson(Generic instance, Snapshot<Generic> attributes) {
		final JsonObject json = new JsonObject();
		json.put("id", String.valueOf(instance.getTs()));
		json.put("value", instance.getValue().toString());
		for (Generic attribute : attributes)
			json.put(getColumnName(attribute), Objects.toString(instance.getValue(attribute)));
		return json;
	}

	public Generic getInstanceById(Generic type, Long id) {
		return type.getInstances().stream().filter(g -> Objects.equals((id), g.getTs())).findFirst().orElse(null);
	}

	private JsonArray getGSJsonCRUD() {

		JsonArray jsonArray = new JsonArray();

		for (Generic type : getTypes()) {
			JsonObject json = new JsonObject();
			json.put("tableName", getTableName(type));
			json.put("columns", getAttributesJsonArray(type));
			jsonArray.add(json);
		}
		return jsonArray;
	}

	public static interface CacheHandler extends Handler<RoutingContext> {
		static CacheHandler create(Engine engine) {
			return ctx -> {
				Session session = ctx.session();
				Cache cache = session.get("cache");
				if (cache == null)
					session.put("cache", cache = engine.newCache());
				cache.start();
				ctx.next();
			};
		}
	}

	private void initREST(Router router) {
		JsonArray jsonArray = getGSJsonCRUD();
		router.get("/api/types").handler(ctx -> {
			ctx.response().end(jsonArray.encode());
		});
		for (int j = 0; j < jsonArray.size(); j++) {
			String typeName = jsonArray.getJsonObject(j).getString("tableName");
			router.get("/api/" + typeName).handler(ctx -> {
				Generic type = engine.getInstance(typeName);
				final JsonArray json = new JsonArray();
				type.getInstances().stream().forEach(i -> json.add(getJson(i, getAttributes(type))));
				ctx.response().end(json.encode());
			});
			router.get("/api/" + typeName + "/:id").handler(ctx -> {
				Generic type = engine.getInstance(typeName);
				Generic instance = getInstanceById(type, Long.valueOf(ctx.request().getParam("id")));
				JsonObject json = getJson(instance, getAttributes(type));
				ctx.response().end(json.encode());
			});
			router.post("/api/" + typeName).handler(ctx -> {
				Generic type = engine.getInstance(typeName);
				JsonObject newInst = ctx.getBodyAsJson();
				Generic instance = type.setInstance(newInst.getString("value"));

				for (Generic attribute : getAttributes(type))
					instance.setHolder(attribute, convert(attribute, newInst.getString(getColumnName(attribute))));
				ctx.response().end(newInst.encode());
			});
			router.put("/api/" + typeName + "/:id").handler(ctx -> {
				Generic type = engine.getInstance(typeName);
				Generic instance = getInstanceById(type, Long.valueOf(ctx.request().getParam("id")));
				JsonObject update = ctx.getBodyAsJson();
				instance = instance.updateValue(update.getString("value"));
				for (Generic attribute : getAttributes(type)) {
					instance.getHolder(attribute).updateValue(convert(attribute, update.getString(getColumnName(attribute))));
				}
				JsonObject json = getJson(instance, getAttributes(type));
				ctx.response().end(json.encode());
			});
			router.delete("/api/" + typeName + "/:id").handler(ctx -> {
				Generic type = engine.getInstance(typeName);
				Generic instance = getInstanceById(type, Long.valueOf(ctx.request().getParam("id")));
				instance.remove();
				ctx.response().end();
			});
			router.put("/api/" + typeName + "/commit").handler(ctx -> {
				engine.getCurrentCache().flush();
				ctx.response().end();
			});

			router.delete("/api/" + typeName + "/shift").handler(ctx -> {
				engine.getCurrentCache().shiftTs();
				ctx.response().end();
			});

			router.delete("/api/" + typeName + "/clear").handler(ctx -> {
				engine.getCurrentCache().clear();
				ctx.response().end();
			});

		}
	}
}
