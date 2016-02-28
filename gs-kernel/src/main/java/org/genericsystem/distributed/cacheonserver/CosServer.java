package org.genericsystem.distributed.cacheonserver;

import io.vertx.core.Handler;
import io.vertx.core.buffer.Buffer;
import io.vertx.core.buffer.impl.BufferFactoryImpl;
import io.vertx.core.http.ServerWebSocket;
import io.vertx.core.json.JsonObject;

import java.io.Serializable;
import java.nio.ByteOrder;
import java.util.Arrays;
import java.util.List;
import java.util.stream.Collectors;

import javafx.event.ActionEvent;

import org.genericsystem.distributed.AbstractGSClient;
import org.genericsystem.distributed.AbstractGSServer;
import org.genericsystem.distributed.GSBuffer;
import org.genericsystem.distributed.GSDeploymentOptions;
import org.genericsystem.distributed.WebSocketsServer;
import org.genericsystem.distributed.cacheonserver.ui.js.HtmlNode;
import org.genericsystem.distributed.cacheonserver.ui.js.HtmlNode.HtmlNodeCheckBox;
import org.genericsystem.distributed.cacheonserver.ui.js.todomvc.HtmlAdmin;
import org.genericsystem.distributed.cacheonserver.ui.js.todomvc.TodoList;
import org.genericsystem.kernel.Engine;

public class CosServer extends AbstractGSServer<Engine> {

	public static void main(String[] args) {
		new CosServer(new GSDeploymentOptions()).start();
	}

	public CosServer(GSDeploymentOptions options) {
		super(options);
	}

	@Override
	protected Buffer getReplyBuffer(int methodId, int op, Engine root, GSBuffer gsBuffer) {
		GSBuffer replyBuffer = new GSBuffer(new BufferFactoryImpl().buffer(Buffer.buffer().getByteBuf().order(ByteOrder.LITTLE_ENDIAN))).appendInt(op);
		switch (methodId) {
		case AbstractGSClient.PICK_NEW_TS:
			return replyBuffer.appendLongThrowException(() -> root.pickNewTs());
		case AbstractGSClient.GET_DEPENDENCIES:
			return replyBuffer.appendGSVertexArrayThrowException(() -> root.getDependencies(gsBuffer.getLong(), gsBuffer.getLong()));
		case AbstractGSClient.GET_VERTEX:
			return replyBuffer.appendGSVertexThrowException(() -> root.getVertex(gsBuffer.getLong()));
		case AbstractGSClient.SHIFT_TS:
			return replyBuffer.appendLongThrowException(() -> root.shiftTs(gsBuffer.getLong()));
		case AbstractGSClient.ADD_INSTANCE: {
			long cacheId = gsBuffer.getLong();
			long meta = gsBuffer.getLong();
			List<Long> supers = Arrays.stream(gsBuffer.getGSLongArray()).mapToObj(l -> l).collect(Collectors.toList());
			Serializable value = gsBuffer.getGSValue();
			List<Long> components = Arrays.stream(gsBuffer.getGSLongArray()).mapToObj(l -> l).collect(Collectors.toList());
			return replyBuffer.appendLongThrowException(() -> root.addInstance(cacheId, meta, supers, value, components));
		}
		case AbstractGSClient.SET_INSTANCE: {
			long cacheId = gsBuffer.getLong();
			long meta = gsBuffer.getLong();
			List<Long> supers = Arrays.stream(gsBuffer.getGSLongArray()).mapToObj(l -> l).collect(Collectors.toList());
			Serializable value = gsBuffer.getGSValue();
			List<Long> components = Arrays.stream(gsBuffer.getGSLongArray()).mapToObj(l -> l).collect(Collectors.toList());
			return replyBuffer.appendLongThrowException(() -> root.setInstance(cacheId, meta, supers, value, components));
		}
		case AbstractGSClient.MERGE: {
			long cacheId = gsBuffer.getLong();
			long meta = gsBuffer.getLong();
			List<Long> supers = Arrays.stream(gsBuffer.getGSLongArray()).mapToObj(l -> l).collect(Collectors.toList());
			Serializable value = gsBuffer.getGSValue();
			List<Long> components = Arrays.stream(gsBuffer.getGSLongArray()).mapToObj(l -> l).collect(Collectors.toList());
			return replyBuffer.appendLongThrowException(() -> root.merge(cacheId, meta, supers, value, components));
		}
		case AbstractGSClient.UPDATE: {
			long cacheId = gsBuffer.getLong();
			long meta = gsBuffer.getLong();
			List<Long> supers = Arrays.stream(gsBuffer.getGSLongArray()).mapToObj(l -> l).collect(Collectors.toList());
			Serializable value = gsBuffer.getGSValue();
			List<Long> components = Arrays.stream(gsBuffer.getGSLongArray()).mapToObj(l -> l).collect(Collectors.toList());
			return replyBuffer.appendLongThrowException(() -> root.update(cacheId, meta, supers, value, components));
		}
		case AbstractGSClient.REMOVE:
			return replyBuffer.appendLongThrowException(() -> root.remove(gsBuffer.getLong(), gsBuffer.getLong()));
		case AbstractGSClient.FORCE_REMOVE:
			return replyBuffer.appendLongThrowException(() -> root.forceRemove(gsBuffer.getLong(), gsBuffer.getLong()));
		case AbstractGSClient.CONSERVE_REMOVE:
			return replyBuffer.appendLongThrowException(() -> root.conserveRemove(gsBuffer.getLong(), gsBuffer.getLong()));
		case AbstractGSClient.TRY_FLUSH:
			return replyBuffer.appendLongThrowException(() -> root.tryFlush(gsBuffer.getLong()));
			// case AbstractGSClient.FLUSH:
			// return replyBuffer.appendLongThrowException(() -> root.flush(gsBuffer.getLong()));
		case AbstractGSClient.MOUNT:
			return replyBuffer.appendLongThrowException(() -> root.mount(gsBuffer.getLong()));
		case AbstractGSClient.UNMOUNT:
			return replyBuffer.appendLongThrowException(() -> root.unmount(gsBuffer.getLong()));
		case AbstractGSClient.GET_CACHE_LEVEL:
			return replyBuffer.appendIntThrowException(() -> root.getCacheLevel(gsBuffer.getLong()));
		case AbstractGSClient.NEW_CACHE: {
			return replyBuffer.appendLongThrowException(() -> root.newCacheId());
		}
		case AbstractGSClient.CLEAR:
			return replyBuffer.appendLongThrowException(() -> root.clear(gsBuffer.getLong()));
		case AbstractGSClient.SUBELEMENTS: {
			System.out.println("receive op : " + op);
			return replyBuffer.appendString("div");
		}
		default:
			throw new IllegalStateException("unable to find method : " + methodId);
		}
	}

	@Override
	protected Engine buildRoot(String value, String persistentDirectoryPath, Class[] userClasses) {
		return new Engine(value, persistentDirectoryPath, userClasses);
	}

	@Override
	protected WebSocketsServer<Engine> buildWebSocketsServer(GSDeploymentOptions options) {
		return new WebSocketsServer<Engine>(this, options.getHost(), options.getPort()) {
			@Override
			public Handler<Buffer> getHandler(Engine root, ServerWebSocket socket) {
				TodoList todolist = new TodoList();
				HtmlNode parent = new HtmlNode(socket);
				HtmlAdmin jsAdmin = new HtmlAdmin(todolist, parent, socket);
				return buffer -> {
					GSBuffer gsBuffer = new GSBuffer(buffer);
					String message = gsBuffer.getString(0, gsBuffer.length());
					JsonObject obj = new JsonObject(message);
					HtmlNode node = jsAdmin.getRootViewContext().getNodeById().get(obj.getString("nodeId"));
					if (node != null) {
						if (obj.getString("msg_type").equals("A"))
							node.getActionProperty().get().handle(new ActionEvent());

						if (obj.getString("msg_type").equals("U")) {
							if (obj.getString("eltType").equals("text"))
								node.getText().setValue(obj.getString("textContent"));

							if ("checkbox".equals(obj.getString("eltType")))
								((HtmlNodeCheckBox) node).getChecked().setValue(obj.getBoolean("checked"));
						}
					}
				};
			};
		};
	}
}
