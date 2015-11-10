package org.genericsystem.distributed.cacheonserver;

import io.vertx.core.buffer.Buffer;
import java.io.Serializable;
import java.util.Arrays;
import java.util.List;
import java.util.stream.Collectors;
import org.genericsystem.distributed.AbstractGSClient;
import org.genericsystem.distributed.AbstractGSServer;
import org.genericsystem.distributed.GSBuffer;
import org.genericsystem.distributed.GSDeploymentOptions;
import org.genericsystem.kernel.HeavyServerEngine;

public abstract class AbstractGSHeavyServer extends AbstractGSServer {
	public AbstractGSHeavyServer(GSDeploymentOptions options) {
		super(options);
	}

	Buffer getReplyBuffer(int methodId, int op, HeavyServerEngine root, GSBuffer gsBuffer) {
		GSBuffer replyBuffer = new GSBuffer();
		replyBuffer.appendInt(op);
		switch (methodId) {
		case AbstractGSClient.PICK_NEW_TS:
			return replyBuffer.appendLong(root.pickNewTs());
		case AbstractGSClient.GET_DEPENDENCIES:
			return replyBuffer.appendGSVertexArray(root.getDependencies(gsBuffer.getLong(), gsBuffer.getLong()));
		case AbstractGSClient.GET_VERTEX:
			return replyBuffer.appendGSVertex(root.getVertex(gsBuffer.getLong()));
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
			return replyBuffer.appendInt(root.getCacheLevel(gsBuffer.getLong()));
		case AbstractGSClient.NEW_CACHE: {
			System.out.println("zzzzzzzzzzzzzzzzzzzz");
			return replyBuffer.appendLong(root.newCacheId());
		}
		case AbstractGSClient.CLEAR:
			return replyBuffer.appendLongThrowException(() -> root.clear(gsBuffer.getLong()));
		default:
			throw new IllegalStateException("unable to find method : " + methodId);
		}
	}
}
