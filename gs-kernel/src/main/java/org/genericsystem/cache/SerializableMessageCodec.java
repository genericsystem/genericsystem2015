package org.genericsystem.cache;

import io.vertx.core.buffer.Buffer;
import io.vertx.core.eventbus.MessageCodec;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.ObjectInputStream;
import java.io.ObjectOutputStream;
import java.io.Serializable;

public class SerializableMessageCodec<T extends Serializable> implements MessageCodec<T, T> {

	private final String name;

	public SerializableMessageCodec(String name) {
		this.name = name;
	}

	@Override
	public void encodeToWire(Buffer buffer, T serialilizable) {
		ByteArrayOutputStream arrayStream = new ByteArrayOutputStream();
		ObjectOutputStream s;
		try {
			s = new ObjectOutputStream(arrayStream);
			s.writeObject(serialilizable);
			s.flush();
		} catch (IOException e) {
			throw new IllegalStateException(e);
		}
		buffer.appendBytes(arrayStream.toByteArray());

	}

	@SuppressWarnings("unchecked")
	@Override
	public T decodeFromWire(int pos, Buffer buffer) {
		try {
			return (T) new ObjectInputStream(new ByteArrayInputStream(buffer.getBytes(/* pos, buffer.length() */))).readObject();
		} catch (ClassNotFoundException | IOException e) {
			throw new IllegalStateException(name() + " : " + e);
		}
	}

	@Override
	public T transform(T s) {
		return s;
	}

	@Override
	public String name() {
		return name;
	}

	@Override
	public byte systemCodecID() {
		return -1;
	}

};
