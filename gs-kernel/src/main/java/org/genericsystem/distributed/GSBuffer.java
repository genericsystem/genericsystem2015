package org.genericsystem.distributed;

import io.netty.buffer.ByteBuf;
import io.netty.util.CharsetUtil;
import io.vertx.core.buffer.Buffer;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.ObjectInputStream;
import java.io.ObjectOutputStream;
import java.io.Serializable;
import java.nio.ByteBuffer;
import java.util.Arrays;
import java.util.List;
import java.util.Map.Entry;
import java.util.stream.Collectors;

import org.genericsystem.api.core.ApiStatics;
import org.genericsystem.api.core.AxedPropertyClass;
import org.genericsystem.api.core.exceptions.ConcurrencyControlException;
import org.genericsystem.common.Vertex;

public class GSBuffer implements Buffer {
	private int index = 0;
	private final Buffer internal;

	public GSBuffer() {
		this(Buffer.buffer());
	}

	public GSBuffer(Buffer internal) {
		this.internal = internal;
	}

	@Override
	public void writeToBuffer(Buffer buffer) {
		internal.writeToBuffer(buffer);
	}

	@Override
	public int readFromBuffer(int pos, Buffer buffer) {
		return internal.readFromBuffer(pos, buffer);
	}

	@Override
	public String toString(String enc) {
		return internal.toString(enc);
	}

	@Override
	public byte getByte(int pos) {
		return internal.getByte(pos);
	}

	@Override
	public int getInt(int pos) {
		return internal.getInt(pos);
	}

	@Override
	public long getLong(int pos) {
		return internal.getLong(pos);
	}

	@Override
	public double getDouble(int pos) {
		return internal.getDouble(pos);
	}

	@Override
	public float getFloat(int pos) {
		return internal.getFloat(pos);
	}

	@Override
	public short getShort(int pos) {
		return internal.getShort(pos);
	}

	@Override
	public byte[] getBytes() {
		return internal.getBytes();
	}

	@Override
	public byte[] getBytes(int start, int end) {
		return internal.getBytes(start, end);
	}

	@Override
	public Buffer getBuffer(int start, int end) {
		return internal.getBuffer(start, end);
	}

	@Override
	public String getString(int start, int end, String enc) {
		return internal.getString(start, end, enc);
	}

	@Override
	public String getString(int start, int end) {
		return internal.getString(start, end);
	}

	@Override
	public Buffer appendBuffer(Buffer buff) {
		return internal.appendBuffer(buff);
	}

	@Override
	public Buffer appendBuffer(Buffer buff, int offset, int len) {
		return internal.appendBuffer(buff, offset, len);
	}

	@Override
	public Buffer appendBytes(byte[] bytes) {
		return internal.appendBytes(bytes);
	}

	@Override
	public Buffer appendBytes(byte[] bytes, int offset, int len) {
		return internal.appendBytes(bytes, offset, len);
	}

	@Override
	public Buffer appendByte(byte b) {
		return internal.appendByte(b);
	}

	@Override
	public GSBuffer appendInt(int i) {
		internal.appendInt(i);
		return this;
	}

	@Override
	public GSBuffer appendLong(long l) {
		internal.appendLong(l);
		return this;
	}

	@Override
	public Buffer appendShort(short s) {
		return internal.appendShort(s);
	}

	@Override
	public Buffer appendFloat(float f) {
		return internal.appendFloat(f);
	}

	@Override
	public Buffer appendDouble(double d) {
		return internal.appendDouble(d);
	}

	@Override
	public Buffer appendString(String str, String enc) {
		return internal.appendString(str, enc);
	}

	@Override
	public Buffer appendString(String str) {
		return internal.appendString(str);
	}

	public Buffer appendGSString(String string) {
		return appendGSBytes(string.getBytes(CharsetUtil.UTF_8));

	}

	public Buffer appendGSVertex(Vertex vertex) {
		appendGSClazz(vertex.getClazz());
		appendLong(vertex.getTs());
		appendLong(vertex.getMeta());
		appendGSLongList(vertex.getSupers());
		appendGSValue(vertex.getValue());
		appendGSLongList(vertex.getComponents());
		appendLong(vertex.getBirthTs());
		return this;
	}

	public Buffer appendGSSignature(long meta, List<Long> supers, Serializable value, List<Long> components) {
		appendLong(meta);
		appendGSLongList(supers);
		appendGSValue(value);
		appendGSLongList(components);
		return this;
	}

	public Buffer appendGSVertexArray(Vertex[] vertexArray) {
		appendInt(vertexArray.length);
		for (Vertex v : vertexArray)
			appendGSVertex(v);
		return this;
	}

	public Buffer appendGSLongList(List<Long> array) {
		appendInt(array.size());
		for (long l : array)
			appendLong(l);
		return this;
	}

	public Buffer appendGSLongArray(long[] array) {
		appendInt(array.length);
		for (long l : array)
			appendLong(l);
		return this;
	}

	public Buffer appendGSValue(Serializable value) {
		// System.out.println("Append value : " + value);
		for (Entry<Integer, Class<?>> entry : ApiStatics.SUPPORTED_VALUE_CLASSES.entrySet()) {
			if (entry.getValue().isInstance(value)) {
				appendInt(entry.getKey());
				switch (entry.getKey()) {
				case 0: {
					appendGSClazz(((AxedPropertyClass) value).getClazz());
					appendInt(((AxedPropertyClass) value).getAxe());
					return this;
				}
				case 1: {
					appendInt(((Boolean) value).booleanValue() ? 1 : 0);
					return this;
				}
				case 2: {
					appendGSBytes((byte[]) value);
					return this;
				}
				case 3: {
					appendDouble((Double) value);
					return this;
				}
				case 4: {
					appendFloat((Float) value);
					return this;
				}
				case 5: {
					appendInt((Integer) value);
					return this;
				}
				case 6: {
					appendLong((Long) value);
					return this;
				}
				case 7: {
					appendShort((Short) value);
					return this;
				}
				case 8: {
					appendGSString((String) value);
					return this;
				}
				case 9: {
					appendGSClazz((Class<?>) value);
					return this;
				}
				default:
					throw new IllegalStateException("unknown class code in appendGSValue" + entry.getKey());
				}
			}
		}
		return this;
	}

	public Buffer appendGSBytes(byte[] bytes) {
		appendInt(bytes.length);
		appendBytes(bytes);
		return this;
	}

	public Buffer appendGSClazz(Class<?> clazz) {
		appendGSString(clazz != null ? clazz.getName() : "");
		return this;
	}

	// public Buffer appendResponse(Response<Long> response) {
	// appendInt(response.isSwitchedToT() ? 1 : 0);
	// if (response.isSwitchedToT())
	// appendLong(response.getResult());
	// else {
	// appendGSSerializable(response.getException().getClass());
	// appendGSClazz(response.getException().getCause().getClass());
	// }
	// return this;
	// }

	public Object getLongThrowException() {
		if (getInt() == 1)
			return getLong();
		return getGSSerializable();
	}

	public interface ConcurrentSupplier<T> {
		T get() throws ConcurrencyControlException;
	}

	public Buffer appendLongThrowException(ConcurrentSupplier<Long> supplier) {
		try {
			long result = supplier.get();
			appendInt(1);
			appendLong(result);
		} catch (Throwable t) {
			appendInt(0);
			t.printStackTrace();
			appendGSSerializable(t);
		}
		return this;
	}

	// public Buffer appendLongThrowException(Object value) {
	// if (value instanceof Long) {
	// appendInt(1);
	// appendLong((Long) value);
	// } else {
	// appendInt(0);
	// appendGSSerializable((Serializable) value);
	// }
	// return this;
	// }

	public Buffer appendGSSerializable(Serializable serializable) {
		ByteArrayOutputStream bos = new ByteArrayOutputStream();
		try {
			ObjectOutputStream oos = new ObjectOutputStream(bos);
			oos.writeObject(serializable);
		} catch (IOException e) {
			e.printStackTrace();
			throw new IllegalStateException(e);
		}
		appendGSBytes(bos.toByteArray());
		return this;
	}

	public Serializable getGSSerializable() {
		ByteArrayInputStream bis = new ByteArrayInputStream(getGSBytes());
		Serializable serializable;
		try {
			ObjectInputStream ois = new ObjectInputStream(bis);
			serializable = (Serializable) ois.readObject();
		} catch (IOException | ClassNotFoundException e) {
			e.printStackTrace();
			throw new IllegalStateException();
		}
		return serializable;
	}

	public int getInt() {
		int result = getInt(index);
		index += 4;
		return result;
	}

	public long getLong() {
		long result = getLong(index);
		index += 8;
		return result;
	}

	public double getDouble() {
		double result = getDouble(index);
		index += 16;
		return result;
	}

	public float getFloat() {
		float result = getFloat(index);
		index += 8;
		return result;
	}

	public short getShort() {
		short result = getShort(index);
		index += 2;
		return result;
	}

	public String getGSString() {
		byte[] result = getGSBytes();
		return new String(result);

	}

	public Vertex getGSVertex() {
		Class<?> clazz = getGSClazz();
		long ts = getLong();
		long meta = getLong();
		List<Long> supers = Arrays.stream(getGSLongArray()).mapToObj(l -> l).collect(Collectors.toList());
		Serializable value = getGSValue();
		List<Long> components = Arrays.stream(getGSLongArray()).mapToObj(l -> l).collect(Collectors.toList());
		long birthTs = getLong();
		return new Vertex(clazz, ts, meta, supers, value, components, birthTs);
	}

	public Vertex[] getGSVertexArray() {
		Vertex[] result = new Vertex[getInt()];
		for (int i = 0; i < result.length; i++)
			result[i] = getGSVertex();
		return result;
	}

	public long[] getGSLongArray() {
		long[] result = new long[getInt()];
		for (int i = 0; i < result.length; i++)
			result[i] = getLong();
		return result;
	}

	public Serializable getGSValue() {
		int code = getInt();
		switch (code) {
		case 0:
			return new AxedPropertyClass((Class) getGSClazz(), getInt());
		case 1:
			return (boolean) (getInt() == 1);
		case 2:
			return getGSBytes();
		case 3:
			return getDouble();
		case 4:
			return getFloat();
		case 5:
			return getInt();
		case 6:
			return getLong();
		case 7:
			return getShort();
		case 8:
			return getGSString();
		case 9:
			return getGSClazz();
		case 10:
			return null;
		default:
			throw new IllegalStateException("unknowned class code");
		}
	}

	public Class<?> getGSClazz() {
		String string = getGSString();
		try {

			return string.isEmpty() ? null : Class.forName(string);
		} catch (ClassNotFoundException e) {
			throw new IllegalStateException(e);
		}
	}

	public byte[] getGSBytes() {
		int length = getInt();
		byte[] result = getBytes(index, index + length);
		if (result.length != length)
			throw new IllegalStateException(result.length + " " + length);
		index += length;
		return result;
	}

	public byte getByte() {
		byte result = getByte(index);
		index++;
		return result;
	}

	@Override
	public Buffer setByte(int pos, byte b) {
		return internal.setByte(pos, b);
	}

	@Override
	public Buffer setInt(int pos, int i) {
		return internal.setInt(pos, i);
	}

	@Override
	public Buffer setLong(int pos, long l) {
		return internal.setLong(pos, l);
	}

	@Override
	public Buffer setDouble(int pos, double d) {
		return internal.setDouble(pos, d);
	}

	@Override
	public Buffer setFloat(int pos, float f) {
		return internal.setFloat(pos, f);
	}

	@Override
	public Buffer setShort(int pos, short s) {
		return internal.setShort(pos, s);
	}

	@Override
	public Buffer setBuffer(int pos, Buffer b) {
		return internal.setBuffer(pos, b);
	}

	@Override
	public Buffer setBuffer(int pos, Buffer b, int offset, int len) {
		return internal.setBuffer(pos, b, offset, len);
	}

	@Override
	public Buffer setBytes(int pos, ByteBuffer b) {
		return internal.setBytes(pos, b);
	}

	@Override
	public Buffer setBytes(int pos, byte[] b) {
		return internal.setBytes(pos, b);
	}

	@Override
	public Buffer setBytes(int pos, byte[] b, int offset, int len) {
		return internal.setBytes(pos, b, offset, len);
	}

	@Override
	public Buffer setString(int pos, String str) {
		return internal.setString(pos, str);
	}

	@Override
	public Buffer setString(int pos, String str, String enc) {
		return internal.setString(pos, str, enc);
	}

	@Override
	public int length() {
		return internal.length();
	}

	@Override
	public Buffer copy() {
		return internal.copy();
	}

	@Override
	public Buffer slice() {
		return internal.slice();
	}

	@Override
	public Buffer slice(int start, int end) {
		return internal.slice(start, end);
	}

	@Override
	public ByteBuf getByteBuf() {
		return internal.getByteBuf();
	}

	@Override
	public Buffer appendUnsignedByte(short arg0) {
		return internal.appendUnsignedByte(arg0);
	}

	@Override
	public Buffer appendUnsignedInt(long arg0) {
		return internal.appendUnsignedInt(arg0);
	}

	@Override
	public Buffer appendUnsignedShort(int arg0) {
		return internal.appendUnsignedShort(arg0);
	}

	@Override
	public short getUnsignedByte(int arg0) {
		return internal.getUnsignedByte(arg0);
	}

	@Override
	public long getUnsignedInt(int arg0) {
		return internal.getUnsignedInt(arg0);
	}

	@Override
	public int getUnsignedShort(int arg0) {
		return internal.getUnsignedShort(arg0);
	}

	@Override
	public Buffer setUnsignedByte(int arg0, short arg1) {
		return internal.setUnsignedByte(arg0, arg1);
	}

	@Override
	public Buffer setUnsignedInt(int arg0, long arg1) {
		return internal.setUnsignedInt(arg0, arg1);
	}

	@Override
	public Buffer setUnsignedShort(int arg0, int arg1) {
		return internal.setUnsignedShort(arg0, arg1);
	}

	@Override
	public Buffer getBytes(byte[] arg0) {
		return internal.getBytes(arg0);
	}

	@Override
	public Buffer getBytes(byte[] arg0, int arg1) {
		return internal.getBytes(arg0, arg1);
	}

	@Override
	public Buffer getBytes(int arg0, int arg1, byte[] arg2) {
		return internal.getBytes(arg0, arg1, arg2);
	}

	@Override
	public Buffer getBytes(int arg0, int arg1, byte[] arg2, int arg3) {
		return internal.getBytes(arg0, arg1, arg2, arg3);
	}
}