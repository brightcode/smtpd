import java.nio.ByteBuffer;
import java.util.List;
import java.util.ArrayList;
import java.io.UnsupportedEncodingException;

/**
 * Helper functions for NIO ByteBuffers
 */
public class ByteBuffers {
    
    /**
     * Create ByteBuffer from ASCII String.
     */
    public static ByteBuffer fromString(String s) {
        byte[] data = asBytes(s);
        return ByteBuffer.wrap(data);
    }

    /**
     *
     */
    public static ByteBuffer fromStrings(List<String> strings) {
        if (strings.size() == 1) {
            return fromString(strings.get(0));
        }
        else {
            assert strings.size() > 0;
            // calculate buffer size
            int totalLength = 0;
            for (String s: strings) {
                totalLength += s.length();  // one ascii char is one byte
            }
            // copy strings to buffer
            ByteBuffer buffer = ByteBuffer.allocate(totalLength);
            int offset = 0;
            for (String s: strings) {
                byte[] data = asBytes(s);
                buffer.put(data, offset, data.length);
                offset += data.length;
            }
            buffer.flip();
            /*
            byte[] buff = new byte[totalLength];
            int offset = 0;
            for (String s: strings) {
                byte[] data = asBytes(s);
                System.arraycopy(data, 0, buff, offset, data.length);
                offset += data.length;
            }
            ByteBuffer.wrap(buff);
            */
            return buffer;
        }
    }
    
    /**
     * Read from ByteBuffer into ASCII String. The buffer's position
     * is updated, so it can only be used once.
     */
    public static String getString(ByteBuffer buffer) {
        int offset = buffer.position();
        int length = buffer.remaining();
        byte[] data = new byte[length];
        buffer.get(data, offset, length);  // also updates position
        return asString(data, 0, data.length);
    }
    
    /**
     * Read from ByteBuffer to list of lines terminated by LF. The line
     * terminator is included in the line. Any trailing data that is
     * not terminated with LF remains in the ByteBuffer. The position
     * is updated to point just after the last LF.
     */
    public static List<String> getLines(ByteBuffer buffer) {
        // get backing byte array, may throw UnsupportedOperationException
        byte[] data = buffer.array();
        int offset = buffer.arrayOffset();
        int length = buffer.remaining();
        
        List<String> lines = new ArrayList<String>();
        int j = 0;
        for (int i = offset; i < length; i++) {
            if (data[i] == '\n') {
                String s = asString(data, j, i + 1 - j);
                lines.add(s);
                j = i + 1;
            }
        }
        
        buffer.position(buffer.position() + j);
        // last character not LF?
        //if (j < length) {
        //    String s = asString(data, j, length - j);
        //    lines.add(s);
        //}
        return lines;
    }
    
    // ===
    
    public static byte[] asBytes(String s) {
        try {
            return s.getBytes("US-ASCII");
        }
        catch (UnsupportedEncodingException e) {
            // convert to unchecked exception
            throw new IllegalArgumentException(e.toString());
        }
    }
    
    
    public static String asString(byte[] data, int offset, int length) {
        try {
            return new String(data, offset, length, "US-ASCII");
        }
        catch (UnsupportedEncodingException e) {
            // convert to unchecked exception
            throw new IllegalArgumentException(e.toString());
        }
    }
    
}
