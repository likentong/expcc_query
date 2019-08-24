package org.exp.cc;

import java.util.Map;

/**
 * Message queue.
 */
public interface MessageQueue {
    void sendMessage(Map<String, Object> message);
}
