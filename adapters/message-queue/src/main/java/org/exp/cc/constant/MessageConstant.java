package org.exp.cc.constant;

/**
 * Message Constant.
 */
public class MessageConstant {
    private MessageConstant() {
    }

    public static final String PERSON_QUEUE_BEAN_NAME = "personQueue";

    public static final class Ampq {
        public static final String DEAD_LETTER_EXCHANGER = "x-dead-letter-exchange";
        public static final String DEAD_LETTER_ROUTING_KEY = "x-dead-letter-routing-key";

        private Ampq() {
        }
    }

}
