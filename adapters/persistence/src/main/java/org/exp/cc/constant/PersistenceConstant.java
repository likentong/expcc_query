package org.exp.cc.constant;

/**
 * Persistence constant
 */
public class PersistenceConstant {
    private PersistenceConstant() {
    }

    public static final class MongoDB {
        public static final String MONGO_OBJECT_ID = "_id";

        private MongoDB() {
        }
    }

    public static final class Entity {
        public static final String DEMOGRAPHIC = "demographic";
        public static final String PERSON = "person";

        private Entity() {
        }
    }

    public static final class Demographic {
        public static final String ID = "ID";

        private Demographic() {
        }
    }

    public static final class Person {
        public static final String ID_PERSON = "idPerson";

        private Person() {
        }
    }
}
