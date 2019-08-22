package org.exp.cc.enums;

/**
 * Logical operator enum.
 */
public enum LogicalOperator {
    AND("$and"), OR("$or");

    private final String operator;

    LogicalOperator(final String operator) {
        this.operator = operator;
    }

    public String getOperator() {
        return operator;
    }
}
