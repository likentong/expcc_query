package org.exp.cc.enums;

/**
 * Aggregation operator enum.
 */
public enum AggregationOperator {
    SUM("$sum");

    private final String operator;

    AggregationOperator(final String operator) {
        this.operator = operator;
    }

    public String getOperator() {
        return operator;
    }
}
