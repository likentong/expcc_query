package org.exp.cc.model.persistence;

import com.fasterxml.jackson.annotation.JsonCreator;

import java.util.List;
import java.util.Map;
import java.util.Objects;

/**
 * Query fields.
 */
public class QueryFields {
    private final List<Map<String, QueryOperator>> fields;

    @JsonCreator
    public QueryFields(final List<Map<String, QueryOperator>> fields) {
        this.fields = fields;
    }

    public List<Map<String, QueryOperator>> getFields() {
        return fields;
    }

    @Override
    public boolean equals(final Object o) {
        if (this == o) {
            return true;
        }
        if (o == null || getClass() != o.getClass()) {
            return false;
        }
        final QueryFields that = (QueryFields) o;
        return Objects.equals(fields, that.fields);
    }

    @Override
    public int hashCode() {
        return Objects.hash(fields);
    }

}
