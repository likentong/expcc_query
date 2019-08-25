package org.exp.cc.model;

import org.exp.cc.enums.AggregationOperator;
import org.exp.cc.model.persistence.QueryFields;

import java.util.List;
import java.util.Map;
import java.util.Objects;

/**
 * Aggregation criteria.
 */
public class AggregationCriteria {
    private final QueryFields queryFields;
    private final List<String> fieldsToGroup;
    private final Map<String, AggregationOperator> fieldsToAggregate;
    private final String groupFieldName;

    public AggregationCriteria(final QueryFields queryFields,
                               final List<String> fieldsToGroup,
                               final Map<String, AggregationOperator> fieldsToAggregate,
                               final String groupFieldName) {
        this.queryFields = queryFields;
        this.fieldsToGroup = fieldsToGroup;
        this.fieldsToAggregate = fieldsToAggregate;
        this.groupFieldName = groupFieldName;
    }

    public QueryFields getQueryFields() {
        return queryFields;
    }

    public List<String> getFieldsToGroup() {
        return fieldsToGroup;
    }

    public Map<String, AggregationOperator> getFieldsToAggregate() {
        return fieldsToAggregate;
    }

    public String getGroupFieldName() {
        return groupFieldName;
    }

    @Override
    public boolean equals(final Object o) {
        if (this == o) {
            return true;
        }
        if (o == null || getClass() != o.getClass()) {
            return false;
        }
        final AggregationCriteria that = (AggregationCriteria) o;
        return Objects.equals(queryFields, that.queryFields) &&
                Objects.equals(fieldsToGroup, that.fieldsToGroup) &&
                Objects.equals(fieldsToAggregate, that.fieldsToAggregate) &&
                Objects.equals(groupFieldName, that.groupFieldName);
    }

    @Override
    public int hashCode() {
        return Objects.hash(queryFields, fieldsToGroup, fieldsToAggregate, groupFieldName);
    }
}
