package org.exp.cc.processor.mongo.impl;

import com.google.common.collect.ImmutableMap;
import org.exp.cc.enums.AggregationOperator;
import org.exp.cc.exception.ApplicationRuntimeException;
import org.exp.cc.model.AggregationCriteria;
import org.exp.cc.processor.mongo.AggregationOperatorProcessor;
import org.exp.cc.processor.mongo.ComparisonOperatorProcessor;
import org.springframework.data.mongodb.core.aggregation.Aggregation;
import org.springframework.data.mongodb.core.aggregation.GroupOperation;
import org.springframework.data.mongodb.core.aggregation.MatchOperation;
import org.springframework.data.mongodb.core.query.Criteria;
import org.springframework.stereotype.Component;

import java.util.Map;
import java.util.function.BiFunction;

@Component
public class MongoAggregationOperatorProcessor implements AggregationOperatorProcessor {
    private final Map<String, BiFunction<GroupOperation, String, GroupOperation>> aggregatorOperatorProcessor =
            ImmutableMap.<String, BiFunction<GroupOperation, String, GroupOperation>>builder()
                    .put(AggregationOperator.SUM.getOperator(), this::sumAggregationProcessor)
                    .build();

    private final ComparisonOperatorProcessor comparisonOperatorProcessor;

    public MongoAggregationOperatorProcessor(final ComparisonOperatorProcessor comparisonOperatorProcessor) {
        this.comparisonOperatorProcessor = comparisonOperatorProcessor;
    }

    @Override
    public Aggregation generateAggregation(final AggregationCriteria aggregationCriteria) {
        final Criteria criteria = new Criteria();
        final Criteria[] matchCriteria = this.comparisonOperatorProcessor.generateCriteria(aggregationCriteria.getQueryFields());
        final MatchOperation matchOperation = Aggregation.match(criteria.andOperator(matchCriteria));

        final GroupOperation groupOperation = Aggregation.group(aggregationCriteria.getFieldsToGroup().toArray(new String[0]));

        aggregationCriteria.getFieldsToAggregate()
                .forEach((fieldName, value) -> {
                    final String operator = value.getOperator();

                    if (this.aggregatorOperatorProcessor.get(operator) == null) {
                        throw new ApplicationRuntimeException(String.format("Aggregation operator processor not found for %s", operator));
                    }

                    this.aggregatorOperatorProcessor.get(operator).apply(groupOperation, fieldName);
                });

        return Aggregation.newAggregation(matchOperation, groupOperation);
    }

    private GroupOperation sumAggregationProcessor(final GroupOperation groupOperation, final String fieldName) {
        return groupOperation.sum(fieldName).as(fieldName + "_sum");
    }
}
