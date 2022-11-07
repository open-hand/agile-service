package io.choerodon.agile.api.vo.search;

import java.util.List;

import io.swagger.annotations.ApiModelProperty;

import org.hzero.core.util.Pair;

/**
 * @author superlee
 * @since 2022-11-02
 */
public class Condition {
    @ApiModelProperty("字段")
    private Field field;
    @ApiModelProperty("条件之间关系")
    private String relationship;
    @ApiModelProperty("操作符")
    private String operation;
    @ApiModelProperty("条件值")
    private Value value;
    @ApiModelProperty("区间条件值")
    private Pair<Value, Value> betweenValues;
    @ApiModelProperty("顺序")
    private Integer order;
    @ApiModelProperty("子条件")
    private List<Condition> subConditions;

    public Field getField() {
        return field;
    }

    public void setField(Field field) {
        this.field = field;
    }

    public String getRelationship() {
        return relationship;
    }

    public void setRelationship(String relationship) {
        this.relationship = relationship;
    }

    public String getOperation() {
        return operation;
    }

    public void setOperation(String operation) {
        this.operation = operation;
    }

    public Value getValue() {
        return value;
    }

    public void setValue(Value value) {
        this.value = value;
    }

    public Pair<Value, Value> getBetweenValues() {
        return betweenValues;
    }

    public void setBetweenValues(Pair<Value, Value> betweenValues) {
        this.betweenValues = betweenValues;
    }

    public Integer getOrder() {
        return order;
    }

    public void setOrder(Integer order) {
        this.order = order;
    }

    public List<Condition> getSubConditions() {
        return subConditions;
    }

    public void setSubConditions(List<Condition> subConditions) {
        this.subConditions = subConditions;
    }
}
