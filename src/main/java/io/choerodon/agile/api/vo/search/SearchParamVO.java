package io.choerodon.agile.api.vo.search;

import java.util.*;

import com.fasterxml.jackson.annotation.JsonInclude;
import io.swagger.annotations.ApiModelProperty;
import org.apache.commons.lang3.ObjectUtils;
import org.apache.commons.lang3.SerializationUtils;

import io.choerodon.agile.api.vo.ObjectSchemeFieldVO;
import io.choerodon.agile.infra.enums.FieldCode;
import io.choerodon.agile.infra.enums.search.SearchConstant;
import io.choerodon.agile.infra.utils.AssertUtilsForCommonException;

import org.hzero.starter.keyencrypt.core.Encrypt;

/**
 * @author superlee
 * @since 2022-11-02
 */
@JsonInclude(JsonInclude.Include.NON_NULL)
public class SearchParamVO {
    @ApiModelProperty("筛选条件")
    private List<Condition> conditions;
    @ApiModelProperty("高级筛选条件")
    private List<Condition> advancedConditions;
    @ApiModelProperty("树形视图")
    private Boolean treeFlag;
    @ApiModelProperty("是否统计子问题的数量")
    private Boolean countSubIssue;
    @Encrypt
    private List<Long> quickFilterIds;
    @Encrypt
    private Set<Long> issueIds;
    @ApiModelProperty("瀑布项目使用，是否包含空冲刺")
    private Boolean containsEmptySprint;
    @ApiModelProperty("展示的字段")
    private List<ObjectSchemeFieldVO> displayFields;
    @ApiModelProperty("空条件字段，用于个人筛选保存和展示")
    private EmptyCondition emptyCondition;
    @ApiModelProperty("甘特图使用，纬度")
    private String dimension;
    @ApiModelProperty("导出issue使用字段")
    private List<String> exportFieldCodes;
    @ApiModelProperty("查父级的时候同时带出所有子级，甄云用")
    private Boolean withSubIssues;

    public Boolean getWithSubIssues() {
        return withSubIssues;
    }

    public void setWithSubIssues(Boolean withSubIssues) {
        this.withSubIssues = withSubIssues;
    }

    public List<String> getExportFieldCodes() {
        return exportFieldCodes;
    }

    public void setExportFieldCodes(List<String> exportFieldCodes) {
        this.exportFieldCodes = exportFieldCodes;
    }

    public String getDimension() {
        return dimension;
    }

    public void setDimension(String dimension) {
        this.dimension = dimension;
    }

    public EmptyCondition getEmptyCondition() {
        return emptyCondition;
    }

    public void setEmptyCondition(EmptyCondition emptyCondition) {
        this.emptyCondition = emptyCondition;
    }

    public List<ObjectSchemeFieldVO> getDisplayFields() {
        return displayFields;
    }

    public void setDisplayFields(List<ObjectSchemeFieldVO> displayFields) {
        this.displayFields = displayFields;
    }

    public Boolean getContainsEmptySprint() {
        return containsEmptySprint;
    }

    public void setContainsEmptySprint(Boolean containsEmptySprint) {
        this.containsEmptySprint = containsEmptySprint;
    }

    public Set<Long> getIssueIds() {
        return issueIds;
    }

    public void setIssueIds(Set<Long> issueIds) {
        this.issueIds = issueIds;
    }

    public Boolean getCountSubIssue() {
        return countSubIssue;
    }

    public void setCountSubIssue(Boolean countSubIssue) {
        this.countSubIssue = countSubIssue;
    }

    public List<Long> getQuickFilterIds() {
        return quickFilterIds;
    }

    public void setQuickFilterIds(List<Long> quickFilterIds) {
        this.quickFilterIds = quickFilterIds;
    }

    public Boolean getTreeFlag() {
        return treeFlag;
    }

    public void setTreeFlag(Boolean treeFlag) {
        this.treeFlag = treeFlag;
    }

    public List<Condition> getConditions() {
        return conditions;
    }

    public void setConditions(List<Condition> conditions) {
        this.conditions = conditions;
    }

    public List<Condition> getAdvancedConditions() {
        return advancedConditions;
    }

    public void setAdvancedConditions(List<Condition> advancedConditions) {
        this.advancedConditions = advancedConditions;
    }

    public boolean sprintEmpty() {
        List<Condition> conditions = queryAllConditions();
        return isSprintEmptyByConditions(conditions);
    }

    private boolean isSprintEmptyByConditions(List<Condition> conditions) {
        boolean isSprintEmpty = true;
        if (ObjectUtils.isEmpty(conditions)) {
            return true;
        }
        for (Condition condition : conditions) {
            Field field = condition.getField();
            if (field == null) {
                continue;
            }
            if (FieldCode.SPRINT.equals(field.getFieldCode())) {
                String operation = condition.getOperation();
                //sprint is null or sprint in
                if (SearchConstant.Operation.isNull(operation)) {
                    isSprintEmpty = false;
                    break;
                } else if (SearchConstant.Operation.isBracket(operation)) {
                    isSprintEmpty = isSprintEmptyByConditions(condition.getSubConditions());
                } else if (SearchConstant.Operation.isIn(operation)) {
                    Value value = condition.getValue();
                    if (value != null) {
                        isSprintEmpty = ObjectUtils.isEmpty(value.getValueIdList());
                        break;
                    }
                }
            }
        }
        return isSprintEmpty;
    }

    public void validateAndSetDisplayFields() {
        List<ObjectSchemeFieldVO> result = new ArrayList<>();
        if (!ObjectUtils.isEmpty(getDisplayFields())) {
            getDisplayFields().forEach(field -> {
                        AssertUtilsForCommonException.notNull(field.getCode(), "error.display.field.code.null");
                        result.add(field);
                    }
            );
        }
        setDisplayFields(result);
    }


    public void addCondition(Condition condition) {
        if (condition == null) {
            return;
        }
        List<Condition> conditions = Optional.ofNullable(getConditions()).orElse(new ArrayList<>());
        setConditions(conditions);
        conditions.add(condition);
    }

    public void overrideCondition(Condition condition) {
        if (condition == null) {
            return;
        }
        String targetFieldCode = Optional.ofNullable(condition.getField()).map(Field::getFieldCode).orElse(null);
        if (targetFieldCode == null) {
            return;
        }
        overrideCondition(condition, targetFieldCode, getConditions());
        overrideCondition(condition, targetFieldCode, getAdvancedConditions());
    }

    private void overrideCondition(Condition condition,
                                   String targetFieldCode,
                                   List<Condition> conditions) {
        List<Condition> newConditions = new ArrayList<>();
        if (conditions != null) {
            for (Condition c : conditions) {
                String fieldCode = Optional.ofNullable(c.getField()).map(Field::getFieldCode).orElse(null);
                if (targetFieldCode.equals(fieldCode)) {
                    newConditions.add(condition);
                } else {
                    newConditions.add(c);
                }
            }
        }
        setConditions(newConditions);
    }

    public void addGanttTypeCodes(List<String> typeCodes) {
        addCondition(
                new Condition()
                        .setField(new Field().setFieldCode(SearchConstant.Field.TYPE_CODE).setPredefined(true))
                        .setRelationship(SearchConstant.Relationship.AND.toString())
                        .setOperation(SearchConstant.Operation.IN.toString())
                        .setValue(new Value().setValueStrList(typeCodes)));
    }

    public List<Condition> queryAllConditions() {
        List<Condition> conditions = Optional.ofNullable(getConditions()).orElse(new ArrayList<>());
        conditions.addAll(Optional.ofNullable(getAdvancedConditions()).orElse(new ArrayList<>()));
        return conditions;
    }

    public List<Long> queryOptionIds(String fieldCode) {
        List<Condition> conditions = queryAllConditions();
        return queryOptionIdsByConditions(fieldCode, conditions);
    }

    public void addConditionByFieldCode(String fieldCode,
                                        List<Long> instanceIds) {
        List<Condition> conditions = Optional.ofNullable(getConditions()).orElse(new ArrayList<>());
        setConditions(conditions);
        List<Condition> advancedConditions = Optional.ofNullable(getAdvancedConditions()).orElse(new ArrayList<>());
        setAdvancedConditions(advancedConditions);
        Condition condition = Condition.filterByFieldCode(fieldCode, conditions);
        if (condition == null) {
            condition = Condition.filterByFieldCode(fieldCode, advancedConditions);
        }
        if (condition == null) {
            condition = buildInCondition(fieldCode, instanceIds);
            addCondition(condition);
        } else {
            String opt = condition.getOperation();
            String deepCopyRelationship = SearchConstant.Relationship.AND.toString();
            //深拷贝
            Condition deepCopyCondition = SerializationUtils.clone(condition);
            if (SearchConstant.Operation.isNull(opt) || SearchConstant.Operation.isNotNull(opt)) {
                deepCopyRelationship = SearchConstant.Relationship.OR.toString();
            }
            condition
                    .setOperation(SearchConstant.Operation.BRACKET.toString())
                    .setSubConditions(Arrays.asList(buildInCondition(fieldCode, instanceIds),
                            deepCopyCondition.setRelationship(deepCopyRelationship)));
        }
    }

    private Condition buildInCondition(String fieldCode, List<Long> instanceIds) {
        Condition condition;
        condition = new Condition()
                .setField(new Field().setFieldCode(fieldCode).setPredefined(true))
                .setRelationship(SearchConstant.Relationship.AND.toString())
                .setOperation(SearchConstant.Operation.IN.toString())
                .setValue(new Value().setValueIdList(instanceIds));
        return condition;
    }


    private List<Long> queryOptionIdsByConditions(String fieldCode, List<Condition> conditions) {
        if (ObjectUtils.isEmpty(conditions)) {
            return new ArrayList<>();
        }
        List<Long> ids = new ArrayList<>();
        for (Condition condition : conditions) {
            Field field = condition.getField();
            String thisFieldCode = Optional.ofNullable(field).map(Field::getFieldCode).orElse(null);
            if (Objects.equals(fieldCode, thisFieldCode)) {
                Value value = condition.getValue();
                if (SearchConstant.Operation.isBracket(condition.getOperation())) {
                    ids.addAll(queryOptionIdsByConditions(fieldCode, condition.getSubConditions()));
                } else {
                    if (FieldCode.SUB_PROJECT.equals(fieldCode)) {
                        ids = Optional.ofNullable(value).map(Value::getNoEncryptIdList).orElse(new ArrayList<>());
                    } else {
                        ids = Optional.ofNullable(value).map(Value::getValueIdList).orElse(new ArrayList<>());
                    }
                }
            }
        }
        return ids;
    }

    @Override
    public String toString() {
        return "SearchParamVO{" +
                "conditions=" + conditions +
                ", advancedConditions=" + advancedConditions +
                ", treeFlag=" + treeFlag +
                ", countSubIssue=" + countSubIssue +
                ", quickFilterIds=" + quickFilterIds +
                ", issueIds=" + issueIds +
                ", containsEmptySprint=" + containsEmptySprint +
                ", displayFields=" + displayFields +
                ", emptyCondition=" + emptyCondition +
                ", dimension='" + dimension + '\'' +
                '}';
    }
}
